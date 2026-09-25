;;; kill-ring-images.el --- Images in the kill ring -*- lexical-binding: t -*-

;; Copyright (C) 2026 kill-ring-images contributors

;; Author: Daskeladden
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, multimedia
;; URL: https://github.com/Daskeladden/kill-ring-images

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; kill-ring-images puts images in the kill ring.  With
;; `kill-ring-images-mode' on, an image that another program copies
;; joins the kill ring at the next yank.
;; `kill-ring-images-copy-image-at-point' copies an image from Emacs,
;; and on X other programs can paste it.  A yank of an image does what
;; fits the buffer.

;;; Code:

(require 'image)
(require 'kill-ring-images-store)
(require 'kill-ring-images-entry)
(require 'kill-ring-images-select)
(require 'kill-ring-images-yank)

;;;; The mode

;;;###autoload
(define-minor-mode kill-ring-images-mode
  "Put clipboard images in the kill ring, and offer copied images.
With the mode on, an image that another program copies joins the kill
ring at the next yank.  On X, other programs can paste an image that
Emacs copied."
  :global t
  :group 'kill-ring-images
  (if kill-ring-images-mode
      (progn
        (kill-ring-images-select-install)
        (kill-ring-images-yank-install)
        (advice-add 'kill-new :filter-args #'kill-ring-images--cap-kill)
        (kill-ring-images--cap-kill-ring))
    (kill-ring-images-select-uninstall)
    (kill-ring-images-yank-uninstall)
    (advice-remove 'kill-new #'kill-ring-images--cap-kill)))

;;;; The kill ring preview

(defun kill-ring-images--capped-image (spec height)
  "SPEC with its image no taller than HEIGHT pixels.
Each property is kept once.  Emacs rejects a specification that repeats
one, and an image shown in a window already carries caps of its own.
The file or the data of the image is untouched."
  (let (plist)
    (cl-loop for (key value) on (cdr spec) by #'cddr
             unless (plist-member plist key)
             do (setq plist (append plist (list key value))))
    (setq plist (plist-put plist :max-height height))
    (setq plist (plist-put plist :max-width (* 4 height)))
    (cons 'image plist)))

(defun kill-ring-images--cap-images (string)
  "STRING with every image it shows capped to `kill-ring-images-preview-lines'."
  (let ((height (* kill-ring-images-preview-lines (default-line-height)))
        (copy (copy-sequence string))
        (start 0))
    (while (< start (length copy))
      (let ((end (or (next-single-property-change start 'display copy)
                     (length copy)))
            (spec (get-text-property start 'display copy)))
        (when (eq (car-safe spec) 'image)
          (put-text-property start end 'display
                             (kill-ring-images--capped-image spec height) copy))
        (setq start end)))
    copy))

(defun kill-ring-images--refresh-entry (string)
  "STRING made afresh when it is an entry whose image the store holds.
An entry made before entries showed their image carries no picture, and
so does one read back from disk while the store was empty.  Any other
string has its images capped."
  (or (and (kill-ring-images-entry-image string)
           (kill-ring-images-entry-make (kill-ring-images-entry-id string)))
      (kill-ring-images--cap-images string)))

(defun kill-ring-images--cap-kill-ring ()
  "Cap the images the kill ring already holds, and refresh its entries.
The yank position is kept, so `yank-pop' goes on where it stood."
  (let ((offset (- (length kill-ring) (length kill-ring-yank-pointer))))
    (setq kill-ring (mapcar #'kill-ring-images--refresh-entry kill-ring)
          kill-ring-yank-pointer (nthcdr offset kill-ring))))

(defun kill-ring-images--sole-image (string)
  "The image STRING shows when it holds that image and nothing else.
Whitespace around the image does not count, so a copied line counts as
one, and so does a marked image."
  (let ((found nil)
        (start 0)
        (alone t))
    (while (and alone (< start (length string)))
      (let* ((end (or (next-single-property-change start 'display string)
                      (length string)))
             (spec (get-text-property start 'display string)))
        (cond ((eq (car-safe spec) 'image)
               (if found (setq alone nil) (setq found spec)))
              ((string-match-p "\\`[ \t\n\r]*\\'" (substring string start end)))
              (t (setq alone nil)))
        (setq start end)))
    (and alone found)))

(defun kill-ring-images--image-entry (spec)
  "An entry for the image SPEC, or nil when kill-ring-images cannot take it.
The format comes from the bytes, because the :type of a spec names the
renderer."
  (when-let* ((bytes (ignore-errors (kill-ring-images--image-bytes spec)))
              (type (kill-ring-images-type-from-image-type
                     (or (image-type-from-data bytes)
                         (plist-get (cdr spec) :type))))
              (id (kill-ring-images-store-put type bytes)))
    (kill-ring-images-entry-make id)))

(defun kill-ring-images--cap-kill (args)
  "ARGS of `kill-new'.
A copy that holds nothing but an image becomes an entry of kill-ring-images\'s
own, so that other programs can paste it and a yank puts it where the
buffer wants it.  Any other copy keeps its text, with the images it
shows capped in height."
  (if (not (stringp (car args)))
      args
    (let* ((string (car args))
           ;; An entry of kill-ring-images's own shows its image too, and
           ;; copying it again must not make a second one.
           (spec (unless (kill-ring-images-entry-id string)
                   (kill-ring-images--sole-image string)))
           (entry (and spec (kill-ring-images--image-entry spec))))
      (when entry
        ;; The command that copied has already said what it did, and a
        ;; line of an image is not a line of text.
        (message "kill-ring-images: copied %s" (substring-no-properties entry)))
      (cons (or entry (kill-ring-images--cap-images string)) (cdr args)))))

;;;; Copying

(defvar kill-ring-images-copy-to-x t
  "Whether an image that Emacs copies is cut to the X clipboard as well.
A package that carries a copy to the clipboard in another way sets it to
nil, so that the text of the entry does not race the image there.")

(defvar kill-ring-images-copy-functions nil
  "Functions that also take an image that Emacs copied.
Each takes the MIME TYPE and the BYTES, after the kill ring has the
entry, for example to send the image to the clipboard of another
machine.")

(defun kill-ring-images-copy (type bytes)
  "Put BYTES of the MIME TYPE in the kill ring as an image.
Return the entry.  `kill-ring-images-copy-functions' get the image too."
  (let* ((id (or (kill-ring-images-store-put type bytes)
                 (user-error "kill-ring-images: the image is larger than %s"
                             "`kill-ring-images-store-limit'")))
         (entry (kill-ring-images-entry-make id)))
    (let ((interprogram-cut-function (and kill-ring-images-copy-to-x
                                          interprogram-cut-function)))
      (kill-new entry))
    (run-hook-with-args 'kill-ring-images-copy-functions type bytes)
    entry))

(defun kill-ring-images--read-file (file)
  "The contents of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun kill-ring-images--image-at-point ()
  "The image spec under point, or a user error."
  (or (ignore-errors (image--get-image))
      (user-error "kill-ring-images: no image under point")))

(defun kill-ring-images--image-type (spec bytes)
  "The MIME type of the image SPEC with BYTES, or a user error.
The format comes from BYTES first, because the :type of a spec names
the renderer, which is imagemagick for an image that ImageMagick shows."
  (let ((image-type (or (image-type-from-data bytes)
                        (plist-get (cdr spec) :type))))
    (or (kill-ring-images-type-from-image-type image-type)
        (user-error "kill-ring-images: cannot copy an image of type %s"
                    image-type))))

(defun kill-ring-images--image-bytes (spec)
  "The bytes of the image SPEC, from its :data or from its :file."
  (let ((data (plist-get (cdr spec) :data))
        (file (plist-get (cdr spec) :file)))
    (cond (data (if (multibyte-string-p data)
                    (encode-coding-string data 'no-conversion)
                  data))
          ((and file (file-readable-p file)) (kill-ring-images--read-file file))
          (t (user-error
              "kill-ring-images: the image has no data and no file")))))

;;;###autoload
(defun kill-ring-images-copy-image-at-point ()
  "Copy the image under point to the kill ring and the clipboard."
  (interactive)
  (let* ((spec (kill-ring-images--image-at-point))
         (bytes (kill-ring-images--image-bytes spec)))
    (kill-ring-images-copy (kill-ring-images--image-type spec bytes) bytes)
    (message "kill-ring-images: copied the image")))

(provide 'kill-ring-images)
;;; kill-ring-images.el ends here
