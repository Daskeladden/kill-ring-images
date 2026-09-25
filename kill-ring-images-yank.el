;;; kill-ring-images-yank.el --- The yank of images -*- lexical-binding: t -*-

;; Copyright (C) 2026 kill-ring-images contributors

;; Author: Daskeladden
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

;; A yank of an image entry does what fits the buffer: the buffer's own
;; yank-media handler, a markdown link to a written file, or the image
;; itself inline.  ghostel and vterm get the path of the image file.

;;; Code:

(require 'cl-lib)
(require 'kill-ring-images-entry)

(defvar yank-media--registered-handlers)

(declare-function org-link-preview-region "ol"
                  (&optional include-linked refresh beg end))
(declare-function org-display-inline-images "org"
                  (&optional include-linked refresh beg end))
(declare-function markdown-display-inline-images "markdown-mode" ())

(defcustom kill-ring-images-file-directory temporary-file-directory
  "The directory of image files for terminals and unsaved markdown."
  :type 'directory
  :group 'kill-ring-images)

(defcustom kill-ring-images-markdown-directory "images"
  "The directory of image files, next to a markdown file."
  :type 'string
  :group 'kill-ring-images)

(defcustom kill-ring-images-yank-preview t
  "Whether a yank shows the image link that it inserts as a picture.
Org and markdown-mode insert an image as a link, and show links as
pictures only when asked.  With this on, a yank asks right after it."
  :type 'boolean
  :group 'kill-ring-images)

(defconst kill-ring-images-yank--dropped-message
  "kill-ring-images: this image is no longer stored"
  "The message for a yank of an image that the store dropped.")

;;;; Files

(defun kill-ring-images-yank-write-file (id type bytes &optional directory)
  "Write BYTES of the MIME TYPE for the image ID and return the file name.
The file goes in DIRECTORY, or in `kill-ring-images-file-directory' when
DIRECTORY is nil.  An existing file with that name is used again."
  (let ((file (expand-file-name
               (format "image-%s.%s" (substring id 0 12)
                       (kill-ring-images-type-extension type))
               (or directory kill-ring-images-file-directory))))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (let ((coding-system-for-write 'no-conversion))
        (write-region bytes nil file nil 'silent)))
    file))

;;;; A normal buffer

(defun kill-ring-images-yank--handles-p (handled type)
  "Non-nil when HANDLED, a MIME type symbol or a regexp, matches TYPE."
  (if (symbolp handled)
      (eq handled type)
    (string-match-p handled (symbol-name type))))

;; Fragile: `yank-media--registered-handlers' is internal to yank-media,
;; which has no public way to hand data to a buffer's handler, since
;; `yank-media' reads only the live clipboard.  If a later Emacs renames
;; it, `boundp' makes every image fall back to the inline display, and
;; kill-ring-images-yank-gives-the-image-to-a-yank-media-handler fails.
(defun kill-ring-images-yank--media-handler (type)
  "The `yank-media' handler of this buffer for the MIME TYPE, or nil."
  (cl-loop for (handled . handler)
           in (and (boundp 'yank-media--registered-handlers)
                   yank-media--registered-handlers)
           when (kill-ring-images-yank--handles-p handled type)
           return handler))

(defun kill-ring-images-yank--markdown (id type bytes)
  "Write the image ID of TYPE with BYTES and insert a markdown link."
  (let* ((dir (and buffer-file-name (file-name-directory buffer-file-name)))
         (file (kill-ring-images-yank-write-file
                id type bytes
                (if dir
                    (expand-file-name kill-ring-images-markdown-directory dir)
                  kill-ring-images-file-directory))))
    (insert (format "![](%s)" (if dir (file-relative-name file dir) file)))))

(defun kill-ring-images-yank--inline (entry type bytes)
  "Insert the text of ENTRY showing the image of TYPE with BYTES."
  (insert (propertize (substring-no-properties entry)
                      'display
                      (create-image bytes
                                    (kill-ring-images-type-image-type type) t
                                    :max-width (window-body-width nil t)))))

(defun kill-ring-images-yank--image (entry type bytes)
  "Insert the image of ENTRY, of TYPE with BYTES, as fits the buffer."
  (let ((handler (kill-ring-images-yank--media-handler type)))
    (cond (handler
           (kill-ring-images-yank--previewing
            (lambda () (funcall handler type bytes))))
          ((derived-mode-p 'markdown-mode)
           (kill-ring-images-yank--previewing
            (lambda ()
              (kill-ring-images-yank--markdown
               (kill-ring-images-entry-id entry) type bytes))))
          (t (kill-ring-images-yank--inline entry type bytes)))))

;;;; The preview of an inserted link

(defun kill-ring-images-yank--preview-org (beg end)
  "Show the image links between BEG and END in this org buffer.
The rule is Org's own, as for \\[org-link-preview]: a link with a
description is left as it is.  Org 9.8 calls this a link preview.
Older Orgs call it inline images."
  (cond ((fboundp 'org-link-preview-region)
         (org-link-preview-region nil nil beg end))
        ((fboundp 'org-display-inline-images)
         (with-no-warnings (org-display-inline-images nil nil beg end)))))

(defun kill-ring-images-yank--preview (beg end)
  "Show the image link between BEG and END as a picture, where the mode can."
  (cond ((derived-mode-p 'org-mode)
         (kill-ring-images-yank--preview-org beg end))
        ((and (derived-mode-p 'markdown-mode)
              (fboundp 'markdown-display-inline-images))
         (markdown-display-inline-images))))

(defun kill-ring-images-yank--previewing (insert)
  "Call INSERT, which inserts an image link at point, and show the link.
The start is a marker, because Org can add a drawer above the link as
it attaches the image.  The image is in the buffer already, so a
preview that fails only says so."
  (let ((beg (copy-marker (point))))
    (funcall insert)
    (when kill-ring-images-yank-preview
      (condition-case err
          (kill-ring-images-yank--preview (marker-position beg) (point))
        (error (message "kill-ring-images: no preview: %s"
                        (error-message-string err)))))
    (set-marker beg nil)))

(defun kill-ring-images-yank--dropped (entry)
  "Insert the text of ENTRY and say that its image is gone."
  (insert (substring-no-properties entry))
  (message "%s" kill-ring-images-yank--dropped-message))

(defun kill-ring-images-yank-entry (entry)
  "Insert the image of the kill ring ENTRY.
This is the function of the entry's `yank-handler'."
  (let ((image (kill-ring-images-entry-image entry)))
    (if image
        (kill-ring-images-yank--image entry (car image) (cdr image))
      (kill-ring-images-yank--dropped entry))))

;;;; Terminals

(defvar kill-ring-images-yank-terminal-functions
  '(ghostel--paste-text vterm-insert)
  "The functions that send yanked text to a terminal.
kill-ring-images advises each of them to send the path of an image file.")

(defvar kill-ring-images-yank-terminal-paste-functions nil
  "Functions run after an image entry went to a terminal as a file path.
Each is called with the buffer and the image id.")

(defun kill-ring-images-yank--terminal-arg (arg)
  "ARG, or the path of its image file when ARG is an entry."
  (let* ((id (kill-ring-images-entry-id arg))
         (image (and id (kill-ring-images-store-get id))))
    (cond ((not id) arg)
          (image (prog1 (kill-ring-images-yank-write-file id (car image)
                                                          (cdr image))
                   (run-hook-with-args
                    'kill-ring-images-yank-terminal-paste-functions
                                       (current-buffer) id)))
          (t (message "%s" kill-ring-images-yank--dropped-message)
             (substring-no-properties arg)))))

(defun kill-ring-images-yank--terminal-args (args)
  "ARGS with each entry changed to the path of its image file."
  (mapcar #'kill-ring-images-yank--terminal-arg args))

(defun kill-ring-images-yank-install ()
  "Advise the terminal functions to send the paths of image files."
  (dolist (function kill-ring-images-yank-terminal-functions)
    (advice-add function :filter-args #'kill-ring-images-yank--terminal-args)))

(defun kill-ring-images-yank-uninstall ()
  "Remove the advice from the terminal functions."
  (dolist (function kill-ring-images-yank-terminal-functions)
    (advice-remove function #'kill-ring-images-yank--terminal-args)))

(provide 'kill-ring-images-yank)
;;; kill-ring-images-yank.el ends here
