;;; kill-ring-images-select.el --- The clipboard side -*- lexical-binding: t -*-

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

;; Converters in `selection-converter-alist' offer the image of an entry
;; to other programs.  A wrapper around `interprogram-paste-function'
;; turns an image that another program copied into an entry.

;;; Code:

(require 'cl-lib)
(require 'kill-ring-images-entry)

;;;; Offering an image

(defun kill-ring-images--offers-p (_selection type value)
  "Non-nil when VALUE is an entry whose stored image has the MIME TYPE."
  (eq (car (kill-ring-images-entry-image value)) type))

(defun kill-ring-images--convert (selection type value)
  "The image of VALUE as (TYPE . BYTES), or nil to decline.
SELECTION is the selection that another program asked for."
  (when (kill-ring-images--offers-p selection type value)
    (cons type (cdr (kill-ring-images-entry-image value)))))

(defun kill-ring-images-select--converters ()
  "The elements for `selection-converter-alist', one for each type."
  (mapcar (lambda (type)
            (cons type (cons #'kill-ring-images--offers-p
                             #'kill-ring-images--convert)))
          kill-ring-images-types))

(defun kill-ring-images-select--converter-p (element)
  "Non-nil when ELEMENT of `selection-converter-alist' is kill-ring-images's."
  (equal (cdr-safe element)
         (cons #'kill-ring-images--offers-p #'kill-ring-images--convert)))

;;;; Receiving an image

(defconst kill-ring-images-select--text-types
  '(UTF8_STRING STRING TEXT COMPOUND_TEXT text/plain text/plain\;charset=utf-8)
  "The clipboard types that carry plain text.")

(defvar kill-ring-images-select--last-hash nil
  "The SHA-1 of the last image taken from the clipboard.")

(defun kill-ring-images-select--get (data-type)
  "The clipboard as DATA-TYPE, or nil when the request fails."
  (with-demoted-errors "kill-ring-images: %S"
    (gui-get-selection 'CLIPBOARD data-type)))

(defun kill-ring-images-select--targets ()
  "The types that the clipboard offers, as a list of symbols."
  (let ((targets (kill-ring-images-select--get 'TARGETS)))
    (and (vectorp targets) (append targets nil))))

(defun kill-ring-images-select--image-type (targets)
  "The first type of `kill-ring-images-types' in TARGETS.
Return nil when TARGETS has none, or when it has a plain text type."
  (unless (cl-intersection targets kill-ring-images-select--text-types)
    (cl-find-if (lambda (type) (memq type targets)) kill-ring-images-types)))

(defun kill-ring-images-select--bytes (data)
  "DATA from the clipboard as a unibyte string without properties, or nil."
  (when (and (stringp data) (> (length data) 0))
    (let ((bytes (substring-no-properties data)))
      (if (multibyte-string-p bytes)
          (encode-coding-string bytes 'no-conversion)
        bytes))))

(defvar kill-ring-images-image-functions nil
  "Functions that give an image which the X clipboard does not offer.
Each takes no argument and gives (TYPE . BYTES) or nil.  A yank asks
them in order when the X clipboard offers no image of another program,
whether or not Emacs owns the X clipboard, so each gives only an image
newer than anything Emacs copied, for example an image from the
clipboard of another machine.")

(defun kill-ring-images-select--x-image ()
  "The image that the X clipboard offers alone, as (TYPE . BYTES), or nil."
  (let* ((type (kill-ring-images-select--image-type
                (kill-ring-images-select--targets)))
         (bytes (and type (kill-ring-images-select--bytes
                           (kill-ring-images-select--get type)))))
    (and bytes (cons type bytes))))

(defun kill-ring-images-select--clipboard-image ()
  "The image that another program offers alone, as (TYPE . BYTES), or nil.
The X clipboard comes first, unless Emacs owns it, since then it holds
Emacs's own copy.  `kill-ring-images-image-functions' come next even so:
an X server that shows Emacs on another machine can leave the X
clipboard to Emacs when a program there copies an image, and each
function knows by itself whether its image is newer than Emacs's copy."
  (or (unless (gui-backend-selection-owner-p 'CLIPBOARD)
        (kill-ring-images-select--x-image))
      (run-hook-with-args-until-success 'kill-ring-images-image-functions)))

(defun kill-ring-images-select--receive (type bytes orig)
  "The entry for BYTES of TYPE from the clipboard.
Return nil when the last image received had the same bytes, and the
value of ORIG when the store refuses the image."
  (let ((hash (secure-hash 'sha1 bytes)))
    (cond ((equal hash kill-ring-images-select--last-hash) nil)
          ((kill-ring-images-store-put type bytes)
           (setq kill-ring-images-select--last-hash hash)
           (kill-ring-images-entry-make hash))
          (t (message "kill-ring-images: the clipboard image is larger than %s"
                      "`kill-ring-images-store-limit'")
             (and orig (funcall orig))))))

(defun kill-ring-images--paste (orig)
  "Take an image from the clipboard, or leave the yank to ORIG.
This is the wrapper around `interprogram-paste-function'."
  (pcase (kill-ring-images-select--clipboard-image)
    (`(,type . ,bytes) (kill-ring-images-select--receive type bytes orig))
    (_ (and orig (funcall orig)))))

;;;; Installing

(defun kill-ring-images-select-remove-converters ()
  "Stop offering images to other programs on X."
  (setq selection-converter-alist
        (cl-remove-if #'kill-ring-images-select--converter-p
                      selection-converter-alist)))

(defun kill-ring-images-select-add-converters ()
  "Offer images to other programs on X, once."
  (kill-ring-images-select-remove-converters)
  (setq selection-converter-alist
        (append (kill-ring-images-select--converters)
                selection-converter-alist)))

(defun kill-ring-images-select-uninstall ()
  "Remove the converters and the paste wrapper."
  (kill-ring-images-select-remove-converters)
  (remove-function interprogram-paste-function #'kill-ring-images--paste))

(defun kill-ring-images-select-install ()
  "Add the converters and the paste wrapper, once."
  (kill-ring-images-select-uninstall)
  (kill-ring-images-select-add-converters)
  (add-function :around interprogram-paste-function #'kill-ring-images--paste))

(provide 'kill-ring-images-select)
;;; kill-ring-images-select.el ends here
