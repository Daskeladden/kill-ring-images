;;; kill-ring-images-store.el --- The image store -*- lexical-binding: t -*-

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

;; The store keeps the bytes of the images that the kill ring names.  A
;; kill ring entry holds only an id, so the kill ring stays small.  The
;; oldest images go when the total size passes `kill-ring-images-store-limit'.

;;; Code:

(require 'cl-lib)

(defgroup kill-ring-images nil
  "Images in the kill ring."
  :group 'killing
  :prefix "kill-ring-images-")

(defconst kill-ring-images-type-table
  '((image/png png "png")
    (image/jpeg jpeg "jpg")
    (image/gif gif "gif")
    (image/svg+xml svg "svg"))
  "The known image types.
Each element is the MIME type, the Emacs image type and the extension.")

(defcustom kill-ring-images-types
  '(image/png image/jpeg image/gif image/svg+xml)
  "The image types taken from and offered to the clipboard.
The first type is the one preferred."
  :type '(repeat (choice (const image/png) (const image/jpeg)
                         (const image/gif) (const image/svg+xml))))

(defcustom kill-ring-images-preview-lines 5
  "How many text lines tall an image in the kill ring is shown.
A kill ring browser draws the picture an entry carries.  An image copied
from `image-mode' is the size of the file, which fills the screen."
  :type 'natnum
  :group 'kill-ring-images)

(defcustom kill-ring-images-store-limit (* 64 1024 1024)
  "The total size in bytes of the stored images."
  :type 'natnum)

(defvar kill-ring-images-store--images nil
  "The stored images, oldest first, as a list of (ID TYPE . BYTES).")

;;;; Types

(defun kill-ring-images--type-row (type)
  "The row of `kill-ring-images-type-table' for the MIME TYPE."
  (or (assq type kill-ring-images-type-table)
      (error "kill-ring-images: unknown image type %S" type)))

(defun kill-ring-images-type-image-type (type)
  "The Emacs image type of the MIME TYPE."
  (nth 1 (kill-ring-images--type-row type)))

(defun kill-ring-images-type-extension (type)
  "The file extension of the MIME TYPE."
  (nth 2 (kill-ring-images--type-row type)))

(defun kill-ring-images-type-from-image-type (image-type)
  "The MIME type in `kill-ring-images-types' of the Emacs IMAGE-TYPE, or nil."
  (cl-find-if (lambda (type)
                (eq (nth 1 (assq type kill-ring-images-type-table)) image-type))
              kill-ring-images-types))

;;;; The store

(defun kill-ring-images-store--check (type bytes)
  "Signal an error unless TYPE is known and BYTES is a unibyte string.
TYPE is known when it is in `kill-ring-images-types'."
  (unless (memq type kill-ring-images-types)
    (error "kill-ring-images: %S is not in `kill-ring-images-types'" type))
  (unless (and (stringp bytes) (not (multibyte-string-p bytes)))
    (error "kill-ring-images: image bytes must be a unibyte string")))

(defun kill-ring-images-store--total ()
  "The total size of the stored images."
  (cl-loop for (_id _type . bytes) in kill-ring-images-store--images
           sum (length bytes)))

(defun kill-ring-images-store--trim ()
  "Drop the oldest images until the total fits `kill-ring-images-store-limit'."
  (while (> (kill-ring-images-store--total) kill-ring-images-store-limit)
    (pop kill-ring-images-store--images)))

(defun kill-ring-images-store-put (type bytes)
  "Store BYTES as TYPE, a MIME type symbol such as `image/png'.
Return the id, or nil when BYTES are larger than
`kill-ring-images-store-limit'."
  (kill-ring-images-store--check type bytes)
  (when (<= (length bytes) kill-ring-images-store-limit)
    (let ((id (secure-hash 'sha1 bytes)))
      (setq kill-ring-images-store--images
            (append (cl-remove id kill-ring-images-store--images
                               :key #'car :test #'equal)
                    (list (cons id (cons type bytes)))))
      (kill-ring-images-store--trim)
      id)))

(defun kill-ring-images-store-get (id)
  "The stored image ID as (TYPE . BYTES), or nil when the store dropped it."
  (cdr (assoc id kill-ring-images-store--images)))

(provide 'kill-ring-images-store)
;;; kill-ring-images-store.el ends here
