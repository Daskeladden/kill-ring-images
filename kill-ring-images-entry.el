;;; kill-ring-images-entry.el --- Kill ring entries -*- lexical-binding: t -*-

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

;; A kill ring entry is a short text such as [image 240x120 png 3f9a1c].
;; It carries the id of a stored image and a yank handler.  The text is
;; different for each image, because Emacs compares clipboard text with
;; `equal', which ignores text properties.

;;; Code:

(require 'image)
(require 'kill-ring-images-store)

;;;; The size in the header

(defun kill-ring-images-entry--u16be (bytes i)
  "The big-endian 16-bit number at I in BYTES."
  (logior (ash (aref bytes i) 8) (aref bytes (1+ i))))

(defun kill-ring-images-entry--u16le (bytes i)
  "The little-endian 16-bit number at I in BYTES."
  (logior (aref bytes i) (ash (aref bytes (1+ i)) 8)))

(defun kill-ring-images-entry--u32be (bytes i)
  "The big-endian 32-bit number at I in BYTES."
  (logior (ash (kill-ring-images-entry--u16be bytes i) 16)
          (kill-ring-images-entry--u16be bytes (+ i 2))))

(defun kill-ring-images-entry--png-size (bytes)
  "The (WIDTH . HEIGHT) in the PNG header of BYTES, or nil."
  (when (and (>= (length bytes) 24)
             (string-prefix-p "\x89PNG\r\n\x1a\n" bytes))
    (cons (kill-ring-images-entry--u32be bytes 16)
          (kill-ring-images-entry--u32be bytes 20))))

(defun kill-ring-images-entry--gif-size (bytes)
  "The (WIDTH . HEIGHT) in the GIF header of BYTES, or nil."
  (when (and (>= (length bytes) 10)
             (or (string-prefix-p "GIF87a" bytes)
                 (string-prefix-p "GIF89a" bytes)))
    (cons (kill-ring-images-entry--u16le bytes 6)
          (kill-ring-images-entry--u16le bytes 8))))

(defun kill-ring-images-entry--jpeg-frame-p (marker)
  "Non-nil when MARKER starts a JPEG frame, the segment with the size."
  (and (<= #xC0 marker #xCF) (not (memq marker '(#xC4 #xC8 #xCC)))))

(defun kill-ring-images-entry--jpeg-size (bytes)
  "The (WIDTH . HEIGHT) in the JPEG frame header of BYTES, or nil."
  (when (and (>= (length bytes) 4)
             (= (aref bytes 0) #xFF) (= (aref bytes 1) #xD8))
    (let ((i 2) size)
      (while (and (not size)
                  (< (+ i 8) (length bytes))
                  (= (aref bytes i) #xFF))
        (if (kill-ring-images-entry--jpeg-frame-p (aref bytes (1+ i)))
            (setq size (cons (kill-ring-images-entry--u16be bytes (+ i 7))
                             (kill-ring-images-entry--u16be bytes (+ i 5))))
          (setq i (+ i 2 (kill-ring-images-entry--u16be bytes (+ i 2))))))
      size)))

(defun kill-ring-images-entry-size (type bytes)
  "The (WIDTH . HEIGHT) in the header of BYTES of the MIME TYPE, or nil.
An SVG has no header size, so it gives nil."
  (pcase type
    ('image/png (kill-ring-images-entry--png-size bytes))
    ('image/gif (kill-ring-images-entry--gif-size bytes))
    ('image/jpeg (kill-ring-images-entry--jpeg-size bytes))))

;;;; The entry

(defun kill-ring-images-entry--text (id type bytes)
  "The text of the entry for the image ID of TYPE with BYTES."
  (let ((size (kill-ring-images-entry-size type bytes)))
    (format "[image %s%s %s]"
            (if size (format "%dx%d " (car size) (cdr size)) "")
            (kill-ring-images-type-image-type type)
            (substring id 0 6))))

(defun kill-ring-images-entry--image-spec (type bytes &optional height)
  "An image of BYTES of the MIME TYPE, no taller than HEIGHT pixels.
The default height is `kill-ring-images-preview-lines' text lines."
  (create-image bytes (kill-ring-images-type-image-type type) t
                :max-height (or height (* kill-ring-images-preview-lines
                                          (default-line-height)))
                :ascent 'center))

(defun kill-ring-images-entry-make (id)
  "The kill ring entry for the stored image ID.
The entry shows the image itself, so that a kill ring browser draws the
picture where the entry stands, and its text says the size, the type and
the beginning of the id.  The bytes travel with the entry, so a kill
ring written to disk carries them."
  (let ((image (or (kill-ring-images-store-get id)
                   (error "kill-ring-images: no stored image %s" id))))
    (propertize (kill-ring-images-entry--text id (car image) (cdr image))
                'kill-ring-images-id id
                'yank-handler '(kill-ring-images-yank-entry)
                'display (kill-ring-images-entry--image-spec (car image)
                                                        (cdr image)))))

(defun kill-ring-images-entry-id (string)
  "The image id of STRING when it is an entry, or nil."
  (and (stringp string)
       (> (length string) 0)
       (get-text-property 0 'kill-ring-images-id string)))

(defun kill-ring-images-entry-image (string)
  "The image of the entry STRING as (TYPE . BYTES).
Return nil when STRING is no entry or the store dropped its image."
  (let ((id (kill-ring-images-entry-id string)))
    (and id (kill-ring-images-store-get id))))

(defun kill-ring-images-entry-thumbnail (entry &optional height)
  "A small image of what ENTRY holds, or nil.
HEIGHT caps it in pixels.  The default is `kill-ring-images-preview-lines'
text lines, the height at which an image copied from elsewhere is shown.
A kill ring browser draws this beside the entry, which is text and
carries no picture of its own.  A later kind of entry plugs in here."
  (when-let* ((image (kill-ring-images-entry-image entry)))
    (kill-ring-images-entry--image-spec (car image) (cdr image) height)))

(provide 'kill-ring-images-entry)
;;; kill-ring-images-entry.el ends here
