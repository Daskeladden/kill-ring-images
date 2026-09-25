;;; kill-ring-images-test-helpers.el --- Test helpers -*- lexical-binding: t -*-
(require 'cl-lib)

(defun kill-ring-images-test--u32 (n)
  "N as four bytes, the most significant first."
  (unibyte-string (logand (ash n -24) 255) (logand (ash n -16) 255)
                  (logand (ash n -8) 255) (logand n 255)))

(defun kill-ring-images-test-png (width height &optional salt)
  "The start of a PNG of WIDTH by HEIGHT, enough for its header.
SALT, an ASCII string, makes the bytes differ from another image of the
same size."
  (concat "\x89PNG\r\n\x1a\n" (kill-ring-images-test--u32 13) "IHDR"
          (kill-ring-images-test--u32 width) (kill-ring-images-test--u32 height)
          (or salt "")))

(defmacro kill-ring-images-test-with-store (limit &rest body)
  "Run BODY with an empty store of LIMIT bytes and the default types."
  (declare (indent 1))
  `(let ((kill-ring-images-store--images nil)
         (kill-ring-images-store-limit ,limit)
         (kill-ring-images-types '(image/png image/jpeg image/gif
                                             image/svg+xml)))
     ,@body))

(defun kill-ring-images-test-read (file)
  "The bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun kill-ring-images-test-write (file bytes)
  "Write BYTES to FILE as they are."
  (let ((coding-system-for-write 'no-conversion))
    (write-region bytes nil file nil 'silent)))

(defmacro kill-ring-images-test-in-directory (var &rest body)
  "Run BODY with VAR bound to a fresh directory, deleted after."
  (declare (indent 1))
  `(let ((,var (make-temp-file "kill-ring-images-test-" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(provide 'kill-ring-images-test-helpers)
;;; kill-ring-images-test-helpers.el ends here
