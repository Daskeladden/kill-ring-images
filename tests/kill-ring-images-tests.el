;;; kill-ring-images-tests.el --- Mode and copy tests -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'kill-ring-images)
(require 'kill-ring-images-test-helpers)

(defmacro kill-ring-images-test--with-kill-ring (&rest body)
  "Run BODY with an empty kill ring and no clipboard."
  (declare (indent 0))
  `(let ((kill-ring nil)
         (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil))
     ,@body))

(defun kill-ring-images-test--copy-at-point-in (display)
  "Copy the image under point in a buffer whose text has DISPLAY."
  (with-temp-buffer
    (insert (propertize "x" 'display display))
    (goto-char (point-min))
    (kill-ring-images-copy-image-at-point)))

;;;; Copying

(ert-deftest kill-ring-images-copies-the-image-under-point ()
  ;; GIVEN a buffer that shows a PNG from its data at point
  ;; WHEN the image under point is copied
  ;; THEN the front of the kill ring is an entry for that PNG
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-kill-ring
      (let ((bytes (kill-ring-images-test-png 6 6)))
        (kill-ring-images-test--copy-at-point-in (create-image bytes 'png t))
        (should (equal (kill-ring-images-entry-image (car kill-ring))
                       (cons 'image/png bytes)))))))

(ert-deftest kill-ring-images-copies-an-image-from-its-file ()
  ;; GIVEN a buffer that shows a GIF from a file at point
  ;; WHEN the image under point is copied
  ;; THEN the front of the kill ring is an entry with the bytes of the file
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-kill-ring
      (kill-ring-images-test-in-directory dir
        (let ((bytes (concat "GIF89a" (unibyte-string 2 0 2 0)))
              (file (expand-file-name "picture.gif" dir)))
          (kill-ring-images-test-write file bytes)
          (kill-ring-images-test--copy-at-point-in (list 'image :type 'gif
                                                         :file file))
          (should (equal (kill-ring-images-entry-image (car kill-ring))
                         (cons 'image/gif bytes))))))))

(ert-deftest kill-ring-images-copy-hands-the-image-to-the-copy-functions ()
  ;; GIVEN a copy function that sends images to another machine
  ;; WHEN an image is copied
  ;; THEN the function gets its type and bytes, after the kill ring has the
  ;;      entry
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 4 4 "copy"))
           (kill-ring nil)
           (handed nil)
           (kill-ring-images-copy-functions
            (list (lambda (type bytes)
                    (setq handed (list type bytes (length kill-ring)))))))
      (kill-ring-images-copy 'image/png bytes)
      (should (equal handed (list 'image/png bytes 1))))))

(ert-deftest kill-ring-images-copy-cuts-to-x-only-while-copy-to-x-is-on ()
  ;; GIVEN an interprogram cut function
  ;; WHEN an image is copied with `kill-ring-images-copy-to-x' on, and again off
  ;; THEN the first copy is cut, AND the second is not, while the kill ring
  ;;      gets both
  (kill-ring-images-test-with-store 1000
    (let* ((kill-ring nil)
           (cut nil)
           (interprogram-cut-function (lambda (text) (push text cut))))
      (let ((kill-ring-images-copy-to-x t))
        (kill-ring-images-copy 'image/png (kill-ring-images-test-png 4 4 "on")))
      (let ((kill-ring-images-copy-to-x nil))
        (kill-ring-images-copy 'image/png
                               (kill-ring-images-test-png 4 4 "off")))
      (should (= (length cut) 1))
      (should (= (length kill-ring) 2)))))

(ert-deftest kill-ring-images-refuses-to-copy-without-an-image-or-of-an-unknown-type ()
  ;; GIVEN a buffer without an image AND a buffer that shows an XPM
  ;; WHEN the image under point is copied in each
  ;; THEN each copy gives a user error AND the kill ring stays empty
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-kill-ring
      (with-temp-buffer
        (insert "no image")
        (goto-char (point-min))
        (should-error (kill-ring-images-copy-image-at-point) :type 'user-error))
      (should-error (kill-ring-images-test--copy-at-point-in
                     (list 'image :type 'xpm :data "/* XPM */"))
                    :type 'user-error)
      (should-not kill-ring))))

(ert-deftest kill-ring-images-copies-an-image-that-imagemagick-shows ()
  ;; GIVEN a buffer that shows a PNG file through ImageMagick
  ;; WHEN the image under point is copied
  ;; THEN the front of the kill ring is an entry for a PNG with the bytes of the
  ;;      file
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-kill-ring
      (kill-ring-images-test-in-directory dir
        (let ((bytes (kill-ring-images-test-png 7 7))
              (file (expand-file-name "shown.png" dir)))
          (kill-ring-images-test-write file bytes)
          (kill-ring-images-test--copy-at-point-in
           (list 'image :type 'imagemagick :file file))
          (should (equal (kill-ring-images-entry-image (car kill-ring))
                         (cons 'image/png bytes))))))))

;;;; The mode

(defun kill-ring-images-test--terminal (text)
  "A terminal function that returns the TEXT it was given."
  text)

(ert-deftest kill-ring-images-mode-adds-its-hooks-and-removes-them ()
  (let* ((selection-converter-alist selection-converter-alist)
         (before selection-converter-alist)
         (interprogram-paste-function #'ignore)
         (kill-ring-images-yank-terminal-functions
          '(kill-ring-images-test--terminal)))
    (unwind-protect
        (progn
          ;; GIVEN the converters, the paste function and a terminal function
          ;; WHEN the mode is turned on
          (kill-ring-images-mode 1)
          ;; THEN the converters, the paste wrapper and the terminal advice are
          ;;      in place
          (should (cl-some #'kill-ring-images-select--converter-p
                           selection-converter-alist))
          (should (advice-function-member-p #'kill-ring-images--paste
                                             interprogram-paste-function))
          (should (advice-member-p #'kill-ring-images-yank--terminal-args
                                   'kill-ring-images-test--terminal))
          ;; WHEN the mode is turned off
          (kill-ring-images-mode -1)
          ;; THEN all three are as they were
          (should (equal selection-converter-alist before))
          (should (eq interprogram-paste-function #'ignore))
          (should-not (advice-member-p #'kill-ring-images-yank--terminal-args
                                       'kill-ring-images-test--terminal)))
      (kill-ring-images-mode -1))))

;;;; Shrinking an image in the kill ring

(defun kill-ring-images-test--image-entry (&optional display)
  "An entry like the one a copy in image-mode makes.
DISPLAY is the display property, an image spec by default."
  (propertize "\211PNG\n" 'display
              (or display (list 'image :type 'png :file "/tmp/kept.png"))))

(defmacro kill-ring-images-test--with-mode (&rest body)
  "Run BODY with `kill-ring-images-mode' on and an empty kill ring."
  (declare (indent 0))
  `(let ((selection-converter-alist selection-converter-alist)
         (interprogram-paste-function #'ignore)
         (kill-ring-images-yank-terminal-functions nil)
         (kill-ring nil)
         (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil))
     (unwind-protect
         (progn (kill-ring-images-mode 1) ,@body)
       (kill-ring-images-mode -1))))

(ert-deftest kill-ring-images-caps-the-height-of-an-image-in-the-kill-ring ()
  ;; GIVEN the mode is on
  ;; WHEN an entry carrying an image is killed
  ;; THEN the entry keeps its characters and its file
  ;; AND the image is capped to `kill-ring-images-preview-lines' text lines
  (kill-ring-images-test--with-mode
    (kill-new (kill-ring-images-test--image-entry))
    (let* ((entry (car kill-ring))
           (image (get-text-property 0 'display entry)))
      (should (equal (substring-no-properties entry) "\211PNG\n"))
      (should (equal (plist-get (cdr image) :file) "/tmp/kept.png"))
      (should (= (plist-get (cdr image) :max-height)
                 (* kill-ring-images-preview-lines (default-line-height)))))))

(ert-deftest kill-ring-images-replaces-a-cap-the-image-already-has ()
  ;; GIVEN the mode is on AND an entry whose image already caps its size,
  ;; as an image shown in a window does
  ;; WHEN it is killed
  ;; THEN each cap stands once, with the new value
  ;; AND a repeated property would make the specification invalid
  (kill-ring-images-test--with-mode
    (kill-new (kill-ring-images-test--image-entry
               (list 'image :type 'png :file "/tmp/kept.png"
                     :max-width 1874 :max-height 921)))
    (let ((plist (cdr (get-text-property 0 'display (car kill-ring)))))
      (should (= (cl-count :max-height plist) 1))
      (should (= (cl-count :max-width plist) 1))
      (should (= (plist-get plist :max-height)
                 (* kill-ring-images-preview-lines (default-line-height)))))))

(ert-deftest kill-ring-images-leaves-text-and-other-display-alone ()
  ;; GIVEN the mode is on
  ;; WHEN a plain string and a string shown as other text are killed
  ;; THEN both reach the kill ring unchanged
  (kill-ring-images-test--with-mode
    (kill-new "plain text")
    (should (equal-including-properties (car kill-ring) "plain text"))
    (kill-new (kill-ring-images-test--image-entry "shown instead"))
    (should (equal (get-text-property 0 'display (car kill-ring))
                   "shown instead"))))

(ert-deftest kill-ring-images-caps-what-the-kill-ring-already-holds ()
  ;; GIVEN a kill ring holding an image, with the mode off
  ;; WHEN the mode is turned on
  ;; THEN that entry is capped too, AND the yank position still points at it
  (let ((selection-converter-alist selection-converter-alist)
        (interprogram-paste-function #'ignore)
        (kill-ring-images-yank-terminal-functions nil)
        (interprogram-cut-function nil)
        (kill-ring (list "newer" (kill-ring-images-test--image-entry)))
        (kill-ring-yank-pointer nil))
    (setq kill-ring-yank-pointer (cdr kill-ring))
    (unwind-protect
        (progn
          (kill-ring-images-mode 1)
          (let ((image (get-text-property 0 'display (nth 1 kill-ring))))
            (should (= (plist-get (cdr image) :max-height)
                       (* kill-ring-images-preview-lines
                          (default-line-height))))
            (should (eq (car kill-ring-yank-pointer) (nth 1 kill-ring)))))
      (kill-ring-images-mode -1))))

(ert-deftest kill-ring-images-turns-a-copied-image-into-an-entry ()
  ;; GIVEN the mode is on AND a copied line that holds only an image
  ;; WHEN it is killed
  ;; THEN the kill ring holds a kill-ring-images entry for those bytes
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-mode
      (let ((bytes (kill-ring-images-test-png 12 9 "copied")))
        (kill-new (concat (propertize "x" 'display
                                      (list 'image :type 'png :data bytes))
                          "\n"))
        (should (equal (kill-ring-images-entry-image (car kill-ring))
                       (cons 'image/png bytes)))))))

(ert-deftest kill-ring-images-turns-an-image-file-into-an-entry ()
  ;; GIVEN the mode is on AND a copied image that names a file
  ;; WHEN it is killed
  ;; THEN the entry holds the bytes of that file
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-mode
      (kill-ring-images-test-in-directory dir
        (let ((bytes (kill-ring-images-test-png 5 5 "from-file"))
              (file (expand-file-name "copied.png" dir)))
          (kill-ring-images-test-write file bytes)
          (kill-new (propertize "x" 'display
                                (list 'image :type 'imagemagick :file file)))
          (should (equal (kill-ring-images-entry-image (car kill-ring))
                         (cons 'image/png bytes))))))))

(ert-deftest kill-ring-images-does-not-copy-its-own-entry-again ()
  ;; GIVEN the mode is on AND an entry already in the kill ring
  ;; WHEN that entry is copied again, as a browser or a yank does
  ;; THEN it stays the same entry, and nothing is said about a new image
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-mode
      (let* ((bytes (kill-ring-images-test-png 8 8 "again"))
             (entry (kill-ring-images-entry-make
                     (kill-ring-images-store-put 'image/png bytes)))
             messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest args) (push (apply #'format args) messages))))
          (kill-new entry))
        (should (equal (kill-ring-images-entry-id (car kill-ring))
                       (kill-ring-images-entry-id entry)))
        (should-not messages)))))

(ert-deftest kill-ring-images-says-when-a-copy-became-an-image ()
  ;; GIVEN the mode is on
  ;; WHEN a line holding only an image is copied
  ;; THEN the echo area names the image, after whatever the command said
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-mode
      (let ((bytes (kill-ring-images-test-png 12 9 "spoken"))
            messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest args) (push (apply #'format args) messages))))
          (message "Copied line")
          (kill-new (concat (propertize "x" 'display
                                        (list 'image :type 'png :data bytes))
                            "\n")))
        (should (equal (car messages)
                       (format "kill-ring-images: copied %s"
                               (substring-no-properties (car kill-ring)))))))))

(ert-deftest kill-ring-images-keeps-a-copy-that-holds-more-than-an-image ()
  ;; GIVEN the mode is on AND a copy of text with an image in it
  ;; WHEN it is killed
  ;; THEN the text stands as it was, and only the image is capped
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-mode
      (let ((bytes (kill-ring-images-test-png 12 9 "mixed")))
        (kill-new (concat "see "
                          (propertize "x" 'display
                                      (list 'image :type 'png :data bytes))))
        (should-not (kill-ring-images-entry-id (car kill-ring)))
        (should (equal (substring-no-properties (car kill-ring)) "see x"))
        (should (plist-get (cdr (get-text-property 4 'display (car kill-ring)))
                           :max-height))))))

(ert-deftest kill-ring-images-keeps-an-image-whose-bytes-it-cannot-read ()
  ;; GIVEN the mode is on AND a copied image naming a file that is gone
  ;; WHEN it is killed
  ;; THEN the entry stays as text, with the image capped
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test--with-mode
      (kill-new (propertize "x" 'display
                            (list 'image :type 'png
                                  :file "/nowhere/gone.png")))
      (should-not (kill-ring-images-entry-id (car kill-ring)))
      (should (equal (substring-no-properties (car kill-ring)) "x")))))

(ert-deftest kill-ring-images-gives-an-older-entry-its-picture-back ()
  ;; GIVEN a kill ring holding an entry made before entries showed their
  ;; image, whose bytes the store still has
  ;; WHEN the mode is turned on
  ;; THEN that entry shows its image again
  (kill-ring-images-test-with-store 1000
    (let* ((id (kill-ring-images-store-put 'image/png
                                      (kill-ring-images-test-png 40 20
                                                                 "older")))
           (bare (propertize (substring-no-properties
                              (kill-ring-images-entry-make id))
                             'kill-ring-images-id id
                             'yank-handler '(kill-ring-images-yank-entry)))
           (selection-converter-alist selection-converter-alist)
           (interprogram-paste-function #'ignore)
           (kill-ring-images-yank-terminal-functions nil)
           (interprogram-cut-function nil)
           (kill-ring (list bare))
           (kill-ring-yank-pointer nil))
      (should-not (get-text-property 0 'display bare))
      (unwind-protect
          (progn
            (kill-ring-images-mode 1)
            (should (eq (car-safe (get-text-property 0 'display
                                                     (car kill-ring)))
                        'image)))
        (kill-ring-images-mode -1)))))

(ert-deftest kill-ring-images-caps-nothing-with-the-mode-off ()
  ;; GIVEN the mode is off
  ;; WHEN an entry carrying an image is killed
  ;; THEN the image is as it was
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil))
    (kill-new (kill-ring-images-test--image-entry))
    (should-not (plist-get (cdr (get-text-property 0 'display (car kill-ring)))
                           :max-height))))

;;; kill-ring-images-tests.el ends here
