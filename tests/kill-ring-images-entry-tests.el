;;; kill-ring-images-entry-tests.el --- Entry tests -*- lexical-binding: t -*-
(require 'ert)
(require 'kill-ring-images-entry)
(require 'kill-ring-images-test-helpers)

(defun kill-ring-images-entry-test--jpeg (width height &optional frame-marker)
  "The start of a JPEG of WIDTH by HEIGHT with an APP0 segment first.
FRAME-MARKER is the frame marker byte, #xC0 when nil."
  (concat (unibyte-string #xFF #xD8 #xFF #xE0 0 16)
          (make-string 14 0)
          (unibyte-string #xFF (or frame-marker #xC0) 0 17 8
                          (ash height -8) (logand height 255)
                          (ash width -8) (logand width 255) 3)))

(ert-deftest kill-ring-images-entry-names-the-size-type-and-hash-of-a-png ()
  ;; GIVEN a stored PNG of 240 by 120
  ;; WHEN its entry is made
  ;; THEN the text names the size, the type and the first six characters of the
  ;;      id
  (kill-ring-images-test-with-store 1000
    (let ((id (kill-ring-images-store-put 'image/png
                                          (kill-ring-images-test-png 240 120))))
      (should (equal (substring-no-properties (kill-ring-images-entry-make id))
                     (format "[image 240x120 png %s]" (substring id 0 6)))))))

(ert-deftest kill-ring-images-entry-reads-the-size-of-a-gif-and-a-jpeg ()
  ;; GIVEN the header of a GIF, of a baseline JPEG and of a progressive JPEG
  ;; THEN each header gives its width and height
  (should (equal (kill-ring-images-entry-size
                  'image/gif (concat "GIF89a" (unibyte-string 240 0 120 0)))
                 '(240 . 120)))
  (should (equal (kill-ring-images-entry-size
                  'image/jpeg (kill-ring-images-entry-test--jpeg 640 480))
                 '(640 . 480)))
  (should (equal (kill-ring-images-entry-size
                  'image/jpeg (kill-ring-images-entry-test--jpeg 512 512 #xC2))
                 '(512 . 512))))

(ert-deftest kill-ring-images-entry-leaves-out-a-size-it-cannot-read ()
  ;; GIVEN a stored SVG AND a stored PNG whose header is broken
  ;; WHEN their entries are made
  ;; THEN neither text names a size
  (kill-ring-images-test-with-store 1000
    (let ((svg (kill-ring-images-store-put 'image/svg+xml "<svg/>"))
          (png (kill-ring-images-store-put 'image/png "not a png")))
      (should (equal (substring-no-properties (kill-ring-images-entry-make svg))
                     (format "[image svg %s]" (substring svg 0 6))))
      (should (equal (substring-no-properties (kill-ring-images-entry-make png))
                     (format "[image png %s]" (substring png 0 6)))))))

(ert-deftest kill-ring-images-entry-text-differs-for-two-images-of-one-size ()
  ;; GIVEN two stored PNGs of the same size with different bytes
  ;; THEN their entries have different texts
  (kill-ring-images-test-with-store 1000
    (let ((a (kill-ring-images-store-put 'image/png
                                         (kill-ring-images-test-png 8 8 "a")))
          (b (kill-ring-images-store-put 'image/png
                                         (kill-ring-images-test-png 8 8 "b"))))
      (should-not (equal (kill-ring-images-entry-make a)
                         (kill-ring-images-entry-make b))))))

(ert-deftest kill-ring-images-entry-gives-the-id-and-image-of-an-entry-only ()
  ;; GIVEN an entry for a stored PNG AND a plain text
  ;; THEN the entry gives its id and its image
  ;; AND the plain text and an empty text give neither
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 2 2))
           (id (kill-ring-images-store-put 'image/png bytes))
           (entry (kill-ring-images-entry-make id)))
      (should (equal (kill-ring-images-entry-id entry) id))
      (should (equal (kill-ring-images-entry-image entry)
                     (cons 'image/png bytes)))
      (should-not (kill-ring-images-entry-id "plain text"))
      (should-not (kill-ring-images-entry-image "")))))

(ert-deftest kill-ring-images-entry-has-no-image-after-the-store-dropped-it ()
  ;; GIVEN an entry for a stored PNG in a store with room for one image
  ;; WHEN another image pushes it out of the store
  ;; THEN the entry still gives its id AND no longer gives an image
  (kill-ring-images-test-with-store 30
    (let* ((id (kill-ring-images-store-put 'image/png
                                           (kill-ring-images-test-png 1 1 "a")))
           (entry (kill-ring-images-entry-make id)))
      (kill-ring-images-store-put 'image/png
                                  (kill-ring-images-test-png 1 1 "b"))
      (should (equal (kill-ring-images-entry-id entry) id))
      (should-not (kill-ring-images-entry-image entry)))))

(ert-deftest kill-ring-images-entry-carries-the-yank-handler ()
  ;; GIVEN an entry for a stored PNG
  ;; THEN a yank of it goes through kill-ring-images-yank-entry
  (kill-ring-images-test-with-store 1000
    (let ((entry (kill-ring-images-entry-make
                  (kill-ring-images-store-put
                   'image/png (kill-ring-images-test-png 1 1)))))
      (should (equal (get-text-property 0 'yank-handler entry)
                     '(kill-ring-images-yank-entry))))))

;;;; The thumbnail of an entry

(ert-deftest kill-ring-images-entry-shows-its-image-in-place-of-its-text ()
  ;; GIVEN a stored PNG
  ;; WHEN its entry is made
  ;; THEN the entry shows the image, capped to the preview height
  ;; AND its text still names the size, the type and the hash
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 40 20 "shown"))
           (id (kill-ring-images-store-put 'image/png bytes))
           (entry (kill-ring-images-entry-make id))
           (image (get-text-property 0 'display entry)))
      (should (eq (car-safe image) 'image))
      (should (= (plist-get (cdr image) :max-height)
                 (* kill-ring-images-preview-lines (default-line-height))))
      (should (equal (substring-no-properties entry)
                     (format "[image 40x20 png %s]" (substring id 0 6))))
      (should (= (next-single-property-change 0 'display entry (length entry))
                 (length entry))))))

(ert-deftest kill-ring-images-entry-gives-a-small-image-of-itself ()
  ;; GIVEN an entry for a stored PNG
  ;; WHEN a thumbnail of it is asked for, twenty pixels tall
  ;; THEN it is an image of those bytes, capped to that height
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 40 20 "thumb"))
           (entry (kill-ring-images-entry-make
                   (kill-ring-images-store-put 'image/png bytes)))
           (image (kill-ring-images-entry-thumbnail entry 20)))
      (should (eq (car image) 'image))
      (should (eq (plist-get (cdr image) :type) 'png))
      (should (equal (plist-get (cdr image) :data) bytes))
      (should (= (plist-get (cdr image) :max-height) 20)))))

(ert-deftest kill-ring-images-entry-thumbnail-is-as-tall-as-a-preview ()
  ;; GIVEN an entry for a stored PNG
  ;; WHEN a thumbnail is asked for without a height
  ;; THEN it is as tall as an image in the kill ring, five text lines
  (kill-ring-images-test-with-store 1000
    (let* ((entry (kill-ring-images-entry-make
                   (kill-ring-images-store-put 'image/png
                                          (kill-ring-images-test-png 40 20
                                                                     "tall"))))
           (image (kill-ring-images-entry-thumbnail entry)))
      (should (= (plist-get (cdr image) :max-height)
                 (* kill-ring-images-preview-lines (default-line-height)))))))

(ert-deftest kill-ring-images-entry-has-no-thumbnail-without-an-image ()
  ;; GIVEN a plain text AND an entry whose image the store dropped
  ;; THEN neither gives a thumbnail
  (kill-ring-images-test-with-store 30
    (let ((entry (kill-ring-images-entry-make
                  (kill-ring-images-store-put 'image/png
                                         (kill-ring-images-test-png 1 1 "a")))))
      (kill-ring-images-store-put 'image/png
                                  (kill-ring-images-test-png 1 1 "b"))
      (should-not (kill-ring-images-entry-thumbnail "plain text" 20))
      (should-not (kill-ring-images-entry-thumbnail entry 20)))))

;;; kill-ring-images-entry-tests.el ends here
