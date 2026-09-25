;;; kill-ring-images-store-tests.el --- Store tests -*- lexical-binding: t -*-
(require 'ert)
(require 'kill-ring-images-store)
(require 'kill-ring-images-test-helpers)

(ert-deftest kill-ring-images-store-gives-back-what-it-stored ()
  ;; GIVEN an empty store
  ;; WHEN a PNG is stored
  ;; THEN the id is the SHA-1 of the bytes
  ;; AND the store gives back the type and the bytes for that id
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 4 3))
           (id (kill-ring-images-store-put 'image/png bytes)))
      (should (equal id (secure-hash 'sha1 bytes)))
      (should (equal (kill-ring-images-store-get id)
                     (cons 'image/png bytes))))))

(ert-deftest kill-ring-images-store-keeps-the-same-bytes-once-as-the-newest ()
  ;; GIVEN a store that holds image A and then image B
  ;; WHEN image A is stored again
  ;; THEN the store holds each image once AND A is now the newest
  (kill-ring-images-test-with-store 1000
    (let ((a (kill-ring-images-store-put 'image/png
                                         (kill-ring-images-test-png 1 1 "a")))
          (b (kill-ring-images-store-put 'image/png
                                         (kill-ring-images-test-png 1 1 "b"))))
      (kill-ring-images-store-put 'image/png
                                  (kill-ring-images-test-png 1 1 "a"))
      (should (equal (mapcar #'car kill-ring-images-store--images)
                     (list b a))))))

(ert-deftest kill-ring-images-store-drops-the-oldest-over-the-limit ()
  ;; GIVEN a store with room for two images of 25 bytes, holding A and B
  ;; WHEN a third image C is stored
  ;; THEN A is gone AND B and C remain
  (kill-ring-images-test-with-store 50
    (let* ((a (kill-ring-images-store-put 'image/png
                                          (kill-ring-images-test-png 1 1 "a")))
           (b (kill-ring-images-store-put 'image/png
                                          (kill-ring-images-test-png 1 1 "b")))
           (c (kill-ring-images-store-put 'image/png
                                          (kill-ring-images-test-png 1 1 "c"))))
      (should-not (kill-ring-images-store-get a))
      (should (kill-ring-images-store-get b))
      (should (kill-ring-images-store-get c)))))

(ert-deftest kill-ring-images-store-refuses-an-image-larger-than-the-limit ()
  ;; GIVEN a store of 30 bytes that holds one image of 25 bytes
  ;; WHEN an image of 40 bytes is stored
  ;; THEN the store refuses it AND still holds the first image
  (kill-ring-images-test-with-store 30
    (let ((a (kill-ring-images-store-put 'image/png
                                         (kill-ring-images-test-png 1 1 "a"))))
      (should-not (kill-ring-images-store-put
                   'image/png (kill-ring-images-test-png 1 1
                                                         "0123456789abcdef")))
      (should (kill-ring-images-store-get a)))))

(ert-deftest kill-ring-images-store-signals-for-multibyte-bytes ()
  ;; GIVEN an empty store
  ;; WHEN a multibyte string is stored as image bytes
  ;; THEN the store signals an error
  (kill-ring-images-test-with-store 1000
    (should-error (kill-ring-images-store-put 'image/png (string #xF8)))))

(ert-deftest kill-ring-images-store-signals-for-a-type-outside-the-types ()
  ;; GIVEN an empty store
  ;; WHEN bytes are stored as a type that is not in `kill-ring-images-types'
  ;; THEN the store signals an error
  (kill-ring-images-test-with-store 1000
    (should-error
     (kill-ring-images-store-put 'image/webp (kill-ring-images-test-png 1 1)))))

(ert-deftest kill-ring-images-store-names-the-image-type-and-the-extension ()
  ;; GIVEN the MIME type of a JPEG
  ;; THEN its Emacs image type is jpeg AND its file extension is jpg
  (should (eq (kill-ring-images-type-image-type 'image/jpeg) 'jpeg))
  (should (equal (kill-ring-images-type-extension 'image/jpeg) "jpg")))

(ert-deftest kill-ring-images-store-finds-the-mime-type-of-an-image-type ()
  ;; GIVEN the default types
  ;; THEN svg belongs to image/svg+xml AND xpm belongs to no type
  (kill-ring-images-test-with-store 1000
    (should (eq (kill-ring-images-type-from-image-type 'svg) 'image/svg+xml))
    (should-not (kill-ring-images-type-from-image-type 'xpm))))

;;; kill-ring-images-store-tests.el ends here
