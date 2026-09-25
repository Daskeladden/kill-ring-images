;;; kill-ring-images-select-tests.el --- Select tests -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'kill-ring-images-select)
(require 'kill-ring-images-test-helpers)

;;;; Offering an image

(defun kill-ring-images-select-test--image-targets (value)
  "The image types in the TARGETS that Emacs builds for VALUE."
  (let ((selection-converter-alist
         (append (kill-ring-images-select--converters)
                 selection-converter-alist)))
    (seq-filter (lambda (target)
                  (string-prefix-p "image/" (symbol-name target)))
                (append (xselect-convert-to-targets 'CLIPBOARD 'TARGETS value)
                        nil))))

(ert-deftest kill-ring-images-select-lists-only-the-type-of-the-copied-image ()
  ;; GIVEN a PNG entry on the clipboard
  ;; WHEN another program asks for the list of types
  ;; THEN the only image type in the list is image/png
  (kill-ring-images-test-with-store 1000
    (let ((entry (kill-ring-images-entry-make
                  (kill-ring-images-store-put
                   'image/png (kill-ring-images-test-png 1 1)))))
      (should (equal (kill-ring-images-select-test--image-targets entry)
                     '(image/png))))))

(ert-deftest kill-ring-images-select-lists-no-image-type-for-plain-text ()
  ;; GIVEN plain text on the clipboard
  ;; WHEN another program asks for the list of types
  ;; THEN the list names no image type
  (kill-ring-images-test-with-store 1000
    (should-not (kill-ring-images-select-test--image-targets "plain text"))))

(ert-deftest kill-ring-images-select-sends-the-bytes-of-the-image ()
  ;; GIVEN a PNG entry on the clipboard
  ;; WHEN another program asks for image/png
  ;; THEN it gets the type and the bytes of the stored image
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 3 3))
           (entry (kill-ring-images-entry-make
                   (kill-ring-images-store-put 'image/png bytes))))
      (should (equal (kill-ring-images--convert 'CLIPBOARD 'image/png entry)
                     (cons 'image/png bytes))))))

(ert-deftest kill-ring-images-select-declines-what-it-cannot-send ()
  ;; GIVEN plain text, a GIF entry and a PNG entry whose image the store dropped
  ;; WHEN another program asks each of them for image/png
  ;; THEN each request is declined
  (kill-ring-images-test-with-store 40
    (let* ((dropped (kill-ring-images-entry-make
                     (kill-ring-images-store-put
                      'image/png (kill-ring-images-test-png 1 1 "a"))))
           (gif (kill-ring-images-entry-make
                 (kill-ring-images-store-put 'image/gif
                                        (concat "GIF89a"
                                                (unibyte-string 1 0 1 0))))))
      (kill-ring-images-store-put 'image/png
                                  (kill-ring-images-test-png 1 1 "b"))
      (should (kill-ring-images-entry-image gif))
      (should-not (kill-ring-images-entry-image dropped))
      (should-not (kill-ring-images--convert 'CLIPBOARD 'image/png
                                             "plain text"))
      (should-not (kill-ring-images--convert 'CLIPBOARD 'image/png gif))
      (should-not (kill-ring-images--convert 'CLIPBOARD 'image/png dropped)))))

;;;; Receiving an image

(defun kill-ring-images-select-test--text ()
  "The text that the normal paste function gives."
  "clipboard text")

(defmacro kill-ring-images-select-test--with-clipboard (owner targets data
                                                              &rest body)
  "Run BODY with a clipboard that offers TARGETS and gives DATA.
OWNER non-nil means that Emacs owns the clipboard.  DATA is the answer
to a request for any type other than TARGETS."
  (declare (indent 3))
  `(let ((kill-ring-images-select--last-hash nil))
     (cl-letf (((symbol-function 'gui-backend-selection-owner-p)
                (lambda (_selection) ,owner))
               ((symbol-function 'gui-get-selection)
                (lambda (_selection data-type)
                  (if (eq data-type 'TARGETS) ,targets ,data))))
       ,@body)))

(ert-deftest kill-ring-images-select-takes-an-image-offered-alone ()
  ;; GIVEN another program that offers a PNG and no text
  ;; WHEN Emacs yanks
  ;; THEN the yank gets an entry for that PNG
  (kill-ring-images-test-with-store 1000
    (let ((bytes (kill-ring-images-test-png 5 5)))
      (kill-ring-images-select-test--with-clipboard nil
          [TARGETS image/png] bytes
        (should (equal (kill-ring-images-entry-image
                        (kill-ring-images--paste
                         #'kill-ring-images-select-test--text))
                       (cons 'image/png bytes)))))))

(ert-deftest kill-ring-images-select-leaves-text-with-an-image-to-the-text-path ()
  ;; GIVEN another program that offers text and a PNG
  ;; WHEN Emacs yanks
  ;; THEN the yank gets the text AND the store stays empty
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-select-test--with-clipboard
        nil [TARGETS UTF8_STRING image/png] (kill-ring-images-test-png 5 5)
      (should
       (equal (kill-ring-images--paste #'kill-ring-images-select-test--text)
                     "clipboard text"))
      (should-not kill-ring-images-store--images))))

(ert-deftest kill-ring-images-select-leaves-its-own-clipboard-to-the-text-path ()
  ;; GIVEN a clipboard that Emacs owns
  ;; WHEN Emacs yanks
  ;; THEN the normal paste function decides AND the store stays empty
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-select-test--with-clipboard
        t [TARGETS image/png] (kill-ring-images-test-png 5 5)
      (should
       (equal (kill-ring-images--paste #'kill-ring-images-select-test--text)
                     "clipboard text"))
      (should-not kill-ring-images-store--images))))

(ert-deftest kill-ring-images-select-takes-the-preferred-type ()
  ;; GIVEN another program that offers a JPEG and a PNG
  ;; WHEN Emacs yanks
  ;; THEN the entry holds a PNG, the first type in `kill-ring-images-types'
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-select-test--with-clipboard
        nil [TARGETS image/jpeg image/png] (kill-ring-images-test-png 5 5)
      (should (eq (car (kill-ring-images-entry-image
                        (kill-ring-images--paste
                         #'kill-ring-images-select-test--text)))
                  'image/png)))))

(ert-deftest kill-ring-images-select-takes-the-same-image-once ()
  ;; GIVEN another program that offers a PNG alone
  ;; WHEN Emacs yanks twice
  ;; THEN the first yank gets an entry AND the second gets nothing new
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-select-test--with-clipboard
        nil [TARGETS image/png] (kill-ring-images-test-png 5 5)
      (should (kill-ring-images--paste #'kill-ring-images-select-test--text))
      (should-not
       (kill-ring-images--paste #'kill-ring-images-select-test--text)))))

(ert-deftest kill-ring-images-select-asks-the-image-functions-when-the-clipboard-has-no-image ()
  ;; GIVEN a clipboard that offers only text, and an image function that
  ;;       gives a PNG, as the clipboard of another machine can
  ;; WHEN Emacs yanks
  ;; THEN the yank gets an entry for the PNG of the image function
  ;; WHEN the X clipboard offers a PNG of its own
  ;; THEN the image function is not asked, because the X clipboard is the
  ;;      one that Emacs is displayed on
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 5 5 "other"))
           (asked 0)
           (kill-ring-images-image-functions
            (list (lambda () (setq asked (1+ asked)) (cons 'image/png bytes)))))
      (kill-ring-images-select-test--with-clipboard nil
          [TARGETS UTF8_STRING] "text"
        (should (equal (kill-ring-images-entry-image
                        (kill-ring-images--paste
                         #'kill-ring-images-select-test--text))
                       (cons 'image/png bytes))))
      (should (= asked 1))
      (kill-ring-images-select-test--with-clipboard nil
          [TARGETS image/png] (kill-ring-images-test-png 6 6)
        (should (kill-ring-images--paste #'kill-ring-images-select-test--text)))
      (should (= asked 1)))))

(ert-deftest kill-ring-images-select-asks-the-image-functions-even-when-emacs-owns-the-clipboard ()
  ;; GIVEN an X clipboard that Emacs owns from an earlier copy, and an image
  ;;       function that has a newer PNG, as when a program on another
  ;;       machine copied an image and its X server did not take the X
  ;;       clipboard for it
  ;; WHEN Emacs yanks
  ;; THEN the yank gets an entry for that PNG, because an image function
  ;;      decides by itself whether its image is newer than Emacs's copy
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 5 5 "newer"))
           (kill-ring-images-image-functions (list (lambda ()
                                                     (cons 'image/png bytes)))))
      (kill-ring-images-select-test--with-clipboard t
          [TARGETS UTF8_STRING] "text"
        (should (equal (kill-ring-images-entry-image
                        (kill-ring-images--paste
                         #'kill-ring-images-select-test--text))
                       (cons 'image/png bytes)))))))

(ert-deftest kill-ring-images-select-leaves-the-yank-to-text-when-no-image-function-has-one ()
  ;; GIVEN a clipboard that offers only text, and an image function that
  ;;       has no image
  ;; WHEN Emacs yanks
  ;; THEN the normal paste function decides AND the store stays empty
  (kill-ring-images-test-with-store 1000
    (let ((kill-ring-images-image-functions (list #'ignore)))
      (kill-ring-images-select-test--with-clipboard nil
          [TARGETS UTF8_STRING] "text"
        (should
         (equal (kill-ring-images--paste #'kill-ring-images-select-test--text)
                       "clipboard text"))
        (should-not kill-ring-images-store--images)))))

(ert-deftest kill-ring-images-select-falls-back-to-text-when-a-request-fails ()
  ;; GIVEN a clipboard whose requests signal an error
  ;; WHEN Emacs yanks, outside the debugger
  ;; THEN the normal paste function decides
  ;; ERT in Emacs 29 binds `debug-on-error' to t, and then
  ;; `with-demoted-errors' lets the error through, as it must for a
  ;; person who debugs.
  (kill-ring-images-test-with-store 1000
    (let ((debug-on-error nil))
      (cl-letf (((symbol-function 'gui-backend-selection-owner-p) #'ignore)
                ((symbol-function 'gui-get-selection)
                 (lambda (&rest _) (error "Timed out waiting for reply"))))
        (should
         (equal (kill-ring-images--paste #'kill-ring-images-select-test--text)
                       "clipboard text"))))))

(ert-deftest kill-ring-images-select-falls-back-to-text-for-empty-image-data ()
  ;; GIVEN another program that offers a PNG but sends no bytes
  ;; WHEN Emacs yanks
  ;; THEN the normal paste function decides
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-select-test--with-clipboard nil [TARGETS image/png] ""
      (should
       (equal (kill-ring-images--paste #'kill-ring-images-select-test--text)
                     "clipboard text")))))

(ert-deftest kill-ring-images-select-falls-back-to-text-when-the-store-refuses ()
  ;; GIVEN a store of 10 bytes AND another program that offers a larger PNG
  ;;       alone
  ;; WHEN Emacs yanks
  ;; THEN the normal paste function decides AND the store stays empty
  (kill-ring-images-test-with-store 10
    (kill-ring-images-select-test--with-clipboard
        nil [TARGETS image/png] (kill-ring-images-test-png 5 5)
      (should
       (equal (kill-ring-images--paste #'kill-ring-images-select-test--text)
                     "clipboard text"))
      (should-not kill-ring-images-store--images))))

;;;; Installing

(ert-deftest kill-ring-images-select-installs-once-and-uninstalls-completely ()
  (let* ((selection-converter-alist selection-converter-alist)
         (before selection-converter-alist)
         (interprogram-paste-function #'kill-ring-images-select-test--text))
    ;; GIVEN the converters and the paste function as Emacs has them
    ;; WHEN kill-ring-images installs twice
    (kill-ring-images-select-install)
    (kill-ring-images-select-install)
    ;; THEN each type has one converter AND the paste function goes through the
    ;;      wrapper
    (should (= (cl-count-if #'kill-ring-images-select--converter-p
                            selection-converter-alist)
               (length kill-ring-images-types)))
    (should (advice-function-member-p #'kill-ring-images--paste
                                      interprogram-paste-function))
    ;; WHEN kill-ring-images uninstalls
    (kill-ring-images-select-uninstall)
    ;; THEN the converters and the paste function are as they were
    (should (equal selection-converter-alist before))
    (should (eq interprogram-paste-function
                #'kill-ring-images-select-test--text))))

;;; kill-ring-images-select-tests.el ends here
