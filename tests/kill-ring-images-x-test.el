;;; kill-ring-images-x-test.el --- X server tests -*- lexical-binding: t -*-

;; `make test-x' runs this file in a graphical Emacs under xvfb-run,
;; with xclip as the other program.  A graphical Emacs has no standard
;; output for ERT, so `kill-ring-images-x-test-run' writes the result to a file.

(require 'ert)
(require 'cl-lib)
(require 'kill-ring-images)
(require 'kill-ring-images-test-helpers)

(defun kill-ring-images-x-test--xclip (&rest args)
  "Run xclip on the clipboard with ARGS and return its output as bytes.
Emacs buffers its X requests.  `sit-for' sends them, so the ownership of
a copy reaches the X server before xclip asks, and each reply reaches
xclip while it waits.  `accept-process-output' on the process does not."
  (sit-for 0)
  (let* ((buffer (generate-new-buffer " *kill-ring-images-xclip*" t))
         (process (progn
                    (with-current-buffer buffer (set-buffer-multibyte nil))
                    (make-process :name "kill-ring-images-xclip" :buffer buffer
                                  :command (append '("xclip" "-selection"
                                                     "clipboard")
                                                   args)
                                  :coding 'no-conversion
                                  :connection-type 'pipe
                                  :sentinel #'ignore))))
    (with-timeout (10 (error "xclip did not finish"))
      (while (process-live-p process)
        (sit-for 0.02)))
    (prog1 (with-current-buffer buffer (buffer-string))
      (kill-buffer buffer))))

(defun kill-ring-images-x-test--wait-for-png-offer ()
  "Wait until another program offers image/png on the clipboard."
  (with-timeout (10 (error "xclip did not offer the image"))
    (while (not (and (not (gui-backend-selection-owner-p 'CLIPBOARD))
                     (memq 'image/png
                           (append (ignore-errors
                                     (gui-get-selection 'CLIPBOARD 'TARGETS))
                                   nil))))
      (accept-process-output nil 0.05))))

(ert-deftest kill-ring-images-x-another-program-pastes-an-image-emacs-copied ()
  ;; GIVEN Emacs with the mode on
  ;; WHEN Emacs copies a PNG AND xclip asks the clipboard for its types and for
  ;;      image/png
  ;; THEN the types include image/png AND xclip gets the bytes of the PNG
  (kill-ring-images-test-with-store (* 1024 1024)
    (let ((bytes (kill-ring-images-test-png 16 16 "x-test-out"))
          (kill-ring nil)
          (kill-ring-yank-pointer nil))
      (kill-ring-images-copy 'image/png bytes)
      (should (member "image/png"
                      (split-string
                       (kill-ring-images-x-test--xclip "-t" "TARGETS" "-o"))))
      (should (equal (kill-ring-images-x-test--xclip "-t" "image/png" "-o")
                     bytes)))))

(ert-deftest kill-ring-images-x-emacs-yanks-an-image-another-program-copied ()
  ;; GIVEN Emacs with the mode on
  ;; WHEN xclip offers a PNG alone AND Emacs yanks
  ;; THEN the front of the kill ring is an entry with the bytes of the PNG
  (kill-ring-images-test-with-store (* 1024 1024)
    (kill-ring-images-test-in-directory dir
      (let ((bytes (kill-ring-images-test-png 16 16 "x-test-in"))
            (file (expand-file-name "offer.png" dir))
            (kill-ring nil)
            (kill-ring-yank-pointer nil)
            (kill-ring-images-select--last-hash nil))
        (kill-ring-images-test-write file bytes)
        (call-process "xclip" nil 0 nil
                      "-selection" "clipboard" "-t" "image/png" "-i" file)
        (kill-ring-images-x-test--wait-for-png-offer)
        (current-kill 0)
        (should (equal (kill-ring-images-entry-image (car kill-ring))
                       (cons 'image/png bytes)))))))

(defun kill-ring-images-x-test-run (result-file &optional pattern)
  "Run the X tests with the mode on, write RESULT-FILE and exit Emacs.
PATTERN is a regexp that selects the tests by name, and by default it
selects the tests of this file.  The first line of RESULT-FILE starts
with PASS or FAIL."
  (kill-ring-images-mode 1)
  (let* ((stats (ert-run-tests-batch (or pattern "^kill-ring-images-x-")))
         (failed (ert-stats-completed-unexpected stats)))
    (with-temp-file result-file
      (insert (format "%s %d of %d X tests passed\n\n"
                      (if (zerop failed) "PASS" "FAIL")
                      (ert-stats-completed-expected stats)
                      (ert-stats-total stats)))
      (insert (with-current-buffer "*Messages*" (buffer-string))))
    (kill-emacs (if (zerop failed) 0 1))))

(provide 'kill-ring-images-x-test)
;;; kill-ring-images-x-test.el ends here
