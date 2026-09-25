;;; kill-ring-images-yank-tests.el --- Yank tests -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'kill-ring-images-yank)
(require 'kill-ring-images-test-helpers)

;; Modes that count as markdown-mode and org-mode, which the tests do
;; not need.
(put 'kill-ring-images-yank-test-markdown-mode 'derived-mode-parent
     'markdown-mode)
(put 'kill-ring-images-yank-test-org-mode 'derived-mode-parent 'org-mode)

(defun kill-ring-images-yank-test--entry (bytes)
  "Store BYTES as a PNG and return its entry."
  (kill-ring-images-entry-make (kill-ring-images-store-put 'image/png bytes)))

(defmacro kill-ring-images-yank-test--visiting (file &rest body)
  "Run BODY in a buffer that visits FILE, left unmodified after."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((create-lockfiles nil))
       (setq buffer-file-name ,file)
       (unwind-protect (progn ,@body)
         (set-buffer-modified-p nil)
         (setq buffer-file-name nil)))))

(defun kill-ring-images-yank-test--messages (function)
  "Call FUNCTION and return the messages that it showed."
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push (apply #'format args) messages))))
      (funcall function))
    messages))

;;;; A normal buffer

(ert-deftest kill-ring-images-yank-shows-the-image-inline-in-a-plain-buffer ()
  ;; GIVEN a PNG entry
  ;; WHEN it is yanked into a buffer without a yank-media handler
  ;; THEN the buffer gets the text of the entry AND that text shows the PNG
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 4 4))
           (entry (kill-ring-images-yank-test--entry bytes)))
      (with-temp-buffer
        (insert-for-yank entry)
        (should (equal (buffer-string) (substring-no-properties entry)))
        (let ((image (get-text-property 1 'display)))
          (should (eq (car image) 'image))
          (should (eq (plist-get (cdr image) :type) 'png))
          (should (equal (plist-get (cdr image) :data) bytes)))))))

(ert-deftest kill-ring-images-yank-gives-the-image-to-a-yank-media-handler ()
  ;; GIVEN a buffer with a yank-media handler for images AND a PNG entry
  ;; WHEN the entry is yanked there
  ;; THEN the handler gets the MIME type and the bytes
  ;; AND the text of the entry is not inserted
  (kill-ring-images-test-with-store 1000
    (let* ((bytes (kill-ring-images-test-png 4 4))
           (entry (kill-ring-images-yank-test--entry bytes))
           received)
      (with-temp-buffer
        (yank-media-handler "image/.*"
                            (lambda (type data)
                              (setq received (list type data))))
        (insert-for-yank entry)
        (should (equal received (list 'image/png bytes)))
        (should (equal (buffer-string) ""))))))

(ert-deftest kill-ring-images-yank-links-a-file-next-to-a-markdown-file ()
  ;; GIVEN a markdown buffer that visits doc.md AND a PNG entry
  ;; WHEN the entry is yanked there
  ;; THEN the PNG is written in images next to doc.md
  ;; AND the buffer gets a relative markdown link to it
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test-in-directory dir
      (let* ((bytes (kill-ring-images-test-png 4 4))
             (entry (kill-ring-images-yank-test--entry bytes))
             (name (format "images/image-%s.png"
                           (substring (kill-ring-images-entry-id entry) 0 12))))
        (kill-ring-images-yank-test--visiting (expand-file-name "doc.md" dir)
          (setq major-mode 'kill-ring-images-yank-test-markdown-mode)
          (insert-for-yank entry)
          (should (equal (buffer-string) (format "![](%s)" name)))
          (should (equal (kill-ring-images-test-read
                          (expand-file-name name dir)) bytes)))))))

(ert-deftest kill-ring-images-yank-links-an-absolute-file-in-an-unsaved-markdown-buffer ()
  ;; GIVEN a markdown buffer without a file AND a PNG entry
  ;; WHEN the entry is yanked there
  ;; THEN the PNG is written in `kill-ring-images-file-directory'
  ;; AND the buffer gets an absolute markdown link to it
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test-in-directory dir
      (let* ((kill-ring-images-file-directory dir)
             (bytes (kill-ring-images-test-png 4 4))
             (entry (kill-ring-images-yank-test--entry bytes))
             (file (expand-file-name
                    (format "image-%s.png"
                            (substring (kill-ring-images-entry-id entry) 0 12))
                    dir)))
        (with-temp-buffer
          (setq major-mode 'kill-ring-images-yank-test-markdown-mode)
          (insert-for-yank entry)
          (should (equal (buffer-string) (format "![](%s)" file)))
          (should (equal (kill-ring-images-test-read file) bytes)))))))

(ert-deftest kill-ring-images-yank-inserts-the-text-of-a-dropped-image ()
  ;; GIVEN a PNG entry whose image the store dropped
  ;; WHEN it is yanked
  ;; THEN the buffer gets the text of the entry without an image
  ;; AND a message says that the image is no longer stored
  (kill-ring-images-test-with-store 30
    (let ((entry (kill-ring-images-yank-test--entry
                  (kill-ring-images-test-png 1 1 "a"))))
      (kill-ring-images-store-put 'image/png
                                  (kill-ring-images-test-png 1 1 "b"))
      (with-temp-buffer
        (let ((messages (kill-ring-images-yank-test--messages
                         (lambda () (insert-for-yank entry)))))
          (should (equal (buffer-string) (substring-no-properties entry)))
          (should-not (get-text-property 1 'display))
          (should (member "kill-ring-images: this image is no longer stored"
                          messages)))))))

(ert-deftest kill-ring-images-yank-writes-an-image-file-once ()
  ;; GIVEN a PNG written to its file, whose contents then changed
  ;; WHEN the same image is written again
  ;; THEN the same file name comes back AND the changed contents stay
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test-in-directory dir
      (let* ((bytes (kill-ring-images-test-png 2 2))
             (id (kill-ring-images-store-put 'image/png bytes))
             (file (kill-ring-images-yank-write-file id 'image/png bytes dir)))
        (should (equal (file-name-nondirectory file)
                       (format "image-%s.png" (substring id 0 12))))
        (kill-ring-images-test-write file "kept")
        (should (equal (kill-ring-images-yank-write-file id 'image/png bytes
                                                         dir) file))
        (should (equal (kill-ring-images-test-read file) "kept"))))))

;;;; The preview after a yank

(defconst kill-ring-images-yank-test--link "[[file:x.png]]"
  "The link that the yank-media handler of the tests inserts.")

(defmacro kill-ring-images-yank-test--in-org (&rest body)
  "Run BODY in an org buffer that holds \"before \" and whose
yank-media handler inserts `kill-ring-images-yank-test--link'."
  (declare (indent 0))
  `(with-temp-buffer
     (setq major-mode 'kill-ring-images-yank-test-org-mode)
     (insert "before ")
     (yank-media-handler "image/.*"
                         (lambda (_type _data)
                           (insert kill-ring-images-yank-test--link)))
     ,@body))

(defun kill-ring-images-yank-test--yank-png ()
  "Yank the entry of a new PNG at point."
  (insert-for-yank
   (kill-ring-images-yank-test--entry (kill-ring-images-test-png 4 4))))

(defun kill-ring-images-yank-test--link-bounds ()
  "The bounds of the link after \"before \" in this buffer."
  (let ((beg (1+ (length "before "))))
    (list beg (+ beg (length kill-ring-images-yank-test--link)))))

(ert-deftest kill-ring-images-yank-previews-the-link-that-org-inserts ()
  ;; GIVEN an org buffer whose yank-media handler inserts an image link
  ;; WHEN a PNG entry is yanked there
  ;; THEN Org previews the region of that link by its own default rule,
  ;;      which leaves out links with a description, as C-c C-x C-v does
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-yank-test--in-org
      (let (previewed)
        (cl-letf (((symbol-function 'org-link-preview-region)
                   (lambda (&rest args) (setq previewed args))))
          (kill-ring-images-yank-test--yank-png))
        (should (equal previewed
                       (append '(nil nil)
                               (kill-ring-images-yank-test--link-bounds))))))))

(ert-deftest kill-ring-images-yank-previews-with-an-older-org ()
  ;; GIVEN an org buffer as above, in an Org older than 9.8, which
  ;;       has `org-display-inline-images' and no `org-link-preview-region'
  ;; WHEN a PNG entry is yanked there
  ;; THEN the older function previews the region of the link
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-yank-test--in-org
      (let (previewed)
        (cl-letf (((symbol-function 'org-link-preview-region) nil)
                  ((symbol-function 'org-display-inline-images)
                   (lambda (&rest args) (setq previewed args))))
          (kill-ring-images-yank-test--yank-png))
        (should (equal previewed
                       (append '(nil nil)
                               (kill-ring-images-yank-test--link-bounds))))))))

(ert-deftest kill-ring-images-yank-previews-the-markdown-link ()
  ;; GIVEN a markdown buffer that visits doc.md
  ;; WHEN a PNG entry is yanked there
  ;; THEN markdown-mode shows its images, after the link is in the buffer
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test-in-directory dir
      (kill-ring-images-yank-test--visiting (expand-file-name "doc.md" dir)
        (setq major-mode 'kill-ring-images-yank-test-markdown-mode)
        (let (shown)
          (cl-letf (((symbol-function 'markdown-display-inline-images)
                     (lambda () (setq shown (buffer-string)))))
            (kill-ring-images-yank-test--yank-png))
          (should shown)
          (should (string-prefix-p "![](images/image-" shown)))))))

(ert-deftest kill-ring-images-yank-previews-nothing-when-the-option-is-off ()
  ;; GIVEN an org buffer as above AND `kill-ring-images-yank-preview' nil
  ;; WHEN a PNG entry is yanked there
  ;; THEN the link is inserted AND Org is not asked for a preview
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-yank-test--in-org
      (let ((kill-ring-images-yank-preview nil)
            (previewed nil))
        (cl-letf (((symbol-function 'org-link-preview-region)
                   (lambda (&rest _) (setq previewed t))))
          (kill-ring-images-yank-test--yank-png))
        (should-not previewed)
        (should (equal (buffer-string)
                       (concat "before " kill-ring-images-yank-test--link)))))))

(ert-deftest kill-ring-images-yank-keeps-the-link-when-the-preview-fails ()
  ;; GIVEN an org buffer as above, whose preview signals an error
  ;; WHEN a PNG entry is yanked there
  ;; THEN the yank still inserts the link AND says that no preview came
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-yank-test--in-org
      (let ((messages
             (cl-letf (((symbol-function 'org-link-preview-region)
                        (lambda (&rest _) (error "No image support"))))
               (kill-ring-images-yank-test--messages
                #'kill-ring-images-yank-test--yank-png))))
        (should (equal (buffer-string)
                       (concat "before " kill-ring-images-yank-test--link)))
        (should (cl-some (lambda (m) (string-match-p "no preview" m))
                         messages))))))

(ert-deftest kill-ring-images-yank-previews-nothing-in-other-modes ()
  ;; GIVEN a buffer that is neither org nor markdown, with a yank-media
  ;;       handler for images
  ;; WHEN a PNG entry is yanked there
  ;; THEN neither Org nor markdown-mode is asked for a preview
  (kill-ring-images-test-with-store 1000
    (with-temp-buffer
      (yank-media-handler "image/.*" (lambda (_type _data) (insert "<img>")))
      (let (asked)
        (cl-letf (((symbol-function 'org-link-preview-region)
                   (lambda (&rest _) (setq asked t)))
                  ((symbol-function 'markdown-display-inline-images)
                   (lambda () (setq asked t))))
          (kill-ring-images-yank-test--yank-png))
        (should-not asked)
        (should (equal (buffer-string) "<img>"))))))

;;;; Terminals

(defun kill-ring-images-yank-test--sent (&rest contents)
  "A terminal function that returns the CONTENTS it was given."
  contents)

(defmacro kill-ring-images-yank-test--with-terminal (&rest body)
  "Run BODY with `kill-ring-images-yank-test--sent' as the advised terminal."
  (declare (indent 0))
  `(let ((kill-ring-images-yank-terminal-functions
          '(kill-ring-images-yank-test--sent)))
     (kill-ring-images-yank-install)
     (unwind-protect (progn ,@body)
       (kill-ring-images-yank-uninstall))))

(ert-deftest kill-ring-images-yank-sends-a-terminal-the-path-of-the-image ()
  ;; GIVEN a terminal with the advice AND a PNG entry
  ;; WHEN the entry and a plain text are sent together
  ;; THEN the terminal gets the path of the written PNG AND the plain text as it
  ;;      was
  (kill-ring-images-test-with-store 1000
    (kill-ring-images-test-in-directory dir
      (let* ((kill-ring-images-file-directory dir)
             (bytes (kill-ring-images-test-png 3 3))
             (entry (kill-ring-images-yank-test--entry bytes)))
        (kill-ring-images-yank-test--with-terminal
          (let ((sent (kill-ring-images-yank-test--sent entry "text")))
            (should (equal (cadr sent) "text"))
            (should (equal (kill-ring-images-test-read (car sent)) bytes))))))))

(ert-deftest kill-ring-images-yank-sends-a-terminal-the-text-of-a-dropped-image ()
  ;; GIVEN a terminal with the advice AND a PNG entry whose image the store
  ;;       dropped
  ;; WHEN the entry is sent
  ;; THEN the terminal gets the text of the entry
  ;; AND a message says that the image is no longer stored
  (kill-ring-images-test-with-store 30
    (let ((entry (kill-ring-images-yank-test--entry
                  (kill-ring-images-test-png 1 1 "a")))
          sent)
      (kill-ring-images-store-put 'image/png
                                  (kill-ring-images-test-png 1 1 "b"))
      (kill-ring-images-yank-test--with-terminal
        (let ((messages (kill-ring-images-yank-test--messages
                         (lambda ()
                           (setq sent
                                 (kill-ring-images-yank-test--sent entry))))))
          (should (equal sent (list (substring-no-properties entry))))
          (should (member "kill-ring-images: this image is no longer stored"
                          messages)))))))

(ert-deftest kill-ring-images-yank-uninstall-leaves-the-terminal-as-it-was ()
  ;; GIVEN a terminal whose advice was added and removed AND a PNG entry
  ;; WHEN the entry is sent
  ;; THEN the terminal gets the entry itself
  (kill-ring-images-test-with-store 1000
    (let ((entry
           (kill-ring-images-yank-test--entry (kill-ring-images-test-png 1 1)))
          (kill-ring-images-yank-terminal-functions
           '(kill-ring-images-yank-test--sent)))
      (kill-ring-images-yank-install)
      (kill-ring-images-yank-uninstall)
      (should (eq (car (kill-ring-images-yank-test--sent entry)) entry)))))

;;; kill-ring-images-yank-tests.el ends here
