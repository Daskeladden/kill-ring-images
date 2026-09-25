;;; kill-ring-images-headers-tests.el --- Headers -*- lexical-binding: t -*-

;; Copyright (C) 2026 kill-ring-images contributors

;; This file is not part of GNU Emacs.

;;; Commentary:

;; What a stranger who installs the package meets first: the headers
;; that package.el reads, and the licence file the README points at.

;;; Code:

(require 'ert)
(require 'lisp-mnt)
(require 'package)

(defun kill-ring-images-test--library ()
  "The source file of the kill-ring-images library."
  (let ((file (locate-library "kill-ring-images.el")))
    (should file)
    file))

(defun kill-ring-images-test--package-info ()
  "What package.el reads from the headers of the library."
  (with-temp-buffer
    (insert-file-contents (kill-ring-images-test--library))
    (package-buffer-info)))

(ert-deftest kill-ring-images-package-headers-describe-the-package ()
  ;; GIVEN the library as it is published
  ;; WHEN package.el reads its headers
  ;; THEN it finds the name, a version, a one-line summary and a URL
  (let ((info (kill-ring-images-test--package-info)))
    (should (eq (package-desc-name info) 'kill-ring-images))
    (should (version-list-<= '(0 1) (package-desc-version info)))
    (should-not (equal (package-desc-summary info) "No description available."))
    (should (equal (cdr (assq :url (package-desc-extras info)))
                   "https://github.com/Daskeladden/kill-ring-images"))))

(ert-deftest kill-ring-images-package-requires-emacs-29-alone ()
  ;; GIVEN the library, which needs no other package
  ;; WHEN its requirements are read
  ;; THEN it asks for Emacs 29.1 and nothing else
  (should (equal (package-desc-reqs (kill-ring-images-test--package-info))
                 '((emacs (29 1))))))

(ert-deftest kill-ring-images-package-names-its-author-and-licence ()
  ;; GIVEN the library, published under the GPL
  ;; WHEN its headers and leading comments are read
  ;; THEN it names an author, holds the copyright of its contributors,
  ;;      AND carries a Commentary section and the licence notice
  (with-temp-buffer
    (insert-file-contents (kill-ring-images-test--library))
    (should (string-match-p "[^ ]" (or (lm-header "author") "")))
    (should (save-excursion
              (goto-char (point-min))
              (re-search-forward
               (concat "^;; Copyright (C) [0-9-]+ "
                       "kill-ring-images contributors$")
               nil t)))
    (should (lm-commentary-start))
    (should (save-excursion (re-search-forward "GNU General Public License" nil
                                               t)))))

(ert-deftest kill-ring-images-licence-file-is-the-whole-gpl ()
  ;; GIVEN a package that says GPL-3.0-or-later
  ;; WHEN the LICENSE file beside it is read
  ;; THEN it is the licence itself, not a summary that points elsewhere
  (let ((license (expand-file-name
                  "LICENSE" (file-name-directory
                             (kill-ring-images-test--library)))))
    (should (file-readable-p license))
    (with-temp-buffer
      (insert-file-contents license)
      (should (save-excursion (re-search-forward "TERMS AND CONDITIONS" nil t)))
      (should (save-excursion (re-search-forward "Version 3, 29 June 2007" nil
                                                 t))))))

(ert-deftest kill-ring-images-readme-names-what-other-packages-use ()
  ;; GIVEN the names that another package calls or hooks into
  ;; WHEN the README a stranger reads is searched for them
  ;; THEN each is named there, because a public extension point that
  ;;      nobody can find is one that nobody can use
  (let ((readme (expand-file-name
                 "README.md" (file-name-directory
                              (kill-ring-images-test--library)))))
    (should (file-readable-p readme))
    (with-temp-buffer
      (insert-file-contents readme)
      (dolist (name '("kill-ring-images-copy"
                      "kill-ring-images-copy-functions"
                      "kill-ring-images-image-functions"
                      "kill-ring-images-copy-to-x"
                      "kill-ring-images-yank-terminal-paste-functions"
                      "kill-ring-images-entry-thumbnail"))
        (goto-char (point-min))
        (should (search-forward (concat "`" name "`") nil t))))))

(provide 'kill-ring-images-headers-tests)
;;; kill-ring-images-headers-tests.el ends here
