;;
;; February 19 2019, Christian Hopps <chopps@devhopps.com>
;;
;; Copyright (c) 2015 by Christian E. Hopps
;; All rights reserved.

;; This file is NOT part of GNU Emacs.

;;; License:

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(require 'ob)
(require 'ob-shell)
(require 'ox-rfc)

;;
;; Utility
;;

(defun ox-rfc-load-tidy-xml-as-string (pathname)
  "Pass PATHNAME through tidy and return a string"
  (let ((tidycmd (format "%s %s %s" (executable-find "tidy") ox-rfc-tidy-args pathname)))
    (message "tidy cmd is: '%s'" tidycmd)
    (shell-command-to-string tidycmd)))

(defun test-generated (infile &optional trytidy v2)
  "Check that we produce the expected XML for file named INFILE."
  (let ((ox-rfc-try-tidy trytidy)
        (ox-rfc-xml-version (if v2 2 3))
        (tmpfile (make-temp-file "ert-ox-rfc"))
        (tmpvfile (make-temp-file "ert-ox-rfc"))
        (rubs '("anchor=\"org[^\"]*\"" "target=\"org[^\"]*\""))
        tidyxml
        (verify-name (concat (file-name-base infile) "-verify.xml"))
        (org-export-babel-evaluate t)
        )
    (if v2 (setq verify-name (concat (file-name-base infile) "-verify-v2.xml"))
      (setq verify-name (concat (file-name-base infile) "-verify.xml")))

    (with-temp-buffer
      (insert-file-contents infile)
      (setq tidyxml (if trytidy
                        (ox-rfc-load-file-as-string (ox-rfc-export-to-xml))
                      (ox-rfc-load-tidy-xml-as-string (ox-rfc-export-to-xml)))))
    (if (string= "" (org-trim tidyxml))
        (error "Empty output from tidy"))
    (dolist (x rubs)
      (setq tidyxml (replace-regexp-in-string x "" tidyxml)))
    (with-temp-file tmpfile
      (insert tidyxml))
    (with-temp-file tmpvfile
      (insert (ox-rfc-load-tidy-xml-as-string verify-name))
      (dolist (x rubs)
        (replace-regexp x "" nil (point-min) (point-max)))
      (replace-regexp "1900-01-01" (format-time-string "%Y-%m-%d")))
    (let ((diffout (shell-command-to-string (format "diff -c %s %s" tmpvfile tmpfile))))
      (if (and diffout (not (string= "" diffout)))
          (message "DIFF: %s" diffout))
      (should
       (string= "" diffout)))))

(ert-deftest xml-basic-01 nil
  "Test basic XML generation"
  (test-generated "test-basic.org" nil nil))

(ert-deftest xml-basic-tidy-01 nil
  "Test basic XML generation"
  (test-generated "test-basic.org" t nil))

;; (ert-deftest xml-basic-v2-01 nil
;;   "Test basic XML generation"
;;   (test-generated "test-basic.org" t t)
;;   (test-generated "test-basic.org" nil t))

(ert-deftest xml-lists-01 nil
  "Test lists XML generation"
  "Check that we produce the expected XML"
  (test-generated "test-lists.org" nil))

(ert-deftest xml-lists-tidy-01 nil
  "Test lists XML generation"
  "Check that we produce the expected XML"
  (test-generated "test-lists.org" t))

;; (ert-deftest xml-lists-v2-01 nil
;;   "Test lists XML generation"
;;   "Check that we produce the expected XML"
;;   (test-generated "test-lists.org" nil t)
;;   (test-generated "test-lists.org" t t))

;; (ert-deftest xml-refs-01 nil
;;   "Test References XML generation"
;;   "Check that we produce the expected XML"
;;   (test-generated "test-refs.org" nil))

;; (ert-deftest xml-refs-tidy-01 nil
;;   "Test References XML generation"
;;   "Check that we produce the expected XML"
;;   (test-generated "test-refs.org" t))

;; (ert-deftest xml-lists-v2-01 nil
;;   "Test lists XML generation"
;;   "Check that we produce the expected XML"
;;   (test-generated "test-lists.org" nil t)
;;   (test-generated "test-lists.org" t t))

(ert-deftest xml-table-01 nil
  "Test Table XML generation"
  "Check that we produce the expected XML"
  (test-generated "test-table.org" nil))

(ert-deftest xml-table-tidy-01 nil
  "Test Table XML generation"
  "Check that we produce the expected XML"
  (test-generated "test-table.org" t))

;; (ert-deftest xml-table-v2-01 nil
;;   "Test lists XML generation"
;;   "Check that we produce the expected XML"
;;   (test-generated "test-table.org" nil t)
;;   (test-generated "test-table.org" t t))

(ert-deftest xml-yang-01 nil
  "Test YANG XML generation"
  "Check that we produce the expected XML"
  (let ((org-confirm-babel-evaluate nil)
        (org-export-use-babel t))
    (setq org-babel-load-languages '((shell . t) (yang . t)))
    (test-generated "test-yang.org" nil)))

;; (ert-deftest xml-yang-v2-01 nil
;;   "Test lists XML generation"
;;   "Check that we produce the expected XML"
;;   (let ((org-confirm-babel-evaluate nil)
;;         (org-export-use-babel t))
;;     (setq org-babel-load-languages '((bash . t) (shell . t) (yang . t)))
;;     (test-generated "test-yang.org" nil t)))
