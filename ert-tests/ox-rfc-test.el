;;
;; February 19 2019, Christian Hopps <chopps@gmail.com>
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

(require 'ox-rfc)


;;
;; Utility
;;

(defun ox-rfc-load-tidy-xml-as-string (pathname)
  "Pass PATHNAME through tidy and return a string"
  (let ((tidycmd (format "%s %s %s" (executable-find "tidy") ox-rfc-tidy-args pathname)))
    (shell-command-to-string tidycmd)))

(defun test-generated (infile trytidy)
  "Check that we produce the expected XML for file named INFILE."
  (let ((ox-rfc-try-tidy trytidy)
        (tmpfile (make-temp-file "ert-ox-rfc"))
        (tmpvfile (make-temp-file "ert-ox-rfc"))
        (rubs '("anchor=\"org[^\"]*\"" "target=\"org[^\"]*\""))
        tidyxml
        (verify-name (concat (file-name-base infile) "-verify.xml"))
        )
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
        (replace-regexp x "" nil (point-min) (point-max))))
    (let ((diffout (shell-command-to-string (format "diff -c %s %s" tmpvfile tmpfile))))
      (if (and diffout (not (string= "" diffout)))
          (message "DIFF: %s" diffout))
      (should
       (string= "" diffout)))))

(ert-deftest has-feature-01 nil
  "Check that our feature loaded"
  (should (featurep 'ox-rfc)))

(ert-deftest basic-xml-01 nil
  "Test basic XML generation"
  (test-generated "test-basic.org" t)
  (test-generated "test-basic.org" nil))

(ert-deftest basic-xml-lists-01 nil
  "Test lists XML generation"
  "Check that we produce the expected XML"
  (test-generated "test-lists.org" nil)
  (test-generated "test-lists.org" t))
