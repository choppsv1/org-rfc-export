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


(ert-deftest has-feature-01 nil
  "Check that our feature loaded"
  (should (featurep 'ox-rfc)))


;; (defun org-export-new-reference (references)
;; (defun org-det-new-ref (references)
;;   "Return a unique reference with some determinism, among REFERENCES.
;; REFERENCES is an alist whose values are in-use references, as
;; numbers.  Returns a number, which is the internal representation
;; of a reference.  See also `org-export-format-reference'."
;;   ;; Generate random 7 digits hexadecimal numbers.  Collisions
;;   ;; increase exponentially with the numbers of references.  However,
;;   ;; the odds for encountering at least one collision with 1000 active
;;   ;; references in the same document are roughly 0.2%, so this
;;   ;; shouldn't be the bottleneck.
;;   (let ((new 1))                        ;; Use a hint, this is O(N^2)
;;     (while (rassq new references) (setq new (+ 1 new)))
;;     new))

(defun test-generated (infile)
  "Check that we produce the expected XML for file named INFILE."
  (let ((tmpfile (make-temp-file "ert-ox-rfc"))
        (tmpfile2 (make-temp-file "ert-ox-rfc"))
        (rubs '("anchor=\"org[^\"]*\"" "target=\"org[^\"]*\""))
        tidyxml
        (verify-name (concat (file-name-base infile) "-verify.xml"))
        )
    (with-temp-buffer
      (insert-file-contents infile)
      (setq tidyxml (ox-rfc-load-tidy-xml-as-string (ox-rfc-export-to-xml))))
    (dolist (x rubs)
      (setq tidyxml (replace-regexp-in-string x "" tidyxml)))
    (with-temp-file tmpfile
      (insert tidyxml))
    (with-temp-file tmpfile2
      (insert-file-contents (concat (file-name-base infile) "-verify.xml"))
      (dolist (x rubs)
        (replace-regexp x "" nil (point-min) (point-max))))
    (should
     (string= ""
              (shell-command-to-string (format "diff %s %s" tmpfile2 tmpfile))))))

(ert-deftest basic-xml-01 nil
  "Test basic XML generation"
  (test-generated "test-basic.org"))

(ert-deftest basic-xml-lists-01 nil
  "Test lists XML generation"
  "Check that we produce the expected XML"
  (test-generated "test-lists.org"))

