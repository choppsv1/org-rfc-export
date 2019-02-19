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

(ert-deftest example-doc-xml-01 nil
  "Check that we produce the expected XML"
  (let ((tmpfile (make-temp-file "ert-ox-rfc"))
        (tmpfile2 (make-temp-file "ert-ox-rfc"))
        ;; This doesn't work :(
        ;; (org-export-new-reference (lambda (refs) (org-det-new-ref refs)))
        (rubs '("anchor=\"org[^\"]*\"" "target=\"org[^\"]*\""))
        tidyxml)
    (with-temp-buffer
      (insert-file-contents "../example.org")
      (setq tidyxml (ox-rfc-load-tidy-xml-as-string (ox-rfc-export-to-xml))))
    (dolist (x rubs)
      (setq tidyxml (replace-regexp-in-string x "" tidyxml)))
    (with-temp-file tmpfile
      (insert tidyxml))
    (with-temp-file tmpfile2
      (insert-file-contents "example-verify.xml")
      (dolist (x rubs)
        (replace-regexp x "" nil (point-min) (point-max))))
    (should
     (string= ""
              (shell-command-to-string (format "diff %s %s" tmpfile2 tmpfile))))))

;; (ert-deftest ietf-docs-test-cached ()
;;   :tags '(:causes-redisplay)
;;   (let* ((pathname (ietf-docs-fetch-to-cache "rfc12.txt" t))
;;          (messages-buffer (get-buffer-create "*Messages*"))
;;          (test (make-ert-test :body (lambda () (ietf-docs-fetch-to-cache "rfc12.txt")))))
;;     (with-current-buffer messages-buffer
;;       (let ((result (ert-run-test test)))
;;         (should (equal (concat "Cached path " pathname "\n")
;;                        (ert-test-result-messages result)))))))

;; (defmacro test-with-buffer (body)
;;   `(with-temp-buffer
;;      ;; do not remove trailing white-space!
;;      (insert " RFC-3999
;;  RFC 1222
;;  rfc 1029
;;  RFC5999
;;  draft-ietf-isis-01.txt
;;  draft-ietf-isis-03.xml
;;  draft-ietf-isis-02  ")
;;      ,body))


;; (ert-deftest ietf-docs-test-space ()
;;   (should (equal "rfc3999.txt" (test-with-buffer
;;                                 (progn
;;                                   (goto-char 2)
;;                                   (ietf-docs-at-point))))))

;; (ert-deftest ietf-docs-test-no-doc ()
;;   (should-not (test-with-buffer
;;                (progn (or (and (goto-char 1) (ietf-docs-at-point))
;;                           (and (end-of-line)  (ietf-docs-at-point))
;;                           (and (forward-line)  (ietf-docs-at-point))
;;                           (and (end-of-line)  (ietf-docs-at-point))
;;                           (and (forward-line)  (ietf-docs-at-point))
;;                           (and (end-of-line)  (ietf-docs-at-point))
;;                           (and (forward-line)  (ietf-docs-at-point))
;;                           (and (end-of-line)  (ietf-docs-at-point))
;;                           (and (forward-line)  (ietf-docs-at-point))
;;                           (and (end-of-line)  (ietf-docs-at-point))
;;                           (and (forward-line)  (ietf-docs-at-point))
;;                           (and (end-of-line)  (ietf-docs-at-point))
;;                           (and (forward-line)  (ietf-docs-at-point))
;;                           (and (end-of-line)  (ietf-docs-at-point)))))))

;; (ert-deftest ietf-docs-test-doc-line_2 ()
;;   (should (equal "rfc1222.txt" (test-with-buffer
;;                                 (progn (goto-char 1)
;;                                        (forward-line)
;;                                        (forward-char) (ietf-docs-at-point))))))

;; (ert-deftest ietf-docs-test-doc-line_3 ()
;;   (should (equal "rfc1029.txt" (test-with-buffer
;;                                 (progn (goto-char 1)
;;                                        (forward-line 2)
;;                                        (forward-char 4)
;;                                        (assert (char-equal ?\s (char-after)))
;;                                        (ietf-docs-at-point))))))

;; (ert-deftest ietf-docs-test-doc-line_4 ()
;;   (should (equal "rfc5999.txt" (test-with-buffer
;;                                 (progn (goto-char 1)
;;                                        (forward-line 3)
;;                                        (forward-char 2)
;;                                        (assert (char-equal ?F (char-after)))
;;                                        (ietf-docs-at-point))))))

;; (ert-deftest ietf-docs-test-doc-line_5 ()
;;   (should (equal "draft-ietf-isis-01.txt"
;;                  (test-with-buffer
;;                   (progn (goto-char 1)
;;                          (forward-line 4)
;;                          (forward-char 2)
;;                          (assert (char-equal ?r (char-after)))
;;                          (ietf-docs-at-point))))))

;; (ert-deftest ietf-docs-test-doc-line_6 ()
;;   (should (equal "draft-ietf-isis-03.txt"
;;                  (test-with-buffer
;;                   (progn (goto-char 1)
;;                          (forward-line 5)
;;                          (forward-char 8)
;;                          (assert (char-equal ?e (char-after)))
;;                          (ietf-docs-at-point))))))

;; (ert-deftest ietf-docs-test-doc-line_7 ()
;;   (should (equal "draft-ietf-isis-02.txt"
;;                  (test-with-buffer
;;                   (progn (goto-char 1)
;;                          (forward-line 6)
;;                          (forward-char 8)
;;                          (assert (char-equal ?d (char-after)))
;;                          (ietf-docs-at-point))))))

;; (ert-deftest ietf-docs-test-doc-line_7 ()
;;   (should (equal "draft-ietf-isis-02.txt"
;;                  (test-with-buffer
;;                   (progn (goto-char 1)
;;                          (forward-line 6)
;;                          (end-of-line)
;;                          (backward-char 2)
;;                          (assert (char-equal ?\s (char-after)))
;;                          (ietf-docs-at-point))))))

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
;;

;;; ox-rfc-test.el ends here
