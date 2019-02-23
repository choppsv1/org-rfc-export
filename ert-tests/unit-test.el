;;
;; February 23 2019, Christian E. Hopps <chopps@gmail.com>
;;
;; Copyright (c) 2019 by Christian E. Hopps
;; All rights reserved.
;;

(require 'ox-rfc)

;;
;; Utility
;;

(ert-deftest has-feature-01 nil
  "Check that our feature loaded"
  (should (featurep 'ox-rfc)))

(ert-deftest ref-names-01 nil
  "Test basic XML generation"
  (let ((good "reference.RFC.8122.xml"))
    (should (string= good (ox-rfc-std--basename "RFC8122")))
    (should (string= good (ox-rfc-std--basename "RFC.8122"))))

  (let ((good "reference.IEEE.802.11.xml"))
    (should (string= good (ox-rfc-std--basename "IEEE802.11")))
    (should (string= good (ox-rfc-std--basename "IEEE.802.11"))))

  ;; ...
)

(ert-deftest ref-url-01 nil
  "Test basic XML generation"
  (let ((good "http://www.rfc-editor.org/refs/bibxml/reference.RFC.8122.xml"))
    (should (string= good (ox-rfc-std--url "RFC8122")))
    (should (string= good (ox-rfc-std--url "RFC.8122"))))

  ;; ...
  )

(ert-deftest ref-cache-name-01 nil
  "Test basic XML generation"
  (let ((good (concat ox-rfc-ref-cache-directory "reference.RFC.8122.xml")))
    (should (string= good (ox-rfc-std--cache-name "RFC8122")))
    (should (string= good (ox-rfc-std--cache-name "RFC.8122"))))

  ;; ...
  )
