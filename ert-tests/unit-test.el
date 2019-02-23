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

(setq utvals '((("RFC0791" "RFC.0791") ;; "RFC791" "RFC.791" "RFC 791"
                "http://www.rfc-editor.org/refs/bibxml/reference.RFC.0791.xml")
               (("I-D.chopps-isis-bfd-tlv")
                "http://xml2rfc.ietf.org/public/rfc/bibxml-ids/reference.I-D.chopps-isis-bfd-tlv.xml")
               (("W3C.CR-geolocation-API-20100907")
                "http://xml2rfc.ietf.org/public/rfc/bibxml-w3c/reference.W3C.CR-geolocation-API-20100907.xml")
               (("XEP-0043" "XEP0043" "XSF.XEP0043" "XSF.XEP-0043") ;; "XEP43"
                "http://www.xmpp.org/extensions/refs/reference.XSF.XEP-0043.xml")
               (("3GPP.36.457" "SDO-3GPP.36.457")
                "http://xml2rfc.ietf.org/public/rfc/bibxml-3gpp/reference.3GPP.36.457.xml")
               (("IEEE802.1Q_2014" "IEEE.802.1Q_2014")
               "http://xml2rfc.ietf.org/public/rfc/bibxml-ieee/reference.IEEE.802.1Q_2014.xml")
               (("ISO.10589.1992")
                "http://xml2rfc.ietf.org/public/rfc/bibxml-misc/reference.ISO.10589.1992.xml")))

(ert-deftest ref-url-01 nil
  "Test URL for reference fetching."
  (dolist (x utvals)
    (let ((variants (car x))
          (good (cadr x)))
      (dolist (v variants)
        (should (string= good (ox-rfc-std--url v)))))))

(ert-deftest ref-cache-name-01 nil
  "Test cache name for ref fetching."
  (dolist (x utvals)
    (let ((variants (car x))
          (good (concat ox-rfc-ref-cache-directory (replace-regexp-in-string ".*/" "" (cadr x)))))
      (dolist (v variants)
        (should (string= good (ox-rfc-std--cache-name v)))))))
