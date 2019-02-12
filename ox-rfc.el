;;; ox-rfc.el --- RFC Back-End for Org Export Engine -*- lexical-binding: t -*-

;; Copyright (C) 2019 Christian E. Hopps
;; Copyright (C) 2012-2019 Free Software Foundation, Inc.

;; Author: Christian Hopps <chopps@gmail.com>
;; URL: https://github.com/choppsv1/org-rfc-export
;; Keywords: org, rfc, wp, xml
;; Package-Requires: ((emacs "24"))

:; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an RFC (xml2rfc) back-end for Org exporter, code was
;; taken or modified from the `md' and `html' back-ends. It exports into XML as
;; defined in RFC7991, and can then use `xml2rfc` to further convert to text.
;; See Org manual for more information.

;;; Credits:

;; Started with ox-md.el by Nicolas Goaziou as a template also used code from
;; ox-html.el by Carsten Dominik and Jambunathan K.

;;; Code:

(require 'cl-lib)
(require 'ox-publish)


;;; User-Configurable Variables

(defgroup org-export-rfc nil
  "Options specific to RFC export back-end."
  :tag "Org Export RFC"
  :group 'org-export)

(defcustom org-rfc-ref-cache-directory (expand-file-name (concat temporary-file-directory ".org-rfc-ref-cache/"))
  "Local directory to store downloaded IETF references. Created if necessary."
  :type 'directory
  :group 'org-export-rfc)

(defcustom org-rfc-ref-draft-url-directory "http://xml2rfc.ietf.org/public/rfc/bibxml-ids/"
  "The base URL to fetch IETF drafts references from."
  :type 'string
  :group 'org-export-rfc)

(defcustom org-rfc-ref-ieee-url-directory "http://xml2rfc.ietf.org/public/rfc/bibxml-ieee/"
  "The base URL to fetch IEEE references from."
  :type 'string
  :group 'org-export-rfc)

(defcustom org-rfc-ref-rfc-url-directory  "http://www.rfc-editor.org/refs/bibxml/"
  "The base URL to fetch IETF RFCs references from."
  :type 'string
  :group 'org-export-rfc)



;;; Define Back-End

(org-export-define-backend 'rfc
  '((bold . org-rfc-bold)
    (code . org-rfc-verbatim)
    ;; (entity . org-rfc-entity) ; What's this.
    (example-block . org-rfc-example-block)
    (export-block . org-rfc-export-block)
    (fixed-width . org-rfc-example-block)
    (headline . org-rfc-headline)
    (inline-src-block . org-rfc-verbatim)
    (inner-template . org-rfc-inner-template)
    (italic . org-rfc-italic)
    (item . org-rfc-item)
    (line-break . org-rfc-line-break)
    (link . org-rfc-link)
    (node-property . org-rfc-node-property) ; What's this.
    (paragraph . org-rfc-paragraph)
    (plain-list . org-rfc-plain-list)
    (plain-text . org-rfc-plain-text)
    (quote-block . org-rfc-quote-block)
    (section . org-rfc-section)
    (special-block . org-rfc-special-block)
    (src-block . org-rfc-src-block)
    (subscript . org-rfc-subscript)
    (superscript . org-rfc-superscript)
    (table . org-rfc-table)
    (table-cell . org-rfc-table-cell)
    (table-row . org-rfc-table-row)
    (template . org-rfc-template)
    ;; (underline . org-rfc-underline) ; Not supported by xml2rfc.
    (verbatim . org-rfc-verbatim)
    )
  :menu-entry
  '(?r "Export to RFC"
       ((?X "To XML temporary buffer"
	    (lambda (a s v b) (org-rfc-export-as-xml a s v)))
	(?x "To XML file" (lambda (a s v b) (org-rfc-export-to-xml a s v)))
        (?H "To HTML file and open."
            (lambda (a s v b) (if a
                                  (org-rfc-export-to-html t s v)
                                (org-open-file (org-rfc-export-to-html nil s v)))))
        ;; (?H "To HTML file" (lambda (a s v b) (org-rfc-export-to-html a s v)))
        (?P "To PDF file and open."
            (lambda (a s v b) (if a
                                  (org-rfc-export-to-pdf t s v)
                                (org-open-file (org-rfc-export-to-pdf nil s v)))))
        ;; (?p "To PDF file" (lambda (a s v b) (org-rfc-export-to-pdf a s v)))
        (?T "To TEXT temporary buffer" (lambda (a s v b) (org-rfc-export-as-text a s v)))
        ;; (?t "To TEXT file" (lambda (a s v b) (org-rfc-export-to-text a s v)))
	(?o "To TEXT file and open"
            (lambda (a s v b)
	      (if a
                  (org-rfc-export-to-text t s v)
                (org-open-file (org-rfc-export-to-text nil s v)))))))
  :options-alist
  '((:rfc-authors "RFC_AUTHORS" nil nil t)
    (:rfc-category "RFC_CATEGORY" nil "std" t)
    (:rfc-consensus "RFC_CONSENSUS" nil "true" t)
    (:rfc-ipr "RFC_IPR" nil "trust200902" t)
    (:rfc-name "RFC_NAME" nil nil t)
    (:rfc-stream "RFC_STREAM" nil "IETF" t)
    (:rfc-version "RFC_VERSION" nil nil t)
    (:rfc-xml-version "RFC_XML_VERSION" nil "3" t)
    ))


;;; Utility Functions

(defun org-rfc-ref-fetch-to-cache (basename &optional reload)
  "Fetch the bibliography xml.
The document is identified by BASENAME. If RELOAD is specified
then the cache is overwritten."
  (let* ((pathname (concat (file-name-as-directory org-rfc-ref-cache-directory) "reference." basename ".xml"))
         url)
    (unless (and (file-exists-p pathname) (not reload))
      (make-directory org-rfc-ref-cache-directory t)
      (cond
       ((string-prefix-p "RFC" basename)
        (setq url (concat org-rfc-ref-rfc-url-directory (concat "reference." basename ".xml"))))
       ((string-prefix-p "I-D" basename)
        (setq url (concat org-rfc-ref-draft-url-directory (concat "reference." basename ".xml"))))
       ((string-prefix-p "IEEE" basename)
        (setq url (concat org-rfc-ref-ieee-url-directory (concat "reference." basename ".xml"))))
       (t (error)))
      (url-copy-file url pathname t)
      pathname)
    pathname))

(defun org-rfc-load-file-as-string (pathname)
  "Return a string containing file PATHNAME."
  (with-temp-buffer
    (insert-file-contents pathname)
    (buffer-string)))

(defun org-rfc-load-ref-file-as-string (pathname)
  "Return a string containing file PATHNAME for use as a reference."
  (replace-regexp-in-string "<\\?xml [^>]+>" ""
                            (org-rfc-load-file-as-string pathname)))

(defun org-rfc-docname-from-buffer ()
  "Get the internet draft name using the buffer's filename."
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))))

(defun org-rfc-export-output-file-name (extension)
  "Get the I-D document name adding the given EXTENSION."
  (let ((docname (plist-get (org-export-get-environment 'rfc) :rfc-name))
        (verstr (plist-get (org-export-get-environment 'rfc) :rfc-version)))
    (if (not verstr)
        (error "#+RFC_VERSION: must be provided for export"))
    (if (not docname)
        (org-export-output-file-name (concat "-" verstr extension))
      (concat docname "-" verstr extension))))

(defun org-rfc-author-list-from-prop (pname &optional item)
  "Return the XML for and author or list of authors.
The author list is looked for in ITEM using property named PNAME."
  (let ((author (if item
                    (org-element-property pname item)
                  (plist-get (org-export-get-environment 'rfc) pname)))
        (lfmt (concat "<author fullname=\"%s\">\n"
                      "  <organization>%s</organization>\n"
                      "  <address><email>%s</email></address>\n"
                      "</author>"))
        (sfmt "<author fullname=\"%s\"/>"))
    (if (string-prefix-p "(" author)
        (mapconcat
         (lambda (x) (if (not (listp x))
                         (format sfmt x)
                       (format lfmt (car x) (caddr x) (cadr x))))
         (read author)
         "\n")
      (format sfmt author))))

(defun org-rfc-render-v3 ()
  "Return t if rendering in xml2rfc version 3 format."
  (let ((v (plist-get (org-export-get-environment 'rfc) :rfc-xml-version)))
    (not (or (not v) (< (string-to-number v) 3)))))


;;; Transcode Functions

;;;; Bold

(defun org-rfc-bold (_bold contents _info)
  "Transcode BOLD object into XML format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "<em>%s</em>" contents))


;;;; Example Block, Src Block and Export Block

(defun org-rfc-example-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into RFC format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."

  (let* ((name (org-element-property :name example-block))
         (nameattr (if name (format "<name>%s</name>" name) "")))
    (if name
        (concat (format "<figure>%s<artwork><![CDATA[\n" nameattr)
                (org-export-format-code-default example-block info)
                "]]></artwork></figure>")
      (concat "<artwork><![CDATA[\n"
              (org-export-format-code-default example-block info)
              "]]></artwork>"))))


(defun org-rfc-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to RFC.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (member (org-element-property :type export-block) '("RFC" "RFC"))
      (org-remove-indentation (org-element-property :value export-block))
    ;; Also include HTML export blocks.
    (org-export-with-backend 'rfc export-block contents info)))


(defun org-rfc-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((block-type (org-element-property :type special-block)))
    (cond
     ((string= (downcase block-type) "abstract")
      (plist-put info :abstract (format "<abstract>%s</abstract>" (org-trim contents)))
      "")
     ;; ((string= (downcase block-type) "xml")
     ;;  (org-remove-indentation (org-element-property :value special-block)))
     (t (org-trim contents)))))

(defun org-rfc-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into RFC format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((name (org-element-property :name src-block))
         (nameattr (if name (format "<name>%s</name>" name) ""))
         (figopen (if name (format "<figure>%s" nameattr)) "")
         (figclose (if name "/<figure>" "")))
    (if (org-rfc-render-v3)
        ;; Do we always want figure? Or like export block only when there's a name?
        (concat figopen "<sourcecode><![CDATA[\n"
                ;; (org-remove-indentation
                ;;  (org-export-format-code-default example-block info))
                (org-export-format-code-default src-block info)
                "]]></sourcecode>" figclose)
      (concat figopen "<artwork><![CDATA[\n"
              ;; (org-remove-indentation
              ;;  (org-export-format-code-default example-block info))
              (org-export-format-code-default src-block info)
              "]]></artwork>" figclose))))

;;;; Headline

(defun org-rfc-headline (headline contents info)
  "Transcode HEADLINE element into RFC format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (ptitle (org-export-data (org-element-property :title (org-export-get-parent headline)) info))
           (title (org-trim (org-export-data (org-element-property :title headline) info)))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   ;; Headline text without tags.
	   (heading (concat todo title)))
      (let ((anchor (or (and (org-rfc--headline-referred-p headline info)
		             (format " anchor=\"%s\""
			             (or (org-element-property :CUSTOM_ID headline)
			                 (org-export-get-reference headline info))))
                        "")))
        (cond
         ((string= "References" title) (plist-put info :in-back t)
          (concat "</middle>\n<back>\n" contents))
         ((string= "Normative References" title)
          (format "<references title=\"%s\">\n%s</references>\n" title contents))
         ((string= "Informative References" title)
          (format "<references title=\"%s\">\n%s</references>\n" title contents))
         ((string= "Normative References" ptitle)
          (org-rfc-reference headline contents info))
         ((string= "Informative References" ptitle)
          (org-rfc-reference headline contents info))
         (t (format "<section title=\"%s\"%s>\n%s\n</section>\n" title anchor
                 contents)))))))

(defun org-rfc--headline-referred-p (headline info)
  "Non-nil when HEADLINE is being referred to.
INFO is a plist used as a communication channel.  Links and table
of contents can refer to headlines."
  (unless (org-element-property :footnote-section-p headline)
    (or
     ;; Global table of contents includes HEADLINE.
     (and (plist-get info :with-toc)
	  (memq headline
		(org-export-collect-headlines info (plist-get info :with-toc))))
     ;; A local table of contents includes HEADLINE.
     (cl-some
      (lambda (h)
	(let ((section (car (org-element-contents h))))
	  (and
	   (eq 'section (org-element-type section))
	   (org-element-map section 'keyword
	     (lambda (keyword)
	       (when (equal "TOC" (org-element-property :key keyword))
		 (let ((case-fold-search t)
		       (value (org-element-property :value keyword)))
		   (and (string-match-p "\\<headlines\\>" value)
			(let ((n (and
				  (string-match "\\<[0-9]+\\>" value)
				  (string-to-number (match-string 0 value))))
			      (local? (string-match-p "\\<local\\>" value)))
			  (memq headline
				(org-export-collect-headlines
				 info n (and local? keyword))))))))
	     info t))))
      (org-element-lineage headline))
     ;; A link refers internally to HEADLINE.
     (org-element-map (plist-get info :parse-tree) 'link
       (lambda (link)
	 (eq headline
	     (pcase (org-element-property :type link)
	       ((or "custom-id" "id") (org-export-resolve-id-link link info))
	       ("fuzzy" (org-export-resolve-fuzzy-link link info))
	       (_ nil))))
       info t))))

(defun org-rfc--headline-title (style level title &optional anchor tags)
  "Generate a headline title in the preferred RFC headline style.
STYLE is the preferred style (`atx' or `setext').  LEVEL is the
header level.  TITLE is the headline title.  ANCHOR is the HTML
anchor tag for the section as a string.  TAGS are the tags set on
the section."
  (let ((anchor-lines (and anchor (concat anchor "\n\n"))))
    ;; Use "Setext" style
    (if (and (eq style 'setext) (< level 3))
        (let* ((underline-char (if (= level 1) ?= ?-))
               (underline (concat (make-string (length title) underline-char)
				  "\n")))
          (concat "\n" anchor-lines title tags "\n" underline "\n"))
        ;; Use "Atx" style
        (let ((level-mark (make-string level ?#)))
          (concat "\n" anchor-lines level-mark " " title tags "\n\n")))))

;;;; Italic

(defun org-rfc-italic (_italic contents _info)
  "Transcode ITALIC object into RFC format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (if (org-rfc-render-v3)
      (format "<em>%s</em>" contents)
    contents))

;;;; Item


(defun org-rfc-item (item contents info)
  "Transcode ITEM element into RFC format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (pname (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "-"
		   (concat (number-to-string
			    (car (last (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct)))))
			   "."))))
    (let ((tag (org-element-property :tag item)))
      (cond
       (tag (if (org-rfc-render-v3)
	        (format "<dt>%s</dt> <dd>%s</dd>" (org-export-data tag info) contents)
	      (replace-regexp-in-string "<t>" (format "<t hangText=\"%s:\">" (org-export-data tag info)) contents)))
       (t (concat bullet
	          (and contents
		       (concat "<t>" (org-trim contents) "</t>"))))))))

;;;; Line Break

(defun org-rfc-line-break (_line-break _contents _info)
  "Transcode LINE-BREAK object into RFC format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (if (org-rfc-render-v3) "<br>" "<vspace/>"))


;;;; Link

;;;; XXX Not done testing this conversion from org-md yet.

(defun org-rfc-link (link contents info)
  "Transcode LINK object into RFC format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((link-org-files-as-rfc
	 (lambda (raw-path)
	   ;; Treat links to `file.org' as links to `file.xml'.
	   (if (string= ".org" (downcase (file-name-extension raw-path ".")))
	       (concat (file-name-sans-extension raw-path) ".xml")
	     raw-path)))
	(type (org-element-property :type link)))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link contents 'rfc))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  (`headline
	   (let* ((dparent (org-export-get-parent-element destination))
                  (dpparent (org-export-get-parent-element dparent))
                  (ppname (org-element-property :raw-value dpparent)))
	     (if (string= "References" ppname)
                 (let ((xtarget (org-export-data (org-element-property :title destination) info)))
                   (format "<xref target=\"%s\"/>" xtarget))
               ;; Need to normalize references to allow for non leading zeros
               (format
                "<xref%s target=\"%s\">%s</xref>"
	        "" ;; None isn't working! (if (org-string-nw-p contents) " format=\"none\"" "")
	        ;; Reference.
	        (or (org-element-property :CUSTOM_ID destination)
		    (org-export-get-reference destination info))
	        ;; Description.
	        (cond ((org-string-nw-p contents))
		      (t ""))
                ))))
	  (_
	   (let ((description
		  (or (org-string-nw-p contents)
		      (let ((number (org-export-get-ordinal destination info)))
			(cond
			 ((not number) nil)
			 ((atom number) (number-to-string number))
			 (t (mapconcat #'number-to-string number ".")))))))
	     (when description
	       (format "<xref target=\"%s\">%s</xref>"
		       (org-export-get-reference destination info)
                       contents)))))))
     ;; Need to test this case.
     (t (let* ((raw-path (org-element-property :path link))
	       (path
		(cond
		 ((member type '("http" "https" "ftp" "mailto"))
		  (concat type ":" raw-path))
		 ((string= type "file")
		  (org-export-file-uri (funcall link-org-files-as-rfc raw-path)))
		 (t raw-path))))
	  (if (not contents) (format "<eref target=\"%s\"/>" path)
	    (format "<eref target=\"%s\">%s</eref>" path contents)))))))


;;;; Node Property (XXX what's this used for?)

(defun org-rfc-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element into RFC syntax.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))

;;;; Paragraph

(defun org-rfc-paragraph (paragraph contents _info)
  "Transcode PARAGRAPH element into RFC format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (format "<t>%s</t>" (org-trim contents)))


;;;; Plain List

(defun org-rfc-plain-list (plain-list contents _info)
  "Transcode PLAIN-LIST element into RFC format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  (if (org-rfc-render-v3)
      (let* ((type (pcase (org-element-property :type plain-list)
	             (`ordered "ol")
	             (`unordered "ul")
	             (`descriptive "dl")
	             (other (error "Unknown HTML list type: %s" other)))))
        (format "<%s>\n%s</%s>" type contents type))
    (let* ((style (pcase (org-element-property :type plain-list)
	           (`ordered "numbers")
	           (`unordered "symbols")
	           (`descriptive "hanging")
	           (other (error "Unknown HTML list type: %s" other)))))
      (format "<t><list style=\"%s\">\n%s</list></t>" style contents))))


;;;; Plain Text

(defun org-rfc-plain-text (text info)
  "Convert plain text characters from TEXT to HTML equivalent.
INFO is a plist used as a communication channel."
  (let ((protect-alist '(("&" . "&amp;")
                         ("<" . "&lt;")
                         (">" . "&gt;"))))
    (dolist (pair protect-alist text)
      (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))))


;;;; Quote Block

(defun org-rfc-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into RFC format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (if (org-rfc-render-v3)
      (format "<blockquote>%s</blockquote>" (org-trim contents))
    (org-rfc-example-block quote-block contents info)))

;;;; Reference (headline)

(defun org-rfc-reference (headline _contents info)
  "Transcode reference to HEADLINE into RFC format.
INFO is a plist used as a communication channel."
  (let ((title (org-export-data (org-element-property :title headline) info)))
    (cond
     ((string-prefix-p "RFC" title t)
      (let ((rfcref (format "%04d" (string-to-number (substring title 3)))))
        (org-rfc-load-ref-file-as-string (org-rfc-ref-fetch-to-cache (concat "RFC." rfcref)))))
     ((string-prefix-p "I-D." title t)
      (org-rfc-load-ref-file-as-string (org-rfc-ref-fetch-to-cache title)))
     (t
      ;; (message "PROP: %s %s" title (org-element-context headline))
      (let ((refann (org-trim (org-export-data (org-element-property :REF_ANNOTATION headline) info)))
            (author (org-rfc-author-list-from-prop :REF_AUTHOR headline))
            (refcontent (org-trim (org-export-data (org-element-property :REF_CONTENT headline) info)))
            (refdate (org-export-data (org-element-property :REF_DATE headline) info))
            (reftitle (org-export-data (org-element-property :REF_TITLE headline) info)))
        (if (not reftitle)
            (setq reftitile title))
        (if (not refdate)
            (setq refdate "")
          (let* ((date (nthcdr 3 (parse-time-string refdate)))
                 (day (and (car date)))
                 (month (cadr date))
                 (year (caddr date)))
            (setq day (or (and day (format " day=\"%s\"" day)) ""))
            (setq month (or (and month (format " month=\"%s\"" month)) ""))
            (setq year (or (and year (format " year=\"%s\"" year)) ""))
            (setq refdate (format "<date%s%s%s/>" day month year))))
        (if (not refcontent)
            (setq refcontent "")
          (setq refcontent (format "<refcontent>%s</refcontent>" refcontent)))
        (if (not refann)
            (setq refann "")
          (setq refann (format "<annotation>%s</annotation>" refann)))
        (if (org-rfc-render-v3)
            (format "<reference anchor=\"%s\"><front><title>%s</title>%s%s</front>%s%s</reference>" title reftitle author refdate refcontent refann)
          (format "<reference anchor=\"%s\"><front><title>%s</title>%s%s</front>%s</reference>" title reftitle author refdate refann)))))))

;;;; Section

(defun org-rfc-section (_section contents _info)
  "Transcode SECTION element into RFC format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

;;;; Subscript

(defun org-rfc-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-rfc-render-v3)
      (format "<sub>%s</sub>" contents)
    contents))

;;;; Superscript

(defun org-rfc-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-rfc-render-v3)
      (format "<sup>%s</sup>" contents)
    contents))


;;;; Template

(defun org-rfc-inner-template (contents info)
  "Return body of document after converting it to RFC syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  ;; Make sure CONTENTS is separated from table of contents and
  ;; footnotes with at least a blank line.
  ;; Table of contents.
  (let ((author (plist-get info :author))
        (category (or (plist-get info :rfc-category) "std"))
        (consensus (or (plist-get info :rfc-consensus) "yes"))
        (company (plist-get info :company))
        (depth (plist-get info :with-toc))
        (docname (org-rfc-export-output-file-name ""))
        (email (plist-get info :email))
        (ipr (or (plist-get info :rfc-ipr) "trust200902"))
        (stream (or (plist-get info :rfc-stream) "IETF"))
        (title (org-export-data (plist-get info :title) info))
        (with-toc (if (plist-get info :with-toc) "yes" "no"))
        (toc-inc (if (plist-get info :with-toc) "true" "false"))
        )
    (concat
     ;; Replace this with actual code.
     ;; <?rfc toc=\"" with-toc "\" ?>
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

     (if (not (org-rfc-render-v3))
         "<!DOCTYPE rfc SYSTEM \"rfc2629.dtd\" [
  ]>\n"
       "")
"<?xml-stylesheet type=\"text/xsl\" href=\"rfc2629.xslt\"?>
<?rfc toc=\"" with-toc "\"?>
<?rfc compact=\"no\"?>
<?rfc subcompact=\"no\"?>
<?rfc symrefs=\"yes\" ?>
<?rfc sortrefs=\"yes\"?>
<?rfc iprnotified=\"no\"?>
<?rfc strict=\"yes\"?>
<rfc ipr=\"" ipr "\"
     category=\"" category "\"
     docName=\"" docname "\"
     submissionType=\"" stream "\""
     (if (org-rfc-render-v3)
         (concat
          "    consensus=\"" consensus "\""
          "    tocInclude=\"" toc-inc "\""
          "    version=\"3\""))
     ">
  <front>
    <title abbrev=\"" title "\">" title "</title>\n"
  (org-rfc-author-list-from-prop :rfc-authors)
  "  <date/>"
  (or (plist-get info :abstract) "")
  "  </front>"
  "  <middle>\n"
  ;; Document contents.
  contents
  "  </back>
</rfc>")))

(defun org-rfc-template (contents _info)
  "Return complete document string after RFC conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)


;;;; Table

(defun org-rfc-table (table contents info)
  "Transcode a TABLE element from Org to RFC format.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (not (org-rfc-render-v3))
      (org-rfc-example-block table contents info)
    (if (eq (org-element-property :type table) 'table.el)
        ;; "table.el" table.  Convert it using appropriate tools.
        (org-rfc-table--table.el-table table info)
      ;; Standard table.
      (let* ((caption (org-export-get-caption table))
	     (number (org-export-get-ordinal
		      table info nil #'org-html--has-caption-p))
	     (alignspec
	      (if (bound-and-true-p org-html-format-table-no-css)
		  "align=\"%s\""
	        "class=\"org-%s\"")))
        (format "<table>\n%s\n%s</table>"
	        (if (not caption) ""
		  (format "<name>%s</name>" (org-export-data caption info)))
	        contents))
        )))

(defun org-rfc-table-first-row-data-cells (table info)
  "Transcode the first row of TABLE.
INFO is a plist used as a communication channel."
  (let ((table-row
	 (org-element-map table 'table-row
	   (lambda (row)
	     (unless (eq (org-element-property :type row) 'rule) row))
	   info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun org-rfc-table--table.el-table (table _info)
  "Format table.el TABLE into HTML.
INFO is a plist used as a communication channel."
  (when (eq (org-element-property :type table) 'table.el)
    (require 'table)
    (let ((outbuf (with-current-buffer
		      (get-buffer-create "*org-export-table*")
		    (erase-buffer) (current-buffer))))
      (with-temp-buffer
	(insert (org-element-property :value table))
	(goto-char 1)
	(re-search-forward "^[ \t]*|[^|]" nil t)
	(table-generate-source 'html outbuf))
      (with-current-buffer outbuf
	(prog1 (org-trim (buffer-string))
	  (kill-buffer))))))

(defun org-rfc-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to RFC format.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (not (org-rfc-render-v3))
      contents
    (when (eq (org-element-property :type table-row) 'standard)
      (let* ((group (org-export-table-row-group table-row info))
	     (number (org-export-table-row-number table-row info))
	     (start-group-p
	      (org-export-table-row-starts-rowgroup-p table-row info))
	     (end-group-p
	      (org-export-table-row-ends-rowgroup-p table-row info))
	     (topp (and (equal start-group-p '(top))
		        (equal end-group-p '(below top))))
	     (bottomp (and (equal start-group-p '(above))
			   (equal end-group-p '(bottom above))))
             (row-open-tag "<tr>")
             (row-close-tag "</tr>")
	     (group-tags
	      (cond
	       ;; Row belongs to second or subsequent groups.
	       ((not (= 1 group)) '("<tbody>" . "\n</tbody>"))
	       ;; Row is from first group.  Table has >=1 groups.
	       ((org-export-table-has-header-p
	         (org-export-get-parent-table table-row) info)
	        '("<thead>" . "\n</thead>"))
	       ;; Row is from first and only group.
	       (t '("<tbody>" . "\n</tbody>")))))
        (concat (and start-group-p (car group-tags))
	        (concat row-open-tag contents row-close-tag)
	        (and end-group-p (cdr group-tags)))))))

(defun org-rfc-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to RFC format.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (not (org-rfc-render-v3))
      contents
    (let* ((table-row (org-export-get-parent table-cell))
	   (table (org-export-get-parent-table table-cell))
	   (cell-attrs ""))
      (when (or (not contents) (string= "" (org-trim contents)))
        (setq contents "&#xa0;"))
      (cond
       ((and (org-export-table-has-header-p table info)
	     (= 1 (org-export-table-row-group table-row info)))
        (let ((header-tags '("<th%s>" . "</th>")))
	  (concat (format (car header-tags) cell-attrs)
		  contents
		  (cdr header-tags))))
       (t (let ((data-tags '("<td%s>" . "</td>")))
	    (concat (format (car data-tags) cell-attrs)
		    contents
		    (cdr data-tags))))))))

;;;; Verbatim

(defun org-rfc-verbatim (verbatim _contents _info)
  "Transcode VERBATIM object into RFC format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (if (org-rfc-render-v3)
        (format "<tt>%s</tt>" value)
      (format "<spanx>%s</spanx>" value))))


;;; Interactive function

;;;###autoload
(defun org-rfc-export-as-xml (&optional async subtreep visible-only)
  "Export current buffer to a XML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RFC Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'rfc "*Org RFC Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(defun org-rfc-export-as-text (&optional async subtreep visible-only buffer outfile)
  "Export current buffer to a XML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

If BUFFER is not given, export is done in a buffer named \"*Org
RFC Export*\", which will be displayed when
`org-export-show-temporary-export-buffer' is non-nil.

If OUTFILE is non-nil the results are stored in that file."
  (interactive)
  (if (not buffer) (setq buffer "*Org RFC TEXT Export*"))
  (if (not outfile) (setq outfile "/dev/stdout"))
  (org-export-to-buffer 'rfc buffer
    async subtreep visible-only nil nil
    (lambda ()
      (let ((exitcode (shell-command-on-region
                       (point-min)
                       (point-max)
                       (format "xml2rfc --quiet -o %s --text /dev/stdin" outfile)
                       (current-buffer) t "*Org RFC Error*" t)))
        (deactivate-mark)
        (if exitcode
            (pop-to-buffer "*Org RFC Error*"))))))

;;;###autoload
(defun org-rfc-export-to-xml (&optional async subtreep visible-only)
  "Export current buffer to a XML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-rfc-export-output-file-name ".xml")))
    (org-export-to-file 'rfc outfile async subtreep visible-only)))

(defun org-rfc-export-to-x (ext cli-arg &optional async subtreep visible-only)
  "Export the current buffer to a file with the extension EXT.
CLI-ARG is passed to xml2rfc to specify the file type.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (let ((xmlfile (org-rfc-export-output-file-name ".xml"))
        (outfile (org-rfc-export-output-file-name ext)))
    (org-export-to-file
        'rfc xmlfile async subtreep visible-only nil nil
        (lambda (file) (shell-command
                        (format "xml2rfc --quiet %s -o %s %s" cli-arg outfile xmlfile))
          outfile))))


;;;###autoload
(defun org-rfc-export-to-html (&optional async subtreep visible-only)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (org-rfc-export-to-x ".html" "--html" async subtreep visible-only))


;;;###autoload
(defun org-rfc-export-to-pdf (&optional async subtreep visible-only)
  "Export current buffer to a PDF file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (org-rfc-export-to-x ".pdf" "--pdf" async subtreep visible-only))


;;;###autoload
(defun org-rfc-export-to-text (&optional async subtreep visible-only)
  "Export current buffer to a TEXT file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (org-rfc-export-to-x ".txt" "--text" async subtreep visible-only))


;;;###autoload
(defun org-rfc-publish-to-xml (plist filename pub-dir)
  "Publish an org file to RFC.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'rfc filename ".xml" plist pub-dir))

(provide 'ox-rfc)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-rfc.el ends here
