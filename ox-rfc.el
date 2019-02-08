;;; ox-rfc.el --- RFC Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Christian E. Hopps

;; Author: Christian Hopps <chopps@gmail.com>
;; Keywords: org, rfc

;;; Commentary:

;; This library implements an RFC back-end for Org exporter, based on `md'
;; and `man' back-ends. It exports into XML defined in RFC7749.
;; See Org manual for more information.

;;; Code:

(require 'cl-lib)
(require 'ox-html)
(require 'ox-publish)


;;; User-Configurable Variables

(defgroup org-export-rfc nil
  "Options specific to RFC export back-end."
  :tag "Org Export XML2RC"
  :group 'org-export)


;;; Define Back-End
(defun org-rfc-company (info)
  (message "gotcompany"))



(org-export-define-derived-backend 'rfc 'html
  :filters-alist '((:filter-parse-tree .
                                       (org-rfc-separate-elements)))
  :menu-entry

  '(?x "Export to RFC"
       ((?X "To XML temporary buffer"
	    (lambda (a s v b) (org-rfc-export-as-xml a s v)))
	(?x "To XML file" (lambda (a s v b) (org-rfc-export-to-xml a s v)))
        (?T "To TEXT temporary buffer"
	    (lambda (a s v b) (org-rfc-export-as-text a s v)))
	(?o "To XML file and open"
	    (lambda (a s v b)
	      (if a (org-rfc-export-to-xml t s v)
		(org-open-file (org-rfc-export-to-xml nil s v))))))
       )
  :translate-alist '(
                     (bold . org-rfc-bold)
		     (code . org-rfc-verbatim)
		     (example-block . org-rfc-example-block)
		     (export-block . org-rfc-export-block)
		     (fixed-width . org-rfc-example-block)
		     (headline . org-rfc-headline)
		     (horizontal-rule . org-rfc-horizontal-rule)
		     (inline-src-block . org-rfc-verbatim)
		     (inner-template . org-rfc-inner-template)
		     (italic . org-rfc-italic)
		     (item . org-rfc-item)
		     (keyword . org-rfc-keyword)
		     (line-break . org-rfc-line-break)
		     (link . org-rfc-link)
		     (node-property . org-rfc-node-property)
		     (paragraph . org-rfc-paragraph)
		     (plain-list . org-rfc-plain-list)
		     (property-drawer . org-rfc-property-drawer)
		     (quote-block . org-rfc-quote-block)
		     (section . org-rfc-section)
                     (special-block . org-rfc-special-block)
		     (src-block . org-rfc-example-block)
                     (subscript . org-html-subscript)
                     (superscript . org-html-superscript)
		     (template . org-rfc-template)
		     (verbatim . org-rfc-verbatim)
                     )
  :options-alist
  '((:rfc-category "RFC_CATEGORY" nil nil t)
    (:rfc-ipr "RFC_IPR" nil nil t)
    (:rfc-stream "RFC_STREAM" nil nil t)
    (:rfc-consensus "RFC_CONSENSUS" nil nil t)
    ))

  ;; '((:rfc-footnote-format nil nil org-rfc-footnote-format)
  ;;   (:rfc-footnotes-section nil nil org-rfc-footnotes-section)
  ;;   (:rfc-headline-style nil nil org-rfc-headline-style)))


(defun org-rfc-separate-elements (tree _backend info)
  "Fix blank lines between elements.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Enforce a blank line between elements.  There are two exceptions
to this rule:

  1. Preserve blank lines between sibling items in a plain list,

  2. In an item, remove any blank line before the very first
     paragraph and the next sub-list when the latter ends the
     current item.

Assume BACKEND is `rfc'."
  (org-element-map tree (remq 'item org-element-all-elements)
    (lambda (e)
      (org-element-put-property
       e :post-blank
       (if (and (eq (org-element-type e) 'paragraph)
		(eq (org-element-type (org-element-property :parent e)) 'item)
		(org-export-first-sibling-p e info)
		(let ((next (org-export-get-next-element e info)))
		  (and (eq (org-element-type next) 'plain-list)
		       (not (org-export-get-next-element next info)))))
	   0
	 1))))
  ;; Return updated tree.
  tree)



;;; Transcode Functions

;;;; Bold

(defun org-rfc-bold (_bold contents _info)
  "Transcode BOLD object into XML format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "<em>%s</em>" contents))


;;;; Code and Verbatim

(defun org-rfc-verbatim (verbatim _contents _info)
  "Transcode VERBATIM object into RFC format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (format (cond ((not (string-match "`" value)) "`%s`")
		  ((or (string-prefix-p "`" value)
		       (string-suffix-p "`" value))
		   "`` %s ``")
		  (t "``%s``"))
	    value)))


;;;; Example Block, Src Block and Export Block

(defun org-rfc-example-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into RFC format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (concat "<figure><artwork><![CDATA[\n"
          ;; (org-remove-indentation
          ;;  (org-export-format-code-default example-block info))
           (org-export-format-code-default example-block info)
          "]]></artwork></figure>"))

(defun org-rfc-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((block-type (org-element-property :type special-block)))
    (cond
     ((string= (downcase block-type) "abstract")
      (plist-put info :abstract (format "<abstract>%s</abstract>" (org-trim contents)))
      "")
     (t (org-trim contents)))))

(defun org-rfc-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to RFC.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (member (org-element-property :type export-block) '("RFC" "RFC"))
      (org-remove-indentation (org-element-property :value export-block))
    ;; Also include HTML export blocks.
    (org-export-with-backend 'html export-block contents info)))

;; This is too simple and messes up diagrams
;; (defun org-rfc-indent (spaces contents)
;;   (let ((indent (make-string spaces ? )))
;;     (replace-regexp-in-string "^" indent contents)))

(defun org-rfc-indent (spaces contents)
  contents)

;;;; Headline

(defun org-rfc-headline (headline contents info)
  "Transcode HEADLINE element into RFC format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (concat "     " (org-make-tag-string tag-list))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title))
	   (style (plist-get info :rfc-headline-style)))
      (let ((anchor (or (and (org-rfc--headline-referred-p headline info)
		             (format " anchor=\"%s\""
			             (or (org-element-property :CUSTOM_ID headline)
			                 (org-export-get-reference headline info))))
                        "")))

        (org-rfc-indent 2 (format "<section title=\"%s\"%s>\n%s\n</section>\n"
                                      title anchor
                                      (org-rfc-indent 2 contents)))))))



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

;;;; Horizontal Rule

(defun org-rfc-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode HORIZONTAL-RULE element into RFC format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "---")


;;;; Italic

(defun org-rfc-italic (_italic contents _info)
  "Transcode ITALIC object into RFC format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "*%s*" contents))


;;;; Item

;; (defun org-rfc-item (item contents info)
;;   "Transcode ITEM element into RFC format.
;; CONTENTS is the item contents.  INFO is a plist used as
;; a communication channel."
;;   (let* ((type (org-element-property :type (org-export-get-parent item)))
;; 	 (struct (org-element-property :structure item))
;; 	 (bullet (if (not (eq type 'ordered)) "-"
;; 		   (concat (number-to-string
;; 			    (car (last (org-list-get-item-number
;; 					(org-element-property :begin item)
;; 					struct
;; 					(org-list-prevs-alist struct)
;; 					(org-list-parents-alist struct)))))
;; 			   "."))))
;;     (concat bullet
;; 	    (make-string (- 4 (length bullet)) ? )
;; 	    (pcase (org-element-property :checkbox item)
;; 	      (`on "[X] ")
;; 	      (`trans "[-] ")
;; 	      (`off "[ ] "))
;; 	    (let ((tag (org-element-property :tag item)))
;; 	      (and tag (format "<dt><em>%s</em></dt> " (org-export-data tag info))))
;; 	    (and contents
;; 		 (concat "<dd>" (org-trim (replace-regexp-in-string "^" "    " contents) "</dd>"))
;;                  )))

(defun org-rfc-item (item contents info)
  "Transcode ITEM element into RFC format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
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
       (tag
	(format "<dt>%s</dt> <dd>%s</dd>" (org-export-data tag info) contents))
       (t (concat bullet
	    (make-string (- 4 (length bullet)) ? )
	    (pcase (org-element-property :checkbox item)
	      (`on "[X] ")
	      (`trans "[-] ")
	      (`off "[ ] "))
	    (and contents
		 (concat "<dd>" (org-trim (replace-regexp-in-string "^" "    " contents) "</dd>"))
            )))))))



;;;; Keyword

(defun org-rfc-keyword (keyword contents info)
  "Transcode a KEYWORD element into RFC format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (pcase (org-element-property :key keyword)
    ((or "RFC" "RFC") (org-element-property :value keyword))
    ("TOC"
     (let ((case-fold-search t)
	   (value (org-element-property :value keyword)))
       (cond
	((string-match-p "\\<headlines\\>" value)
	 (let ((depth (and (string-match "\\<[0-9]+\\>" value)
			   (string-to-number (match-string 0 value))))
	       (local? (string-match-p "\\<local\\>" value)))
	   (org-remove-indentation
	    (org-rfc--build-toc info depth keyword local?)))))))
    (_ (org-export-with-backend 'html keyword contents info))))


;;;; Line Break

(defun org-rfc-line-break (_line-break _contents _info)
  "Transcode LINE-BREAK object into RFC format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  \n")


;;;; Link

(defun org-rfc-link (link contents info)
  "Transcode LINE-BREAK object into RFC format.
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
	  (`plain-text			; External file.
	   (let ((path (funcall link-org-files-as-rfc destination)))
	     (if (not contents) (format "<%s>" path)
	       (format "[%s](%s)" contents path))))
	  (`headline
	   (format
	    "[%s](#%s)"
	    ;; Description.
	    (cond ((org-string-nw-p contents))
		  ((org-export-numbered-headline-p destination info)
		   (mapconcat #'number-to-string
			      (org-export-get-headline-number destination info)
			      "."))
		  (t (org-export-data (org-element-property :title destination)
				      info)))
	    ;; Reference.
	    (or (org-element-property :CUSTOM_ID destination)
		(org-export-get-reference destination info))))
	  (_
	   (let ((description
		  (or (org-string-nw-p contents)
		      (let ((number (org-export-get-ordinal destination info)))
			(cond
			 ((not number) nil)
			 ((atom number) (number-to-string number))
			 (t (mapconcat #'number-to-string number ".")))))))
	     (when description
	       (format "[%s](#%s)"
		       description
		       (org-export-get-reference destination info))))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (let ((path (let ((raw-path (org-element-property :path link)))
		    (cond ((not (equal "file" type)) (concat type ":" raw-path))
			  ((not (file-name-absolute-p raw-path)) raw-path)
			  (t (expand-file-name raw-path)))))
	    (caption (org-export-data
		      (org-export-get-caption
		       (org-export-get-parent-element link)) info)))
	(format "![img](%s)"
		(if (not (org-string-nw-p caption)) path
		  (format "%s \"%s\"" path caption)))))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref contents)
		(org-export-resolve-coderef ref info))))
     ((equal type "radio") contents)
     (t (let* ((raw-path (org-element-property :path link))
	       (path
		(cond
		 ((member type '("http" "https" "ftp" "mailto"))
		  (concat type ":" raw-path))
		 ((string= type "file")
		  (org-export-file-uri (funcall link-org-files-as-rfc raw-path)))
		 (t raw-path))))
	  (if (not contents) (format "<%s>" path)
	    (format "[%s](%s)" contents path)))))))


;;;; Node Property

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
  (format "<t>\n%s\n</t>" (org-rfc-indent 2 (org-trim contents))))


;;;; Plain List

(defun org-rfc-plain-list (plain-list contents _info)
  "Transcode PLAIN-LIST element into RFC format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  (let* ((type (pcase (org-element-property :type plain-list)
	         (`ordered "ol")
	         (`unordered "ul")
	         (`descriptive "dl")
	         (other (error "Unknown HTML list type: %s" other))))
         (class (format "org-%s" type))
         (attributes (org-export-read-attribute :attr_html plain-list)))
    (format "<%s>\n%s</%s>" type contents type)))


;;;; Property Drawer

(defun org-rfc-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element into RFC format.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (replace-regexp-in-string "^" "    " contents)))


;;;; Quote Block

(defun org-rfc-quote-block (_quote-block contents _info)
  "Transcode QUOTE-BLOCK element into RFC format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))


;;;; Section

(defun org-rfc-section (_section contents _info)
  "Transcode SECTION element into RFC format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

;; (defun org-rfc-section (section contents info)
;;   "Transcode a SECTION element from Org to HTML.
;; CONTENTS holds the contents of the section.  INFO is a plist
;; holding contextual information."
;;   (let ((parent (org-export-get-parent-headline section)))
;;     ;; Before first headline: no container, just return CONTENTS.
;;     (if (not parent) contents
;;       (progn (plist-put info :abstract (format "<abstract>%s</abstract>" (org-trim contents))) "")
;;       (concat "SECSTART" contents "SECEND"))))

;;       ;; ;; Get div's class and id references.
;;       ;; (let* ((class-num (+ (org-export-get-relative-level parent info)
;;       ;;   		   (1- (plist-get info :html-toplevel-hlevel))))
;;       ;;        (section-number
;;       ;;         (and (org-export-numbered-headline-p parent info)
;;       ;;   	   (mapconcat
;;       ;;   	    #'number-to-string
;;       ;;   	    (org-export-get-headline-number parent info) "-"))))
;;       ;;   ;; Build return value.
;;       ;;   (format "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>\n"
;;       ;;   	class-num
;;       ;;   	(or (org-element-property :CUSTOM_ID parent)
;;       ;;   	    section-number
;;       ;;   	    (org-export-get-reference parent info))
;;       ;;   	(or contents ""))))))


;;;; Subscript

(defun org-rfc-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sub>%s</sub>" contents))

;;;; Superscript

(defun org-rfc-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sup>%s</sup>" contents))

;;;; Template

(defun org-rfc--build-toc (info &optional n keyword local)
  "Return a table of contents.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is an integer specifying the
depth of the table.

Optional argument KEYWORD specifies the TOC keyword, if any, from
which the table of contents generation has been initiated.

When optional argument LOCAL is non-nil, build a table of
contents according to the current headline."
  (concat
   (unless local
     (let ((style (plist-get info :rfc-headline-style))
	   (title (org-html--translate "Table of Contents" info)))
       (org-rfc--headline-title style 1 title nil)))
   (mapconcat
    (lambda (headline)
      (let* ((indentation
	      (make-string
	       (* 4 (1- (org-export-get-relative-level headline info)))
	       ?\s))
	     (bullet
	      (if (not (org-export-numbered-headline-p headline info)) "-   "
		(let ((prefix
		       (format "%d." (org-last (org-export-get-headline-number
						headline info)))))
		  (concat prefix (make-string (max 1 (- 4 (length prefix)))
					      ?\s)))))
	     (title
	      (format "[%s](#%s)"
		      (org-export-data-with-backend
		       (org-export-get-alt-title headline info)
		       (org-export-toc-entry-backend 'rfc)
		       info)
		      (or (org-element-property :CUSTOM_ID headline)
			  (org-export-get-reference headline info))))
	     (tags (and (plist-get info :with-tags)
			(not (eq 'not-in-toc (plist-get info :with-tags)))
			(org-make-tag-string
			 (org-export-get-tags headline info)))))
	(concat indentation bullet title tags)))
    (org-export-collect-headlines info n (and local keyword)) "\n")
   "\n"))

(defun org-rfc--footnote-formatted (footnote info)
  "Formats a single footnote entry FOOTNOTE.
FOOTNOTE is a cons cell of the form (number . definition).
INFO is a plist with contextual information."
  (let* ((fn-num (car footnote))
         (fn-text (cdr footnote))
         (fn-format (plist-get info :rfc-footnote-format))
         (fn-anchor (format "fn.%d" fn-num))
         (fn-href (format " href=\"#fnr.%d\"" fn-num))
         (fn-link-to-ref (org-html--anchor fn-anchor fn-num fn-href info)))
    (concat (format fn-format fn-link-to-ref) " " fn-text "\n")))

(defun org-rfc--footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist (cl-loop for (n _type raw) in fn-alist collect
                            (cons n (org-trim (org-export-data raw info)))))
         (headline-style (plist-get info :rfc-headline-style))
         (section-title (org-html--translate "Footnotes" info)))
    (when fn-alist
      (format (plist-get info :rfc-footnotes-section)
              (org-rfc--headline-title headline-style 1 section-title)
              (mapconcat (lambda (fn) (org-rfc--footnote-formatted fn info))
                         fn-alist
                         "\n")))))

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
        (email (plist-get info :email))
        (ipr (or (plist-get info :rfc-ipr) "trust200902"))
        (stream (or (plist-get info :rfc-stream) "IETF"))
        (title (org-export-data (plist-get info :title) info))
        (with-toc (if (plist-get info :with-toc) "yes" "no"))
        )
    (message "AUTHOR: %s EMAIL: %s COMPANY: %s" author email company)
    (concat

     ;; Replace this with actual code.
     "<?xml version=\"1.0\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"rfc2629.xslt\"?>
<?rfc toc=\"" with-toc "\"?>
<?rfc compact=\"no\"?>
<?rfc subcompact=\"no\"?>
<?rfc symrefs=\"yes\" ?>
<?rfc sortrefs=\"yes\"?>
<?rfc iprnotified=\"no\"?>
<?rfc strict=\"yes\"?>
<rfc ipr=\"" ipr "\"
     category=\"" category "\"
     docName=\"draft-chopps-ipsecme-iptfs-00\"
     submissionType=\"" submissionType "\"
     consensus=\"" consensus "\"
     version=\"3\">
 <front>
  <title abbrev=\"\">" title "</title>
    <author initials=\"C\" surname=\"Hopps\" fullname='Christian E. Hopps' >
      <organization>LabN Consulting, L.L.C.</organization>
      <address>
        <email>chopps@chopps.org</email>
      </address>
    </author>
<date/>"
  (or (plist-get info :abstract) "")
  "</front>"

  ;; (when depth
  ;;   (concat (org-rfc--build-toc info (and (wholenump depth) depth)) "\n"))

  "<middle>\n"
  ;; Document contents.
  contents
  "</middle>\n"
  "<back>"

  (with-temp-buffer
    (insert-file-contents "references.xml")
    (buffer-string))

  "</back></rfc>"
  ;; Footnotes section.
  (org-rfc--footnote-section info))))

(defun org-rfc-template (contents _info)
  "Return complete document string after RFC conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)



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
  (org-export-to-buffer 'rfc "*Org XML2RC Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(defun org-rfc-export-as-text (&optional async subtreep visible-only)
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
  (org-export-to-buffer 'rfc "*Org XML2RC Export*"
    async subtreep visible-only nil nil
    (lambda ()
      (shell-command-on-region
       (point-min) (point-max)
       "rfc -q --v3 -o /dev/stdout --text /dev/stdin"
       (current-buffer) t "*Org RFC Error*" t)
      (deactivate-mark))))

;;;###autoload
(defun org-rfc-convert-region-to-xml ()
  "Assume the current region has Org syntax, and convert it to RFC.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in a RFC buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'rfc))


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
  (let ((outfile (org-export-output-file-name ".xml" subtreep)))
    (org-export-to-file 'rfc outfile async subtreep visible-only)))

;;;###autoload
(defun org-rfc-publish-to-xml (plist filename pub-dir)
  "Publish an org file to XML.

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
