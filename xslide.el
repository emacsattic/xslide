;;;; xslide.el --- XSL Integrated Development Environment
;; $Id$

;; Copyright (C) 1998, 1999, 2000, 2001, 2003 Tony Graham

;; Author: Tony Graham <tkg@menteith.com>
;; Contributors: Simon Brooke, Girard Milmeister, Norman Walsh,
;;               Moritz Maass, Lassi Tuura, Simon Wright, KURODA Akira,
;;               Ville Skyttä, Glen Peterson
;; Created: 21 August 1998
;; Version: $Revision$
;; Keywords: languages, xsl, xml

;;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Functions for editing XSL stylesheets

;; Requires xslide-font.el, xslide-data.el, xslide-abbrev.el, xslide-process.el
;; Requires 'etags for `find-tag-default'
;; Requires 'reporter for `xsl-submit-bug-report'
;; Requires 'imenu for "Goto" menu
;;
;; Send bugs to xslide-bug@menteith.com
;; Use `xsl-submit-bug-report' for bug reports

;;;; Code:
(provide 'xslide)

(require 'cl)
(require 'compile)
(require 'font-lock)
;; XEmacs users don't always have imenu.el installed, so use
;; condition-case to cope if xslide causes an error by requiring imenu.
(eval-and-compile
  (condition-case nil
	(require 'imenu)
    (error nil)))
;; Need etags for `find-tag-default'
(require 'etags)
(require 'speedbar)

(require 'xslide-data "xslide-data")
(require 'xslide-abbrev "xslide-abbrev")
(require 'xslide-font "xslide-font")
(require 'xslide-process "xslide-process")
(require 'xslide-indent "xslide-indent")

;; Work out if using XEmacs or Emacs
;; Inspired by 'vm'
(defconst xsl-xemacs-p nil)
(defconst xsl-fsfemacs-p nil)
(defun xsl-xemacs-p () xsl-xemacs-p)
(defun xsl-fsfemacs-p () xsl-fsfemacs-p)
(defun xsl-note-emacs-version ()
  (setq xsl-xemacs-p (string-match "XEmacs" emacs-version)
	xsl-fsfemacs-p (not xsl-xemacs-p)))
(xsl-note-emacs-version)

;; Define core `xsl' group.
(defgroup xsl nil
  "Major mode for editing XSL."
  :prefix "xsl-"
  :group 'languages)

(defgroup xsl-faces nil
  "Font faces used in XSL mode."
  :prefix "xsl-"
  :group 'xsl
  :group 'faces)

(defgroup xsl-process nil
  "Running XSL processors from XSL mode."
  :group 'xsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version information

(defconst xslide-version "0.2.3"
  "Version number of xslide XSL mode.")

(defun xslide-version ()
  "Returns the value of the variable `xslide-version'."
  xslide-version)

(defconst xslide-maintainer-address "xslide-bug@menteith.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables


(defconst xsl-node-name-prefix-regex "[a-zA-Z_:]"
  "Regular expression to match the first letter of XML node names.")

(defconst xsl-node-name-regex
  (concat xsl-node-name-prefix-regex "[-a-zA-Z0-9:_.]*")
  "Regular expression to match an xml node name.
XML names must begin with a letter, underscore, or colon.  Subsequent name
characters must be letters, numbers, and \"_:-.\".  The colon character should
not be used except as a namespace delimiter.  Letters are not limited to ASCII
characters, so non-English speakers may use their own language for mark-up.
Also, 'xml' (in any combination of upper and lower case) is an illegal start
of a node name, unless it's part of a name defined by the W3C (this last
limit is ignored here).")

(defconst xsl-ows-regex "[ \t\n]*"
  "Regular expression to match optional white space of any length.")

(defconst xsl-att-val-pair-regex
  (concat xsl-node-name-regex xsl-ows-regex "="
          xsl-ows-regex "\\(\"[^\"]*\"\\|\'[^\']*\'\\)")

  "Regular expression to match a single attribute-value pair.
Includes white-space around the equals sign but not before and after
the pair.  With care, this can be used to match single or double
outer-quotes in cases like the following:

<xsl:when test=\"(ItemType='TCMN')\">

Be sure you use this regex within a tag, but not within an attribute
value or this match could give a false positive.  The false positive
in the above example would match ItemType='TCMN' instead of
test=\"(ItemType='TCMN')\".")

(defconst xsl-quick-close-regex
  (concat "<" xsl-node-name-regex
          "\\(" xsl-ows-regex xsl-att-val-pair-regex "\\)*"
          xsl-ows-regex "/" xsl-ows-regex ">")
  "Regular expression to match a quick-close tag.")

(defconst xsl-comment-start "<!--" "*Comment start string")

(defconst xsl-comment-end "-->" "*Comment end string")

(defconst xsl-cdata-start "<![CDATA["
  "CDATA start string.  This is NOT a regular expression!") 

(defconst xsl-cdata-end "]]>"
  "CDATA end string.  This is NOT a regular expression!") 

(defvar xsl-default-filespec "*.xsl"
  "*Initial prompt value for `xsl-etags''s FILESPEC argument.")

(defvar xsl-filespec-history (list xsl-default-filespec)
  "Minibuffer history list for `xsl-etags' and `xsl-grep''s FILESPEC argument.")

(defvar xsl-grep-pattern-history nil
  "Minibuffer history list for `xsl-grep''s PATTERN argument.")

(defvar xsl-grep-case-sensitive-flag nil
  "*Non-nil disables case insensitive searches by `xsl-grep'.")

(defvar xsl-comment-max-column 70
  "*Maximum column number for text in a comment.")

(defcustom xsl-initial-stylesheet-file (locate-library "xslide-initial.xsl" t)
  "*File containing initial stylesheet inserted into empty XSL buffers."
  :type '(choice (file :must-match t) (const :tag "No initial stylesheet" nil))
  :group 'xsl)

(defcustom xsl-initial-stylesheet-initial-point 0
  "*Initial position of point in initial stylesheet"
  :type '(integer)
  :group 'xsl)

(defcustom xsl-initial-fo-file (locate-library "xslide-initial.fo" t)
  "*File containing initial FO stylesheet inserted into empty XSL buffers."
  :type '(choice (file :must-match t) (const :tag "No initial FO file" nil))
  :group 'xsl)

(defcustom xsl-initial-fo-initial-point 0
  "*Initial position of point in initial FO stylesheet."
  :type '(integer)
  :group 'xsl)

; (defcustom xsl-indent-attributes nil
;   "*Whether to indent attributes on lines following an open tag.
; If non-nil, attributes will be aligned with the space after the
; element name, otherwise by two spaces."
;   :type '(choice (const :tag "Yes" t) (const :tag "No" nil))
;   :group 'xsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun xsl-read-from-minibuffer (prompt default history)
  "Read from minibuffer with default and command history."
(let ((value nil))
  (if (string-equal
       ""
       (setq value
	     (read-from-minibuffer (if default
				       (format
					"%s(default `%s') "
					prompt default)
				     (format "%s" prompt))
				   nil nil nil
				   history)))
	     default
	     value)))

;; XSLIDE house style puts all comments starting on a favourite column
(defun xsl-comment (comment)
  "Insert COMMENT starting at the usual column.

With a prefix argument, e.g. \\[universal-argument] \\[xsl-comment], insert separator comment
lines above and below COMMENT in the manner of `xsl-big-comment'."
  (interactive "sComment: ")
  (insert "\n")
  (backward-char)
  (xsl-electric-tab)
  (let ((fill-column (1- xsl-comment-max-column))
	(fill-prefix (make-string (1+ (length xsl-comment-start)) ?\ ))
;;	(comment-start xsl-init-comment-fill-prefix)
	(saved-auto-fill-function auto-fill-function))
    (auto-fill-mode 1)
    (insert xsl-comment-start)
    (insert " ")
    (indent-to (length fill-prefix))
    (fill-region (point) (save-excursion
			   (insert comment)
			   (point))
		 nil
		 1
		 1)
    ;; The fill does the right thing, but it always ends with
    ;; an extra newline, so delete the newline.
    (delete-backward-char 1)
    (if (not saved-auto-fill-function)
	(auto-fill-mode 0))
    (insert " ")
    (insert xsl-comment-end)
    (insert "\n")
    (if font-lock-mode
	(save-excursion
	  (font-lock-fontify-keywords-region
	   (xsl-font-lock-region-point-min)
	   (xsl-font-lock-region-point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode map stuff

(defvar xsl-mode-map nil
  "Keymap for XSL mode.")

(if xsl-mode-map
    ()
  (setq xsl-mode-map (make-sparse-keymap))
  (define-key xsl-mode-map [tab]	  'xsl-electric-tab)
  (define-key xsl-mode-map [(meta tab)]	  'xsl-complete)
  (define-key xsl-mode-map "/"   	  'xsl-electric-slash)
  (define-key xsl-mode-map "<"   	  'xsl-electric-less-than)
  (define-key xsl-mode-map ">"            'xsl-electric-greater-than)
  (define-key xsl-mode-map [(control c) (control c)]
				   	  'xsl-comment)
  (define-key xsl-mode-map [(control c) (control p)]
				   	  'xsl-process)
  (define-key xsl-mode-map [(control o)]
				   	  'xsl-open-line)
  (define-key xsl-mode-map "\C-c<"  	  'xsl-insert-tag)
  (define-key xsl-mode-map "\C-c\C-t"     'xsl-if-to-choose)
  (define-key xsl-mode-map "\C-m" 'xsl-electric-return)
  (define-key xsl-mode-map "\177"	  'xsl-delete)
)

(defun xsl-if-to-choose ()
"Converts <xsl:if> to <xsl:choose>.  Works on a single 'if' or on a region.
So:

<xsl:if test=\"$isFive = 5\"><p>It's five!</p></xsl:if>

Becomes:
<xsl:choose>
   <xsl:when test=\"$isFive = 5\"><p>It's five!</p></xsl:when>
   <xsl:otherwise></xsl:otherwise>
</xsl:choose>

If you put your cursor inside the open-tag of the if, it will work on that tag
only.  If you highlight a region, it will convert every 'if' whose start tag is
within that region.  It is very easy to convert consecutive 'if's to a single
choose by deleting the appropriate lines after executing this command.

Bound to C-c C-t by default."
  (interactive)
  (let ( (single-if (not (mark)))
         (the-start (point))
         (the-end (if (mark) (mark) (point))))
    (if (and (not (null (mark)))
             (< (mark) (point)))
        (progn (exchange-point-and-mark)
               (setq the-start (point))
               (setq the-end (mark))))
    (save-excursion
      (if single-if
        (progn (search-backward "<" nil t)
;               (message "xsl-if-to-choose: single if mode")
               (xsl-convert-if-to-choose-slave))
        (save-excursion
;          (message (concat "xsl-if-to-choose: Region mode: "
;                           (int-to-string the-start)
;                           " "
;                           (int-to-string the-end)))
          (goto-char the-end)
          (if (save-excursion (search-backward "<xsl:if" the-start t))
              (while (search-backward "<xsl:if" the-start t)
                (xsl-convert-if-to-choose-slave))
              (message "xsl-if-to-choose error: There's no <xsl:if> within the selected region.")))))))

(defun xsl-convert-if-to-choose-slave ()
  (if (looking-at "<xsl:if")
      (let ( (start (save-excursion (beginning-of-line) (point))) )
        (delete-char 7)
        (insert "<xsl:choose>\n<xsl:when")
        (search-forward "</xsl:if>" nil t)
        (backward-delete-char 9)
        (insert "</xsl:when>\n<xsl:otherwise></xsl:otherwise>\n</xsl:choose>")
        (indent-region start (point) nil))
    (message "xsl-if-to-choose error: point is not within the start tag of an <xsl:if>.")))

(defun xsl-electric-greater-than (arg)
  "Insert a \">\" and, optionally, insert a matching end-tag.

If the \">\" closes a start-tag and the start-tag is the last thing on
the line, `xsl-electric-greater-than' inserts the matching end-tag.
Providing a prefix argument, e.g., \\[universal-argument] \\[xsl-electric-greater-than], stops the inserting of the
matching end-tag.

If the element being terminated is listed as a block element in
`xsl-all-elements-alist', then point is left on the next line at the
correct indent and the end-tag is inserted on the following line at
the correct indent.

`xsl-electric-greater-than' also fontifies the region around the
current line."
  (interactive "P")
  (insert ">")
  (if (and
       (not arg)
       (looking-at "$")
       (save-excursion
	 (let ((limit (point)))
	   (backward-char)
	   (search-backward "<")
;;	   (message "%s:%s" (point) limit)
	   (and
	    (looking-at "<\\(\\(\\sw\\|\\s_\\)+\\)\\(\\s-+\\(\\sw\\|\\s_\\)+[ 	]*=[ 	]*\\('[^']*'\\|\"[^\"]*\"\\)\\)*\\s-*\\(/?\\)>")
;;	    (message "%s:%s" limit (match-end 0))
	    (= (match-end 0) limit)
;;	    (message ":%s:" (match-string 6))
	    (not (string-equal (match-string 6) "/"))
	    (not (save-match-data
		   (string-match "^/" (match-string 1))))))))
      (if (string-equal (nth 1 (assoc (match-string 1) xsl-all-elements-alist)) "block")
	  (progn
	    (xsl-electric-return)
	    (save-excursion
	      (insert "\n<")
	      (xsl-electric-slash)))
	(save-excursion
	  (insert (format "</%s>" (match-string 1))))))
  (if font-lock-mode
      (save-excursion
	(font-lock-fontify-region
	 (xsl-font-lock-region-point-min)
	 (xsl-font-lock-region-point-max)))))

(defun xsl-electric-less-than ()
  "Function called when \"<\" is pressed in XSL mode."
  (interactive)
  (insert "<")
  (xsl-indent-line)
  (if (looking-at "[ \t]+$")
      (delete-region (point) (re-search-forward "$" nil t))))

(defun xsl-match-opening-tag (a)
  "Function called to match the next opening tag to a closing tag."
  (if (looking-at "</")
      (catch 'start-tag
        (while (re-search-backward
                (concat "\\(<\\|</\\)" a "[ \t\n\r>]") nil t)
          (cond
           ((looking-at (concat "</" a))
            (xsl-match-opening-tag a))
           ((looking-at (concat "<" a))
            (throw 'start-tag a))
           )))
    nil)
)
(defun xsl-electric-slash ()
  "Function called when '/' is pressed in XSL mode.
Important because we may need to auto-insert a closing tag."
  (interactive)
  (insert "/")
  (xsl-indent-line)
  (let
      ( (slash-in-comment (xsl-is-in-comment)) )
    ; I'm using "or" here like a "cond" with fewer parenthesis.
    (or
      ; Do nothing if we are looking at a ">" because we probably just
      ; typed a quick-close
      (looking-at (concat xsl-ows-regex ">"))
  
      ; If we are looking at a valid tag-name followed by a ">"
      ; we are already done.
      (looking-at (concat xsl-ows-regex xsl-node-name-regex xsl-ows-regex ">"))
  
      ; For now, if we are within a comment, allow a match with tags
      ; outside the comment.
      ; (xsl-is-in-comment)
  
      ; Do nothing if we're in a cdata section because the user might be typing
      ; JavaScript or whatever.
      (xsl-is-in-cdata)
  
      ; Here, the user is probably typing an end-tag, so try to make a completion
      (let*
          ( (element-name
             (save-excursion
               (backward-char 2)
               (if (looking-at "</")
                   ; Find the start tag
                   (catch 'start-tag
                     (while (search-backward "<" nil t)
                             ; skip comment, cdata, or any other tags starting with !
                       (cond ( (looking-at "<!") )
                             ; if we are in a comment, skip back to the beginning of it.
                             ( (and (xsl-is-in-comment) (not slash-in-comment))
                               (search-backward xsl-comment-start nil t) )
                             ; if we are in a CDATA, skip back to the beginning of it.
                             ( (xsl-is-in-cdata)
                               (search-backward xsl-cdata-start nil t) )
                             ; if looking at an end tag, skip to the matching start tag.
                             ( (looking-at (concat "</\\(" xsl-node-name-regex "\\)"))
                               ; find matching tag:
                               (xsl-match-opening-tag (match-string 1)))
                             ; skip quick-closes
                             ( (looking-at xsl-quick-close-regex) )
                             ( (looking-at (concat "<\\(" xsl-node-name-regex "\\)"))
                               (throw 'start-tag (match-string 1)) )
                             ( (bobp)
                               (throw 'start-tag nil) )))) ; end catch
                 nil)))) ; end let initialization block
        ; If we have an element, finish the closing tag.
        (if element-name
            (progn
              (insert element-name)
              (insert ">")
              (if font-lock-mode
                  (save-excursion
                    (font-lock-fontify-region
                     (xsl-font-lock-region-point-min)
                     (xsl-font-lock-region-point-max))))))))))

(defun xsl-open-line (arg)
  (interactive "p")
  (if (not arg)
      (setq arg 1))
  (save-excursion (while (> arg 0)
                    (setq arg (1- arg))
;                   (insert "\n")
                    (xsl-electric-return))
                  (if (looking-at "<") (xsl-electric-tab))))

(defun xsl-complete ()
  "Complete the tag or attribute before point.
If it is a tag (starts with < or </) complete with allowed tags,
otherwise complete with allowed attributes."
  (interactive "*")
  (let ((tab				; The completion table
	 nil)
	(pattern nil)
	(c nil)
	(here (point)))
    (skip-chars-backward "^ \n\t</!&%")
    (setq pattern (buffer-substring (point) here))
    (setq c (char-after (1- (point))))
;;    (message "%s" c)
    (cond
     ;; entity
;;     ((eq c ?&)
;;      (sgml-need-dtd)
;;      (setq tab
;;	    (sgml-entity-completion-table
;;	     (sgml-dtd-entities (sgml-pstate-dtd sgml-buffer-parse-state)))))
     ;; start-tag
     ((eq c ?<)
;;      (save-excursion
;;	(backward-char 1)
;;	(sgml-parse-to-here)
	(setq tab xsl-all-elements-alist))
     ;; end-tag
;;     ((eq c ?/)
;;      (save-excursion
;;	(backward-char 2)
;;	(sgml-parse-to-here)
;;	(setq tab (sgml-eltype-completion-table
;;		   (sgml-current-list-of-endable-eltypes)))))
     ;; markup declaration
;;     ((eq c ?!)
;;      (setq tab sgml-markup-declaration-table))
     ((eq c ? )
;;      (save-excursion
;;	(backward-char 1)
;;	(sgml-parse-to-here)
	(setq tab xsl-all-attribute-alist))
     (t
      (goto-char here)
      (ispell-complete-word)))
    (when tab
      (let ((completion (try-completion pattern tab)))
	(cond ((null completion)
	       (goto-char here)
	       (message "Can't find completion for \"%s\"" pattern)
	       (ding))
	      ((eq completion t)
	       (goto-char here)
	       (message "[Complete]"))
	      ((not (string= pattern completion))
	       (delete-char (length pattern))
	       (insert completion))
	      (t
	       (goto-char here)
	       (message "Making completion list...")
	       (let ((list (all-completions pattern tab)))
		 (with-output-to-temp-buffer " *Completions*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done")))))))

(defun xsl-insert-tag (tag)
  "Insert a tag, reading tag name in minibuffer with completion."
  (interactive
   (list
    (completing-read "Tag: " xsl-all-elements-alist)))
  ;;  (xsl-find-context-of (point))
  ;;  (assert (null xsl-markup-type))
  ;; Fix white-space before tag
  ;;  (unless (xsl-element-data-p (xsl-parse-to-here))
  (skip-chars-backward " \t")
  (cond
   ((looking-at "^\\s-*$")
    (xsl-electric-tab))
   ((looking-at "^\\s-*</")
    (save-excursion
      (insert "\n"))
    (xsl-electric-tab))
   ((looking-at "$")
    (insert "\n")
    (xsl-electric-tab)))
  (let ((tag-type (nth 1 (assoc tag xsl-all-elements-alist))))
    (cond
     ((or
       (equal tag-type "block")
       (equal tag-type nil))
      (insert "<")
      (insert tag)
      (insert ">")
      (save-excursion
	(insert "\n")
	(xsl-electric-tab)
	(insert "<")
	(if (looking-at "<")
	    (progn
	      (insert "\n")
	      (backward-char)))
	(xsl-electric-slash)))
     ((equal tag-type "inline")
      (insert "<")
      (insert tag)
      (insert ">")
      (save-excursion
	(insert "</")
	(insert tag)
	(insert ">")))
     (t
      (insert "<")
      (insert tag)
      (save-excursion
	(insert "/>"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xsl-insert-template (match)
  "Insert a template."
  (interactive "smatch=")
  (xsl-electric-tab)
  (insert (format "<xsl:template match=\"%s\">\n" match))
  (xsl-electric-tab)
  (save-excursion
    (insert "\n<")
    (xsl-electric-slash)
    (insert "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table stuff

(defvar xsl-mode-syntax-table nil
  "Syntax table used while in XSL mode.")

(if xsl-mode-syntax-table
    ()
  (setq xsl-mode-syntax-table (make-syntax-table))
  ;; set the non-alphanumeric characters in XML names to
  ;; 'symbol constituent' class
  (modify-syntax-entry ?: "_" xsl-mode-syntax-table)
  (modify-syntax-entry ?_ "_" xsl-mode-syntax-table)
  (modify-syntax-entry ?- "_ 1234" xsl-mode-syntax-table)
  (modify-syntax-entry ?. "_" xsl-mode-syntax-table)
  ;; "-" is a special case because it is the first and second characters
  ;; of the start- and end-comment sequences.
  (modify-syntax-entry ?- "_ 1234" xsl-mode-syntax-table)
  ;; "%" does double duty in parameter entity declarations and references.
  ;; Not necessary to make "%" and ";" act like parentheses since the
  ;; font lock highlighting tells you when you've put the ";" on the
  ;; end of a parameter entity reference.
  (modify-syntax-entry ?% "_" xsl-mode-syntax-table)
  (modify-syntax-entry ?\; "_" xsl-mode-syntax-table)
  ;; "/" is just punctuation in XSLs, and really only has a role in
  ;; Formal Public Identifiers
  (modify-syntax-entry ?/ "." xsl-mode-syntax-table)
  ;; Sometimes a string is more than just a string, Dr Freud.
  ;; Unfortunately, the syntax stuff isn't fussy about matching
  ;; on paired delimiters, and will happily match a single quote
  ;; with a double quote, and vice versa.  At least the font
  ;; lock stuff is more fussy and won't change colour if the
  ;; delimiters aren't paired.
  (modify-syntax-entry ?\" "$" xsl-mode-syntax-table)
  (modify-syntax-entry ?\' "$" xsl-mode-syntax-table)
  ;; The occurrence indicators and connectors are punctuation to us.
  (modify-syntax-entry ?| "." xsl-mode-syntax-table)
  (modify-syntax-entry ?, "." xsl-mode-syntax-table)
  (modify-syntax-entry ?& "." xsl-mode-syntax-table)
  (modify-syntax-entry ?? "." xsl-mode-syntax-table)
  (modify-syntax-entry ?+ "." xsl-mode-syntax-table)
  (modify-syntax-entry ?* "." xsl-mode-syntax-table)
  ;; `<' and `>' are also punctuation
  (modify-syntax-entry ?< "." xsl-mode-syntax-table)
  (modify-syntax-entry ?> "." xsl-mode-syntax-table)
  ;; "#" is syntax too
  (modify-syntax-entry ?# "_" xsl-mode-syntax-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu stuff

(defun xsl-sort-alist (alist)
  "Sort an alist."
  (sort
   alist
   (lambda (a b) (string< (car a) (car b)))))

(defun xsl-imenu-create-index-function ()
  "Create an alist of elements, etc. suitable for use with `imenu'."
  (interactive)
  (let ((template-alist '())
	(mode-alist '())
	(key-alist '())
	(attribute-set-alist '())
	(name-alist '()))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^\\s-*<xsl:template\\(\\s-+\\)" nil t)
      ;; Go to the beginning of the whitespace after the element name
      (goto-char (match-beginning 1))
      ;; Match on either single-quoted or double-quoted attribute value.
      ;; The expression that doesn't match will have return nil for
      ;; `match-beginning' and `match-end'.
      ;; Don't move point because the 'mode' attribute may be before
      ;; the 'match' attribute.
      (if (save-excursion
	    (re-search-forward
	     "match\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
	     (save-excursion
	       (save-match-data
		 (re-search-forward "<\\|>" nil t)))
	     t))
	  (let* ((pattern (buffer-substring-no-properties
			   ;; Rely on the pattern that didn't match
			   ;; returning nil and on `or' evaluating the
			   ;; second form when the first returns nil.
			   (or
			    (match-beginning 2)
			    (match-beginning 3))
			   (or
			    (match-end 2)
			    (match-end 3))))
		 (pattern-position (or
				    (match-beginning 2)
				    (match-beginning 3))))
	    ;; Test to see if there is a 'mode' attribute.
	    ;; Match on either single-quoted or double-quoted attribute value.
	    ;; The expression that doesn't match will have return nil for
	    ;; `match-beginning' and `match-end'.
	    (if (save-excursion
		  (re-search-forward
		   "mode\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
		   (save-excursion
		     (save-match-data
		       (re-search-forward "<\\|>" nil t)))
		   t))
		(let* ((mode-name (buffer-substring-no-properties
				   ;; Rely on the pattern that didn't match
				   ;; returning nil and on `or' evaluating the
				   ;; second form when the first returns nil.
				   (or
				    (match-beginning 2)
				    (match-beginning 3))
				   (or
				    (match-end 2)
				    (match-end 3))))
		       (mode-name-alist (assoc mode-name mode-alist)))
		  (if mode-name-alist
		      (setcdr mode-name-alist
			      (list (car (cdr mode-name-alist))
				    (cons pattern pattern-position)))
		    (setq mode-alist
			  (cons
			   (list mode-name (cons pattern pattern-position))
			   mode-alist))))
	      (setq template-alist
		    (cons (cons pattern pattern-position)
			  template-alist)))))
      ;; When there's no "match" attribute, can still have "name"
      ;; attribute
      (if (save-excursion
	    (re-search-forward
	     "\\s-+name\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
	     (save-excursion
	       (save-match-data
		 (re-search-forward "<\\|>" nil t)))
	     t))
	  (setq name-alist
		(cons
		 (cons (buffer-substring-no-properties
			;; Rely on the pattern that didn't match
			;; returning nil and on `or' evaluating the
			;; second form when the first returns nil.
			(or
			 (match-beginning 2)
			 (match-beginning 3))
			(or
			 (match-end 2)
			 (match-end 3)))
		       (or
			(match-beginning 2)
			(match-beginning 3)))
		 name-alist))))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^\\s-*<xsl:attribute-set\\(\\s-+\\)" nil t)
      ;; Go to the beginning of the whitespace after the element name
      (goto-char (match-beginning 1))
      ;; Match on either single-quoted or double-quoted attribute value.
      ;; The expression that doesn't match will have return nil for
      ;; `match-beginning' and `match-end'.
      (if (save-excursion
	    (re-search-forward
	     "name\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
	     (save-excursion
	       (save-match-data
		 (re-search-forward "<\\|>$" nil t)))
	     t))
	  (setq attribute-set-alist
		(cons
		 (cons (buffer-substring-no-properties
			;; Rely on the pattern that didn't match
			;; returning nil and on `or' evaluating the
			;; second form when the first returns nil.
			(or
			 (match-beginning 2)
			 (match-beginning 3))
			(or
			 (match-end 2)
			 (match-end 3)))
		       (or
			(match-beginning 2)
			(match-beginning 3)))
		 attribute-set-alist))))
    (goto-char (point-min))
    (while
	(re-search-forward
	 "^\\s-*<xsl:key\\(\\s-+\\)" nil t)
      ;; Go to the beginning of the whitespace after the element name
      (goto-char (match-beginning 1))
      ;; Match on either single-quoted or double-quoted attribute value.
      ;; The expression that doesn't match will have return nil for
      ;; `match-beginning' and `match-end'.
      (if (save-excursion
	    (re-search-forward
	     "name\\s-*=\\s-*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)"
	     (save-excursion
	       (save-match-data
		 (re-search-forward "<\\|>$" nil t)))
	     t))
	  (setq key-alist
		(cons
		 (cons (buffer-substring-no-properties
			;; Rely on the pattern that didn't match
			;; returning nil and on `or' evaluating the
			;; second form when the first returns nil.
			(or
			 (match-beginning 2)
			 (match-beginning 3))
			(or
			 (match-end 2)
			 (match-end 3)))
		       (or
			(match-beginning 2)
			(match-beginning 3)))
		 key-alist))))
    (append
     (if key-alist
	 (list (cons "xsl:key" (xsl-sort-alist key-alist))))
     (if attribute-set-alist
	 (list (cons "xsl:attribute-set"
		     (xsl-sort-alist attribute-set-alist))))
     (if name-alist
	 (list (cons "name=" (xsl-sort-alist name-alist))))
     (if mode-alist
	 ;; Sort the mode-alist members, format the mode names nicely,
	 ;; and sort the templates within each mode.
	 (append
	  (mapcar (lambda (x)
		    (cons (format "mode=\"%s\"" (car x))
			  (xsl-sort-alist (cdr x))))
		  (xsl-sort-alist mode-alist))))
     (xsl-sort-alist template-alist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep stuff

;;;###autoload
(defun xsl-grep (pattern filespec)
  "Grep for PATTERN in files matching FILESPEC.

Runs `grep' with PATTERN and FILESPEC as arguments.

PATTERN is the pattern on which `grep' is to match.  PATTERN is quoted
with single quotes in the `grep' command arguments to avoid
interpretation of characters in PATTERN.  `xsl-grep' maintains a
history of PATTERNs so you can easily re-use a previous value.

FILESPEC is the names or regular expression for the files to be
scanned by grep.  Since `xsl-grep' uses `grep', regular expressions
and multiple filenames are supported, and \"*.xsl\" and \"*.XSL
*.ent\" are both valid FILESPEC values.

When called interactively, the initial FILESPEC is taken from
xsl-default-filespec, but `xsl-grep' also maintains a history of
FILESPEC arguments so you can easily re-use a previous value.  The
history is shared with `xsl-etags' so you can re-use the same FILESPEC
with both functions."
  (interactive
   (list
    (xsl-read-from-minibuffer "Pattern: "
			      (find-tag-default)
			      'xsl-grep-pattern-history)
    (xsl-read-from-minibuffer "Files: "
			      (car xsl-filespec-history)
			      'xsl-filespec-history)))
  ;; Include "--" in the command in case the pattern starts with "-"
  (grep (format "grep -n %s -- '%s' %s"
		(if (not xsl-grep-case-sensitive-flag)
		    "-i")
		pattern
		filespec)))


;;;###autoload
(defun xsl-mode ()
  "Major mode for editing XSL stylesheets.

Special commands:
\\{xsl-mode-map}
Turning on XSL mode calls the value of the variable `xsl-mode-hook',
if that value is non-nil.

Abbreviations:

XSL mode includes a comprehensive set of XSL-specific abbreviations
preloaded into the abbreviations table.

Font lock mode:

Turning on font lock mode causes various XSL syntactic structures to be
highlighted. To turn this on whenever you visit an XSL file, add
the following to your .emacs file:
  \(add-hook 'xsl-mode-hook 'turn-on-font-lock\)

Processing stylesheets:

\\[xsl-process] runs a shell command, in a separate process
asynchronously with output going to the buffer *XSL process*.  You can
then use the command \\[next-error] to find the next error message and
move to the line in the XSL document that caused it.

The first time that the program is run and whenever you provide a
prefix argument, e.g. \\[universal-argument] \\[xsl-process], prompts
for input filename, stylesheet file, and output filename.  Those
values are used with the templates in `xsl-process-command' to
populate this command's command history with the command lines to run
several XSLT processors using those values.  Use M-p and M-n to step
through the predefined commands, edit a command if necessary, or enter
a new command line.  The next time that this command is run, the
previously executed command is used as the default.

Searching multiple files:

To search multiple files, use \"\\[execute-extended-command] xsl-grep\" and supply the pattern to
search for and the specification of files to search in response to
the prompts.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map xsl-mode-map)
  (setq mode-name "XSL")
  (setq major-mode 'xsl-mode)
  (speedbar-add-supported-extension (list ".xsl" ".fo"))
  (setq local-abbrev-table xsl-mode-abbrev-table)

  (defadvice yank (around my-yank activate compile)
    "Wraps the yank command so that it performs an indent-region
  on the yanked material but only if the buffer is in xsl-mode."
    (interactive)
    (if (string-equal major-mode 'xsl-mode)
      (let
;        ( (pt (previous-line 1)) )
        ( (pt (- (point) (current-column))) )
        ad-do-it
        (if (looking-at "[ \t]+$")
          (delete-region (point) (re-search-forward "$" nil t)))
        (indent-region pt (point) nil)
        (xsl-indent-line)
        (message "xsl-yank"))
      ad-do-it))

  ;; XEmacs users don't all have imenu
  (if (featurep 'imenu)
      (progn
        ;; If you don't have imenu, you'll get a "free variable"
        ;; warning for imenu-create-index-function when you
        ;; byte-compile, but not having imenu won't cause problems
        ;; when you use xslide
        (setq imenu-create-index-function 'xsl-imenu-create-index-function)
        (setq imenu-extract-index-name-function 'xsl-imenu-create-index-function)
        (imenu-add-to-menubar "Templates")))
  ;; comment stuff
;;  (make-local-variable 'comment-column)
;;  (setq comment-column 32)
;;  (make-local-variable 'comment-start)
;;  (setq comment-start "; ")
;;  (make-local-variable 'comment-end)
;;  (setq comment-end "\n")
;;  (make-local-variable 'comment-start-skip)
;;  (setq comment-start-skip ";;* *")
  ;;
  ;; later we should move this into the xsl-mode-hook in
  ;; our local .emacs file
  ;; (abbrev-mode t)
  ;;
  ;; XSL font-lock highlighting setup
;;  (xsl-font-make-faces)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(xsl-font-lock-keywords t))
  (eval-and-compile
    ; XEmacs users might not have this function.
    (make-variable-buffer-local 'font-lock-mark-block-function))
  (setq font-lock-mark-block-function 'xsl-font-lock-mark-block-function)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function `xsl-indent-line)
;;  (make-local-variable 'font-lock-defaults)
;;  (setq font-lock-defaults
;;	'(xsl-font-lock-keywords nil t ((?- . "w")
;;					(?_ . "w")
;;					(?. . "w"))))
  ;; add an entry to compilation-error-regexp-alist for XSL
  ;; compiler errors
;;  (setq compilation-error-regexp-alist
;;	(cons '("file:/c:/projects/xslide/test.xsl:29:
;;XSL Error on line \\([0-9]*\\) in file \\(.*\\):$" 2 1)
;;	      compilation-error-regexp-alist))

  (set-syntax-table xsl-mode-syntax-table)

  ; define xsl-element-indent-step if necessary for backward
  ; compatibility (pre version 0.2.2)
  (eval-and-compile
    (if (assoc 'xsl-element-indent-step (buffer-local-variables))
        nil ; it's defined: do nothing
      (make-variable-buffer-local 'xsl-element-indent-step)
      (setq xsl-element-indent-step nil)))

  ; NOTES ON INDENT-TO: (an epic comment by Glen Peterson)
  ; ======================================================
  ; indent-tabs-mode: if nil, spaces only.
  ;                   if t, indent-line converts tab-width groups of
  ;                   spaces to tabs.
  ; tab-width: controls how many spaces to convert to a tab, or how
  ;            many spaces to insert for each indentation step.
  ; tab-stops-list: controls how tabs actually display on the screen.
  ;
  ; Spaces mode just works.  In tabs-mode, if tab-stops-list and
  ; tab-width do not match it will indent one way and display another
  ; creating a mess.  We will avoid this by calculating tab-stop-list
  ; to be in multiples of tab-width (Tony Grahm's excellent idea).
  ;
  ; Sometimes, files are indented with a combination of tabs and
  ; spaces - usually because the author's tab-width is not the same as
  ; the indent-step width.  Although these files are more challenging
  ; to display than ones using pure spaces or pure tabs, a user can
  ; still display these files as the author saw them by viewing them
  ; in spaces-mode with the same tab-width setting as the author (the
  ; user must figure this out, but it's usually 8)
  ; 
  ; Since many editors make a mess of displaying files with mixed tabs
  ; and spaces, we will endeavor not to create any xsl files of that
  ; sort.  Users who are particular about indentation will want to
  ; convert existing files to all tabs or all spaces anyway.  In
  ; spaces mode, we must indent using only spaces so that the file is
  ; guaranteed to look the same on all machines.  In tabs mode, we
  ; should try to indent using only tabs so that the file will appear
  ; indented using the viewer's preferred indentation step size.
  ;
  ; The one xsl exception to using tabs exclusively in tabs mode is
  ; for indenting attributes in multi-line tags.  In order for
  ; pretty-print indented attributes to look right in tabs-mode
  ; regardless of tab-width we must:
  ;
  ; - indent to the proper indentation depth of the element using tabs
  ;
  ; - indent from the start of the element to the start of the
  ;   attribute using spaces.
  ;
  ; - indent subsequent lines using only tabs
  ;
  ; For example:
  ;
  ; tab<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  ; tab................version="1.0"
  ; tab................xmlns:java="http://xml.apache.org/xslt/java">
  ; tabtab<xsl:output method="html" />
  ;
  ; For simplicity, we could indent attributes by two indentation
  ; steps instead of pretty-printing them below the first attribute.
  ; For now, I have ignored this issue altogether since it could be
  ; done manually by tabbers the few times they use multiple lines for
  ; long tags.

  ; If xsl-tab-width is defined we're set.
  ; If it's not, and xsl-element-indent-step is, use that for backward
  ;   compatibility
  ; If neither is defined, use tab-width
  (cond ( xsl-tab-width ) ; use as is
        ( (integerp xsl-element-indent-step)
          (setq xsl-tab-width xsl-element-indent-step))
        (t (setq xsl-tab-width tab-width)))

  ; Set to xsl- equivalents (for indent-to)
  (setq indent-tabs-mode xsl-indent-tabs-mode)
  (setq tab-width xsl-tab-width)

  ; If we're in tabs-mode, create a buffer-local tab-stop-list that
  ; will work properly with our desired xsl-tab-width.
  (if xsl-indent-tabs-mode
      (progn
        (make-variable-buffer-local 'tab-stop-list)
        (setq tab-stop-list (let ( (idx 1)
                                   (tsl ()))
                              (while (< idx 70)
                                (setq idx (+ idx xsl-tab-width))
                                (setq tsl (append tsl (list idx))))
                              tsl))))

  ; Load stylesheet template if one is present and we have an empty
  ; buffer.
  (if (and xsl-initial-stylesheet-file
           (eq (point-min) (point-max)))
      (progn
        (insert-file-contents xsl-initial-stylesheet-file)
        (goto-char xsl-initial-stylesheet-initial-point)))
  (run-hooks 'xsl-mode-hook))


;;;; Bug reporting

(defun xsl-submit-bug-report ()
  "Submit via mail a bug report on 'xslide'."
  (interactive)
  (require 'reporter)
  (require 'sendmail)
  (and (y-or-n-p "Do you really want to submit a report on XSL mode? ")
       (reporter-submit-bug-report
	xslide-maintainer-address
	(concat "xslide.el " xslide-version)
	(list
	 ; Need to find a way to automatically insert all of the
	 ; xslide-specific variables here.
	 )
	nil
	nil
     "Please change the Subject header to a concise bug description.\nRemember to cover the basics, that is, what you expected to\nhappen and what in fact did happen.  Please remove these\ninstructions from your message.")
    (save-excursion
      (goto-char (point-min))
      (mail-position-on-field "Subject")
      (beginning-of-line)
      (delete-region (point) (progn (forward-line) (point)))
      (insert
       "Subject: xslide " xslide-version " is wonderful but...\n"))))


(autoload 'reporter-submit-bug-report "reporter")

;;;; Last provisions
;;;(provide 'xslide)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:xsl\\|fo\\)$" . xsl-mode))

;;; xslide.el ends here
