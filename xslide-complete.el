;;;; xslide-complete.el --- XSL text completion routines

;; Copyright (C) 2003 Glen Peterson and Tony Graham

;; Author: Glen Peterson based on the work of
;;               Tony Graham <tkg@menteith.com>
;;               Simon Brooke, Girard Milmeister, Norman Walsh,
;;               Moritz Maass, Lassi Tuura, Simon Wright, KURODA Akira,
;;               Ville Skyttä
;; Created: 13 October 2003
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


(require 'xslide "xslide")
(require 'xslide-nodes "xslide-nodes")

(provide 'xslide-complete)

(defcustom xsl-html-mode t
  "Enables auto-completion of html tags and attributes.
call the function (xsl-html-mode) to toggle.  Turn this off for
working with XML output where you don't want to autocomplete
tags as though they were HTML."
  :type '(choice (const :tag "Off" nil) (const :tag "On" t))
  :group 'xsl)

(defun xsl-electric-greater-than (&optional arg)
  "Insert a \">\" and, optionally, insert a matching end-tag.

If the \">\" closes a start-tag and the start-tag is the last thing on
the line, and it's not within a comment or cdata section,
`xsl-electric-greater-than' inserts the matching end-tag.
Providing a prefix argument, e.g.,
\\[universal-argument] \\[xsl-electric-greater-than],
stops the inserting of the
matching end-tag.

If the element being terminated is listed as a block element in
`xsl-all-elements-alist', then point is left on the next line at the
correct indent and the end-tag is inserted on the following line at
the correct indent.

`xsl-electric-greater-than' also fontifies the region around the
current line."

  (interactive "P")
  (insert ">")
  (xsl-indent-line)

  ; Don't do any completion if and we just closed a quick-close,
  ; a comment, or a CDATA section (none is required)
  ; What do we want to do in a xsl:text tag?
        ; we just closed a quick-close
  (cond ( (save-excursion (search-backward "/>" (- (point) 2) t)) )
        ; we just closed a comment
        ( (save-excursion
            (search-backward xsl-comment-end
                             (- (point) (length xsl-comment-end)) t)) )
        ; we just closed a cdata
        ( (save-excursion
            (search-backward xsl-cdata-end
                             (- (point) (length xsl-cdata-end)) t)) )
        ; we must be at the end of the line (except for possible white-space)
        ; if we are to perform a completion
        ( (and (looking-at "[ \t]*$")
               (progn
                 ; find start of last open tag.  Ignore special tags starting with ! or ?
                 ; this will match tags with attributes.
                 (save-excursion
                   ( re-search-backward
                     (concat "<\\(" xsl-node-name-regex "\\)\\(" xsl-ows-regex
                             xsl-att-val-pair-regex "\\)*" xsl-ows-regex ">")
                     nil t))
                 ; does matched expression end at the > we just inserted
                 (= (match-end 0) (point))))
          ; True: we could autocomplete if it's the right kind of tag.
          ; Check all xsl tags we complete first
          (cond ( (string-equal (match-string 1) "xsl:choose")
                  (xsl-electric-return)
                  (insert "<xsl:when test=\"")
                  (save-excursion
                    (insert "\"></xsl:when>")
                    (xsl-indent-line)
                    (xsl-electric-return)
                    (insert "<xsl:otherwise></xsl:otherwise>")
                    (xsl-indent-line)
                    (xsl-electric-return)
                    (insert "</xsl:choose>")
                    (xsl-indent-line))))
          ; Only complete html tags if we are in xsl-html-mode
          (if xsl-html-mode
              (cond ( (string-equal (match-string 1) "table")
                      (xsl-electric-return)
                      (insert "<tr>")
                      (xsl-indent-line)
                      (xsl-electric-return)
                      (insert "<td>")
                      (save-excursion
                        (insert "</td>")
                        (xsl-indent-line)
                        (xsl-electric-return)
                        (insert "</tr>")
                        (xsl-indent-line)
                        (xsl-electric-return)
                        (insert "</table>")
                        (xsl-indent-line)))
                    ( (string-equal (match-string 1) "frameset")
                      (xsl-electric-return)
                      (insert "<frame src=\"")
                      (save-excursion        
                        (insert "\" />")
                        (xsl-indent-line)
                        (xsl-electric-return)
                        (insert "<noframes>")
                        (xsl-indent-line)
                        (xsl-electric-return)
                        (insert "<body><p>This site uses frames.  If you can read this, your browser does not support frames.</p></body>")
                        (xsl-indent-line)
                        (xsl-electric-return)
                        (insert "</noframes>")
                        (xsl-indent-line)
                        (xsl-electric-return)
                        (insert "</frameset>")
                        (xsl-indent-line)))
                    ( (or (string-equal (match-string 1) "ul")
                          (string-equal (match-string 1) "ol"))
                      (let
                          ( (tag-name (match-string 1)) )
                        (xsl-electric-return)
                        (insert "<li>")
                        (save-excursion        
                          (insert "</li>")
                          (xsl-indent-line)
                          (xsl-electric-return)
                          (insert (concat "</" tag-name ">"))
                          (xsl-indent-line))))
                    ( (string-equal (match-string 1) "select")
                      (xsl-electric-return)
                      (insert "<option></option>")
                      (xsl-indent-line)
                      (xsl-electric-return)
                      (insert "</select")
                      (xsl-overwrite-tag-end ">")
                      (xsl-indent-line)))))) ; end of our possible auto-completion
                                        ; Fontify
  (if font-lock-mode
      (save-excursion
        (font-lock-fontify-region
         (xsl-font-lock-region-point-min)
         (xsl-font-lock-region-point-max))))) ; end defun xsl-electric-greater-than

(defun xsl-overwrite-tag-end (the-end)
  "Internal xslide function.  Do not use."
  (save-excursion
    ; if we aren't looking at another attribute
    (if (not (looking-at (concat xsl-ows-regex xsl-node-name-regex xsl-ows-regex "="
                 xsl-ows-regex "\\(\"\\([^\"]*\\)\"\\|\'\\([^\']*\\)\'\\)")))
      (progn
        (insert the-end)
        ; if we are looking at a right-carrot with or without a quote and/or slash
        (if (looking-at (concat xsl-ows-regex "\"?" xsl-ows-regex "/?" xsl-ows-regex ">"))
          (delete-region (point) (search-forward ">" nil t)))))))

(defun xsl-list-contains-string (L S)
  "Returns true if list L contains string S."
  (if L
      (let ( (idx 0)
             (len (length L)))
        (while (and (< idx len)
                    (not (string-equal (nth idx L) S)))
          (setq idx (+ idx 1)))
        (if (< idx len)
            idx
          nil))
    nil))

(defun xsl-electric-space ()
  "Function called when the space key is pressed in XSL mode.
This will attempt to auto-complet tags by adding each required attribute.
If there is one optional attribute and the required is already present,
it will autocomplete that.  If no more attributes can be added, it will
quick-close the tag (if appropriate)."
  (interactive)
  (insert " ")
  ; Look to see if we just started an xsl tag and need autocompletion
  (let
      ( (ins-pt (point))
        (node-name nil)
        (attribs nil)
        (vals nil) )
    (cond
     ; Do auto-complete in comments or xsl:text sections
  ;    ((xsl-is-in-comment))
      ; Don't auto-complete in cdata sections
      ((xsl-is-in-cdata))
      ; See if we are about to type an attribute
      ((save-excursion
            (and
              ; find start of tag we might be in.  Also remember the node name for
              ; later.  Limit the search to 1000 characters back.
;TODO: consider concatting "!?\??" to node name regex to match more nodes.
              (re-search-backward (concat "<\\(" xsl-node-name-regex "\\) ") (- ins-pt 1000) t 1)
              ; remember the node name for later (always evaluates to t so the (and)
              (setq node-name (match-string 1))
              ; It must be the start of THIS tag and not just some text after the
              ; last tag.
              (not (search-forward ">" ins-pt t 1))
              ; Make sure we are not within a quoted attribute value.  Also
              ; load all attribute names and values into lists for later
              ;
              ; The regex for arbitrarily matching the " or ' as an outer quote
              ; with the opposite inner quote is sufficiently complicated for me
              ; to not bother.  Consider when the user adds a space after the
              ; closing paren in the following:
              ; <xsl:when test="(ItemType='TCMN')">
              ; Obviously, point is considered inside the value of the test
              ; attribute.  But using the following regex:
              ;(concat xsl-node-name-regex "="
              ;        "\\(\"\\([^\"]*\\)\"\\|\'\\([^\']*\\)\'\\)"))
              ; When the double-quote match fails, it keeps trying until it
              ; finds the match starting with ItemType='TCMN' which is a valid
              ; attribute-value pair.  Whoops!
              ;
              ; So, for now, I'm going to assume that outer quotes are always double,
              ; and inner quotes are single.  Apologies to anyone who prefers
              ; the opposite style.
              (progn
                (while (re-search-forward (concat "\\(" xsl-node-name-regex "\\)" xsl-ows-regex "="
                     xsl-ows-regex "\"\\([^\"]*\\)\"") ins-pt t)
                  (setq attribs (cons (match-string 1) attribs))
                  (setq vals (cons (match-string 2) vals))
                )
                ; look for extra unmatched quote and don't proceed if we find one.
                (not (re-search-forward "\"" ins-pt t))
              )
            )
          )
          ; If there are attributes to the right of point, count them too!
          (if (looking-at (concat xsl-ows-regex xsl-node-name-regex xsl-ows-regex "="
                 xsl-ows-regex "\\(\"\\([^\"]*\\)\"\\|\'\\([^\']*\\)\'\\)"))
            (let*
                ( (end-of-this-tag (save-excursion(search-forward ">" nil t)))
                  (start-of-next-tag (save-excursion(search-forward "<" nil t)))
                  (forward-search-lim (min end-of-this-tag start-of-next-tag)) )
;              (message "have attributes right of point")
              (save-excursion
                (while (re-search-forward (concat "\\(" xsl-node-name-regex "\\)" xsl-ows-regex "="
                     xsl-ows-regex "\\(\"\\([^\"]*\\)\"\\|\'\\([^\']*\\)\'\\)") forward-search-lim t)
                  (setq attribs (cons (match-string 1) attribs))
                  (setq vals (cons (match-string 3) vals)))))) ; end if

; TODO: Check each of these tags for required and optional attributes.
; While all required attributes are not present, autocomplete them.
; Once all required attributes are found, if there is a single optional
; attribute and the tag cannot be a quick-close, auto-complete that attribute.
; When all possible attributes are used, close the tag.

; Also seperate into xsl:, xsl-fo: and no-prefix tags
                ; Do not autocomplete:
                ;   Optional attributes & only sometimes empty
                ;     xsl:apply-templates
                ;     xsl:copy
                ;   Optional attributes only and always empty
                ;     xsl:template

                ; Required: none
                ; Optional: none
                ; Empty: sometimes
                ; Just print a message
          (cond ( (string-equal node-name "xsl:apply-templates")
                  (message "Optional attributes: select, mode"))

                ; Autocomplete any of these below:
                ; Required: none
                ; Optional: none
                ; Empty: never
                ( (or (string-equal node-name "xsl:fallback")
                      (string-equal node-name "xsl:otherwise"))
                  (message (concat "XSL-WARNING: " node-name
                                   " takes no attributes and can never be empty.")))
                ; Required: none
                ; Optional: none to many
                ; Empty: always
                ( (or (string-equal node-name "xsl:apply-imports")
                      (string-equal node-name "xsl:decimal-format")
                      (string-equal node-name "xsl:number")
                      (string-equal node-name "xsl:output"))
                  (xsl-overwrite-tag-end "/>") )
                ; Required: none
                ; Optional: none to many
                ; Empty: always
                ( (string-equal node-name "xsl:sort")
                  (message "Optional attributes: select, lang, data-type, order, case-order")
                  (xsl-overwrite-tag-end "/>") )

                ; Required: elements
                ; Optional: none
                ; Empty: always
                ( (or (string-equal node-name "xsl:preserve-space")
                      (string-equal node-name "xsl:strip-space"))
                  (cond ( (not (xsl-list-contains-string attribs "elements"))
                          (insert "elements=\"")
                          (xsl-overwrite-tag-end "\" />") )) )

                ; Required: href
                ; Optional: none
                ; Empty: always
                ( (or (string-equal node-name "xsl:import")
                      (string-equal node-name "xsl:include"))
                  (cond ( (not (xsl-list-contains-string attribs "href"))
                          (insert "href=\"")
                          (xsl-overwrite-tag-end "\" />") )) )

                ; Required: name
                ; Optional: namespace
                ; Empty: never
                ( (string-equal node-name "xsl:attribute")
                  (cond ( (not (xsl-list-contains-string attribs "name"))
                          (insert "name=\"")
                          (save-excursion (insert "\"")) )
                        ( (not (xsl-list-contains-string attribs "namespace"))
                          (insert "namespace=\"")
                          (xsl-overwrite-tag-end "\">") )) )

                ; Required: name
                ; Optional: use-attribute-sets
                ; Empty: possibly with use-attribute-sets, no without.
                ((string-equal node-name "xsl:attribute-set")
                  (cond
                    ((not (xsl-list-contains-string attribs "name"))
                      (insert "name=\"")
                      (save-excursion (insert "\""))
                    )
                    ((not (xsl-list-contains-string attribs "use-attribute-sets"))
                      (insert "use-attribute-sets=\"")
                      (save-excursion (insert "\""))
                    )
                  )
                )
                ; Required: name
                ; Optional: namespace use-attribute-sets
                ; Empty: never
                ((string-equal node-name "xsl:element")
                  (cond
                    ((not (xsl-list-contains-string attribs "name"))
                      (insert "name=\"")
                      (save-excursion (insert "\""))
                    )
                  )
                )
                ; Required: name
                ; Optional: none
                ; Empty: never
                ((string-equal node-name "xsl:processing-instruction")
                  (cond
                    ((not (xsl-list-contains-string attribs "name"))
                      (insert "name=\"")
                      (xsl-overwrite-tag-end "\">")
                    )
                  )
                )
                ; Required: name
                ; Optional: more than one
                ; Empty: optional
                ((string-equal node-name "xsl:call-template")
                  (cond
                    ((not (xsl-list-contains-string attribs "name"))
                      (insert "name=\"")
                      (save-excursion (insert "\""))
                    )
                    (t
                      (message "Optional Attributes: select, mode")
                    )
                  )
                )
                ; Required: name
                ; Optional: select
                ; Empty: yes with select attribute, maybe without
                ((string-equal node-name "xsl:param")
                  (cond
                    ((not (xsl-list-contains-string attribs "name"))
                      (insert "name=\"")
                      (xsl-overwrite-tag-end "\">")
                    )
                    ((not (xsl-list-contains-string attribs "select"))
                      (message "Optional attribute: select (prohibits children)")
                    )
                    (t
                      (xsl-overwrite-tag-end "/>")
                    )
                  )
                )
                ; Required: name
                ; Optional: select
                ; Empty: yes with select attribute, no without
                ((or (string-equal node-name "xsl:variable")
                     (string-equal node-name "xsl:with-param"))
                  (cond
                    ((not (xsl-list-contains-string attribs "name"))
                      (insert "name=\"")
                      (xsl-overwrite-tag-end "\">")
                    )
                    ; If they have a second attribute it will always be select
                    ((not (xsl-list-contains-string attribs "select"))
                      (insert "select=\"")
                      (xsl-overwrite-tag-end "\" />")
                    )
                  )
                )
                ; Required: name, match, use
                ; Optional: none
                ; Empty: always
                ((string-equal node-name "xsl:key")
                  (cond
                    ((not (xsl-list-contains-string attribs "name"))
                      (insert "name=\"")
                      (save-excursion (insert "\""))
                    )
                    ((not (xsl-list-contains-string attribs "match"))
                      (insert "match=\"")
                      (save-excursion (insert "\""))
                    )
                    ((not (xsl-list-contains-string attribs "use"))
                      (insert "use=\"")
                      (xsl-overwrite-tag-end "\" />")
                    )
                  )
                )
                ; Required: select
                ; Optional: none
                ; Empty: always
                ((string-equal node-name "xsl:copy-of")
                  (cond
                    ((not (xsl-list-contains-string attribs "select"))
                      (insert "select=\"")
                      (xsl-overwrite-tag-end "\" />")
                    )
                    (t
                      (message "XSL-WARNING: xsl:copy-of only accepts the select attribute.")
                    )
                  )
                )
                ; Required: select
                ; Optional: disable-output-escaping
                ; Empty: always
                ((string-equal node-name "xsl:value-of")
                  (cond
                    ((not (xsl-list-contains-string attribs "select"))
                      (insert "select=\"")
                      (xsl-overwrite-tag-end "\" />")
                    )
    ;                ((not (xsl-list-contains-string attribs "disable-output-escaping"))
    ;                  ; no is the default, so assume that "yes" goes here.
    ;                  (xsl-overwrite-tag-end "disable-output-escaping=\"yes\" />")
    ;                )
                    (t
                      (message "Optional attribute: disable-output-escaping=\"yes/no\" (default is no)")
                    )
                  )
                )
                ; Required: select
                ; Optional: none
                ; Empty: never
                ((string-equal node-name "xsl:for-each")
                  (cond
                    ((not (xsl-list-contains-string attribs "select"))
                      (insert "select=\"")
                      (xsl-overwrite-tag-end "\">")
                    )
                    (t
                      (message "XSL-WARNING: xsl:for-each only allows the select attribute and may never be empty.")
                    )
                  )
                )
                ; Required: none
                ; Optional: terminate
                ; Empty: never
                ((string-equal node-name "xsl:message")
                  (cond
                    ((not (xsl-list-contains-string attribs "terminate"))
                      (insert "terminate=\"yes\">")
                      (message "The default value is terminate=\"no\"")
                    )
                    (t
                      (message "XSL-WARNING: xsl:message only allows the terminate attribute and may never be empty.")
                    )
                  )
                )
                ; Required: none
                ; Optional: disable-output-escaping
                ; Empty: never
                ((string-equal node-name "xsl:text")
                  (cond
                    ((not (xsl-list-contains-string attribs "disable-output-escaping"))
                      ; no is the default, so assume that "yes" goes here.
                      (xsl-overwrite-tag-end "disable-output-escaping=\"yes\">")
                      (message "The default value is: disable-output-escaping=\"no\"")
                      ; No nodes can go inside xsl:text, so if we're looking at a
                      ; node, auto-fill the end tag.
                      (if (looking-at (concat xsl-ows-regex "<"))
                        (save-excursion (insert "</xsl:text>"))
                      )
                    )
                    (t
                      (message "XSL-WARNING: xsl:text only takes disable-output-escaping and may never be empty.")
                    )
                  )
                )
                ; Required: test
                ; Optional: none
                ; Empty: never
                ( (or (string-equal node-name "xsl:if")
                      (string-equal node-name "xsl:when"))
                  (cond
                    ( (not (xsl-list-contains-string attribs "test"))
                      (insert "test=\"")
                      (xsl-overwrite-tag-end "\">"))
                    ( t
                      (message (concat "XSL-WARNING: " node-name
                                       " only takes a test attribute and can never be empty.")))))
                ; Required: stylesheet-prefix result-prefix
                ; Optional: none
                ; Empty: always
                ((string-equal node-name "xsl:namespace-alias")
                  (cond
                    ( (not (xsl-list-contains-string attribs "stylesheet-prefix"))
                      (insert "stylesheet-prefix=\"")
                      (save-excursion (insert "\"")))
                    ( (not (xsl-list-contains-string attribs "result-prefix"))
                      (insert "result-prefix=\"")
                      (xsl-overwrite-tag-end "\" />"))))
                ; Required: version
                ; Optional: many
                ; Empty: never
                ((or (string-equal node-name "xsl:stylesheet")
                     (string-equal node-name "xsl:transform"))
                  (if (not (xsl-list-contains-string attribs "version"))
                    (xsl-overwrite-tag-end "version=\"1.0\">")))
              )
          (if xsl-html-mode
            (cond
              ; Required: src
              ; Optional: width, height, alt
              ; Empty: never
              ( (string-equal node-name "img")
                (cond
                  ( (not (xsl-list-contains-string attribs "src"))
                    (insert "src=\"")
                    (save-excursion (insert "\"")))
                  ( t
                    (message "Optional attributes: width, height, alt..."))))
              ; Required: action, method
              ; Optional: tons
              ; Empty: never
              ( (string-equal node-name "form")
                (cond
                  ( (not (xsl-list-contains-string attribs "action"))
                    (insert "action=\"")
                    (save-excursion (insert "\"")))
                  ( (not (xsl-list-contains-string attribs "method"))
                    (insert "method=\"")
                    (save-excursion (insert "\""))
                    (message "Values: GET, POST"))
                  ( t
                    (message "Optional attributes: enctype, accept-charset, target, id, name(NN), title..."))))
              ; Required: type
              ; Optional: varies with type
              ; Empty: always
              ( (string-equal node-name "input")
                (cond
                  ( (not (xsl-list-contains-string attribs "type"))
                    (insert "type=\"")
                    (save-excursion (insert "\""))
                    (message "Values: button, checkbox, file, hidden, image, password, radio, reset, submit, text."))
                  ( t
                    ;;TODO: Make it add required attributes for whatever type it is.
                    (let
                        ( (type (nth (xsl-list-contains-string attribs "type") vals)) )
                      (cond
                        ; Required: name
                        ; Optional: lots
                        ; Empty: always
                        ((or (string-equal type "file")
                             (string-equal type "text"))
                          (cond
                            ((not (xsl-list-contains-string attribs "name"))
                              (insert "name=\"")
                              (xsl-overwrite-tag-end "\" />"))))
                        ; Required: name, value
                        ; Optional: lots (except hidden has none)
                        ; Empty: always
                        ( (or (string-equal type "button")
                              (string-equal type "checkbox")
                              (string-equal type "hidden")
                              (string-equal type "password")
                              (string-equal type "radio"))
                          (cond
                            ( (not (xsl-list-contains-string attribs "name"))
                              (insert "name=\"")
                              (save-excursion(insert "\"")))
                            ( (not (xsl-list-contains-string attribs "value"))
                              (insert "value=\"")
                              (xsl-overwrite-tag-end "\" />"))))
                        ; Required: name
                        ; Optional: lots
                        ; Empty: always
                        ( (string-equal type "image")
                          (cond
                            ((not (xsl-list-contains-string attribs "src"))
                              (insert "src=\"")
                              (xsl-overwrite-tag-end "\" />"))))
                        ; Required: none
                        ; Optional: lots
                        ; Empty: always
                        ( t
                          (xsl-overwrite-tag-end "/>"))
                      ) ; end con
                    ) ; end let
                  ) ; end has type attribute - add other attributes
                ) ; end con
              ) ; end node-name = input
              ( (string-equal node-name "script")
                (cond
                  ( (not (xsl-list-contains-string attribs "type"))
                    (insert "type=\"")
                    (save-excursion (insert "\""))
                    (message "Values: text/javascript, text/vbscript"))
                  ( t
                    (message "Other attributes: src, charset, defer, language=\"JavaScript\"(dep)"))))
              ( (string-equal node-name "link")
                (cond
                  ( (not (xsl-list-contains-string attribs "href"))
                    (insert "href=\"")
                    (xsl-overwrite-tag-end "\" />"))
                  ( (not (xsl-list-contains-string attribs "rel"))
                    (insert "width=\"")
                    (xsl-overwrite-tag-end "\" />"))
                  ( t (xsl-overwrite-tag-end " />") )))
              ((string-equal node-name "select")
                (if (not (xsl-list-contains-string attribs "name"))
                  (progn
                    (insert "name=\"")
                    (save-excursion
                      (insert "\"")
                      (xsl-electric-greater-than)
;                      (xsl-indent-line)
;                      (insert "\n<option></option>")
;                      (xsl-indent-line)
;                      (insert "\n</select")
;                      (xsl-overwrite-tag-end ">")
;                      (xsl-indent-line)

                  ))))
              ((string-equal node-name "option")
                (if (not (xsl-list-contains-string attribs "name"))
                  (progn
                    (message "Common optional attributes: value (text is default value), selected, label")
                    (xsl-overwrite-tag-end ">"))))
          )
        ) ; end if xsl-html-mode
      )
    ) ; end of big cond
  ) ; end let
;  (xsl-indent-line)
)
