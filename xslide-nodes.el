;;;; xslide.el --- XSL Integrated Development Environment
;; $Id$

;; Copyright (C) 1998, 1999, 2000, 2001 Tony Graham

;; Author: Tony Graham <tkg@menteith.com>
;; Contributors: Simon Brooke, Girard Milmeister, Norman Walsh,
;;               Moritz Maass, Lassi Tuura, Simon Wright, KURODA Akira
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

(provide 'xslide-nodes)

;; Functions for editing XSL stylesheets
(defconst xsl-node-type "xsl-node-type" "xsl-node-type")

; Tag name
; can be empty
; can have children
; required attributes
; optional attribute
; child name list
(defstruct (xsl-node
	    (:type vector)
	    (:constructor xsl-make-node-info (node-name can-be-empty can-have-children req-atts opt-atts child-names)))
  'xsl-node-type     ; The type of this variable
  node-name				; STRING, name of node
  can-be-empty
  can-have-children
  req-atts
  opt-atts
  child-names)

(defun xsl-node-get-name (node)
  (if (and (vectorp node) (eq (elt node 0) xsl-node-type))
    (elt node 1)
    (error "xsl-node-get-name: %s is not a proper %s" node xsl-node-type)
    nil))

(defun xsl-node-get-can-be-empty (node)
  (if (and (vectorp node) (eq (elt node 0) xsl-node-type))
    (elt node 2)
    (error "xsl-node-get-can-be-empty: %s is not a proper %s" node xsl-node-type)
    nil))

(defun xsl-node-get-can-have-children (node)
  (if (and (vectorp node) (eq (elt node 0) xsl-node-type))
    (elt node 3)
    (error "xsl-node-get-can-have-children: %s is not a proper %s" node xsl-node-type)
    nil))

(defun xsl-node-get-req-attr-vector (node)
  (if (and (vectorp node) (eq (elt node 0) xsl-node-type))
    (elt node 4)
    (error "xsl-node-get-req-attr-vector: %s is not a proper %s" node xsl-node-type)
    nil))

(defun xsl-node-get-opt-attr-vector (node)
  (if (and (vectorp node) (eq (elt node 0) xsl-node-type))
    (elt node 5)
    (error "xsl-node-get-opt-attr-vector: %s is not a proper %s" node xsl-node-type)
    nil))

(defun xsl-node-get-child-name-list (node)
  (if (and (vectorp node) (eq (elt node 0) xsl-node-type))
    (elt node 6)
    (error "xsl-node-get-child-name-list: %s is not a proper %s" node xsl-node-type)
    nil))


; attributes
(defconst xsl-attr-type "xsl-attr-type" "xsl-attr-type")

(defstruct (xsl-attr
	    (:type vector)
	    (:constructor xsl-make-attribute-info (att-name val-list
                                              def-val req-empty
                                              allow-empty add-req-attr)))
  'xsl-attr-type ; The type of this variable
  att-name           ; name
  val-list           ; value list
  def-val            ; default value
  req-empty          ; requires empty tag?
  allow-empty        ; allows tag to be empty?
  add-req-attr)      ; list of other attributes this attribute requires

(defun xsl-attr-get-name (attr)
  (if (and (vectorp attr) (eq (elt attr 0) xsl-attr-type))
    (elt attr 1)
    (error "xsl-attr-get-name: %s is not a proper %s" attr xsl-attr-type)
    nil))

(defun xsl-attr-get-val-vector (attr)
  (if (and (vectorp attr) (eq (elt attr 0) xsl-attr-type))
    (elt attr 2)
    (error "xsl-attr-val-list: %s is not a proper %s" attr xsl-attr-type)
    nil))

(defun xsl-attr-get-def-val (attr)
  (if (and (vectorp attr) (eq (elt attr 0) xsl-attr-type))
    (elt attr 3)
    (error "xsl-attr-def-val: %s is not a proper %s" attr xsl-attr-type)
    nil))

(defun xsl-attr-get-req-empty (attr)
  (if (and (vectorp attr) (eq (elt attr 0) xsl-attr-type))
    (elt attr 4)
    (error "xsl-attr-req-empty: %s is not a proper %s" attr xsl-attr-type)
    nil))

(defun xsl-attr-get-allow-empty (attr)
  (if (and (vectorp attr) (eq (elt attr 0) xsl-attr-type))
    (elt attr 5)
    (error "xsl-attr-allow-empty: %s is not a proper %s" attr xsl-attr-type)
    nil))

(defun xsl-attr-get-add-req-attr-vector (attr)
  (if (and (vectorp attr) (eq (elt attr 0) xsl-attr-type))
    (elt attr 6)
    (error "xsl-attr-get-add-req-attr-vector: %s is not a proper %s" attr xsl-attr-type)
    nil))

(defun xsl-show-attr-info (attr &optional indent)
  (let* ( (the-indent (if indent indent ""))
          (ret (concat the-indent (xsl-attr-get-name attr)))
          (values (xsl-attr-get-val-vector attr))
          (default (xsl-attr-get-def-val attr))
          (other-attr (xsl-attr-get-add-req-attr-vector attr)) )
    (if (xsl-attr-get-req-empty attr)
      (setq ret (concat ret " - requires this tag to be empty (quick-close)")))
    (if (xsl-attr-get-allow-empty attr)
      (setq ret (concat ret " - allows this tag to be empty (quick-close)")))
    (if (elt values 0)
      (let
        ((idx 0))
        (setq ret (concat ret "\n" the-indent "  "))
        (while (< idx (length values))
          (setq ret (concat ret (elt values idx)
                            (if (< idx (- (length values) 1))
                              ", ")))
          (setq idx (+ idx 1)))))
    (if default
      (setq ret (concat ret "\n" the-indent "  default: " default)))
    (if (elt other-attr 0)
      (let
        ((idx 0))
        (setq ret (concat ret "\n" the-indent "This attribute requires these additional attributes:\n" the-indent "  "))
        (while (< idx (length other-attr))
          (setq ret (concat ret (elt other-attr idx)
                            (if (< idx (- (length values) 1))
                              ", ")))
          (setq idx (+ idx 1)))))
    ret))

(defconst xsl-tag-info-vector
;  "Contains the rules for all xsl tags.
;Specifies required and optional parameters, valid values,
;where the tag can occur, and whether it can have children."

; Comprised of:
; Tag name
; can be empty
; can have children
; required attributes (each a vector)
;   name
;   value list
;   default value
;   requires empty tag
;   allows empty tag
;   list of other attributes this attribute requires
; optional attribute (each a vector: same as attributes above)
; child name list
  (vector
;    (xsl-make-node-info "myNode" nil t
;      (vector
;        (xsl-make-attribute-info "reqAtt1" ["val11" "val12" "val13"] "val1" nil nil ["opt11Att" "opt12Att"])
;        (xsl-make-attribute-info "reqAtt2" ["val21" "val22" "val23"] "val2" nil nil ["opt21Att" "opt22Att"]))
;      (vector
;        (xsl-make-attribute-info "optAtt1" ["val11" "val12" "val13"] "val1" nil nil [nil])
;        (xsl-make-attribute-info "optAtt2" ["val21" "val22" "val23"] "val2" nil nil [nil]))
;      '( ("child1" . "+")
;         ("child2" . "*")
;         ("chlid3" . "")))
    (xsl-make-node-info "xsl:apply-imports" t nil [nil] [nil] '(nil))
    (xsl-make-node-info "xsl:apply-templates" t t
                        [nil]
                        (vector
                         (xsl-make-attribute-info "select" [nil] nil t nil [nil])
                         (xsl-make-attribute-info "mode" [nil] nil nil nil [nil]))
                        '( ("xsl:sort" . "*")
                           ("xsl:with-param" . "*")))
    (xsl-make-node-info "xsl:attribute" nil t
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "namespace" [nil] nil nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:attribute-set" nil t
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "use-attribute-sets" [nil] nil nil t [nil]))
                        '( ("xsl:attribute" . "*")))
    (xsl-make-node-info "xsl:call-template" t t
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "select" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "mode" [nil] nil nil nil [nil]))
                        '( ("xsl:with-param" . "*") ))
    (xsl-make-node-info "xsl:copy" t t
                        [nil]
                        ( vector
                          (xsl-make-attribute-info "use-attribute-sets" [nil] nil nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:copy-of" t nil
                        ( vector
                          (xsl-make-attribute-info "select" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:choose" nil t
                        [nil]
                        [nil]
                        '( ("xsl:when" . "+")
                           ("xsl:otherwise" . "?")))
    (xsl-make-node-info "xsl:decimal-format" t nil
                        [nil]
                        (vector
                         (xsl-make-attribute-info "name" [nil] nil t nil [nil])
                         (xsl-make-attribute-info "decimal-separator" [nil] "." nil nil [nil])
                         (xsl-make-attribute-info "grouping-separator" [nil] "," nil nil [nil])
                         (xsl-make-attribute-info "infinity" [nil] "Infinity" nil nil [nil])
                         (xsl-make-attribute-info "minus-sign" [nil] "-" nil nil [nil])
                         (xsl-make-attribute-info "NaN" [nil] "NaN" nil nil [nil])
                         (xsl-make-attribute-info "percent" [nil] "%" nil nil [nil])
                         (xsl-make-attribute-info "per-mille" [nil] nil nil nil [nil])
                         (xsl-make-attribute-info "zero-digit" [nil] "0" nil nil [nil])
                         (xsl-make-attribute-info "digit" [nil] "#" nil nil [nil])
                         (xsl-make-attribute-info "pattern-separator" [nil] ";" nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:element" nil t
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "namespace" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "use-attribute-sets" [nil] nil nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:fallback" nil t [nil] [nil] '(nil))
    (xsl-make-node-info "xsl:for-each" nil t
                        ( vector
                          (xsl-make-attribute-info "select" [nil] nil nil nil [nil]))
                        [nil]
                        '( ("xsl:sort" . "*") ))
    (xsl-make-node-info "xsl:if" nil t
                        ( vector
                          (xsl-make-attribute-info "test" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:import" t nil
                        ( vector
                          (xsl-make-attribute-info "href" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:include" t nil
                        ( vector
                          (xsl-make-attribute-info "href" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:key" t nil
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "match" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "use" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:message" t t
                        [nil]
                        ( vector
                          (xsl-make-attribute-info "terminate" 
                                                   ( vector "yes" "no" )
                                                   "no" nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:namespace-alias" t nil
                        ( vector
                          (xsl-make-attribute-info "stylesheet-prefix" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "result-prefix" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:number" t nil
                        [nil]
                        ( vector
                          (xsl-make-attribute-info "level"
                                                   ( vector "single" "multiple" "any" )
                                                   nil nil nil [nil])
                          (xsl-make-attribute-info "count" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "from" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "value" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "format" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "lang" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "letter-value"
                                                   ( vector "alphabetic" "traditional" )
                                                   nil nil nil [nil])
                          (xsl-make-attribute-info "grouping-separator" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "grouping-size" [nil] nil nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:otherwise" nil t [nil] [nil] '(nil))
    (xsl-make-node-info "xsl:output" t nil
                        [nil]
                        ( vector
                          (xsl-make-attribute-info "method"
                                                   ( vector "xml" "html" "text" )
                                                   nil nil nil [nil])
                          (xsl-make-attribute-info "version" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "encoding" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "omit-xml-declaration"
                                                   ( vector "yes" "no" )
                                                   "no" nil nil [nil])
                          (xsl-make-attribute-info "standalone"
                                                   ( vector "yes" "no" )
                                                   nil nil nil [nil])
                          (xsl-make-attribute-info "doctype-public" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "doctype-system" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "cdata-section-elements" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "indent"
                                                   ( vector "yes" "no" )
                                                   "no" nil nil [nil])
                          (xsl-make-attribute-info "media-type" [nil] nil nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:param" t t
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "select" [nil] nil t nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:preserve-space" t nil
                        ( vector
                          (xsl-make-attribute-info "elements" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:processing-instruction" nil t
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:sort" t nil
                        [nil]
                        ( vector
                          (xsl-make-attribute-info "select" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "order"
                                                   ( vector "ascending" "descending")
                                                   "ascending"
                                                   nil nil [nil])
                          (xsl-make-attribute-info "case-order" 
                                                   ( vector "upper-first" "lower-first")
                                                   nil nil nil [nil])
                          (xsl-make-attribute-info "lang" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "data-type"
                                                   ( vector "text" "number" )
                                                   "text"
                                                   nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:strip-space" t nil
                        ( vector
                          (xsl-make-attribute-info "elements" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:stylesheet" nil t
                        ( vector
                          (xsl-make-attribute-info "version"
                                                   ( vector "1.0" )
                                                   "1.0"
                                                   nil nil [nil])
                          (xsl-make-attribute-info "xmlns:xsl"
                                                   ( vector "http://www.w3.org/1999/XSL/Transform" )
                                                   "http://www.w3.org/1999/XSL/Transform"
                                                   nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "id" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "extension-element-prefixes" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "exclude-result-prefixes" [nil] nil nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:template" nil t
                        [nil]
                        ( vector
                          (xsl-make-attribute-info "match" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "priority" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "mode" [nil] nil nil nil [nil]))
                        '( ("xsl:param" . "*") ))
    (xsl-make-node-info "xsl:text" nil t
                        [nil]
                        ( vector
                          (xsl-make-attribute-info "disable-output-escaping" 
                                                   ( vector "yes" "no" )
                                                   "no" nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:transform" nil t
                        ( vector
                          (xsl-make-attribute-info "version"
                                                   ( vector "1.0" )
                                                   "1.0"
                                                   nil nil [nil])
                          (xsl-make-attribute-info "xmlns:xsl"
                                                   ( vector "http://www.w3.org/1999/XSL/Transform" )
                                                   "http://www.w3.org/1999/XSL/Transform"
                                                   nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "id" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "extension-element-prefixes" [nil] nil nil nil [nil])
                          (xsl-make-attribute-info "exclude-result-prefixes" [nil] nil nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:value-of" t nil
                        ( vector
                          (xsl-make-attribute-info "select" [nil] nil nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "disable-output-escaping" 
                                                   ( vector "yes" "no" )
                                                   "no" nil nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:variable" nil t
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "select" [nil] nil t nil [nil]))
                        '(nil))
    (xsl-make-node-info "xsl:when" nil t
                        ( vector
                          (xsl-make-attribute-info "test" [nil] nil nil nil [nil]))
                        [nil]
                        '(nil))
    (xsl-make-node-info "xsl:with-param" nil t
                        ( vector
                          (xsl-make-attribute-info "name" [nil] nil nil nil [nil]))
                        ( vector
                          (xsl-make-attribute-info "select" [nil] nil t nil [nil]))
                        '(nil))
;    (xsl-make-node-info "xsl:test" t nil
;      (vector
;        (xsl-make-attribute-info "reqAtt1" ["val1" "val2" "val3"] "val1" nil nil ["opt1Att" "opt2Att"])
;        (xsl-make-attribute-info "reqAtt2" ["val4" "val5"] nil nil nil [nil]))
;      (vector
;        (xsl-make-attribute-info "optAtt1" [nil] nil nil nil [nil])
;        (xsl-make-attribute-info "optAtt2" [nil] nil nil nil [nil]))
;      '( ("child1" . "+")
;         ("child2" . "*")
;         ("chlid3" . "")))
))

;(xsl-tag-info "xsl:sort")
;(xsl-tag-info "xsl:test")
;(xsl-tag-info "xsl:apply-templates")
;(xsl-tag-info "xsl:apply-imports")
;(xsl-tag-info "xsl:choose")
;(xsl-tag-info "xsl:attribute-set")
;(xsl-tag-info "xsl:value-of")
;(xsl-tag-info "xsl:processing-instruction")
(defun xsl-print-alist (the-list &optional indent)
  (if (car the-list)
    (let ( (ret "\n")
           (the-indent (if indent indent "")) )
      (while (car the-list)
        (setq ret (concat ret the-indent (car (car the-list)) (cdr (car the-list)) "\n" ))
        (setq the-list (cdr the-list)))
      ret)
    "none\n"))

(defun xsl-show-all-attributes-in-vector (att-vector &optional indent)
  (if (elt att-vector 0)
    (let ( (idx 0)
           (ret "\n")
           (curr-att nil)
           (the-indent (if indent indent "")) )
      (while (< idx (length att-vector))
        (setq curr-att (elt att-vector idx))
        (setq ret (concat ret (xsl-show-attr-info curr-att the-indent) "\n"))
        (setq idx (+ idx 1)))
      ret) ; end let
    "none\n"))

;<hello world='a="b"' there="c='d'" />

(defun xsl-tag-info (&optional tag-name)
  "Call this for information on the current tag.
Specifies required and optional parameters, valid values,
where the tag can occur, and whether it can have children."
  (interactive)
  (if (not tag-name)
      (let ( (pt (point)) )
        (save-excursion
          (if (not (looking-at "<"))
              (search-backward "<" nil t)
            t)
          (if (and (looking-at xsl-any-tag-regex)
                   (< (- pt (length (match-string 0)))
                      (point)))
              (progn
                (re-search-forward xsl-node-name-regex)
                (setq tag-name (match-string 0)))
            (message "xsl-tag-info called outside a tag without parameters"))))
    t)
  (let ( (idx 0) )
    ; while we haven't searched the whole vector
    ; and we haven't found our tag name
    (while (and (< idx (length xsl-tag-info-vector))
                (not (equal tag-name (xsl-node-get-name (elt xsl-tag-info-vector idx))))
      ; go to the next item
      (setq idx (+ idx 1))))

    ; If we found one
    (if (< idx (length xsl-tag-info-vector))
      (let
        ( (our-node (elt xsl-tag-info-vector idx)) )
        ; select this sublist
        (with-displaying-help-buffer
          (lambda ()
            (print
              (concat
                ; display the first item of the selected list
                "<"
                (xsl-node-get-name our-node)
                ; if can be empty tag
                (if (xsl-node-get-can-be-empty our-node)
                  ; if can have children
                  (if (xsl-node-get-can-have-children our-node)
                    "> May be empty or may have children\n"
                    " /> Must be empty\n")
                  "> Must not be empty\n")
                "\nRequired Attributes: "
                (xsl-show-all-attributes-in-vector (xsl-node-get-req-attr-vector our-node) "  ")
                "Optional Attributes: "
                (xsl-show-all-attributes-in-vector (xsl-node-get-opt-attr-vector our-node) "  ")
;                "\nOptional Attributes:"
;                (xsl-show-all-attributes-in-list (xsl-node-get-opt-attr-vector our-node))
                "Children: "
                (xsl-print-alist (xsl-node-get-child-name-list our-node) "  ")))
            "XSL Help Buffer Displayed")
          (format "XSL-Help for %s" tag-name))))))

(defun xsl-show-all-attributes-in-list (all-att-list &optional indent)
  (let ( (ret "")
         (the-indent (if indent indent "")) )
    (while (not (null all-att-list))
      (setq ret (concat ret the-indent (xsl-get-attribute-info (car all-att-list))))
      (setq all-att-list (cdr all-att-list)))
    ret))

(defun xsl-get-attribute-info (att-info)
; get the name
  (if (car att-info)
    (let
      ( (ret (concat "\n   " (car att-info))) )
      ; advance past name
      (setq att-info (cdr att-info))
      ; if value-list is non-nil
      (if (car (car att-info))
        ; cut value-list from parent list and show it
        (setq ret (concat ret "\n      Values: " (xsl-show-val-list (car att-info))))
        ; No value-list: do nothing
      )
      ; advance past value-list
      (setq att-info (cdr att-info))
      (if (car att-info)
        (setq ret (concat ret ". Default: " (car att-info) "   "))
      )
      ; advance past default value
      (setq att-info (cdr att-info))
      (if (car att-info)
        (setq ret (concat ret "\n      Using this attribute requires an empty tag."))
      )
      ; advance past requires empty tag flag
      (setq att-info (cdr att-info))
      (if (car (car att-info))
        ; cut additional required attributes from parent list and show it
        (setq ret (concat ret "\n      This attribute requires these additional attributes: " (xsl-show-val-list (car att-info)) "   "))
        ; No value-list: do nothing
      )
      ret
    )
    " none."
  )
)

(defun xsl-show-val-list (val-list)
  (let
    (
      ; add first value to return string
      (ret (car val-list))
    )
    ; advance to next value
    (setq val-list (cdr val-list))
    ; while there are more values in the list
    (while (not (null val-list))
      (setq ret (concat ret ", " (car val-list)))
      (setq val-list (cdr val-list))
    )
    ret
  )
)

;(defun xsl-node-get-name (node-info-list) (car node-info-list))
;
;(defun xsl-get-empty-allowed (node-info-list) (car (cdr node-info-list)))
;
;(defun xsl-get-children-allowed (node-info-list) (car (nthcdr 2 node-info-list)))
;
;; nthcdr 3 gives all the attributes lists
;; 1st car gives just the required attributes list
;(defun xsl-get-required-attributes (node-info-list) (car (nthcdr 3 node-info-list)))
;
;(defun xsl-get-optional-attributes (node-info-list) (car (nthcdr 4 node-info-list)))
;
;(defun xsl-get-attribute-name (att-info-list) (car att-info-list))


; (defun xsl-tag-info (&optional tag-name)
;   "Call this for information on the current tag.
; Specifies required and optional parameters, valid values,
; where the tag can occur, and whether it can have children."
;   (interactive)
;   (let
;     (
;       (local-tag-list xsl-tag-info-list)
;     )
;     ; while the first item in the first sublist is not our tag name
;     (while (not (string-equal tag-name (xsl-node-get-name (car local-tag-list))))
;       ; go to the next sublist
;       (setq local-tag-list (cdr local-tag-list))
;     )
;     (if (> (length local-tag-list) 0)
;       (progn
;         ; select this sublist
;         (setq local-tag-list (car local-tag-list))
;         (with-displaying-help-buffer
;           (lambda ()
;             (print
;               (concat
;                 ; display the first item of the selected list
;                 "<"
;                 (xsl-node-get-name local-tag-list)
;                 ; if can be empty tag
;                 (if (xsl-get-empty-allowed local-tag-list)
;                   ; if can have children
;                   (if (xsl-get-children-allowed local-tag-list)
;                     "> May be empty or may have children\n"
;                     " /> Must be empty\n"
;                   )
;                   "> Must not be empty\n"
;                 )
;                 "Required Attributes:"
; 
;                 (xsl-show-all-attributes-in-list (xsl-get-required-attributes local-tag-list))
;                 "\nOptional Attributes:"
;                 (xsl-show-all-attributes-in-list (xsl-get-optional-attributes local-tag-list))
;               )
;             )
;             "XSL Help Buffer Displayed"
;           )
;           (format "XSL-Help for %s" tag-name)
;         )
;       )
;     )
;   )
; )



; (defun xsl-tag-info (&optional tag-name)
;   "Call this for information on the current tag.
; Specifies required and optional parameters, valid values,
; where the tag can occur, and whether it can have children."
;   (interactive)
;   (let
;     ((local-tag-list xsl-tag-info-list))
;     (while (not (string-equal tag-name (xsl-node-get-name (car local-tag-list))))
;       (setq local-tag-list (cdr local-tag-list)))
;     (if (> (length local-tag-list) 0)
;       (progn
;         (setq local-tag-list (car local-tag-list))
;         (with-displaying-help-buffer
;           (lambda ()
;             (print
;               (concat
;                 "<"
;                 (xsl-node-get-name local-tag-list)
;                 (if (xsl-get-empty-allowed local-tag-list)
;                      (if (xsl-get-children-allowed local-tag-list)
;                          "> May be empty or may have children\n"
;                          " /> Must be empty\n")
;                       "> Must not be empty\n")
;                 "Required Attributes:"
;                 (xsl-show-all-attributes-in-list (xsl-get-required-attributes local-tag-list))
;                 "\nOptional Attributes:"
;                 (xsl-show-all-attributes-in-list (xsl-get-optional-attributes local-tag-list))))
;             "XSL Help Buffer Displayed")
;           (format "XSL-Help for %s" tag-name))))))



