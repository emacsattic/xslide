;;;; xslide-data.el --- XSL IDE element and attribute data
;; $Id$

;; Copyright (C) 1998, 1999, 2000, 2001 Tony Graham

;; Author: Tony Graham <tkg@menteith.com>

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

;; Data about elements and attributes in XSL stylesheets collected
;; in one place

;; Send bugs to xslide-bug@menteith.com
;; Use `xsl-submit-bug-report' for bug reports


;;;; Variables

(defvar xsl-xslt-ns-prefix "xsl"
  "*Prefix for the XSL namespace")

(defvar xsl-fo-ns-prefix "fo"
  "*Prefix for the Formatting Object namespace")

(defvar xsl-element-symbol-alist
  (list
   '("apply-imports"
     "empty"
     ()
     "xai")
   '("apply-templates"
     "block"
     ("select" "mode")
     "xat")
   '("attribute"
     "block"
     ("name" "namespace")
     "xa")
   '("attribute-set"
     "block"
     ("name" "use-attribute-sets")
     "xas")
   '("call-template"
     "block"
     ("name")
     "xct")
   '("choose"
     "block"
     ()
     "xc")
   '("comment"
     "block"
     ()
     "xcm")
   '("copy"
     "block"
     ("use-attribute-sets")
     "xcp")
   '("copy-of"
     "block"
     ("select")
     "xco")
   '("decimal-format"
     "block"
     ("name" "decimal-separator" "grouping-separator" "infinity"
      "minus-sign" "NaN" "percent" "per-mille" "zero-digit"
      "digit" "pattern-separator")
     "xdf")
   '("element"
     "block"
     ("name" "namespace" "use-attribute-sets")
     "xe")
   '("fallback"
     "block"
     ()
     "xfb")
   '("for-each"
     "block"
     ("select")
     "xfe")
   '("if"
     "block"
     ("test")
     "xif")
   '("import"
     "empty"
     ("href")
     "xim")
   '("include"
     "empty"
     ("href")
     "xinc")
   '("key"
     "block"
     ("name" "match" "use")
     "xk")
   '("message"
     "block"
     ("terminate")
     "xme")
   '("namespace-alias"
     "block"
     ("stylesheet-prefix" "result-prefix")
     "xna")
   '("number"
     "empty"
     ("level" "count" "from" "value" "format" "lang" "letter-value"
      "grouping-separator" "grouping-size")
     "xn")
   '("otherwise"
     "block"
     ()
     "xo")
   '("output"
     "empty"
     ("method" "version" "encoding" "omit-xml-declaration"
      "standalone" "doctype-public" "doctype-system"
      "cdata-section-elements" "indent" "media-type")
     "xout")
   '("param"
     "block"
     ("name" "select")
     "xpa")
   '("preserve-space"
     "empty"
     ("elements")
     "xps")
   '("processing-instruction"
     "block"
     ("name")
     "xpi")
   '("sort"
     "empty"
     ("select" "lang" "data-type" "order" "case-order")
     "xso")
   '("strip-space"
     "empty"
     ("elements")
     "xss")
   (list "stylesheet"
     "block"
     (list
      '("id" nil)
      '("extension-element-prefixes" nil)
      '("exclude-result-prefixes" nil)
      '("version" nil)
      '("xmlns" nil)
      '("xmlns:xsl" t)
      '("xmlns:fo" nil))
     "xs")
   '("template"
     "block"
     ("match" "mode" "priority" "name")
     "xt")
   '("text"
     "inline"
     ("disable-output-escaping")
     "xtxt")
   (list "transform"
     "block"
     (list
      '("id" nil)
      '("extension-element-prefixes" nil)
      '("exclude-result-prefixes" nil)
      '("version" nil)
      '("xmlns" nil)
      '("xmlns:xsl" t)
      '("xmlns:fo" nil))
     "xtran")
   '("value-of"
     "empty"
     ("select" "disable-output-escaping")
     "xvo")
   '("variable"
     "block"
     ("name" "select")
     "xva")
   '("when"
     "block"
     ("test")
     "xw")
   '("with-param"
     "block"
     ("name" "select")
     "xwp")))

(defvar xsl-attributes-alist
  (list
   '("NaN" "nan" ())
   '("cdata-section-elements" "cds" ())
   '("count" "cnt" ())
   '("data-type" "dt" ())
   '("decimal-separator" "ds" ())
   '("digit" "dig" ())
   '("disable-output-escaping" "doe" ())
   '("doctype-public" "dtp" ())
   '("doctype-system" "dts" ())
   '("elements" "ele" ())
   '("encoding" "enc" ())
   '("exclude-result-prefixes" "erp" ())
   '("extension-element-prefixes" "eep" ())
   '("format" "fmt" ())
   '("from" "fr" ())
   '("grouping-separator" "gsep" ())
   '("grouping-size" "gsiz" ())
   '("href" "href" ())
   '("id" "id" ())
   '("indent" "ind" ())
   '("infinity" "inf" ())
   '("lang" "l" ())
   '("letter-value" "lv" ())
   '("level" "lvl" ())
   '("match" "m" ())
   '("media-type" "mt" ())
   '("method" "meth" ())
   '("minus-sign" "ms" ())
   '("mode" "mo" ())
   '("n-digits-per-group" "ndpg" ())
   '("name" "n" ())
   '("namespace" "ns" ())
   '("omit-xml-declaration" "oxml" ())
   '("order" "o" ())
   '("pattern-separator" "ps" ())
   '("per-mille" "pm" ())
   '("percent" "perc" ())
   '("priority" "p" ())
   '("result-prefix" "rp" ())
   '("select" "s" ())
   '("standalone" "stand" ())
   '("stylesheet-prefix" "spr" ())
   '("terminate" "ter" ())
   '("test" "t" ())
   '("use" "use" ())
   '("use-attribute-sets" "ua" ())
   '("value" "v" ())
   '("version" "ver" ())
   '("xmlns" "xn" ())
   '("xmlns:fo" "xnf" ())
   '("xmlns:xsl" "xnx" ("http://www.w3.org/1999/XSL/Transform"))
   '("zero-digit" "zd" ())))

(defvar xsl-fo-symbol-alist
  (list
   '("block" "block" () "fb")
   '("block-level-box" "block" () "fblb")
   '("character" "block" () "fc")
   '("external-graphic" "block" () "feg")
   '("inline-box" "block" () "fib")
   '("link-end-locator" "block" () "flel")
   '("list-item" "block" () "fli")
   '("list-item-body" "block" () "flib")
   '("list-item-label" "block" () "flil")
   '("link" "block" () "flnk")
   '("list" "block" () "fl")
   '("page-number" "block" () "fpn")
   '("page-sequence" "block" () "fps")
   '("queue" "block" () "fq")
   '("root" "block" () "fr")
   '("rule-graphic" "block" () "frg")
   '("sequence" "block" () "fs")
   '("simple-page-master" "block" () "fspm")))

(defvar xsl-fo-attribute-symbol-alist
  (list
   '("background-attachment" "batt")
   '("background-color" "bc")
   '("background-image" "bi")
   '("background-position-x" "bpx")
   '("background-position-y" "bpy")
   '("background-repeat" "br")
   '("block-justification-letterspace-max-add" "bjlma")
   '("block-justification-letterspace-max-remove" "bjlmr")
   '("block-justification-wordspace-max" "bjwmax")
   '("block-justification-wordspace-min" "bjwmin")
   '("block-line-breaking" "blb")
   '("body-overflow" "bo")
   '("body-writing-mode" "bwm")
   '("break-after" "ba")
   '("break-before" "bb")
   '("char" "ch")
   '("char-kern" "ck")
   '("char-kern-mode" "ckm")
   '("char-ligature" "cl")
   '("color" "co")
   '("contents-alignment" "ca")
   '("contents-rotation" "cr")
   '("destination" "d")
   '("direction-embedded-text" "det")
   '("end-side-overflow" "eso")
   '("end-side-separation" "essep")
   '("end-side-size" "ess")
   '("end-side-writing-mode" "eswm")
   '("external-graphic-id" "egi")
   '("font-family" "ff")
   '("font-size" "fsi")
   '("font-size-adjust" "fsa")
   '("font-stretch" "fstr")
   '("font-style" "fs")
   '("font-style-math" "fsm")
   '("font-variant" "fv")
   '("font-weight" "fw")
   '("footer-overflow" "fo")
   '("footer-precedence" "fp")
   '("footer-separation" "fsep")
   '("footer-size" "fsi")
   '("footer-writing-mode" "fwm")
   '("graphic-line-offset" "glo")
   '("graphic-line-thickness" "glt")
   '("graphic-max-height" "gmh")
   '("graphic-max-width" "gmw")
   '("graphic-notation-id" "gni")
   '("graphic-scale" "gs")
   '("header-overflow" "ho")
   '("header-precedence" "hp")
   '("header-separation" "hsep")
   '("header-size" "hs")
   '("header-writing-mode" "hwm")
   '("height" "h")
   '("id" "id")
   '("indent-end" "ie")
   '("indent-first-line-start" "ifls")
   '("indent-start" "is")
   '("inhibit-wrap" "iwr")
   '("inline" "in")
   '("input-record-end-ignore" "irei")
   '("input-tab" "it")
   '("input-tab-expand" "ite")
   '("input-whitespace" "iw")
   '("input-whitespace-treatment" "iwt")
   '("keep" "k")
   '("keep-with-next" "kwn")
   '("keep-with-previous" "kwp")
   '("label-alignment" "la")
   '("label-width" "lw")
   '("language" "lang")
   '("letterspacing-after-maximum" "lamax")
   '("letterspacing-after-minimum" "lamin")
   '("letterspacing-after-optimum" "lao")
   '("linespacing" "l")
   '("margin-bottom" "mb")
   '("margin-end" "me")
   '("margin-left" "ml")
   '("margin-right" "mr")
   '("margin-top" "mt")
   '("master-name" "mn")
   '("orphans" "o")
   '("overflow" "ov")
   '("page-height" "ph")
   '("page-width" "pw")
   '("page-writing-mode" "pwm")
   '("position-point-shift" "pps")
   '("position-point-x" "ppx")
   '("position-point-y" "ppy")
   '("queue-name" "qn")
   '("rule-grahic-orientation" "rgo")
   '("rule-graphic-length" "rgl")
   '("scale" "sc")
   '("score-spaces" "ss")
   '("space-after" "sa")
   '("space-after.maximum" "samax")
   '("space-after.minimum" "samin")
   '("space-after.optimum" "sao")
   '("space-before" "sb")
   '("space-before.maximum" "sbmax")
   '("space-before.minimum" "sbmin")
   '("space-before.optimum" "sbo")
   '("start-side-overflow" "sso")
   '("start-side-separation" "ssep")
   '("start-side-size" "ssz")
   '("start-side-writing-mode" "sswm")
   '("text-align" "ta")
   '("text-align-last" "tal")
   '("widows" "wdw")
   '("width" "w")
   '("wordspacing-maximum" "wmax")
   '("wordspacing-minimum" "wmin")
   '("wordspacing-optimum" "wo")
   '("writing-mode" "wm")))

(setq xsl-all-attribute-alist
      (sort
       (append
	xsl-attributes-alist
	xsl-fo-attribute-symbol-alist)
       (lambda (a b) (string< (car a) (car b)))))

(setq xsl-all-elements-alist
      (sort
       (append
	(mapcar (lambda (x)
		  (cons (if xsl-xslt-ns-prefix
			    (concat xsl-xslt-ns-prefix ":" (car x))
			  (car x))
			(cdr x)))
		xsl-element-symbol-alist)
	(mapcar (lambda (x)
		  (if xsl-fo-ns-prefix
		      (cons
		       (concat xsl-fo-ns-prefix ":" (car x))
		       (cdr x))
		    x))
		xsl-fo-symbol-alist))
       (lambda (a b) (string< (car a) (car b)))))

(provide 'xslide-data)

;; end of xslide-data.el
