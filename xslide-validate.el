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
(provide 'xslide-validate)

(defconst xslide-validate-on

  "turns validation test code on or off.  Release builds should have
this off for now."
  t)

(require 'xslide "xslide")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun xsl-validate-buffer ()
  "Verifies an xsl document spotting certain errors"
  (interactive)
  (let
      ( (found-error nil)
        (curr-node-name "")
        (curr-attr-name ""))
    (goto-char (point-min))
    ; This could be valid in a CDATA so fix it.
    (cond ( (search-forward ">>" nil t)
            (message "Found >>")))
            ; Loop through document looking for bad tag names and/or attributes.
    (while (and (not found-error) (search-forward "<" nil t))
      (cond ( (looking-at xsl-node-name-regex)
              (re-search-forward (concat "\\(" xsl-node-name-regex "\\)" xsl-ows-regex) nil t)
              (setq curr-node-name (match-string 1))
              (while (and (not found-error) (looking-at xsl-node-name-regex))
                        (re-search-forward (concat "\\(" xsl-node-name-regex "\\)" xsl-ows-regex))
                        (setq curr-attr-name (match-string 1))
                        (cond ( (and (string-equal curr-attr-name "select")
                                     (or (string-equal curr-node-name "xsl:copy-of")
                                         (string-equal curr-node-name "xsl:param")
                                         (string-equal curr-node-name "xsl:value-of")
                                         (string-equal curr-node-name "xsl:variable"))
                                     (or (looking-at (concat "=" xsl-ows-regex "\"" xsl-ows-regex "\""))
                                         (looking-at (concat "=" xsl-ows-regex "'" xsl-ows-regex "'"))))
                                (message "Error: select attribute of params or variables cannot be empty")
                                (setq found-error t))
                              ; TODO: Look for unmatched parenthesis
                              ; in attributes of xsl: tags.  Make a
                              ; function to check for all kinds of
                              ; stuff within an attribute.  Pass it
                              ; the start and end points of the
                              ; attribute (within the quotes) and let
                              ; it go nuts.
                              ( (looking-at (concat "=" xsl-ows-regex "\""))
                                (search-forward "\"")
                                ; (re-search-forward (concat "\"" xsl-ows-regex))
                                (setq found-error (not (xsl-validate-attribute-value "\""))))
                              ( (looking-at (concat "=" xsl-ows-regex "'"))
                                (search-forward "'")
                                (setq found-error (not (xsl-validate-attribute-value "'"))))
                              ( t
                                (message "Error: Attribute names must always be followed by an equal sign and a quoted value.")
                                (setq found-error t))))
              (cond ( found-error )
                    ( (looking-at ">"))
                    ( (looking-at (concat "/" xsl-ows-regex ">")))
                    ( (looking-at "<")
                      (message "Error: new tag starts before previous tag completed.")
                      (setq found-error t))
                    ( t (message "Illegal characters within a start tag.")
                        (setq found-error t))))
            ( (looking-at (concat "/" xsl-node-name-regex)))
            ( (looking-at "!")
              (cond ( (looking-at "!--[^-]")
                      (goto-char (+ (point) 3))
                      (search-forward "--")
                      (if (looking-at ">")
                          t
                        (message "Bad Comment: -- is not allowed in a comment except to end it with -->")
                        (setq found-error t)))
                    ( (looking-at "![[]CDATA[[]")
                      (search-forward "]]>"))
                    ( (looking-at (concat "!" xsl-node-name-regex)))
                    ( t (message "Bad special (!) tag.  Must be <!--, <![CDATA[, or <!TAGNAME.")
                        (setq found-error t))))
            ( t (message "Tags must start with a-z, A-Z, _ or : and continue with the same characters plus . - and 0-9.")
               (setq found-error t))))))

;(setq outer-quote "\"")
(defun xsl-validate-attribute-value (outer-quote)
  (let
      ( (end-pos nil)
        (found-error nil) )
    (save-excursion (setq end-pos (search-forward outer-quote nil t)))
    (cond ( (save-excursion (search-forward ">" end-pos t))
            (message "Error: Tag ends before attribute value does (missing closing quote).")
            (setq found-error t))
          ( (save-excursion (search-forward "<" end-pos t))
            (message "Error: Attribute values may not contain the < character (missing closing quote).")
            (setq found-error t)))
    (if (or (save-excursion (search-forward "(" end-pos t))
            (save-excursion (search-forward ")" end-pos t)))
        (let
            ( (open-paren 0)
              (close-paren 0) )
          (save-excursion (while (search-forward "(" end-pos t)
                            (setq open-paren (+ open-paren 1))))
          (save-excursion (while (search-forward ")" end-pos t)
                            (setq close-paren (+ close-paren 1))))
          (if (= open-paren close-paren)
              t
            (message "Error: Unmatched parenthesis in attribute value")
            (setq found-error t))))
    (if found-error
        nil
      (re-search-forward (concat outer-quote xsl-ows-regex))
      t)))

;(defun xsl-verify-nested-parens (start-paren end-paren)
;  (let
;      ( (count-nesting 0) )

;;; xslide-validate.el ends here
