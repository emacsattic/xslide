;;;; xslide-indent.el --- XSL indentation functions
;; $Id$

;; Copyright (C) 2003 Tony Graham / Glen Peterson

;; Author: Tony Graham <tkg@menteith.com> / Glen Peterson
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

(provide 'xslide-indent)
(require 'xslide "xslide")

(defcustom xsl-tab-width 2
  "Sets the indentation step size on entering xsl-mode.

You must run \"M-x xsl-mode\" (or just reload your xsl file) before
changing this variable will take effect.

If undefined, this variable will inherit its value from tab-width (or
from xsl-element-indent-step for backward compatibility if it is
defined).

If `xsl-indent-tabs-mode' is Spaces (nil), this controls the number of
spaces to insert for each indentation level.

If `xsl-indent-tabs-mode' is Tabs (t), this controls the width of the
tab stops for this buffer.

Consider using the same setting for this variable as those you work
with - regardless of your personal preference.

If you defined `xsl-element-indent-step' in your dot emacs file (for
an older version of xslide), please remove it since it is no longer
used (since 0.2.3).  Use xsl-tab-width instead."

  :type '(choice (const :tag "nil" nil)
                 (integer))
  :group 'xsl)

(defcustom xsl-indent-tabs-mode nil
  "Whether to indent using tabs or spaces.

You must run \"M-x xsl-mode\" (or just reload your xsl file) before
changing this variable will take effect.

To summarize:
  1.) Files indented with spaces will always look the same on every
      machine.  This feature is the main argument for using spaces.

  2.) Files indented with tabs will show up with each users preferred
      tab width on their machine if they have xsl-indent-tabs-mode set
      to Tabs (t).  This feature and a slightly reduced file size are
      the main arguments for using tabs.

  3.) For files indented with tabs and spaces, it's best to convert
      all the tabs to spaces.  Then (if desired) convert the spaces to
      tabs.  If you want to edit a file of this type and make minimal
      changes it is probably best to set xsl-indent-tabs-mode to
      Spaces (nil) and adjust xsl-tab-width until it matches the
      author's tab-width (8 is a common default).  Issue
      \"M-x xsl-mode\" to make your new settings take effect.  You
      will need to use the space key to do indentation in this case.

Consider using the same setting for this variable as those you work
with - regardless of your personal preference.

If `xsl-indent-tabs-mode' is Tabs (t), xslide makes a buffer-local
copy of tab-stop-list based on xsl-tab-width.  Your global
tab-stop-list will be ignored."

  :type '(choice (const :tag "Spaces" nil)
                 (const :tag "Tabs" t))
  :group 'xsl)

(defconst xsl-comment-backsearch-limit 50000

  "How many characters back to search when indenting to see if we are
in a comment, cdata, xsl:text section, or tag.  Set this very high,
then ratchet it down until you get the performance you want.  50K
seems pretty good on a 2GHz Pentium Windows machine.  Test by using
indent-region on about 500 lines near the end of files over 1 meg.")

(defconst xsl-invasive-comment-backsearch-limit 500

  "This overrides `xsl-comment-backsearch-limit' in the
xsl-invasive-indent function for better performance on massive
indentation requests.  Testing on a 2GHz Pentium Windows machine, 500
seemed fast enough that this was no longer the primary limit on
performance.  Setting this lower than the maximum length of a comment
in a file you invasively-indent will cause the indentation to break.
Setting it too high will make the indentation very slow.")

(defun xsl-delete ()
  "Function called when <delete> is pressed in XSL mode."
  (interactive)
  (if indent-tabs-mode
      (backward-delete-char 1)
      (backward-delete-char-untabify 1)))

(defun xsl-electric-return ()
  (interactive)
  (insert "\n")
  ; if in comment, cdata, or xsl:text, default to same indent level as
  ; previous line.
  (let
      ( (in-text (xsl-is-in-text)) )
    (if (or (xsl-is-in-comment) (xsl-is-in-cdata) in-text)
        (indent-to 0
                   (save-excursion
                     (previous-line 1)
                     (beginning-of-line)
                     (while (and (not (bobp))
                                 (looking-at "[ \t]*$"))
                       (previous-line 1)
                       (beginning-of-line))
                     (beginning-of-line-text)
                     (if (looking-at (concat "</?" xsl-node-name-regex))
                         ; our previous non-blank line starts with an
                         ; xml tag
                         (if in-text
                             ; xsl:text indentation should default to 0
                             0
                           ; comments or cdata should indent as though
                           ; this is the real thing
                           (let
                               ( (the-indent (xsl-calculate-indent t)) )
                             (message (concat "electric-return: normal indent within a comment: "
                                              (int-to-string the-indent)))
                             the-indent))
                       ; our previous blank line doesn't start with an
                       ; xml tag.
                       (current-column))))
      ; Otherwise tab out to default indent level.
      (xsl-electric-tab))))

(defun xsl-is-in-tag ()
  "Returns true if point is within an xml tag"
  ; Tag sections cannot be nested.  Search backward for the start,
  ; then forward for the end before the current point.  If we find a
  ; start tag but not an end before the point, we're in one.
  (let ( (pnt (point)) )
    (save-excursion
      (if (search-backward "<" (- pnt xsl-comment-backsearch-limit) t)
          (not (search-forward ">" pnt t))))))

(defun xsl-is-in-text ()
  "Returns true if point is within an xsl:text tag"
  ; xsl:text tags cannot contain child tags.  I don't know if they can
  ; contain cdata or comments, but comments and cdata sections can
  ; definitely contain xsl:text tags.  For now, we'll assume that
  ; xsl:text cannot contain comments or CDATA and see how far that
  ; gets us.
  ;
  ; Search backward for a start-tag, then forward for the end before
  ; the current point.  If we find a start xsl:text but not an end
  ; before the point, we're in one.
  (let ( (pnt (point)) )
    (save-excursion
      (if (search-backward "<xsl:text" (- pnt xsl-comment-backsearch-limit) t)
          (not (search-forward "</xsl:text>" pnt t))))))

(defun xsl-is-in-comment ()
  "Returns true if point is within an xsl comment"
  ; Comment sections cannot be nested.  Search backward for the start,
  ; then forward for the end before the current point.  If we find a
  ; start Comment but not an end before the point, we're in one.
  (let ( (pnt (point)) )
    (save-excursion
      (if (search-backward xsl-comment-start (- pnt xsl-comment-backsearch-limit) t)
          (not (search-forward xsl-comment-end pnt t))))))

(defun xsl-is-in-cdata ()
  "Returns true if point is within an xsl cdata section"
  ; CDATA sections cannot be nested.  Search backward for the start,
  ; then forward for the end before the current point.  If we find a
  ; start CDATA but not an end before the point, we're in one.
  (let ( (pnt (point)) )
    (save-excursion
      (if (search-backward xsl-cdata-start (- pnt xsl-comment-backsearch-limit) t)
         (not (search-forward xsl-cdata-end pnt t))))))

(defun xsl-indent-line ()
  "Indents each line when tab is NOT pressed manually in XSL mode."
  (interactive)
  (save-excursion
    (beginning-of-line)
    ; Comments can contain commented out code, so leave indentation as
    ; it is.  Don't try to "fix" the indentation of tags that may not
    ; have matching start or end tags.
    ;
    ; Anything within a CDATA section is copied exactly to the output
    ; result tree fragment.  This is very important for preserving
    ; JavaScript or other special formatting.  Don't mess it up by
    ; auto-indenting it!
    (cond ( (xsl-is-in-comment) )
          ( (xsl-is-in-cdata) )
          ( (xsl-is-in-text) )
          ; if outside comment and CDATA, calculate indent
          ; If we are in a tag, or the line doesn't start with a <
          ; perform an indent
          ( (or (looking-at (concat xsl-ows-regex "<")) (xsl-is-in-tag))
            (delete-horizontal-space)
            (indent-to (xsl-calculate-indent)) ))))

(defun xsl-electric-tab ()
  "Function called when TAB is pressed in XSL mode."
  (interactive)
  ;If we are in a comment, cdata, or xsl:text section, insert a real tab
  (cond ( (or (xsl-is-in-comment) (xsl-is-in-cdata) (xsl-is-in-text))
          (message "in comment, cdata, or xsl:text")
          (if xsl-indent-tabs-mode
              (insert "\t")
            (let ((count 0))
              (while (< count xsl-tab-width)
                (insert " ")
                (setq count (1+ count))))))
        ; here we are outside a comment.  Perform an indent.
        ( (save-excursion (beginning-of-line)
                          (delete-horizontal-space)
                          (indent-to 0 (xsl-calculate-indent)))
          ; if we are looking at a blank line
          (if (looking-at "[ \t]*$")
              (end-of-line)))))

(defun xsl-invasive-indent ()
  "Indents a whole file invasively.  Use with XML but not HTML!"
  (interactive)
  (let ( (prev-font-lock font-lock-mode)
         (xsl-start-tag-regex (concat "<" xsl-node-name-regex
                                      "\\(" xsl-ows-regex xsl-att-val-pair-regex "\\)*"
                                      xsl-ows-regex ">")) )
    (turn-off-font-lock)
    (beginning-of-buffer)

    ; This is really fast - faster than (replace-regexp ">[ \t\n]*<" ">\n<")
    (while (re-search-forward ">[ \t]*<" nil t)
      (backward-char 1)
      (insert "\n"))

    ; This is slower but not as slow
    ; as (replace-regexp "<\\([^/>]+\\)>[ \t\n]*</[^/>]+>" "<\\1 />")

    ; In understanding the following code, it is important to note
    ; that due to XML nesting rules, if an open tag is followed by a
    ; close tag, they must be the same tag.  Here we will turn them
    ; into a quick-close so that the following operation (the slowest)
    ; has less to process.
    (beginning-of-buffer)
    (while (re-search-forward xsl-start-tag-regex nil t)
      (if (looking-at "[ \t\n]*</")
          (let ( (pt (point)) )
            (search-forward ">")
            (delete-region (- pt 1) (point))
            (insert " />"))))



    ; This is the slowest.  Indent each line according to the usual rules.
    (beginning-of-buffer)
    (let
        ( (tmp-backsearch-max xsl-comment-backsearch-limit) )
      (setq xsl-comment-backsearch-limit xsl-invasive-comment-backsearch-limit)
      (while (not (eobp))
        (indent-to (xsl-calculate-indent nil))
        (end-of-line)
        (forward-char 1))
      (setq xsl-comment-backsearch-limit tmp-backsearch-max))

    (if prev-font-lock
        (turn-on-font-lock)
      t)))

; Let's examine the possibilities here in terms of two tags on separate lines:
;Indent -1:
;   <x />
;</x>
;   ---
;   </x>
;</x>
;
;Indent +1:
;   <x>
;      <x>
;   ---
;   <x>
;      <x />
;
;Indent +0:
;   ---
;   <x>
;   </x>
;(the last case is only legal if the nodes are the same)
;   <x />
;   <x>
;   ---
;   <x />
;   <x />
;   ---
;   </x>
;   <x>
;   ---
;   </x>
;   <x />
(defconst xsl-quick-close-no-lt-regex
  (concat xsl-node-name-regex
          "\\(" xsl-ows-regex xsl-att-val-pair-regex "\\)*"
          xsl-ows-regex "/" xsl-ows-regex ">")
  "Regular expression to match a quick-close tag without the leading <.")

(defconst xsl-normal-tag-start-line-regex (concat "^[ \t]*</?"
                                                  xsl-node-name-prefix-regex)
  "Short regular Expression to match a line that starts with
whitespace and the start of a tag")


(defun xsl-calculate-indent (&optional already-in-comment)
  "Calculate what the indent should be for the current line."
  ; if at beginning of buffer, indentation is zero
  (cond ( (bobp) 0 )
        ( t
          (save-excursion
            ; go to beginning of this line
            (beginning-of-line)
            ; Calculate indent up-to point
            (let* ( (xsl-end-calc-indent-area (point))
                    ; Are we currently looking at an end tag?
                    (xsl-curr-end-tag (looking-at "[ \t]*</"))
                    ; counter for later
                    (xsl-count-open-tags 0)
                    ; does the "good line" we base our indentation on
                    ; start with an end tag?
                    (xsl-good-line-is-end-tag nil)
                    ; What column is our good line indented to?
                    (xsl-good-line-indent-col 0)
                    )
              ; Find a line that starts with a good tag (not a special
              ; ! or ? tag) We will calculate our indent based on this.
              (re-search-backward xsl-normal-tag-start-line-regex nil t)
              (if (not already-in-comment)
                  (while (cond ( (looking-at "</xsl:text>")
                                 (re-search-backward xsl-normal-tag-start-line-regex nil t)
                                 t )
                               ( (xsl-is-in-comment)
                                 (search-backward xsl-comment-start)
                                 (re-search-backward xsl-normal-tag-start-line-regex nil t)
                                 t )
                               ( (xsl-is-in-cdata)
                                 (search-backward xsl-cdata-start)
                                 (re-search-backward xsl-normal-tag-start-line-regex nil t)
                                 t ))))
              ; do we need this line?
 ;             (re-search-backward "^" nil t)
              (beginning-of-line)
              ; Remember if our anchor line is a start or end tag
              (setq xsl-good-line-is-end-tag (looking-at "[ \t]*</"))
              (search-forward "<" xsl-end-calc-indent-area t)
              ; Remember indentation of our anchor line
              (setq xsl-good-line-indent-col
                    (1- (current-column)))
              (backward-char 1)
              ; count open tags between our anchor and our starting point.
              (while (search-forward "<" xsl-end-calc-indent-area t)
                      ; Just indent same as previous line in comments or cdata
                (cond ( (and (or (xsl-is-in-comment) (xsl-is-in-cdata))
                             (not already-in-comment))
                        (message "indent in comment") )
                      ; end-tag
                      ( (char-equal (char-after) ?/)
                        (setq xsl-count-open-tags (1- xsl-count-open-tags)))
                      ; comment, CDATA, or DOCTYPE - do nothing
                      ( (char-equal (char-after)  ?!) )
                      ; xml declaration - do nothing
                      ( (char-equal (char-after)  ??) )
                      ; quick-close - do nothing
                      ( (looking-at xsl-quick-close-no-lt-regex) )
                      ; must be an open tag
                      ( t
                        (setq xsl-count-open-tags (1+ xsl-count-open-tags)))))

;             ; count open tags between our anchor and our starting point.
;             (save-excursion
;               (while (re-search-forward (concat "<" xsl-node-name-prefix-regex)
;                                         xsl-end-calc-indent-area t)
;                 (if (and (or (xsl-is-in-comment) (xsl-is-in-cdata))
;                          (not already-in-comment))
;                     nil
;                   (setq xsl-count-open-tags (1+ xsl-count-open-tags)))))
;             ; Now count close tags and subtract them from the open tag count
;             (save-excursion
;               (while (search-forward "</" xsl-end-calc-indent-area t)
;                 (if (and (or (xsl-is-in-comment) (xsl-is-in-cdata))
;                          (not already-in-comment))
;                     nil
;                   (setq xsl-count-open-tags (1- xsl-count-open-tags)))))
;             ; Now count quick-close tags and subtract them from the open tag count
;             (save-excursion
;               (while (search-forward "/>" xsl-end-calc-indent-area t)
;                 (if (and (or (xsl-is-in-comment) (xsl-is-in-cdata))
;                          (not already-in-comment))
;                     nil
;                   (setq xsl-count-open-tags (1- xsl-count-open-tags)))))

              ; Don't un-indent twice if good line starts with </
              (if xsl-good-line-is-end-tag
                  (setq xsl-count-open-tags (1+ xsl-count-open-tags)))
              ; Un-indent one more if current line starts with </
              (if xsl-curr-end-tag
                  (setq xsl-count-open-tags (1- xsl-count-open-tags)))
              ; Calculate indent based on information we have collected
              ; if it's an end tag, we indent one less.
;             (message "Indent level: %d" xsl-count-open-tags)
              (+ xsl-good-line-indent-col
                 (* xsl-count-open-tags xsl-tab-width)))))))
;; end of xslide-indent.el
