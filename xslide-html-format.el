(require 'xslide)

(defun xsl-html-format-code-sample ()
  "Fontify an html fragment - missing #888888 attribute values"
  (interactive)
  (let* ( (xsl-tag-w-attr-regex (concat "&lt;" xsl-node-name-regex
                                        "\\(" xsl-ows-regex xsl-att-val-pair-regex "\\)+"
                                        xsl-ows-regex "/?" xsl-ows-regex "&gt;"))
          (xsl-html-format-code-sample-color-tag "#0000AA")
          (xsl-html-format-code-sample-color-att-val "#990000")
          (xsl-html-format-code-sample-color-comment "#888888")
          )
          ; translate <> to entities
          (save-excursion (replace-string "<" "&lt;"))
          (save-excursion (replace-string ">" "&gt;"))
  
          ; Color groups of tags without attributes dark blue
          (save-excursion (replace-regexp (concat "\\(\\(&lt;" xsl-ows-regex "/?"
                                                    xsl-ows-regex
                                                    xsl-node-name-regex
                                                    xsl-ows-regex "/?"
                                                    xsl-ows-regex "&gt;"
                                                    xsl-ows-regex "\\)+\\)")
                                          (concat "<b><font color=\""
                                                  xsl-html-format-code-sample-color-tag
                                                  "\">\\1</font></b>")))
          ; Pick up tags with attributes
          (save-excursion
            (while (re-search-forward xsl-tag-w-attr-regex nil t)
              ; We have a tag, now modify it.
              (let ( (start-of-tag (- (point) (length (match-string 0)))) )
                ; code end of tag first and work back to beginning
                ; (because the start point of the tag won't change when we
                ; insert stuff) coding each attribute and value as we go.
                (insert "</font></b>")
                ; Find end of previous attribute
                (while (re-search-backward "[\'\"]" start-of-tag t)
                  (cond ( (looking-at "\"")
                          (xsl-insert-after-point (concat "</font><font color=\""
                                                          xsl-html-format-code-sample-color-tag
                                                          "\">"))
                          (search-backward "\"" start-of-tag t))
                        ( t
                          (xsl-insert-after-point (concat "</font><font color=\""
                                                          xsl-html-format-code-sample-color-tag
                                                          "\">"))
                          (search-backward "\'" start-of-tag t)))
                  ; advance past open-quote
                  (forward-char 1)
                  (xsl-insert-after-point (concat "</font><font color=\""
                                                  xsl-html-format-code-sample-color-att-val
                                                  "\">"))
                  ; reverse to before open-quote
                  (backward-char 1))
                ; code the beginning of the tag
                (search-backward "&lt;" start-of-tag t)
                (insert (concat "<b><font color=\""
                                xsl-html-format-code-sample-color-tag
                                "\">")))))
          (replace-regexp "\\(&lt;!--[^-]*\\(-[^-]+\\)*--&gt;\\)"
                          (concat "<b><font color=\""
                                  xsl-html-format-code-sample-color-comment
                                  "\">\\1</font></b>"))
))

(defun xsl-insert-after-point (str)
  "Inserts the given string after the point."
  (insert str)
  (backward-char (length str)))

(provide 'xslide-html-format)
