;;; DO NOT MODIFY THIS FILE
(if (featurep 'xslide-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "xslide/_pkg.el")

(package-provide 'xslide :version 1.04 :type 'regular)

(autoload 'xsl-invasive-indent "xslide"
"Indents an xml file invasively by adding line-breaks between
parent and child nodes." t nil)

;;;***

;;;### (autoloads (xsl-mode xsl-grep) "xslide" "xslide/xslide.el")

(autoload 'xsl-grep "xslide" "\
Grep for PATTERN in files matching FILESPEC.

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
with both functions." t nil)

(autoload 'xsl-mode "xslide" "\
Major mode for editing XSL stylesheets.

Special commands:
\\{xsl-mode-map}
Turning on XSL mode calls the value of the variable `xsl-mode-hook',
if that value is non-nil.

Abbreviations:

XSL mode includes a comprehensive set of XSL-specific abbreviations
preloaded into the abbreviations table.

Font lock mode:

Turning on font lock mode causes various XSL syntactic structures to be
hightlighted. To turn this on whenever you visit an XSL file, add
the following to your .emacs file:
  (add-hook 'xsl-mode-hook 'turn-on-font-lock)

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
" t nil)
(add-to-list 'auto-mode-alist '("\\.\\(?:xsl\\|fo\\|xml\\)$" . xsl-mode))

;;;***

(provide 'xslide-autoloads)
