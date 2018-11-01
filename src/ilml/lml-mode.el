;; lml-mode.el. Major mode for editing Lazy ML.
;; Copyright (C) 1989, Free Software Foundation, Inc., Lars Bo Nielsen
;; and Lennart Augustsson

;; This file is not officially part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LML Mode. A major mode for editing and running LML. (Version 0.0)
;; =================================================================
;;
;; This is a mode for editing and running Lazy ML (LML).
;; It is very much based on the sml mode for GNU Emacs. It
;; features:
;;
;;      - Automatic indentation of sml code. With a number of
;;        constants to customize the indentation.
;;
;;      - Short cut insertion of commonly used structures, like
;;        "let in end", etc.
;;
;;      - Short cut insertion of "||". Will automaticly determine if
;;        "||" is used after a "case", "let" etc. If used after a "let"
;;        automaticly insert the name of the function, used after
;;        case automaticly insert ":" etc.
;;
;;      - Inferior shell running lml. No need to leave emacs, just
;;        keep right on editing while lml runs in another window.
;;
;;      - Automatic "load file" in inferior shell. Send regions of code
;;        to the lml program.
;;
;;      - Parsing of errors in the inferior shell. Like the next-error
;;        function used in c-mode (This will only work with LML of
;;        New Jersey).
;;
;;
;; 1. HOW TO USE THE LML-MODE
;; ==========================
;;
;; Here is a short introduction to the mode.
;;
;; 1.1 GETTING STARTED
;; -------------------
;;
;; If you are an experienced user of Emacs, just skip this section.
;;
;; To use the lml-mode, insert this in your "~/.emacs" file (Or ask your
;; emacs-administrator to help you.):
;;
;;    (setq auto-mode-alist (cons '("\\.m$" . lml-mode) 
;;                           auto-mode-alist)) 
;;    (autoload 'lml-mode "lml-mode" "Major mode for editing LML." t)
;;
;; Now every time a file with the extension `.m' is found, it is
;; automatically started up in lml-mode.
;;
;; You will also have to specify the path to this file, so you will have
;; to add this as well:
;;
;;    (setq load-path (cons "/usr/me/emacs" load-path))
;;
;; where "/usr/me/emacs" is the directory where this fill is.
;;
;; You may also want to compile the this file (M-x byte-compile-file)
;; for speed.
;;
;; You are now ready to start using lml-mode. If you have tried other
;; language modes (like lisp-mode or C-mode), you should have no
;; problems. There are only a few extra functions in this mode.
;;
;; 1.2. EDITING COMMANDS.
;; ----------------------
;;
;; The following editing and inferior-shell commands can ONLY be issued
;; from within a buffer in lml-mode.
;;
;; M-| (lml-electric-pipe). 
;;     In the case you are typing in a case expression, a function with
;;     patterns etc., this function will give you some help. First of all
;;     it  will put the `||' on a line by itself. If it is used in the
;;     definition of a function it inserts the functions name, else it
;;     inserts `:'. Just try it, you will like it.
;;
;; LFD (reindent-then-newline-and-indent).  
;;     This is probably the function you will be using the most (press
;;     CTRL while you press Return, press C-j or press Newline). It
;;     will reindent the line, then make a new line and perform a new
;;     indentation.
;;
;; TAB (lml-indent-line).  
;;     This function indents the current line.
;;
;; C-c TAB (lml-indent-region).
;;     This function indents the current region (be patient if the
;;     region is large).
;;
;; M-; (indent-for-comment).
;;     Like in other language modes, this command will give you a comment
;;     at the of the current line. The column where the comment starts is
;;     determined by the variable comment-column (default: 40).
;;    
;; C-c C-v (lml-mode-version). 
;;     Get the version of the lml-mode.
;;
;;
;; 1.3. COMMANDS RELATED TO THE INFERIOR SHELL
;; -------------------------------------------
;;
;; C-c C-s (lml-pop-to-shell).
;;     This command starts up an inferior shell running lml. If the shell
;;     is running, it will just pop up the shell window.
;;
;; C-c C-u (lml-save-buffer-use-file).
;;     This command will save the current buffer and send a "load file",
;;     where file is the file visited by the current buffer, to the
;;     inferior shell running lml.
;;
;; C-c C-f (lml-run-on-file).
;;     Will send a "load file" to the inferior shell running lml,
;;     prompting you for the file name.
;;    
;; C-c ' (lml-next-error). 
;;     This function parses the buffer of the inferior shell running lml,
;;     and finds the errors one by one. The cursor is positioned at the
;;     line of the file indicated by the error message, while the
;;     inferior shell buffer running lml will be positioned at the error
;;     message in another window. 
;;
;;     IMPORTANT: This function only knows the syntax of error messages
;;     ********** produced by LML of New Jersey. To have this function
;;     working with other implementations of lml you will have to
;;     rewrite it.
;;
;; C-c C-r (lml-send-region). 
;;     Will send region, from point to mark, to the inferior shell
;;     running lml.
;;
;; C-c C-c (lml-send-function). 
;;     Will send function to the inferior shell running lml. (This only
;;     sends the paragraph, so you might prefer the lml-send-region
;;     instead. Paragraphs are separated by blank lines only).
;;
;; C-c C-b (lml-send-buffer). 
;;     Will send whole buffer to inferior shell running lml.
;;
;;
;; 2. INDENTATION
;; ==============
;;
;; The indentation algorithm has been the hardest one to implement.
;;
;;         What is the standard way of indenting lml-code?
;;
;; The algorithm has its own view of the right way to indent code,
;; according to the constants you can set to control the mode.
;;
;; 2.1. CONSTANTS CONTROLLING THE INDENTATION
;; ------------------------------------------
;;
;; lml-indent-level (default 4)
;;     Indentation of blocks in lml.
;;
;; lml-pipe-indent (default -3)
;;     The amount of extra indentation (negative) of lines beginning
;;     with "||". 
;;
;; lml-case-indent (default nil)
;;     How to indent case-of expressions.
;;
;;     If t:   case expr              If nil:   case expr in
;;               in exp1 => ...                     exp1 => ...
;;               || exp2 => ...                  || exp2 => ...
;;
;;     The first seems to be the standard in NJ-LML. The second is the
;;     default.
;;
;; lml-nested-if-indent (default nil)
;;     If set to t, nested if-then-else expressions will have the
;;     indentation:
;;
;;                if exp1 then exp2
;;                else if exp3 then exp4
;;                else if exp5 then exp6
;;                     else exp7
;;
;;     If nil, they will be indented as:
;;
;;                if exp1 then exp2
;;                else if exp3 then exp4
;;                     else if exp5 then exp6
;;                          else exp7
;;
;;     With the "else" at the same column as the matching "if".
;;
;; lml-type-of-indent (default t)
;;     Determines how to indent `let', etc.
;;     
;;     If t:      let foo bar = let
;;                                  val p = 4
;;                              in
;;                                  bar + p
;;
;;     If nil:    let foo bar = let
;;                    val p = 4
;;                in
;;                    bar + p
;;
;;     Will not have any effect if the starting keyword is first on the
;;     line.
;;
;; lml-paren-lookback (default 200)
;;     Determines how far back (in chars) the indentation algorithm
;;     should look for open parenthesis. High value means slow
;;     indentation algorithm. A value of 200 (being the equivalent of
;;     4-6 lines) should suffice most uses. (A value of nil, means do
;;     not look at all).
;;
;; To change these constants, See CUSTOMIZING YOUR LML-MODE below.
;;
;;
;; 3. INFERIOR SHELL.
;; ==================
;;
;; The mode for Standard ML also contains a mode for an inferior shell
;; running lml. The mode is the same as the shell-mode, with just one
;; extra command.
;;
;; 3.1. INFERIOR SHELL COMMANDS
;; ----------------------------
;;
;; C-c C-f (lml-run-on-file).  Send a `load file' to the process running
;; lml.
;;
;; 3.2. CONSTANTS CONTROLLING THE INFERIOR SHELL MODE
;; --------------------------------------------------
;;
;; Because lml is called differently on various machines, and the
;; lml-systems have their own command for reading in a file, a set of
;; constants controls the behavior of the inferior shell running lml (to
;; change these constants: See CUSTOMIZING YOUR LML-MODE below).
;;
;; lml-prog-name (default "lmli").
;;     This constant is a string, containing the command to invoke
;;     Standard ML on your system. 
;;
;; lml-use-right-delim (default "\"")
;; lml-use-left-delim  (default "\"")
;;     The left and right delimiter used by your version of lml, for
;;     `use file-name'.
;;
;; lml-process-name (default "LML"). 
;;     The name of the process running lml. (This will be the name
;;     appearing on the mode line of the buffer)
;;
;; NOTE: The lml-mode functions: lml-send-buffer, lml-send-function and
;; lml-send-region, creates temporary files (I could not figure out how
;; to send large amounts of data to a process). These files will be
;; removed when you leave emacs.
;;
;;
;; 4. CUSTOMIZING YOUR LML-MODE
;; ============================
;;
;; If you have to change some of the constants, you will have to add a
;; `hook' to the lml-mode. Insert this in your "~/.emacs" file.
;;
;;    (setq lml-mode-hook 'my-lml-constants)
;;
;; Your function "my-lml-constants" will then be executed every time
;; "lml-mode" is invoked.  Now you only have to write the emacs-lisp
;; function "my-lml-constants", and put it in your "~/.emacs" file.
;;
;; Say you are running a version of lml that uses the syntax `load
;; ["file"]', is invoked by the command "OurLML" and you don't want the
;; indentation algorithm to indent according to open parenthesis, your
;; function should look like this:
;;
;;    (defun my-lml-constants ()
;;       (setq lml-prog-name "OurLML")
;;       (setq lml-use-left-delim "[\"")
;;       (setq lml-use-right-delim "\"]")
;;       (setq lml-paren-lookback nil))
;;
;; Other things could be added to this function. As an example:
;;
;;       (setq lml-paren-lookback 1000) ; Look back 1000 chars for open
;;                                      ;  parenthesis.
;;       (setq lml-case-indent t)       ; Select other type of case
;;                                      ;  expressions 
;;       (turn-on-auto-fill)            ; Turn on auto-fill
;;       (setq lml-indent-level 8)      ; change the indentation to 8
;;
;;       (define-key lml-mode-map "|"   ; Let "|" be electric,
;;           'lml-electric-pipe)        ;  like M-|
;;
;; The lml-shell also runs a `hook' (lml-shell-hook) when it is invoked.
;;
;;
;;
;;
;; 6. THINGS TO DO
;; ===============
;;
;; LOTS!
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; ORIGINAL AUTHOR
;;         Lars Bo Nielsen
;;         Aalborg University
;;         Computer Science Dept.
;;         9000 Aalborg
;;         Denmark
;;
;;         lbn@iesd.dk
;;         or: ...!mcvax!diku!iesd!lbn
;;         or: mcvax!diku!iesd!lbn@uunet.uu.net
;;
;; MODIFIED FOR LML BY
;;	   Lennart AUgustsson
;;
;;
;; Please let me know if you come up with any ideas, bugs, or fixes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst lml-mode-version-string
  "LML-MODE, Version 0.1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS CONTROLLING THE MODE.
;;;
;;; These are the constants you might want to change
;;; 

;; The amount of indentation of blocks
(defconst lml-indent-level 4 "*Indentation of blocks in lml.")

;; The amount of negative indentation of lines beginning with "|"
(defconst lml-pipe-indent -4
  "*Extra (negative) indentation for lines beginning with |.") ;

;; How do we indent case-of expressions.
(defconst lml-case-indent nil
  "*How to indent case-of expressions.
  If t:   case expr              If nil:   case expr in
            in exp1 => ...                     exp1 => ...
            || exp2 => ...                  || exp2 => ...
\nThe first seems to be the standard in NJ-LML. The second is the default.")

(defconst lml-nested-if-indent t
  "*If set to t, nested if-then-else expression will have the same
indentation as:
                 if exp1 then exp2
                 else if exp3 then exp4
                 else if exp5 then exp6
                      else exp7")

(defconst lml-type-of-indent t
  "*How to indent `let' etc.
If t:
          let foo bar = let
                           val p = 4
                        in
                           bar + p
If nil:
          let foo bar = let
              val p = 4
          in
              bar + p
\nWill not have any effect if the starting keyword is first on the line.")

;; How far should the indentation algorithm look to find open parenthesis 
(defconst lml-paren-lookback 200
  "*Determines how far back (in chars) the indentation algorithm
should look for open parenthesis. High value means slow indentation
algorithm. A value of 200 (being the equivalent of 4-6 lines) should
suffice most uses. (A value of nil, means do not look at all)")

;; The command used to start up the lml-program.
(defconst lml-prog-name "lmli" "*Name of program to run as lml.")

;; The left delimmitter for `load file'
(defconst lml-use-left-delim "\""
  "*The left delimiter for the filename when using \"load\".")

;; The right delimmitter for `load file'
(defconst lml-use-right-delim "\""
  "*The right delimiter for the filename when using \"load\".")

;; A regular expression matching the prompt pattern in the inferior
;; shell
(defconst lml-shell-prompt-pattern "^> *"
  "*The prompt pattern for the inferion shell running lml.")

;; The template used for temporary files, created when a region is
;; send to the inferior process running lml.
(defconst lml-tmp-template "/tmp/lml.tmp."
  "*Template for the temporary file, created by lml-simulate-send-region.")

;; The name of the process running lml (This will also be the name of
;; the buffer).
(defconst lml-process-name "LML" "*The name of the LML-process")

;;;
;;; END OF CONSTANTS CONTROLLING THE MODE.
;;;
;;; If you change anything below, you are on your own.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar lml-mode-syntax-table nil "The syntax table used in lml-mode.")

(defvar lml-mode-map nil "The mode map used in lml-mode.")

(defun lml-mode ()
  "Major mode for editing LML code.
Tab indents for LML code.
Comments are delimited with (* ... *).
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Key bindings:
=============

\\[lml-indent-line]\t  Indent current line.
\\[reindent-then-newline-and-indent]\t  Reindent line, newline and indent.
\\[lml-indent-region]\t  Indent region.
\\[lml-electric-pipe]\t  Insert a \"||\". Insert function name, \":\" etc.
\\[lml-region]\t  Insert a common used structure.
\\[lml-pop-to-shell]\t  Pop to the lml window.
\\[lml-next-error]\t  Find the next error.
\\[lml-save-buffer-use-file]\t  Save the buffer, and send a \"load file\".
\\[lml-send-region]\t  Send region (point and mark) to lml.
\\[lml-run-on-file]\t  Send a \"load file\" to lml.
\\[lml-send-function]\t  Send function to lml.
\\[lml-send-buffer]\t  Send whole buffer to lml.
\\[lml-mode-version]\t  Get the version of lml-mode


Variables controlling the indentation
=====================================

lml-indent-level (default 4)
    The indentation of a block of code.

lml-pipe-indent (default -4)
    Extra indentation of a line starting with \"||\".

lml-case-indent (default nil)
    Determine the way to indent case-of expression.
       If t:   case expr              If nil:   case expr in
                 in exp1 : ...                     exp1 : ...
                 || exp2 : ...                  || exp2 : ...
               end                              end


lml-nested-if-indent (default t)
    If set to t, nested if-then-else expression will have the same
    indentation as:
                     if exp1 then exp2
                     else if exp3 then exp4
                     else if exp5 then exp6
                          else exp7

lml-type-of-indent (default t)
    How to indent `let' etc.
    If t:
              let foo bar = let
                               val p = 4
                            in
                               bar + p
    If nil:
              let foo bar = let
                  val p = 4
              in
                  bar + p
    Will not have any effect if the starting keyword is first on the line.


Mode map
========
\\{lml-mode-map}
Runs lml-mode-hook if non nil."
  (interactive)
  (kill-all-local-variables)
  (if lml-mode-map
      ()
    (setq lml-mode-map (make-sparse-keymap))
    (define-key lml-mode-map "\C-c'" 'lml-next-error)
    (define-key lml-mode-map "\C-c\C-v" 'lml-mode-version)
    (define-key lml-mode-map "\C-c\C-u" 'lml-save-buffer-use-file)
    (define-key lml-mode-map "\C-c\C-s" 'lml-pop-to-shell)
    (define-key lml-mode-map "\C-c\C-r" 'lml-send-region)
    (define-key lml-mode-map "\C-c\C-m" 'lml-region)
    (define-key lml-mode-map "\C-c\C-f" 'lml-run-on-file)
    (define-key lml-mode-map "\C-c\C-c" 'lml-send-function)
    (define-key lml-mode-map "\C-c\C-b" 'lml-send-buffer)
    (define-key lml-mode-map "\e|" 'lml-electric-pipe)
    (define-key lml-mode-map "\C-j" 'reindent-then-newline-and-indent)
    (define-key lml-mode-map "\177" 'backward-delete-char-untabify)
    (define-key lml-mode-map "\C-c\t" 'lml-indent-region)
    (define-key lml-mode-map "\t" 'lml-indent-line))
  (use-local-map lml-mode-map)
  (setq major-mode 'lml-mode)
  (setq mode-name "LML")
  (define-abbrev-table 'lml-mode-abbrev-table ())
  (setq local-abbrev-table lml-mode-abbrev-table)
  (if lml-mode-syntax-table
      ()
    (setq lml-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?\( "()" lml-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" lml-mode-syntax-table)
;    (modify-syntax-entry ?\\ "\\" lml-mode-syntax-table)
    (modify-syntax-entry ?/ ". 14" lml-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" lml-mode-syntax-table)
    ;; Special characters in lml-mode to be treated as normal
    ;; characters:
    (modify-syntax-entry ?_ "w" lml-mode-syntax-table)
    (modify-syntax-entry ?\' "w" lml-mode-syntax-table)
    )
  (set-syntax-table lml-mode-syntax-table)
  ;; A paragraph is seperated by blank lines (or ^L) only.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[\t ]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lml-indent-line)
  (make-local-variable 'require-final-newline) ; Always put a new-line
  (setq require-final-newline t)	; in the end of file
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 39)		; Start of comment in this column
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]?") ; This matches a start of comment
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'lml-comment-indent)
  ;;
  ;; Adding these will fool the matching of parens. I really don't
  ;; know why. It would be nice to have comments treated as
  ;; white-space
  ;; 
  ;; (make-local-variable 'parse-sexp-ignore-comments)
  ;; (setq parse-sexp-ignore-comments t)
  ;; 
  (run-hooks 'lml-mode-hook))		; Run the hook

(defconst lml-pipe-matchers-reg
  "\\bcase\\b\\|\\blet\\b\\|\\band\\b\\|\\brec\\b\\|\\bwhere\\b"
  "The keywords a `|' can follow.")

(defun lml-electric-pipe ()
  "Insert a \"||\". Depending on the context insert the name of
function, a \":\" etc."
  (interactive)
  (let ((here (point))
	(match (save-excursion
		 (lml-find-matching-starter lml-pipe-matchers-reg)
		 (point)))
	(tmp "  : ")
	(case-or-handle-exp t))
    (if (/= (save-excursion (beginning-of-line) (point))
	    (save-excursion (skip-chars-backward "\t ") (point)))
	(insert "\n"))
    (insert "||")
    (save-excursion
      (goto-char match)
      (cond
       ;; It was a function, insert the function name
       ((looking-at "let\\b\\|and\\b\\|rec\\b\\|where\\b")
	(setq tmp (concat "  " (buffer-substring
			       (progn (forward-char 3)
				      (skip-chars-forward "\t\n ") (point))
			       (progn 
				 (forward-word 1)
				 (point)))
			  " "))
	(setq case-or-handle-exp nil))))
    (insert tmp)
    (lml-indent-line)
    (end-of-line)
    (if case-or-handle-exp
	(forward-char -3))))

(defun lml-mode-version ()
  (interactive)
  (message lml-mode-version-string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SHORT CUTS (lml-region)
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst lml-region-alist
  '(("let") ("local") ("type") ("case"))
  "The list of regions to auto-insert.")

(defun lml-region ()
  "Interactive short-cut. Insert a common used structure in lml."
  (interactive)
  (let ((newline nil)			; Did we insert a newline
	(name (completing-read "Region to insert: (default let) "
			       lml-region-alist nil t nil)))
    ;; default is "let"
    (if (string= name "") (setq name "let"))
    ;; Insert a newline if point is not at empty line
    (if (save-excursion (beginning-of-line) (skip-chars-forward "\t ") (eolp))
	()
      (setq newline t)
      (insert "\n"))
    (condition-case ()
	(cond
	 ((string= name "let") (lml-let))
	 ((string= name "local") (lml-local))
	 ((string= name "case") (lml-case))
	 ((string= name "type") (lml-datatype)))
      (quit (if newline 
		(progn
		  (delete-char -1)
		  (beep)))))))

(defun lml-let () 
  "Insert a `let in'."
  (interactive)
  (let (indent)
    (insert "let")
    (lml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ lml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "in\n") (indent-to (+ lml-indent-level indent))
    (previous-line 2) (end-of-line)))
    

(defun lml-local ()
  "Insert a `local in end'."
  (interactive)
  (let (indent)
    (insert "local")
    (lml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ lml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "in\n") (indent-to (+ lml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "end") (previous-line 3) (end-of-line)))
    

(defun lml-case ()
  "Insert a case, prompting for case-expresion."
  (interactive)
  (let (indent (expr (read-string "Case expr: ")))
    (insert (concat "case " expr))
    (lml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (if lml-case-indent
	(progn
	  (insert "\n")
	  (indent-to (+ 2 indent))
	  (insert "in "))
      (insert " in\n")
      (indent-to (+ indent lml-indent-level)))
    (save-excursion (insert " : "))))

(defun lml-datatype ()
  "Insert a `type ??? =', prompting for name."
  (let (indent 
	(type (read-string (concat "Type of datatype (default none): ")))
	(name (read-string (concat "Name of datatype: "))))
    (insert (concat "type "
		    (if (string= type "") "" (concat type " "))
		    name " ="))
    (lml-indent-line)
    (setq indent (current-indentation))
    (end-of-line) (insert "\n") (indent-to (+ lml-indent-level indent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSING ERROR MESSAGES 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lml-last-error 1 "Last position of error. Initially 1.")

(defun lml-next-error ()
  "Find the next error by passing the *LML* buffer.\n
NOTE: This function only knows about the syntax of errors generated by
LML, and will only work with this."
  (interactive)
  (let ((found t) (tmp-file nil) found-line found-file error-line tmp)
    (save-excursion
      (condition-case ()
	  (progn
	    (set-buffer (concat "*" lml-process-name "*" ))
	    (goto-char lml-last-error)
	    (re-search-forward "^.+\\(Error:\\|Warning:\\)")
	    (save-excursion
	      (beginning-of-line)
	      (if (looking-at lml-tmp-template)
		  (setq tmp-file t)))
	    (setq lml-last-error (point))
	    (beginning-of-line)
	    (setq error-line (point))
	    (search-forward ",")
	    (setq found-file (buffer-substring error-line (1- (point))))
	    (search-forward "line ")
	    (setq tmp (point))
	    (skip-chars-forward "[0-9]")
	    (setq found-line (string-to-int (buffer-substring tmp (point)))))
	(error (setq found nil))))
    (if found
	(progn
	  (set-window-start
	   (display-buffer (concat "*" lml-process-name "*")) error-line)
	  (if tmp-file
	      (let ((loop t) (n 0) (tmp-list lml-tmp-files-list))
		(while loop
		  (setq tmp (car tmp-list))
		  (if (string= (car tmp) found-file)
		      (setq loop nil)
		    (setq tmp-list (cdr tmp-list)))
		  (if (null tmp-list) (setq loop nil)))
		(if (null tmp)
		    (error "Temporary file not associated with buffer.")
		  (condition-case ()
		      (progn
			(switch-to-buffer (nth 1 tmp))
			(message
			 (concat "Error found in temporary file "
				 "(line number may not match)."))
			(goto-line (1- (+ found-line (nth 2 tmp)))))
		    (error (error "Sorry, buffer doesn't exist any more.")))))
	    (if (file-exists-p found-file)
		(progn
		  (condition-case ()
		      (progn
			(find-file found-file)
			(goto-line found-line))
		    (error ())))
	      (error (concat "File not found: " found-file)))))
      (if (= lml-last-error 1)
	  (message "No errors.")
	(if (y-or-n-p "No more errors. Reset error count: ")
	    (progn
	      (message (concat
			"You will have to type: "
			(substitute-command-keys "\\[lml-next-error]")
			" again, to get first error/warning."))
	      (setq lml-last-error 1))
	  (message ""))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INDENTATION
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lml-indent-region (begin end)
  "Indent region of lml code."
  (interactive "r")
  (message "Indenting region...")
  (save-excursion
    (goto-char end) (setq end (point-marker)) (goto-char begin)
    (while (< (point) end)
      (skip-chars-forward "\t\n ")
      (lml-indent-line)
      (end-of-line))
    (move-marker end nil))
  (message "Indenting region...done"))

(defun lml-indent-line ()
  "Indent current line of lml code."
  (interactive)
  (let ((indent (lml-calculate-indentation)))
    (if (/= (current-indentation) indent)
	(let ((beg (progn (beginning-of-line) (point))))
	  (skip-chars-forward "\t ")
	  (delete-region beg (point))
	  (indent-to indent))
      ;; If point is before indentation, move point to indentation
      (if (< (current-column) (current-indentation))
	  (skip-chars-forward "\t ")))))

(defconst lml-indent-starters-reg
  "and\\b\\|case\\b\\|else\\b\\|if\\b\
\\|in\\b\\|let\\b\\|local\\b\\|where\\b\\|in\\b\
\\|then\\b\\|\\btype\\b"
  "The indentation starters. The next line, after one starting with
one of these, will be indented.")

(defconst lml-starters-reg
  "\\blet\\b\\|\\blocal\\b\\|\\bwhere\\b\\|\\btype\\b"
  "The starters of new expressions.")

(defconst lml-end-starters-reg
  "\\bcase\\b\\|\\blocal\\b"
  "Matching reg-expression for the \"end\" keyword")

(defconst lml-starters-indent-after
  "let\\b\\|local\\b\\|in\\b\\|where\\b")

(defun lml-calculate-indentation ()
  (save-excursion
    (beginning-of-line)			; Go to first non whitespace
    (skip-chars-forward "\t ")		; on the line.
    (cond
     ;; Indentation for comments alone on a line, matches the
     ;; proper indentation of the next line. Search only for the
     ;; next "*)", not for the matching.
     ((looking-at "/\\*")
      (if (not (search-forward "*/" nil t))
	  (error "Comment not ended."))
      (skip-chars-forward "\n\t ")
      ;; If we are at eob, just indent 0
      (if (eobp) 0 (lml-calculate-indentation)))
     ;; Are we looking at a case expression ?
     ((looking-at "||.*\\(\\|\n.*\\):")
      (lml-skip-block)
      (lml-re-search-backward ":")
      (beginning-of-line)
      (skip-chars-forward "\t ")
      (cond
       ((looking-at "||") (current-indentation))
       ((and lml-case-indent (looking-at "in\\b"))
	(1+ (current-indentation)))
       ((looking-at "let\\b") (1+ (current-indentation)))
       (t (+ (current-indentation) lml-pipe-indent))))
     ((looking-at "and\\b")
      (if (lml-find-matching-starter lml-starters-reg)
	  (current-column)
	0))
     ((looking-at "in\\b")		; Match the beginning let/local
      (lml-find-match-indent "in" "\\bin\\b" "\\blocal\\b\\|\\blet\\b"))
     ((looking-at "end\\b")		; Match the beginning
      (lml-find-match-indent "end" "\\bend\\b" lml-end-starters-reg))
     ((and lml-nested-if-indent (looking-at "else[\t ]*if\\b"))
      (lml-re-search-backward "\\bif\\b\\|\\belse\\b")
      (current-indentation))
     ((looking-at "else\\b")		; Match the if
      (lml-find-match-indent "else" "\\belse\\b" "\\bif\\b" t))
     ((looking-at "then\\b")		; Match the if + extra indentation
      (+ (lml-find-match-indent "then" "\\bthen\\b" "\\bif\\b" t)
	 lml-indent-level))
     ((and lml-case-indent (looking-at "in\\b"))
      (lml-re-search-backward "\\bcase\\b")
      (+ (current-column) 2))
     ((looking-at lml-starters-reg)
      (let ((start (point)))
	(lml-backward-sexp)
	(if (and (looking-at lml-starters-indent-after)
		 (/= start (point)))
	    (+ (if lml-type-of-indent
		   (current-column)
		 (if (progn (beginning-of-line)
			    (skip-chars-forward "\t ")
			    (looking-at "||"))
		     (- (current-indentation) lml-pipe-indent)
		   (current-indentation)))
	       lml-indent-level)
	  (beginning-of-line)
	  (skip-chars-forward "\t ")
	  (if (and (looking-at lml-starters-indent-after)
		   (/= start (point)))
	      (+ (if lml-type-of-indent
		     (current-column)
		   (current-indentation))
		 lml-indent-level)
	    (goto-char start)
	    (if (lml-find-matching-starter lml-starters-reg)
		(current-column)
	      0)))))
     (t
      (let ((indent (lml-get-indent)))
	(cond
	 ((looking-at "||")
	  ;; Lets see if it is the follower of a function definition
	  (if (lml-find-matching-starter
	       "\\blet\\b\\|\\brec\\b\\|\\band\\b\\|\\bwhere\\b")
	      (+ (current-column) lml-indent-level lml-pipe-indent)
	    (+ indent lml-pipe-indent)))
	 (t
	  (if lml-paren-lookback	; Look for open parenthesis ?
	      (max indent (lml-get-paren-indent))
	    indent))))))))

(defun lml-get-indent ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward "\t\n; ")  ;; ???
    (if (looking-at ";") (lml-backward-sexp))
    (cond
     ((save-excursion (lml-backward-sexp) (looking-at "end\\b"))
      (- (current-indentation) lml-indent-level))
     (t
      ;; Go to the beginning of the line and place by first
      ;; non-whitespace, but pass over starting parenthesis
      (beginning-of-line)		
      (skip-chars-forward "\t (|")
      (let ((indent (current-column)))
	(cond
	 ;; Started val/fun/structure...
	 ((looking-at lml-indent-starters-reg) (+ indent lml-indent-level))
	 ;; Indent after ":" pattern
	 ((looking-at ".*:") (+ indent lml-indent-level))
	 ;; else keep the same indentation as previous line
	 (t indent)))))))

(defun lml-get-paren-indent ()
  (save-excursion
    (let ((levelpar 0)			; Level of "()"
          (levelcurl 0)                 ; Level of "{}"
          (levelsqr 0)                  ; Level of "[]"
          (backpoint (max (- (point) lml-paren-lookback) (point-min)))
          (loop t) (here (point)))
      (while (and (/= levelpar 1) (/= levelsqr 1) (/= levelcurl 1) loop)
	(if (re-search-backward "[][{}()]" backpoint t)
	    (if (not (lml-inside-comment-or-string-p))
		(cond
		 ((looking-at "(") (setq levelpar (1+ levelpar)))
		 ((looking-at ")") (setq levelpar (1- levelpar)))
		 ((looking-at "\\[") (setq levelsqr (1+ levelsqr)))
		 ((looking-at "\\]") (setq levelsqr (1- levelsqr)))
		 ((looking-at "{") (setq levelcurl (1+ levelcurl)))
		 ((looking-at "}") (setq levelcurl (1- levelcurl)))))
	  (setq loop nil)))
      (if loop
	  (1+ (current-column))
	0))))

(defun lml-inside-comment-or-string-p ()
  (let ((start (point)))
    (if (save-excursion
	  (condition-case ()
	      (or
	       (progn
		 (search-backward "/*")
		 (search-forward "*/")
		 (forward-char -1)	; A "*)" is not inside the comment
		 (> (point) start))
	       (progn
		 (search-backward "--")
		 (search-forward "\n")
		 (forward-char -1)	; A "*)" is not inside the comment
		 (> (point) start)))
	    (error nil)))
	t
      (let ((numb 0))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (progn (beginning-of-line) (point)) start)
	    (condition-case ()
		(while t
		  (search-forward "\"")
		  (setq numb (1+ numb)))
	      (error (if (and (not (zerop numb))
			      (not (zerop (% numb 2))))
			 t nil)))))))))
		
(defun lml-skip-block ()
  (lml-backward-sexp)
  (cond 
   ;; If what we just passed was a comment, then go backward to
   ;; some code, as code is indented according to other code and
   ;; not according to comments.
   ((looking-at "/\\*")
    (skip-chars-backward "\t\n "))
   ((looking-at "--")
    (skip-chars-backward "\t\n "))
   ;; Skip over let-in-end/local-in-end etc...
   ((looking-at "end\\b")
    (goto-char (lml-find-match-backward "end" "\\bend\\b"
					lml-end-starters-reg))
    (skip-chars-backward "\n\t "))
   ;; Here we will need to skip backwardd past if-then-else
   ;; and case-of expression. Please - tell me how !!
   ))

(defun lml-find-match-backward (unquoted-this this match &optional start)
  (save-excursion
    (let ((level 1) (here (point))
	  (pattern (concat this "\\|" match)))
      (if start (goto-char start))
      (while (not (zerop level))
	(if (lml-re-search-backward pattern)
	    (setq level (cond
			 ((looking-at this) (1+ level))
			 ((looking-at match) (1- level))))
	  ;; The right match couldn't be found
	  (error (concat "Unbalanced: " unquoted-this))))
      (point))))

(defun lml-find-match-indent (unquoted-this this match &optional indented)
  (save-excursion
    (goto-char (lml-find-match-backward unquoted-this this match))
    (if (or lml-type-of-indent indented)
	(current-column)
      (if (progn
	    (beginning-of-line)
	    (skip-chars-forward "\t ")
	    (looking-at "||"))
	  (- (current-indentation) lml-pipe-indent)
	(current-indentation)))))

(defun lml-find-matching-starter (regexp)
  (let ((start-let-point (lml-point-inside-let-etc))
	(start-up-list (lml-up-list))
	(found t))
    (if (lml-re-search-backward regexp)
	(progn
	  (condition-case ()
	      (while (or (/= start-up-list (lml-up-list))
			 (/= start-let-point (lml-point-inside-let-etc)))
		(re-search-backward regexp))
	    (error (setq found nil)))
	  found)
      nil)))

(defun lml-point-inside-let-etc ()
  (let ((last nil) (loop t) (found t) (start (point)))
    (save-excursion
      (while loop
	(condition-case ()
	    (progn
	      (re-search-forward "\\bend\\b")
	      (while (lml-inside-comment-or-string-p)
		(re-search-forward "\\bend\\b"))
	      (forward-char -3)
	      (setq last (lml-find-match-backward "end" "\\bend\\b"
						  lml-end-starters-reg last))
	      (if (< last start)
		  (setq loop nil)
		(forward-char 3)))
	  (error (progn (setq found nil) (setq loop nil)))))
      (if found
	  last
	0))))
		     
(defun lml-re-search-backward (regexpr)
  (let ((found t))
    (if (re-search-backward regexpr nil t)
	(progn
	  (condition-case ()
	      (while (lml-inside-comment-or-string-p)
		(re-search-backward regexpr))
	    (error (setq found nil)))
	  found)
      nil)))

(defun lml-up-list ()
  (save-excursion
    (condition-case ()
	(progn
	  (up-list 1)
	  (point))
      (error 0))))

(defun lml-backward-sexp ()
  (condition-case ()
      (progn
	(backward-sexp 1)
	(if (looking-at "/\\*")
	    (backward-sexp 1)))
    (error nil)))

(defun lml-comment-indent ()
  (if (looking-at "^/\\*")		; Existing comment at beginning
      0					; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (1+ (max (current-column)		; Else indent at comment column
	       comment-column)))))	; except leave at least one space.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INFERIOR SHELL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lml-shell-map nil "The mode map for lml-shell.")

(defun lml-shell ()
  "Inferior shell invoking LML.
It is not possible to have more than one shell running LML.
Like the shell mode with the additional command:

\\[lml-run-on-file]\t Runs lml on the file.
\\{lml-shell-map}
Variables controlling the mode:

lml-prog-name (default \"lmli\")
    The string used to invoke the lml program.

lml-use-right-delim (default \"\\\"\")
lml-use-left-delim  (default \"\\\"\")
    The left and right delimiter used by your version of lml, for
    \"load file-name\".

lml-process-name (default \"LML\")
    The name of the process running lml.

lml-shell-prompt-pattern (default \"^> *\")
    The prompt pattern.

Runs lml-shell-hook if not nil."
  (interactive)
  (if (not (process-status lml-process-name))
      (save-excursion			; Process is not running
	(message "Starting LML...")	; start up a new process
	(require 'shell)
	(set-buffer (make-shell lml-process-name lml-prog-name))
	(erase-buffer)			; Erase the buffer if a previous
	(if lml-shell-map		; process died in there
	    ()
	  (setq lml-shell-map (copy-sequence shell-mode-map))
	  (define-key lml-shell-map "\C-c\C-f" 'lml-run-on-file))
	(use-local-map lml-shell-map)
	(make-local-variable 'shell-prompt-pattern)
	(setq shell-prompt-pattern lml-shell-prompt-pattern)
	(setq major-mode 'lml-shell)
	(setq mode-name "LML Shell")
	(setq mode-line-format 
	      "-----Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
	(set-process-filter (get-process lml-process-name) 'lml-process-filter)
	(message "Starting LML...done.")
	(run-hooks 'lml-shell-hook))))

(defun lml-process-filter (proc str)
  (let ((cur (current-buffer))
	(pop-up-windows t))
    (pop-to-buffer (concat "*" lml-process-name "*"))
    (goto-char (point-max))
    (if (string= str "\b\b\b  \b\b\b")
	(backward-delete-char 4)
      (insert str))
    (set-marker (process-mark proc) (point-max))
    (pop-to-buffer cur)))

(defun lml-pop-to-shell ()
  (interactive)
  (lml-shell)
  (pop-to-buffer (concat "*" lml-process-name "*")))

(defun lml-run-on-file (fil)
  (interactive "FRun LML on : ")
  (lml-shell)
  (save-some-buffers)
  (send-string lml-process-name
	       (concat "load " lml-use-left-delim (expand-file-name fil)
		       lml-use-right-delim ";\n")))

(defun lml-save-buffer-use-file ()
  "Save the buffer, and send a `use file' to the inferior shell
running LML."
  (interactive)
  (let (file)
    (if (setq file (buffer-file-name))	; Is the buffer associated
	(progn				; with file ?
	  (save-buffer)
	  (lml-shell)
	  (send-string lml-process-name
		       (concat "load " lml-use-left-delim
			       (expand-file-name file)
			       lml-use-right-delim ";\n")))
      (error "Buffer not associated with file."))))

(defvar lml-tmp-files-list nil
  "List of all temporary files created by lml-simulate-send-region.
Each element in the list is a list with the format:

      (\"tmp-filename\"  buffer  start-line)")

(defvar lml-simulate-send-region-called-p nil
  "Has lml-simulate-send-region been called previously.")

(defun lml-make-temp-name (pre)
  (concat (make-temp-name pre) ".m"))

(defun lml-simulate-send-region (point1 point2)
  "Simulate send region. As send-region only can handle what ever the
system sets as the default, we have to make a temporary file.
Updates the list of temporary files (lml-tmp-files-list)."
  (let ((file (expand-file-name (lml-make-temp-name lml-tmp-template))))
    ;; Remove temporary files when we leave emacs
    (if (not lml-simulate-send-region-called-p)
	(progn
	  (setq lml-old-kill-emacs-hook kill-emacs-hook)
	  (setq kill-emacs-hook 'lml-remove-tmp-files)
	  (setq lml-simulate-send-region-called-p t)))
    (save-excursion
      (goto-char point1)
      (setq lml-tmp-files-list
	    (cons (list file
			(current-buffer)
			(save-excursion	; Calculate line no.
			  (beginning-of-line)
			  (1+ (count-lines 1 (point)))))
		  lml-tmp-files-list)))
    (write-region point1 point2 file nil 'dummy)
    (lml-shell)
    (message "Using temporary file: %s" file)
    (send-string
     lml-process-name
     ;; string to send: load file;
     (concat "load " lml-use-left-delim file lml-use-right-delim ";\n"))))

(defvar lml-old-kill-emacs-hook nil
  "Old value of kill-emacs-hook")

(defun lml-remove-tmp-files ()
  "Remove the temporary files, created by lml-simulate-send-region, if
they still exist. Only files recorded in lml-tmp-files-list are removed."
  (message "Removing temporary files created by lml-mode...")
  (while lml-tmp-files-list
    (condition-case ()
 	(delete-file (car (car lml-tmp-files-list)))
      (error ()))
    (setq lml-tmp-files-list (cdr lml-tmp-files-list)))
  (message "Removing temporary files created by lml-mode...done.")
  (run-hooks 'lml-old-kill-emacs-hook))

(defun lml-send-region ()
  "Send region."
  (interactive)
  (let (start end)
    (save-excursion
      (setq end (point))
      (exchange-point-and-mark)
      (setq start (point)))
    (lml-simulate-send-region start end)))

(defun lml-send-function ()
  "Does NOT send the function, but the paragraph."
  (interactive)
  (let (start end)
    (save-excursion
      (condition-case ()
	  (progn
	    (backward-paragraph)
	    (setq start (point)))
	(error (setq start (point-min))))
      (condition-case ()
	  (progn
	    (forward-paragraph)
	    (setq end (point)))
	(error (setq end (point-max)))))
    (lml-simulate-send-region start end)))

(defun lml-send-buffer ()
  "Send the buffer."
  (interactive)
  (lml-simulate-send-region (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; END OF LML-MODE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
