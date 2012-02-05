;; A simple mode for editing the TPL language.
;;
;; Author: Tikhon Jelvis
;;
;; For now, I will just use generic mode. I may improve it in the
;; future. (I hope :p)
;;
;; This mode was based on gosu-mode at http://gosu-lang.org/downloads/gosu-mode.el

(require 'generic-x)

(defvar tpl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"'" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\" "\"'" table)
    table))

(defvar tpl-mode-map (make-keymap))
(define-key tpl-mode-map (kbd "RET") 'tpl-condition-indent)
(define-key tpl-mode-map (kbd "}") 'tpl-electric-brace)

(defun init ()
  (set-syntax-table tpl-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'tpl-indent-line)
  (use-local-map cs164-mode-map))

(defgroup tpl nil
  "Customization variables for the glorious tpl language mode."
  :version "23.3.1")

(defcustom tpl-basic-offset 4
  "This is the default size of one indentation. Code in
blocks (like after an if statement) will be indented by this many
spaces when you press <tab>. This probably won't work if it's
negative."
  :group 'tpl
  :type 'integer)
(defcustom tpl-indent-automatically t
  "If this is t, indents whenever you press RET or type a closing
brace (}). Otherwise behaves as normal."
  :group 'tpl
  :type 'boolean)

(defun line-matchesp (regexp offset)
  "Return t if line matches regular expression REGEXP.  The 
selected line is chosen by applying OFFSET as a numeric 
increment or decrement away from the current line number.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (save-excursion
    (forward-line offset)
    (beginning-of-line)
    (looking-at regexp)))

(defun previous-line-matchesp (regexp)
  "Return t if previous line matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (line-matchesp regexp -1))

(defun current-line-matchesp (regexp)
  "Return t if current line matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (line-matchesp regexp 0))

(defun tpl-indent-line ()
  (interactive)
  "Establish a set of conditional cases for the types of lines that
point currently is on, and the associated indentation rules."
  (indent-line-to
   (cond
    ((and
      (previous-line-matchesp "^[ \t]*\\*")
      (current-line-matchesp "^[ \t]*\\*"))
     (save-excursion
       (forward-line -1)
       (current-indentation)))
    ((and
      (previous-line-matchesp "^[ \t]*/\\*")
      (current-line-matchesp "^[ \t]*\\*"))
     (save-excursion
       (forward-line -1)
       (+ (current-indentation) 1)))
    ((and
      (previous-line-matchesp "^[ \t]*\\.")
      (current-line-matchesp "^[ \t]**\\."))
     (save-excursion
       (forward-line -1)
       (current-indentation)))
    ((and
      (not (previous-line-matchesp "^[ \t]*\\."))
      (current-line-matchesp "^[ \t]*\\."))
     (save-excursion
       (forward-line -1)
       (+ (current-indentation) cs164-basic-offset)))
    ((current-line-matchesp "^[ \t]*}")
     (save-excursion
       (beginning-of-line)
       (backward-up-list)
       (current-indentation)))
    (t
     (save-excursion
       (condition-case nil
           (progn
             (beginning-of-line)
             (backward-up-list)
             (+ (current-indentation) cs164-basic-offset))
         (error 0)))))))

(defun tpl-condition-indent ()
  "Indents if automatic indentation is on."
  (interactive)
  (newline)
  (if tpl-indent-automatically
      (tpl-indent-line)))

(defun tpl-electric-brace ()
  "Inserts a } and indents if automatic indentation is on."
  (interactive)
  (insert "}")
  (if tpl-indent-automatically
      (progn (if (current-line-matchesp "[ \t]*}[ \t]*")
                 (tpl-indent-line))
             (if (equal (char-after) ?})
                 (forward-char)))))

(define-generic-mode 'tpl-mode
  ;; comment-list
  nil
  ;; keyword-list
  '("if"
    "else"
    "for"
    "in"
    "while"
    "do"
    "with"
    "let"
    "define"
    "set"
    "get"
    "precedence"
    "precedenceOf"
    "load"
    "require")
  ;; font-lock-list
  '(("\\b\\([0-9]+\\|null\\|true\\|false\\)\\b" . font-lock-constant-face)
    ("^[ \t]*\\([_a-zA-Z0-9]+\\)[ \t]*:=" 1 'font-lock-function-name-face)
    ("^[ \t]*\\([_a-zA-Z0-9]+\\)\\([ \t]+\\([_a-zA-Z0-9]+\\|\\[.*\\]\\)\\)+[ \t]*:=" 1 'font-lock-function-name-face)
    ("[+-=*&^%#@!?/.|~<>:]+" . 'font-lock-type-face)
    ("\\(->\\)" 
     (1 (prog1 ()
          (compose-region (match-beginning 1) (match-end 1) ?→)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'font-lock-builtin-face))))
    ("\\(<-\\)" 
     (1 (prog1 ()
          (compose-region (match-beginning 1) (match-end 1) ?←)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'font-lock-builtin-face))))
    ("\\(\\\\\\)" 
     (1 (prog1 ()
          (compose-region (match-beginning 1) (match-end 1) ?λ)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'font-lock-builtin-face))))
    ("\\(/=\\)" (1 (prog1 ()
          (compose-region (match-beginning 1) (match-end 1) ?≠)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'font-lock-builtin-face)))))
  ;; auto-mode-alist
  '(".tpl\\'")
  ;; function-list
  '(init))

(provide 'tpl-mode)