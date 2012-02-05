;; A simple mode for editing the TPL language.
;;
;; For now, I will just use generic mode. I may improve it in the
;; future. (I hope :p)

(require 'generic-x)

(defvar tpl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"'" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\" "\"'" table)
    table))

(defun init ()
  (set-syntax-table tpl-mode-syntax-table))

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