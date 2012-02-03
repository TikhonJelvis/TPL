;; A simple mode for editing the TPL language.
;;
;; For now, I will just use generic mode. I may improve it in the
;; future. (I hope :p)

(require 'generic-x)

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
    "load")
  ;; font-lock-list
  '(("\\b\\([0-9]+\\|null\\|true\\|false\\)\\b" . font-lock-constant-face)
    ("^[ \t]*\\([_a-zA-Z0-9]+\\)[ \t]*:=" 1 'font-lock-variable-name-face)
    ("^[ \t]*\\([_a-zA-Z0-9]+\\)\\([ \t]+[_a-zA-Z0-9]+\\)+[ \t]*:=" 1 'font-lock-variable-name-face)
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
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'font-lock-builtin-face)))))
  ;; auto-mode-alist
  '(".tpl\\'")
  ;; function-list
  '())

(provide 'tpl-mode)