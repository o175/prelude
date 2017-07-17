;; *********************      MODES
(require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
(require 'prelude-key-chord) ;; Binds useful features to key combinations
(require 'prelude-emacs-lisp)
(require 'prelude-js)
(require 'emmet-mode)
(require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more
(require 'prelude-shell)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)




;; *********************      FUNCTIONS
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(defun just-one-space-new ()
  (interactive)
  (just-one-space -1))


;; *********************       KEY BINDINGS

(global-set-key (kbd "M-p" ) 'backward-paragraph)
(global-set-key (kbd "M-n" ) 'forward-paragraph )
(global-set-key (kbd "\C-o") 'newline-without-break-of-line)
(global-set-key (kbd "C-S-d") 'er/expand-region)
(global-set-key (kbd "C-#" ) 'comment-or-uncomment-region)
(global-set-key (kbd "C-M-#" ) 'comment-box)
(global-set-key (kbd "C-S-h") 'sl/expand-indent-selection)
(global-set-key (kbd "M-SPC") 'just-one-space-new)
(define-key global-map "\C-cc" 'org-capture)

;; **********************      HOOKS

(add-hook 'web-mode-hook 'emmet-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
