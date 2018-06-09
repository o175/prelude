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
(require 'tide)



(set-face-attribute 'default nil :height 105)
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

(defun tslint-fix-current-file ()
  (interactive)
  (save-window-excursion
    (async-shell-command
     (format "tslint --fix -c %stslint.json %s"
             (projectile-project-root)
             (buffer-file-name))
     "*Messages*" "*Messages*")
    ))

(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;; *********************       KEY BINDINGS
(global-set-key (kbd "<f1>" ) 'tide-fix)
(global-set-key (kbd "C-c <f1>") 'tide-project-errors)
(global-set-key (kbd "C-c C-r") 'tide-references)
(global-set-key (kbd "M-p" ) 'backward-paragraph)
(global-set-key (kbd "M-n" ) 'forward-paragraph )
(global-set-key (kbd "\C-o") 'newline-without-break-of-line)
(global-set-key (kbd "C-S-d") 'er/expand-region)
(global-set-key (kbd "C-#" ) 'comment-or-uncomment-region)
(global-set-key (kbd "C-M-#" ) 'comment-box)
(global-set-key (kbd "C-S-h") 'sl/expand-indent-selection)
(global-set-key (kbd "C-S-n") 'flycheck-next-error)
(global-set-key (kbd "C-S-p") 'flycheck-previous-error)
(global-set-key (kbd "M-SPC") 'just-one-space-new)
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-S-l") 'mc/edit-lines)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-v f") 'vimish-fold)
(global-set-key (kbd "C-S-v d") 'vimish-fold-delete)
(global-set-key (kbd "C-S-f") 'vimish-fold-toggle)
(global-set-key (kbd "C-M-.") 'tide-references)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(add-hook 'rjsx-mode-hook
          (lambda () (local-set-key (kbd "C-<f1>") #'tslint-fix-current-file)))

;; **********************      HOOKS

(add-hook 'web-mode-hook 'emmet-mode)
(add-to-list ' auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                      (string-equal "ts" (file-name-extension buffer-file-name))
                      )
              (setup-tide-mode))))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                      (string-equal "ts" (file-name-extension buffer-file-name))
                      )
              (setup-tide-mode))))

;; *********************      SETTINGS
(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))

  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (shell-command (concat eslint " --fix " (buffer-file-name)))
      ))
  )

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))


(defun toggle - cases()
       "Toggle between camelcase and underscore notation for the symbol at point."
       (interactive)
       (save - excursion
             (let * ((bounds(bounds - of - thing - at - point 'symbol))
                     (start(car bounds))
                     (end(cdr bounds))
                     (currently - using - underscores - p(progn(goto - char start)
                                                               (re - search - forward "_" end t))))
                  (if currently - using - underscores - p
                      (progn
                        (upcase - initials - region start end)
                        (replace - string "_" "" nil start end)
                        (downcase - region start(1 + start)))
                      (replace - regexp "\\([A-Z]\\)" "_\\1" nil(1 + start) end)
                      (downcase - region start(cdr(bounds - of - thing - at - point 'symbol)))))))


;;(define-key mc/keymap (kbd "<return>") nil) -- broken

(defun setup-js-mode ()
  (interactive)
  (flycheck-mode +1)
  (emmet-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)
                        )
                (append flycheck-disabled-checkers
                        '(typescript-tslint)
                        )
                (my/use-eslint-from-node-modules)
                (append flycheck-disabled-checkers
                        '(typescript-tide))
                )
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(defun my/use-eslint-from-node-modules ()
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(setq vc-handled-backends nil)
(setq-default flycheck-temp-prefix ".flycheck")
(setq whitespace-line-column 110)
(vimish-fold-global-mode 1)
(setq prelude-flyspell nil)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (emmet-mode t)

  (flycheck-add-mode 'typescript-tide 'rjsx-mode)
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
;;(flycheck-add-mode 'typescript-tslint 'rjsx-mode)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook 'setup-tide-mode)
(add-hook 'before-save-hook 'tide-format-before-save)
(setq helm-exit-idle-delay 0)
(yas-global-mode 1)

(setq org-default-notes-file  "~/notes.org")

(prelude-require-packages '(yasnippet-snippets yasnippet))
