;; TODO: better loading of cask.el
;;(require 'cask "c:/Users/Craig/.cask/cask.el")
(require 'cask "/usr/local/Cellar/cask/0.8.4/cask.el")


;; bell settings - turn off annoying sounds
;;(setq visible-bell t) ;; show an alert instead of sound
(setq ring-bell-function 'ignore)

(cask-initialize)

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
		 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; the list
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

(require 'auto-org-md)

(auto-org-md-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes
   (quote
    ("fd3b1531faea72f67620800a332e790f9f67b04412ef335c396971fc73bee24b" default)))
 '(fci-rule-color "#00000000e665")
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files
   (quote
    ("~/workspace/refinitiv/notes.org" "~/workspace/omg/notes.org" "~/Documents/org/goals.org")))
 '(sql-connection-alist
   (quote
    (("k2a-dev-esms"
      (sql-product
       (quote postgres))
      (sql-user "postgres")
      (sql-database "esms")
      (sql-server "next-data-auroradev.cluster-c6f7hqbczcqo.us-east-1.rds.amazonaws.com")))))
 '(vc-annotate-background "#00000000cccc")
 '(vc-annotate-color-map
   (quote
    ((20 . "#e66500000000")
     (40 . "#EE3932")
     (60 . "#FF7509")
     (80 . "#e146e1460000")
     (100 . "#f0a2f0a20000")
     (120 . "#FFF200")
     (140 . "#0000e6650000")
     (160 . "#57CD7F")
     (180 . "#0f5cffff0f5c")
     (200 . "#1eb8ffff1eb8")
     (220 . "#28f5ffff28f5")
     (240 . "#3d70ffff3d70")
     (260 . "#93E0E3")
     (280 . "#00000000e665")
     (300 . "#00000000fae0")
     (320 . "#33323332ffff")
     (340 . "#66656665ffff")
     (360 . "#DDA0DD"))))
 '(vc-annotate-very-old-color "#DDA0DD"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UUID util
(load "uuid")
(require 'custom-uuid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Javascript
(setq js-indent-level 2)
(put 'downcase-region 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq-default js2-include-node-externs t)
(setq-default js2-global-externs '("describe" "it" "afterEach" "beforeEach" "sinon" "fetch"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp stuff
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (company-mode)
	    (flycheck-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bash stuff
(add-hook 'bash-mode
	  (lambda ()
	    (company-mode)
	    (flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python stuff
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:environment-root "jedi")
(setq jedi:environment-virtualenv (list "python3" "-mvenv"))
(add-hook 'python-mode
	  (lambda ()
	    (company-mode)
	    (flycheck-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; javascript
(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)
			   (setq flycheck-checker 'javascript-eslint)
			   (setq-default flycheck-disabled-checkers
					 (append flycheck-disabled-checkers
						 '(javascript-jshint)))))
(add-hook 'js2-mode-hook (lambda () (npm-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terraform
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json
(add-hook 'json-mode-hook #'flycheck-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; csharp / omnisharp
(eval-after-load
    'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)
  ;; csharp-mode README.md recommends this too
  ;; (electric-pair-mode 1)       ;; Emacs 24
  ;; electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile)

  (local-set-key [f5] 'omnisharp-unit-test-at-point))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
(column-number-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arduino
;(load "arduino-cli")
;(require 'arduino-cli-mode)
;(add-to-list 'auto-minor-mode-alist  '("\\.ino\\'" . arduino-cli-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aws utilities
(load "aws")
(require 'aws)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit setup
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-magit-file-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/helm")
(require 'helm-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nyan!
(nyan-mode)
(nyan-start-animation)
