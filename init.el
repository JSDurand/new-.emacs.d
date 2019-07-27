(setq gc-cons-threshold 10000000)

(require 'package)
(require 'cl-lib)

(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setf use-package-always-ensure t)

(use-package org :ensure t)

(org-babel-load-file (expand-file-name "setting.org" user-emacs-directory))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (setq org-bullets-bullet-list '("⚫" "◯" "✿" "♥" "✸"))
(setq org-bullets-bullet-list '("☸" "◯" "☢" "✿" "♥" "✸"))
;; (setq org-bullets-bullet-list `(,(all-the-icons-material "filter_1") "◯" "☢" "✿" "♥" "✸"))

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)
   (message "gc-cons-threshold restored to %S"
	    gc-cons-threshold)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix [163])
 '(TeX-PDF-mode nil)
 '(TeX-command "xetex")
 '(TeX-engine (quote xetex))
 '(ansi-color-names-vector
   ["#454545" "#d65946" "#6aaf50" "#baba36" "#598bc1" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(ansi-term-color-vector
   [unspecified "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#c5c8c6"] t)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "fb0ef4145e0219518ef684d5dc383af426334b565451a876bc368d7d64509323" "8bdacea3927456379d94e92769aaf5feaf61072055f156825b4a947df0b33b70" "1b48912b3cc838b1387abe6355eb418ed4d0ec2c14a5c006b165fc8aa20ee5f2" "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" "876fe28b6263ef36feeff1fa10db90a08ed899d1c6505b1d040d556db8ed0d2d" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "66aea5b7326cf4117d63c6694822deeca10a03b98135aaaddb40af99430ea237" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "5f21366118535267ae5e4cb8574cd7ab12bf1e9fd5edfce23ad3ed43b75450d2" default)))
 '(define-word-services
    (quote
     ((wordnik "http://wordnik.com/words/%s" define-word--parse-wordnik define-word-display-fn)
      (openthesaurus "https://www.openthesaurus.de/synonyme/%s" define-word--parse-openthesaurus define-word-display-fn)
      (wordreference "http://www.wordreference.com/fren/%s" define-word--parse-wordreference define-word-display-fn))))
 '(evil-search-module (quote evil-search))
 '(package-selected-packages
   (quote
    (popup intero pdf-tools org-noter noccur evil-surround evil-mode all-the-icons-dired all-the-icons o-blog cyphejor mu4e-alert org-drill org-plus-contrib emmet-mode xref-js2 js2-refactor js2-mode cnfonts general define-word sx olivetti dash-functional org-super-agenda amx emms haskell-mode projectile counsel-projectile elfeed org-pdfview undo-tree command-log-mode ivy-hydra tablist esup ox-reveal htmlize slime lispy org-bullets pacmacs rainbow-mode magit dired-details company-flx counsel tex auctex yasnippet wrap-region company-math company paredit expand-region iy-go-to-char org org-mode use-package)))
 '(pdf-tools-handle-upgrades nil)
 '(send-mail-function (quote mailclient-send-it))
 '(wrap-region-global-mode t nil (wrap-region)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(minibuffer-prompt ((nil (:background "gold" :foreground "black" :weight bold :height 1.0))))
;;  '(mode-line ((nil (:background "light blue" :foreground "black" :height 1.3))))
;;  '(mode-line-inactive ((t (:background "gray" :foreground "black" :height 1.0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)
