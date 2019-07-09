(require 'font-lock)
(add-to-list 'load-path "~/.emacs.d/elpa/font-lock+")
(require 'font-lock+)
(setq delete-old-versions t)		; delete excess backup versions silently
(setq version-control nil)		; use version control
(setq vc-make-backup-files nil)		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
;; (setq coding-system-for-read 'utf-8 )	; use utf-8 by default
;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 90)		; toggle wrapping text at the 90th character
(setq-default fill-column 90)
(global-visual-line-mode 1)
(setq initial-scratch-message "Bonjour!") ; print a default message in the empty scratch buffer opened at startup

(setq-default display-line-numbers nil)
(setq-default display-line-numbers-current-absolute t)
(global-set-key (vector ?\s-x) (lambda ()
                                 (interactive)
                                 (setq display-line-numbers
                                       (and (not display-line-numbers)
                                            'relative))))
(set-face-attribute 'line-number nil :foreground "light blue")
(set-face-attribute 'line-number-current-line nil :foreground "gold")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(setq ns-function-modifier 'hyper)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(global-set-key "\M-'" 'insert-quotations)
(global-set-key "\M-\"" 'insert-quotes)
;; (global-set-key (kbd "C-'") 'insert-backquote)

;;;###autoload
(defun insert-quotations (&optional arg)
  "Enclose following ARG sexps in quotation marks.
Leave point after open-paren."
  (interactive "*P")
  (insert-pair arg ?\' ?\'))

;;;###autoload
(defun insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote."
  (interactive "*P")
  (insert-pair arg ?\" ?\"))

;;;###autoload
(defun insert-backquote (&optional arg)
  "Enclose following ARG sexps in quotations with backquote.
Leave point after open-quotation."
  (interactive "*P")
  (insert-pair arg ?\` ?\'))

;; Move more quickly
(global-set-key (kbd "C-S-n")
		(lambda ()
		  (interactive)
		  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
		(lambda ()
		  (interactive)
		  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
		(lambda ()
		  (interactive)
		  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
		(lambda ()
		  (interactive)
		  (ignore-errors (backward-char 5))))

;;;###autoload
(defun copy-line (arg)
  "Copy line accoring to ARG"
  (interactive "p")
  (let ((beg (line-beginning-position))
	(end (line-end-position arg)))
    (kill-ring-save beg end nil)
    (kill-append "\n" nil)))


(global-set-key [?\M-k] 'copy-line)

;;;###autoload
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
	(indent-buffer)
	(message "Indented buffer.")))))
(global-set-key [?\C-c ?/] 'indent-region-or-buffer)

;;;###autoload
(defun backward-kill-line (arg)
  "Kill ARG lines backward"
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key [?\\] 'self-insert-command)
(global-set-key [s-backspace] 'backward-kill-line)
(global-set-key [?\C--] 'undo)
(global-set-key [f12] 'undo)

;;;(set-face-attribute 'default (selected-frame) :height 120)
;;;(set-face-attribute 'mode-line nil :height 200)
(set-default-font "DejaVu Sans Mono for Powerline 20")
(defun my-minibuffer-setup ()
  (let ((message-log-max nil)
        (inhibit-message t))
    (toggle-truncate-lines -1))
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.1)))
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default (:height 1.2) variable-pitch))))

  (with-current-buffer (get-buffer " *Echo Area 1*")
    (setq-local face-remapping-alist '((default (:height 1.2) variable-pitch)))))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

(global-set-key [?\s-w] 'delete-other-windows)

;; (defun scroll-half-page-down ()
;;   "scroll down half the page"
;;   (interactive)
;;   (scroll-down (/ (window-body-height) 2)))

;; (defun scroll-half-page-up ()
;;   "scroll up half the page"
;;   (interactive)
;;   (scroll-up (/ (window-body-height) 2)))

(load-file (expand-file-name "super-org.el" user-emacs-directory))

(setq diary-file "~/org/diary")

(load-file (expand-file-name "custom.el" user-emacs-directory))

;; (load-theme 'leuven)
;; (load-theme 'nimbus t)
(load-theme 'nimbus-tex-im t)
;; (load-theme 'default-black)
;; (load-theme 'my_theme t)

(use-package iy-go-to-char
  :ensure t
  :defer 10
  :config
  (global-set-key "\M-m" 'iy-go-to-char)
  (global-set-key "\M-p" 'iy-go-to-char-backward))

(use-package expand-region
  :ensure t
  :bind ([67108900] . 'er/expand-region) ; (kbd "C-$")
  :defer t
  :config
  ;; (global-set-key (kbd "C-$") 'er/expand-region)
  (pending-delete-mode t)
  (setf expand-region-fast-keys-enabled nil)
  ;; (setf (nthcdr 8 er/try-expand-list)
  ;;       (cdr (nthcdr 8 er/try-expand-list)))
  )

;; (defvar durand-custom-pairs '("()" "[]" "<>"
;;                               "{}" "\\[\\]"
;;                               "\\(\\)" "\\{\\}")
;;   "Some custom pairs to mark inside; I cannot handle identical delimiters at present.")

;; (defun durand-mark-inside-custom-pair ()
;;   "Mark inside some customo pairs"
;;   (interactive)
;;   (let ((open (durand-find-open-pair))
;;         (close (durand-find-close-pair)))
;;     (when (and open close)
;;       (goto-char close)
;;       (set-mark open))))

;;;###autoload
;; (defun durand-find-open-pair ()
;;   "Find the open pair before `(point)'"
;;   (interactive)
;;   (let ((beg (save-excursion
;;                (re-search-backward "\n[\t ]*\n" nil 'go)
;;                (skip-chars-forward "\n\t ")
;;                (point)))
;;         res)
;;     (dolist (pair durand-custom-pairs)
;;       (let* ((open-pair (substring-no-properties pair 0 (/ (length pair) 2)))
;;              (close-pair (substring-no-properties pair (/ (length pair) 2) nil))
;;              (op (save-excursion
;;                    (search-backward open-pair beg 'go)
;;                    (point)))
;;              (cl (save-excursion
;;                    (search-backward close-pair beg 'go)
;;                    (point))))
;;         (when (> op cl)
;;           (push op res))))
;;     (when (and res (consp res)) (apply #'max res))))

;;;###autoload
;; (defun durand-find-close-pair ()
;;   "Find the close pair after `(point)'"
;;   (interactive)
;;   (let ((end (save-excursion
;;                (re-search-forward "\n[\t ]*\n" nil 'go)
;;                (skip-chars-backward "\n\t ")
;;                (point)))
;;         res)
;;     (dolist (pair durand-custom-pairs res)
;;       (let* ((open-pair (substring-no-properties pair 0 (/ (length pair) 2)))
;;              (close-pair (substring-no-properties pair (/ (length pair) 2) nil))
;;              (op (save-excursion
;;                    (search-forward open-pair end 'go)
;;                    (point)))
;;              (cl (save-excursion
;;                    (search-forward close-pair end 'go)
;;                    (point))))
;;         (when (>= op cl)
;;           (push cl res))))
;;     (when (and res (consp res)) (apply #'min res))))

(use-package company
  :ensure t
  :defer 10
  :config
  (global-company-mode 1)
  ;; (global-set-key [tab] 'company-complete)
  ;; (global-set-key [tab] 'indent-for-tab-command)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (company-flx-mode 1)
  (setq company-flx-limit 200           ; flx can be slow
        company-tooltip-limit 15
        company-echo-delay 0)

  (setf company-require-match 'never)
  (setf company-selection-wrap-around t)
  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (setf company-begin-commands '(self-insert-command org-self-insert-command))
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
  ;; just use c-n/p to select
  ;; (define-key company-active-map [?\C-n] 'company-select-next)
  ;; (define-key company-active-map [?\C-p] 'company-select-previous-or-abort)
  (add-to-list 'completion-styles 'initials) ; initials completion style is handy.
  ;; This is convenient.
  (ignore-errors
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "S-TAB") 'company-complete-common-or-previous-cycle)
    (define-key company-active-map (kbd "<backtab>") 'company-complete-common-or-previous-cycle)
    (define-key company-active-map (kbd "RET") nil))

  ;; Modify the original function so that it completes the previous cycle.
  (defun company-complete-common-or-previous-cycle (&optional arg)
    "Insert the common part of all candidates, or select the previous one.
With ARG, move by that many elements."
    (interactive "p")
    (when (company-manual-begin)
      (let ((tick (buffer-chars-modified-tick)))
        (call-interactively 'company-complete-common)
        (when (eq tick (buffer-chars-modified-tick))
          (let ((company-selection-wrap-around t)
                (current-prefix-arg arg))
            (call-interactively 'company-select-previous))))))

  ;; Do not activate company mode in emacs lisp mode as it causes some crashes that even
  ;; abo-abo does not know how to fix for now!
  (add-hook 'emacs-lisp-mode-hook (lambda () (interactive) (company-mode -1)))
  (setq company-show-numbers t))

;; For minibuffer setup
;; I changed my mind now: I do not like company in minibuffer.
;; (defvar-local company-col-offset 0 "Horisontal tooltip offset.")
;; (defvar-local company-row-offset 0 "Vertical tooltip offset.")
;; (defun company--posn-col-row (posn)
;;   (let ((col (car (posn-col-row posn)))
;;         ;; `posn-col-row' doesn't work well with lines of different height.
;;         ;; `posn-actual-col-row' doesn't handle multiple-width characters.
;;         (row (cdr (posn-actual-col-row posn))))
;;     (when (and header-line-format (version< emacs-version "24.3.93.3"))
;;       ;; http://debbugs.gnu.org/18384
;;       (cl-decf row))
;;     (cons (+ col (window-hscroll) company-col-offset) (+ row company-row-offset))))
;; (defun company-elisp-minibuffer (command &optional arg &rest ignored)
;;   "`company-mode' completion back-end for Emacs Lisp in the minibuffer."
;;   (interactive (list 'interactive))
;;   (case command
;;     ('prefix (and (minibufferp)
;;                   (case company-minibuffer-mode
;;                     ('execute-extended-command (company-grab-symbol))
;;                     (t (company-capf `prefix)))))
;;     ('candidates
;;      (case company-minibuffer-mode
;;        ('execute-extended-command (all-completions arg obarray 'commandp))
;;        (t nil)))))

;; (defun minibuffer-company ()
;;   (unless company-mode
;;     (when (and global-company-mode (or (eq this-command #'execute-extended-command)
;;                                        (eq this-command #'eval-expression)))
;;       (setq-local company-minibuffer-mode this-command)
;;       (setq-local completion-at-point-functions
;;                   (list (if (fboundp 'elisp-completion-at-point)
;;                             #'elisp-completion-at-point
;;                           #'lisp-completion-at-point)
;;                         t))
;;       (setq-local company-show-numbers nil)
;;       (setq-local company-backends '((company-elisp-minibuffer company-capf)))
;;       (setq-local company-tooltip-limit 8)
;;       (setq-local company-col-offset 1)
;;       (setq-local company-row-offset 1)
;;       (setq-local company-frontends '(company-tng-frontend
;;                                       company-pseudo-tooltip-unless-just-one-frontend))
;;       (company-mode 1)
;;       ;; (when (eq this-command #'execute-extended-command)
;;       ;;   (company-complete))
;;       )))

;; (add-hook 'minibuffer-setup-hook #'minibuffer-company)

;; Make the doc buffer permanent
(defun durand/company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t)))

(ignore-errors (define-key company-active-map (kbd "<f1>") #'durand/company-show-doc-buffer))

;; (use-package intero
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode))

(org-babel-load-file "/Users/durand/.emacs.d/my_packages/tex.org")

(use-package wrap-region
  :ensure t
  :defer 10
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "$" "$")
  (wrap-region-add-wrapper "=" "=")
  (wrap-region-add-wrapper "-" "-"))

(use-package yasnippet
  :ensure t
  :defer 10
  :config
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
  (setq yas-snippet-dirs '("~/.emacs.d/my_snippets"))
  (yas-global-mode t))

(use-package multiple-cursors :ensure t
  :config
  (global-set-key (kbd "C-&") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-&") 'mc/mark-previous-like-this)
  (global-set-key (kbd "s-&") 'mc/mark-all-like-this)
  (global-set-key (kbd "H-&") 'mc/edit-lines)
  (global-set-key (kbd "C-<") 'mc/mark-pop))

;; (fset 'ud
;;       [?\C-c ?g ?\C-r ?t ?b ?l ?f ?m return ?\C-c ?\C-c ?\C-r ?t ?b ?l ?f ?m return ?\C-c ?\C-c ?\C-r ?s ?u ?m return tab ?\C-$ ?\M-w ?\C-c ?\C-p ?\C-c ?\C-x ?P ?t ?o ?t ?a ?l ?: ?  ?\C-y ?\C-\M-j])
;; (fset 'na
;;       [?\C-c ?h ?\M-: ?\( ?o ?r ?g ?- ?e ?n ?d ?- ?o ?f ?- ?s ?u ?b ?t ?r ?e ?e ?\) return ?\C-a ?\M-x ?o ?r ?g ?c ?o ?p ?y ?s ?u ?b return ?\M-x ?o ?r ?g ?p ?a ?s ?t ?e ?r ?s backspace backspace ?s down return ?\C-c ?\C-n S-tab S-tab S-tab ?\C-l ?\C-l ?\C-s ?n ?a ?m ?e return ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n tab tab ?0 tab ?t ?o ?d ?o tab])

(use-package counsel
  :ensure t
  :defer 1
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (global-set-key [?\s-s] 'counsel-grep-or-swiper)
  (global-set-key [?\C-s] 'counsel-grep-or-swiper)
  (global-set-key [?\H-s] 'isearch-forward)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key [?\s-f] 'counsel-find-file)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s %s")
  ;; (global-set-key [?\M-x] 'counsel-M-x)
  (setq ivy-use-selectable-prompt t))

(use-package ivy
  :ensure t
  :defer 1
  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-ignore-order)
          (swiper-multi . ivy--regex-ignore-order)
          (t . ivy--regex-fuzzy)))
  (setq ivy-wrap t)
  (ivy-set-actions
   'ivy-switch-buffer
   '(("k"
      (lambda (x)
        (kill-buffer-if-possible x)
        (ivy--reset-state ivy-last))
      "kill"))))

;; minibuffer color customisation
(set-face-foreground 'minibuffer-prompt "goldenrod2")
(set-face-background 'minibuffer-prompt "chocolate4")

;; I ended up writing my own version using `ivy-read'...
;; (use-package headlong
;;   :ensure t
;;   :defer 1)

;;;###autoload
(defun kill-buffer-if-possible (buf)
  "remove the buffer from the recentf list"
  (interactive)
  (when (get-buffer buf)
    (kill-buffer buf)))

;;;###autoload
(defun durand-ivy-format-function-arrow (cands)
  "Transform CAND-PAIRS into a string for minibuffer using \"->\" instead of \">\"."
  (ivy--format-function-generic
   (lambda (str)
     (concat (propertize "☸ "
                         'face
                         '(:foreground "gold" :height 300))
             (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "   " str))
   cands
   "\n"))
(setq ivy-format-function 'durand-ivy-format-function-arrow)

;;;###autoload
(defun durand-ivy-format-function-generic (selected-fn other-fn cands separator)
  "Ido style!"
  (let ((i -1))
    (mapconcat
     (lambda (str)
       (let ((curr (eq (cl-incf i) 0)))
         (if curr
             (funcall selected-fn str)
           (funcall other-fn str))))
     (durand-ivy-cycle-collection (- 0 ivy--window-index) cands)
     separator)))

;;;###autoload
(defun durand-ivy-cycle-collection (arg col)
  ;; (message "%d" (length col))
  (let* ((arg (or arg 1))
         (len (length col))
         new-li)
    (dotimes (ind len new-li)
      (push
       (nth (mod (- len ind arg 1) len) col)
       new-li))))

(use-package ivy-hydra :ensure t :defer t)

(use-package counsel-projectile
  :ensure t
  :defer t
  :config
  (counsel-projectile-mode 1))

(use-package projectile
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'projectile
    (projectile-global-mode 1)
    (define-key projectile-mode-map [?\s-d] 'projectile-command-map)
    (setq projectile-completion-system 'ivy
          projectile-git-submodule-command nil
          projectile-indexing-method 'native
          projectile-enable-caching t
          projectile-generic-command "find -L . -type f -print0")))

(use-package amx
  :ensure t
  :defer t
  :config
  (amx-mode 1)
  (global-set-key [?\M-x] 'amx)
  (setq amx-ignored-command-matchers nil)
;; I find it useful to loop up commands based on key bindings
  (setq amx-show-key-bindings nil))

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-char-timer))
  :config
  (setq avy-keys (nconc
                  (number-sequence ?a ?z)
                  (number-sequence ?A ?Z))))

(column-number-mode 1)
(set-face-attribute 'mode-line-buffer-id nil :background "gray20" :foreground "DarkOrange1" :box nil)
;; (set-face-font 'mode-line-buffer-id "DejaVu Sans Mono for Powerline")
;; (set-face-attribute 'mode-line-buffer-id nil :height 1.1)
(set-face-attribute 'mode-line-highlight nil :box nil :background "deep sky blue")
(set-face-attribute 'mode-line-inactive  nil :background "#97cfda" :height 1.1 :foreground "gray20" :box nil)

;; (setq mode-line-position
;; 	   '(;; %p print percent of buffer above top of window, or Top, Bot or All
;; 	     ;; (-3 "%p")
;; 	     " "
;; 	     ;; %I print the size of the buffer, with kmG etc
;; 	     ;; (size-indication-mode ("/" (-4 "%I")))
;; 	     ;; " "
;; 	     ;; %l print the current line number
;; 	     ;; %c print the current column
;; 	     (line-number-mode ("%l" (column-number-mode ":%c")))))

(defface durand-custom-mode-face '((t (:foreground "red")))
  "Face used for displaying hydra presence")

;;;###autoload
(defun my-position ()
  "My function of mode-line-position"
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (org-agenda-show-blocks-number))
   (t
    " %l:%c")))

;;;###autoload
(defun my-percent ()
  "My function of mode-line-position"
  (cond
   ((derived-mode-p 'org-agenda-mode)
    nil)
   (t
    (let* ((str "%p%% ")
           (fstr (format-mode-line str)))
      (cond
       ((string-match " ?\\(Top\\|Bot\\|All\\)" fstr)
        (concat (match-string 1 fstr) " "))
       (t
        str))))))

(setf old-mode-line-position mode-line-position)

(setf mode-line-position '(:eval (my-position)))

(defun propertized-buffer-identification (fmt)
  "Return a list suitable for `mode-line-buffer-identification'.
     FMT is a format specifier such as \"%12b\".  This function adds
     text properties for face, help-echo, and local-map to it."
  (list (propertize fmt
                    'face 'mode-line-buffer-id
                    'help-echo
                    (purecopy "Nom du tampon\nsouris-1: Dernier tampon\nsouris-3: Prochain tampon")
                    'mouse-face 'mode-line-highlight
                    'local-map mode-line-buffer-identification-keymap)))

(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defun my-mode-line-modified ()
  (propertize
   (concat
    (if (buffer-modified-p)
        (propertize "M " 'face '(:foreground "DarkGoldenrod2"))
      " ")
    (if (not (buffer-file-name))
        (propertize "N " 'face '(:foreground "CadetBlue2"))
      " ")
    (if buffer-read-only
        (propertize "R " 'face '(:foreground "IndianRed1"))
      " "))
   'help-echo "M: modifié 
N: peut-être pas un fichier
R: seulement pour lire"))

(defvar durand-custom-modeline ""
  "A custom variable to set for customisation")

(set-face-attribute 'durand-custom-mode-face nil
                    ;; :foreground "#d99f21"
                    :foreground "goldenrod1"
                    :inherit nil
                    :height 1.0)
(setq durand-custom-modeline (propertize "S"
                                         'help-echo "spécifique"))

(defface durand-mode-line-client-face '((t . (:foreground "LightSkyBlue3" :box t)))
  "Face for mode line client construct")

(defun durand-mode-line-buffer-name ()
  "trimmed buffer name"
  (let* ((orig (format-mode-line (propertized-buffer-identification " %b")))
         (max 35)
         (real-name (cond
                     ((> (length orig) max)
                      (truncate-string-to-width orig max nil nil t))
                     (t
                      orig)))
         (tail (when (buffer-modified-p) (propertize "| + " 'face '(:background "gray20"))))
         (not-file (when (null (buffer-file-name)) (propertize "| N " 'face '(:background "gray20"))))
         (RO (when buffer-read-only (propertize " RO |" 'face '(:background "gray20")))))
    (string-join (append
                  (list RO)
                  (propertized-buffer-identification real-name)
                  (list (propertize " " 'face '(:background "gray20")))
                  (list not-file tail)))))

(defun durand-mode-line-client ()
  "Custom mode line client construct"
  (propertize
   (if (frame-parameter nil 'client)
       "@" "")
   'face 'durand-mode-line-client-face
   'help-echo "un client d'emacs"))

(setf my-mode-line-buffer-name '(:eval (durand-mode-line-buffer-name)))

;; From https://emacs.stackexchange.com/questions/26222/
(defvar ml-selected-window nil)

(defun ml-record-selected-window ()
  (setq ml-selected-window (selected-window)))

(defun ml-update-all ()
  (force-mode-line-update t))

;; (remove-hook 'post-command-hook 'ml-record-selected-window)

;; (remove-hook 'buffer-list-update-hook 'ml-update-all)


;; (defun durand-mode-line-right ()
;;   "The right portion of the mode line"
;;   (let* ((mode-line-before (remq 'mode-line-end-spaces mode-line-format))
;;          (mode-line-string-before (format-mode-line mode-line-before))
;;          (length-before (length mode-line-string-before))
;;          (right '(:eval (my-position)))
;;          (right-plain-string (format-mode-line right))
;;          (right-padded-string (cond
;;                                ((string-match "  $" right-plain-string)
;;                                 right-plain-string)
;;                                ((string-match " $" right-plain-string)
;;                                 (mapconcat #'identity `(,right-plain-string " ") ""))
;;                                (t
;;                                 (mapconcat #'identity `(,right-plain-string "  ") ""))))
;;          (right-string (propertize right-padded-string
;;                                    'face '(:foreground "#97cfda" :background "gray20")))
;;          (padding-total-length (cond
;;                                 ((eq ml-selected-window
;;                                      (selected-window))
;;                                  (* 9 (/ (window-total-width) 11)))
;;                                 (t
;;                                  (window-total-width))))
;;          (padding-length (- padding-total-length length-before (length right-string) -1))
;;          (padding (when (> padding-length 0) (make-string padding-length 32))))
;;     (string-join (list padding right-string))))

;;;###autoload
(defun durand-mode-line-right ()
  "The right portion of the mode line"
  (let* ((right '((:eval (my-percent)) (:eval (my-position))))
         (extra-space (cond
                       ((string-match "^ " (format-mode-line '((:eval (my-percent))))) nil)
                       ((string= (format-mode-line '((:eval (my-percent)))) "") nil)
                       (t " ")))
         ;; (right-plain-string (format-mode-line right))
         ;; (right-padded-string (concat " " (cond
         ;;                                   ((string-match " +$" right-plain-string)
         ;;                                    (replace-match "" nil nil right-plain-string))
         ;;                                   (t
         ;;                                    right-plain-string))
         ;;                              " "))
         (right-string (concat
                        (propertize (concat extra-space (format-mode-line '((:eval (my-percent)))))
                                    'face '(:foreground "#97cfda" :background "gray20"))
                        (propertize (concat (format-mode-line '((:eval (my-position)))) " ")
                                    'face '(:foreground "gray20" :background "#97cfda"))))
         (right-length (string-width right-string))
         (fix (cond
               ((string-match "Top\\|Bot\\|All" right-string) 1)
               (t nil)))
         (padding (propertize
                   " "
                   'display `((space :align-to (- (+ right right-margin right-fringe) ,right-length ,fix))))))
    (string-join `(,padding ,right-string))))

(setf mode-line-end-spaces '(:eval (durand-mode-line-right)))

;;;###autoload
(defun propertize-mode-name (fmt)
  (propertize fmt
              'face '(:background "gray20" :foreground "gray")))

;;;###autoload
(defun durand-mode-name ()
  (propertize-mode-name (format-mode-line (mapconcat #'identity
                                                     `("| %[" ,mode-name "%] "
                                                       ,(when (buffer-narrowed-p)
                                                          "| Narrow "))
                                                     ""))))

(setq-default mode-line-format
              '("%e"
                (:eval (unless
                           (string-equal (format-mode-line mode-line-front-space) " ")
                         (propertize mode-line-front-space
                                     'face '(:background "gray20"))))
                ;; mode-line-mule-info -- I'm always on utf-8
                ;; (:eval (durand-mode-line-client))
                ;; (:eval (my-mode-line-modified))
                ;; mode-line-remote -- no need to indicate this specially
                ;; mode-line-frame-identification -- this is for text-mode emacs only
                ;; (:eval (propertize durand-custom-modeline 'face 'durand-custom-mode-face))
                (:eval evil-mode-line-tag)
                ;; " "
                (:eval (durand-mode-line-buffer-name))
                ;; " "
                ;; mode-line-position
                ;; (:eval (my-position))
                ;; (:eval (replace-regexp-in-string "^ Git" "" vc-mode ))  ;; -- I use magit
                ;; (flycheck-mode flycheck-mode-line) -- I don't have this
                (:eval (durand-mode-name))
                ;; Only major mode
                ;; "%n"
                mode-line-misc-info
                mode-line-end-spaces))
;; mode-line-modes -- I don't want all those minor modes information
;; " %I "


(setq durand-default-mode-line-format mode-line-format)

(defvar durand-mode-line-toggled nil
  "Determine if mode line is toggled")

(make-variable-buffer-local 'durand-mode-line-toggled)

(defun durand-toggle-mode-line (&optional arg)
  "If ARG is nil, then toggle the mode-line.
If ARG is a positive integer, then set the mode-line-format to the default one.
If ARG is a negative integer, then set the mode-line-format to NIL."
  (interactive "P")
  (pcase arg
    ((and (pred integerp)
          (pred (lambda (num) (<= num 0))))
     (setq mode-line-format nil)
     (force-mode-line-update))
    ((guard (equal major-mode 'pdf-view-mode))
     (message "Nom: %s,
Pourcentes: %d
page: %d,
total: %d"
              (buffer-name)
              (string-to-number (pdf-misc-size-indication))
              (pdf-view-current-page)
              (pdf-cache-number-of-pages)))
    ((pred null)
     ;; (pcase mode-line-format
     ;;   ((pred null)
     ;;    (setq mode-line-format durand-default-mode-line-format)
     ;;    (redisplay))
     ;;   (_
     ;;    (assert (not (null durand-default-mode-line-format)))
     ;;    (setq mode-line-format '((:eval (propertized-buffer-identification " %b "))))
     ;;    (redisplay)))
     (pcase durand-mode-line-toggled
       ((pred null)
        ;; (setq mode-line-format '((:eval (propertized-buffer-identification " %b "))))
        (setq mode-line-format nil)
        (setf durand-mode-line-toggled (not durand-mode-line-toggled))
        (force-mode-line-update))
       (_
        (setq mode-line-format durand-default-mode-line-format)
        (setf durand-mode-line-toggled (not durand-mode-line-toggled))
        (force-mode-line-update))))
    ((and (pred integerp)
          (pred (lambda (num) (>= num 0))))
     (setq mode-line-format durand-default-mode-line-format)
     (force-mode-line-update))
    (_
     (message "ARG should be either NIL, or an integer, but got %s" arg))))

(global-set-key [?\s-m] 'durand-toggle-mode-line)

(add-hook 'pdf-view-mode-hook (lambda () (durand-toggle-mode-line -1)))

(set-face-attribute 'mode-line nil
                    :background "gray10" :foreground "gray" :height 1.1 :box nil)

;; (defface durand-custom-mode-face '((t (:foreground "red" :inherit mode-line)))
;;   "Face used for displaying hydra presence")

(use-package lispy
  :ensure t
  :defer 5
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'lisp-mode-hook 'lispy-mode)
  (add-hook 'lisp-interaction-mode-hook 'lispy-mode))

(use-package magit
  :ensure t
  :defer 10
  :config
  (global-set-key [?\C-x ?g] 'magit-status)
  (setq magit-completing-read-function 'ivy-completing-read))

;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
;; (use-package slime
;;   :ensure t 
;;   :defer 20
;;   :config
;;   (define-key slime-mode-map [?\C-x ?\C-e] 'slime-eval-last-expression))

(load-file "~/.emacs.d/my_packages/music/music.el")

;; (use-package emms
;;   :ensure t
;;   :config
;;   (require 'emms-setup)
;;   (require 'emms-player-mpd)
;;   (emms-all) ; don't change this to values you see on stackoverflow questions if you expect emms to work
;;   (setq emms-seek-seconds 5)
;;   (setq emms-player-list '(emms-player-mpd))
;;   (setq emms-info-functions '(emms-info-mpd))
;;   (setq emms-player-mpd-server-name "localhost")
;;   (setq emms-player-mpd-server-port "6601")
;;   ;; (define-key global-map [?\s-m] nil)
;;   ;; for mpc
;;   (setq mpc-host "localhost:6601")
;;   :bind
;;   ("C-c m p" . emms)
;;   ("C-c m b" . emms-smart-browse)
;;   ("C-c m r" . emms-player-mpd-update-all-reset-cache)
;;   ("C-c m k" . emms-previous)
;;   ("C-c m j" . emms-next)
;;   ("C-c m P" . emms-pause)
;;   ("C-c m s" . emms-stop))

;; (defun mpd/start-music-daemon ()
;;   "Start MPD, connects to it and syncs the metadata cache."
;;   (interactive)
;;   (shell-command "mpd")
;;   (mpd/update-database)
;;   (emms-player-mpd-connect)
;;   (emms-cache-set-from-mpd-all)
;;   (message "MPD Started!")
;;   (setq emms-playing-time-p nil
;; 	emms-mode-line-active-p nil))
;; (global-set-key (kbd "C-c m c") 'mpd/start-music-daemon)

;; (defun mpd/kill-music-daemon ()
;;   "Stops playback and kill the music daemon."
;;   (interactive)
;;   (emms-stop)
;;   (call-process "killall" nil nil nil "mpd")
;;   (message "MPD Killed!"))
;; (global-set-key (kbd "C-c m d") 'mpd/kill-music-daemon)

;; (defun mpd/update-database ()
;;   "Updates the MPD database synchronously."
;;   (interactive)
;;   (call-process "mpc" nil nil nil "update")
;;   (message "MPD Database Updated!"))
;; (global-set-key (kbd "C-c m u") 'mpd/update-database)

(use-package iedit :ensure t
  :defer 10
  :config
  ;; bind to "C-;", the number is produced by the function kbd
  (global-set-key [67108923] 'iedit-mode))

;; (use-package esup
;;   :ensure t
;;   :defer t)

(use-package pdf-tools
  :ensure t
  :defer 15
  :pin manual ;; manually update
  :config
  (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil))    ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  ;; ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width
                pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "s-s") 'isearch-forward)
  (define-key pdf-view-mode-map [?j] (lambda () (interactive) (pdf-view-scroll-up-or-next-page 1)))
  (define-key pdf-view-mode-map [?k] (lambda () (interactive) (pdf-view-scroll-down-or-previous-page 1)))
  (define-key pdf-view-mode-map [?\r] 'durand-view-pdf-little-help-function)

  (define-key pdf-annot-minor-mode-map [?\C-c ?\C-a ?h] 'durand-pdf-add-highlighs)

  ;; Since the default pdf-links-read-link-action function does not work because of some
  ;; imagemagick errors, I decide to write my own version.
  (defun pdf-links-read-link-action (prompt)
    "Chois un lien dans cette page"
    (pdf-util-assert-pdf-window)
    (let* ((links (pdf-cache-pagelinks
                   (pdf-view-current-page)))
           (keys (pdf-links-read-link-action--create-keys
                  (length links)))
           (key-strings (mapcar (apply-partially 'apply 'string)
                                keys))
           (alist (cl-mapcar 'cons key-strings links)))
      (unless links
        (user-error "Aucun lien dans cette page!"))
      (alist-get (ivy-read "Chois un lien:" alist :require-match t) alist nil nil #'string=)))
  ;; Since the default function to add highlight annotations does not work for some
  ;; readon, I decide to write my own function.
  (defun durand-pdf-add-highlighs ()
    "Choose highlighting area by clicking twice instead of using regions"
    (interactive)
    (pdf-util-assert-pdf-window)
    (let* ((choice-one (pdf-util-read-image-position "Chois le TOP LEFT de la première région."))
           (choice-two (pdf-util-read-image-position "Chois le TOP LEFT de la seconde région."))
           (coords-one (durand-pdf-convert-coors choice-one))
           (coords-two (durand-pdf-convert-coors choice-two)))
      (pdf-annot-add-annotation 'highlight (list (car coords-one)
                                                 (cdr coords-one)
                                                 (car coords-two)
                                                 (cdr coords-two))
                                '(("color" . "#ffff00")))))

  (defun durand-pdf-convert-coors (obj)
    "Convert to relative coordinates"
    (let* ((len (length obj))
           (obj-pos (nth (- len 2) obj))
           (all-pos (nth (1- len) obj)))
      (cons (/ (car obj-pos) (car all-pos) 1.0)
            (/ (cdr obj-pos) (cdr all-pos) 1.0)))))

;; a temporary function to be used in pdf mode

;;;###autoload
(defun durand-view-pdf-little-help-function ()
  "Since the size does not match, I would like to adjust the size
a ilttle bit.
As a reminder, this is defined in setting.org, pdf-tools section."
  (interactive)
  (pdf-view-enlarge 1.56)
  (image-forward-hscroll 34))

;; (use-package key-chord
;;   :ensure t
;;   :load-path "/Users/durand/.emacs.d/elpa/keychord/keychord.el"
;;   :config
;;   (key-chord-mode -1)
;;   (key-chord-define-global ",;" 'general-hydra/body)
;;   (setf key-chord-two-keys-delay 0.1)
;;   (setf key-chord-one-key-delay 0.11))

(add-to-list 'load-path (expand-file-name "my_packages/ideal" user-emacs-directory))

(require 'ideal)

;; (use-package command-log-mode
;;   :ensure t
;;   :demand
;;   :config
;;   (setq command-log-mode-auto-show t)
;;   (setq global-command-log-mode t))

(defun durand-days-between (date1 date2 &optional base)
  "The default `days-between' function is not clear about
what date string format is allowed, so I make my own version.

The date string is delimited by either \"-\" or spaces or \"_\":
anything matched by `[ |_|-]+'.
And it should be in this order: `YEAR-MONTH-DAY'.

If optional BASE is non-nil, then it should be a positive integer between `2' and `36', and
the output will be a string representation of the difference between the two dates
with respect to the given BASE.
If BASE is omitted or does not satisfy the constraints, then it defults to 10."
  (interactive)
  (require 'calc-bin)	    ; required for convenient radix conversion
  (let* ((day1-string (split-string date1 "[ |_|-]+"))
	 (day2-string (split-string date2 "[ |_|-]+"))
	 (day1-list (mapcar #'string-to-number day1-string))
	 (day2-list (mapcar #'string-to-number day2-string))
	 (day1-time (time-to-days (encode-time 0 0 0
					       (caddr day1-list)
					       (cadr day1-list)
					       (car day1-list))))
	 (day2-time (time-to-days (encode-time 0 0 0
					       (caddr day2-list)
					       (cadr day2-list)
					       (car day2-list))))
	 (radix-string-function (lambda (num base)
				  (cond ((and (integerp base)
					      (<= 2 base)
					      (>= 36 base))
					 (let ((calc-number-radix base))
					   (math-format-radix num)))
					(t
					 (format "%d" num))))))
    (funcall radix-string-function (- day1-time day2-time) base)))

;; (mapconcat (lambda (x) (if (= x ?1) "嘰" "咕"))
;; 	   (durand-days-between "2018-08-07"
;; 				"2017-06-25"
;; 				2)
;; 	   "")

(defun durand-MAOBAOBAO-date-calc (&optional 咕嘰)
  "Calculate the difference between today and \"2017-06-25\".

If `咕嘰' is non-nil, then convert the result to a string of the two characters:
\"咕\" means 0 and \"嘰\" means 1."
  (interactive)
  (let ((result (durand-days-between
		 (mapconcat
		  (lambda (x) (format "%d" x))
		  ((lambda (x)
		     (list
		      (caddr x)
		      (car x)
		      (cadr x)))
		   (calendar-current-date))
		  " ")
		 "2017_06_25"
		 2))
	(convert-function (or (and 咕嘰
				   '(lambda (x) (if (= x ?1) "嘰" "咕")))
			      'char-to-string)))
    (mapconcat convert-function result "")))

;; (load-file (expand-file-name "mu-el.el" user-emacs-directory))

(use-package undo-tree
  :ensure t
  :defer 10
  :config
  (define-key global-map [remap undo] 'undo-tree-undo)
  (define-key global-map [?\C--] 'undo-tree-redo)
  (define-key global-map [?\H-u] 'undo-tree-visualize)
  (define-key undo-tree-visualizer-mode-map (kbd "(") 'undo-tree-visualize-undo-to-x)
  (define-key undo-tree-visualizer-mode-map (kbd ")") 'undo-tree-visualize-redo-to-x)
  (setf undo-tree-enable-undo-in-region nil))

(require 'org-mu4e)

(setq org-mu4e-link-query-in-headers-mode nil)

(org-link-set-parameters "mu4e" :follow #'org-mu4e-open
                         :store #'org-mu4e-store-link)

(use-package org-pdfview
  :ensure t
  :demand
  :config
  ;; custom store link function to store the height as well
  (defun org-pdfview-store-link ()
    "Store a link to a pdfview buffer."
    (when (eq major-mode 'pdf-view-mode)
      ;; This buffer is in pdf-view-mode
      (let* ((path buffer-file-name)
             (page (pdf-view-current-page))
             (height (let ((ori (substring-no-properties (pdf-misc-size-indication) 1)))
                       (cond
                        ((string= ori "Bot")
                         "55")
                        ((string= ori "Top")
                         nil)
                        (t
                         (if (string-match "%%" ori)
                             (replace-match "" nil nil ori)
                           ori)))))
             (real-height (when height
                            (number-to-string (/ (string-to-number height) 100.0))))
             (link (concat "pdfview:" path "::" (number-to-string page)
                           (when height (concat "++" real-height)))))
        (org-store-link-props
         :type "pdfview"
         :link link
         :description path)))))

(defun org-elfeed-store-link ()
  "Store a link to an elfeed search or entry buffer."
  (cond ((derived-mode-p 'elfeed-search-mode)
	 (org-store-link-props
	  :type "elfeed"
	  :link (format "elfeed:%s" elfeed-search-filter)
	  :description elfeed-search-filter))
	((derived-mode-p 'elfeed-show-mode)
	 (org-store-link-props
	  :type "elfeed"
	  :link (format "elfeed:%s#%s"
			(car (elfeed-entry-id elfeed-show-entry))
			(cdr (elfeed-entry-id elfeed-show-entry)))
	  :description (elfeed-entry-title elfeed-show-entry)))))

(defun org-elfeed-open (filter-or-id)
  "Jump to an elfeed entry or search, depending on what FILTER-OR-ID looks like."
  (message "filter-or-id: %s" filter-or-id)
  (if (string-match "\\([^#]+\\)#\\(.+\\)" filter-or-id)
      (elfeed-show-entry (elfeed-db-get-entry (cons (match-string 1 filter-or-id)
						    (match-string 2 filter-or-id))))
    (switch-to-buffer (elfeed-search-buffer))
    (unless (eq major-mode 'elfeed-search-mode)
      (elfeed-search-mode))
    (elfeed-search-set-filter filter-or-id)))

(org-link-set-parameters
 "elfeed"
 :follow 'org-elfeed-open
 :store 'org-elfeed-store-link)

;; (defun sx-org-store-link (link)
;;   "Store the search for org mode"
;;   (interactive)
;;   (when (derived-mode-p 'sx-question-link-mode)
;;     (org-store-link-props
;;      :type sx-question
;;      :link)))

(add-to-list 'org-file-apps '(directory . emacs))
(add-to-list 'org-file-apps '("mp4" . "mpv --no-terminal --autofit=100%x100% --no-border --geometry=+0+-24 %s"))
(add-to-list 'org-file-apps '("mkv" . "mpv --no-terminal --autofit=100%x100% --no-border --geometry=+0+-24 %s"))

(use-package olivetti
  :ensure t
  :defer t
  :config
  (setq-default olivetti-body-width 90))

;; (use-package sx :ensure t)

(use-package define-word :ensure t
  :config
  (global-set-key [?\C-c ?d ?e] 'define-word-at-point)
  (global-set-key [?\C-c ?d ?f] 'define-french-word-at-point)
  (defun define-french-word-at-point ()
    "Define the french word at point"
    (interactive)
    (define-word (substring-no-properties (thing-at-point 'word))
      'wordreference)))

;;;###autoload
(defun define-word--parse-wordreference ()
  "Parse output from wordreference site and return formatted list"
  (save-match-data
    (let (results beg part)
      (while (re-search-forward "tr.*class=.*even.*?>" nil t)
        (re-search-forward "strong>" nil t)
        (setq part (buffer-substring-no-properties
                    (point)
                    (1- (re-search-forward "<" nil t))))
        (unless (= 0 (length part))
          (setq part (decode-coding-string (concat part " ") 'utf-8)))
        (re-search-forward "<td>")
        (setq middle (buffer-substring-no-properties
                      (1- (re-search-forward "(" nil t))
                      (re-search-forward ")" nil t)))
        (unless (= 0 (length middle))
          (setq middle (concat middle " ")))
        (setq middle (replace-regexp-in-string
                      "<[^>]*>\\([^<]*\\)<[^>]*>"
                      "\\1"
                      (decode-coding-string middle 'utf-8)))
        (re-search-forward "ToWrd[^>]*>" nil t)
        (setq def (decode-coding-string
                   (buffer-substring-no-properties
                    (point) (1- (re-search-forward "<" nil t)))
                   'utf-8))
        (push (concat (propertize part 'face 'define-word-face-1)
                      (propertize middle 'face 'define-word-face-1)
                      (propertize def 'face 'define-word-face-2))
              results))
      (setq results (nreverse results))
      (cond ((= 0 (length results))
             (message "0 definitions found"))
            ;; ((and define-word-unpluralize
            ;;       (cl-every (lambda (x) (string-match "[Pp]\\(?:lural\\|l\\.\\).*of \\(.*\\)\\." x))
            ;;                 results))
            ;;  (define-word (match-string 1 (car (last results))) 'wordnik))
            (t
             (when (> (length results) define-word-limit)
               (setq results (cl-subseq results 0 define-word-limit)))
             (mapconcat #'identity results "\n"))))))

;;;###autoload
(defun define-word-display-fn (res)
  "Display RES in a separate buffer"
  (interactive)
  (with-current-buffer-window
   "*define-word-results*"
   nil nil
   (insert res)))

(use-package general
  :ensure t
  :defer t)

(require 're-builder)
(define-key reb-mode-map [?§] (lambda () (interactive) (insert "\\")))

;; (use-package haskell-mode
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq haskell-hoogle-command "~/.local/bin/hoogle")
;;   (with-eval-after-load "haskell-mode"
;;     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)
;;     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)))

(use-package cnfonts
  :ensure t)

(load-file (expand-file-name "cyphejor.el" user-emacs-directory))
(require  'cyphejor)
(setf cyphejor-rules
      '(("emacs" "ε")
        ("lisp" "λ")
        ("mode" "")
        ("org" "ω")
        ("agenda" "α")
        ("tex" "τ")
        ("plain" "π")
        ("latex" "Λ" :prefix)
        ("buffer" "b")
        ("dired" "δ")
        ("messages" "mes")
        ("fundamental" "F")
        ("mu4e" "m")
        ("main" "Μ")
        ("update" "u")
        ("headers" "h")
        ("help" "H")
        ("pdf" "P")
        ("view" "v")
        ("durand" "D")
        ("timer" "t")
        ("list" "L")
        ("interaction" "int")))
(cyphejor-mode 1)

;; (advice-remove 'mu4e-headers-search-bookmark  (lambda (&rest args) (cyphejor-mode 1)))
;; (advice-remove 'mu4e-headers-search  (lambda (&rest args) (cyphejor-mode 1)))
;; (advice-remove 'mu4e~headers-jump-to-maildir  (lambda (&rest args) (cyphejor-mode 1)))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(add-to-list 'load-path (expand-file-name "evil-numbers"
                                          (expand-file-name "elpa" user-emacs-directory)))

(require 'evil-numbers)

(use-package noccur
  :ensure t)

(add-to-list 'load-path (expand-file-name "evil" user-emacs-directory))

(require 'evil)

(evil-mode 1)


;; evil-surround

;; load evil-surround before others in order to avoid conflicts

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (add-hook 'emacs-lisp-mode-hook
   (lambda ()
     (push '(?` . ("`" . "'"))
           evil-surround-pairs-alist))))

(setf evil-normal-state-cursor   'box
      evil-emacs-state-cursor    '(bar . 7)
      evil-insert-state-cursor   'bar
      evil-visual-state-cursor   'bar
      evil-motion-state-cursor   'bar
      evil-replace-state-cursor  'hbar
      evil-operator-state-cursor 'hbar)

(setf evil-cross-lines t)
(setf evil-emacs-state-modes (delq 'pdf-view-mode evil-emacs-state-modes))
(add-to-list 'evil-emacs-state-modes 'durand-greek-search-mode)
(setf evil-search-module 'evil-search)

(setf evil-overriding-maps (delete '(grep-mode-map) evil-overriding-maps))

(evil-set-initial-state 'dired-mode 'motion)

(define-key dired-mode-map [?\d] nil)
(define-key dired-mode-map [32] nil)
(define-key dired-mode-map [?x] nil)
(ignore-errors
  (define-key grep-mode-map [?x] nil)
  (define-key grep-mode-map [32] nil))

;; (setq evil-normal-state-tag   (propertize " NORMAL " 'face '((:foreground "cornflowerblue" :background "gray20")))
;;       evil-emacs-state-tag    (propertize " EMACS " 'face '((:foreground "#39bf4c" :background "gray20")))
;;       evil-insert-state-tag   (propertize " INSERT " 'face '((:foreground "green" :background "gray20")))
;;       evil-replace-state-tag  (propertize " REPLACE " 'face '((:foreground "red" :background "gray20")))
;;       evil-motion-state-tag   (propertize " MOTION " 'face '((:foreground "orange" :background "gray20")))
;;       evil-visual-state-tag   (propertize " VISUAL " 'face '((:foreground "goldenrod1" :background "gray20")))
;;       evil-operator-state-tag (propertize " OPERATOR " 'face '((:foreground "pink" :background "gray20"))))

(setq evil-normal-state-tag   (propertize " NORMAL " 'face '((:foreground "gray20" :background "cornflowerblue")))
      evil-emacs-state-tag    (propertize " EMACS " 'face '((:foreground "gray20" :background "#39bf4c")))
      evil-insert-state-tag   (propertize " INSERT " 'face '((:foreground "gray20" :background "green")))
      evil-replace-state-tag  (propertize " REPLACE " 'face '((:foreground "gray20" :background "red")))
      evil-motion-state-tag   (propertize " MOTION " 'face '((:foreground "gray20" :background "orange")))
      evil-visual-state-tag   (propertize " VISUAL " 'face '((:foreground "gray20" :background "goldenrod1")))
      evil-operator-state-tag (propertize " OPERATOR " 'face '((:foreground "gray20" :background "pink"))))

(evil-define-key nil evil-insert-state-map
  [home] 'evil-normal-state)

(evil-define-key nil evil-visual-state-map
  [home] 'evil-exit-visual-state)

(evil-define-key nil evil-replace-state-map
  [home] 'evil-normal-state)

(evil-define-key nil evil-emacs-state-map
  [home] 'evil-normal-state)


;;;###autoload
(define-prefix-command 'evil-projectile-map)

(define-key evil-projectile-map [?f] 'projectile-find-file)
(define-key evil-projectile-map [?p] 'projectile-switch-project)


;; ret
(define-prefix-command 'durand-evil-ret-map)

(evil-define-key nil durand-evil-ret-map
  "\"" 'transpose-chars-back-4
  [?j] 'jump-to-other-window-link
  [?P] 'evil-backward-sentence-begin
  [?p] 'evil-forward-sentence-begin
  [?s] 'durand-open-browser
  [?,] 'evil-window-top
  [?\r] 'durand-open-link
  [?\;] 'evil-window-middle
  [?:] 'evil-window-bottom
  [?o] 'evil-jump-backward
  [?a] (lambda ()
         (interactive)
         (org-agenda nil "o"))
  [?e] 'durand-eval
  [?t] (lambda ()
         (interactive)
         (if current-prefix-arg
             (make-process
              :name "terminal"
              :command '("open" "-a" "terminal")
              :buffer nil)
           (eshell)))
  [?x] 'exchange-point-and-mark
  [?w] 'durand-next-window-or-frame
  [?W] 'ace-select-window
  [?u] 'undo-tree-visualize)

;; spc ret
(define-prefix-command 'durand-evil-spc-ret-map)

(evil-define-key nil durand-evil-spc-ret-map
  [?g] 'revert-buffer
  [?b] 'org-open-bookmarks
  [?B] 'describe-bindings
  [?f] 'counsel-describe-function
  [?k] 'describe-key
  [?v] 'counsel-describe-variable
  [?l] 'org-open-articles
  [?n] 'org-open-novels
  [?y] 'org-open-youtube
  [?d] 'delete-rectangle
  [?c] (lambda ()
         (interactive)
         (cond (current-prefix-arg (durand-capture))
               (t (org-capture))))
  [?s] 'durand-start-counting
  [?r] 'string-rectangle)

;;;###autoload
(defun durand-start-counting (&optional arg)
  "With ARG, ask for the number of minutes."
  (interactive "P")
  (cond
   ((null durand-stop-timer)
    (setf durand-stop-timer
          (run-with-timer
           (* (cond
               ((null arg) 60)
               (t
                (read-number "Quels minutes?" 60)))
              60)
           nil
           'durand-stop-reminder))
    (list-timers))
   ((timerp durand-stop-timer)
    (cancel-timer durand-stop-timer)
    (setf durand-stop-timer nil))
   (t
    (user-error "Unknown situation"))))

;; space
(define-prefix-command 'durand-evil-space-map)

(evil-define-key nil durand-evil-space-map
  [?x] ctl-x-map
  [?n] 'evil-ex-nohighlight
  [?=] 'align-regexp
  [?m] 'durand-mu4e
  [?M] 'mu4e
  [?%] 'widen
  [?u] (lambda ()
         (interactive)
         (if current-prefix-arg (org-store-link nil t)
           (mu4e-update-mail-and-index nil)))
  [?é] 'split-window-below
  [?\"] 'split-window-right
  [?X] 'ace-swap-window
  [?o] 'durand-new-buffer
  [?O] 'make-blank-space
  [?c] 'clean-up-buffers
  [?C] 'clean-up-buffers-regex
  [?j] 'durand-bookmark-jump-headlong
  [?J] 'bookmark-set
  [?r] 'durand-recentf-jump-headlong
  [?g] 'backward-or-up-sexp
  [?h] 'forward-or-up-sexp
  [?H] help-map
  [?s] 'durand-cap-sentence
  [?-] 'negative-argument
  [?p] evil-projectile-map
  [?\r] durand-evil-spc-ret-map
  [?b] 'durand-switch-buffer
  [?k] 'durand-kill-buffer
  [?l] 'kill-other-buffer-window
  [?f] 'counsel-find-file
  ;; [?'] 'durand-edit-special
  ;; try the newer function
  [?'] 'durand-narrow-dwim
  [?w] 'delete-other-windows
  [?W] 'delete-window
  [?q] 'quit-other-window
  [?z] 'capitalize-region-or-word
  [?t] 'avy-goto-char-timer
  [?:] 'evil-commenter
  ;; [?:] 'durand-comment-dwim
  [?,] 'evil-goto-first-line
  [?\;] 'evil-goto-line
  [?\s-x] (lambda ()
            (interactive)
            (setq display-line-numbers
                  (and (null display-line-numbers)
                       'relative)))
  [?e] 'durand-wrap-region-with)

;; $
(define-prefix-command 'durand-evil-dollar-map)

(evil-define-key nil durand-evil-dollar-map
  [?\d] 'back-to-indentation
  [?t] (lambda () (interactive) (recenter 0))
  [?z] (lambda () (interactive) (recenter (/ (window-body-height) 2)))
  [?b] (lambda () (interactive) (recenter -3))
  [?£] 'org-retreat)

;; motion map

(evil-define-key nil evil-motion-state-map
  [home] 'evil-normal-state
  [32] durand-evil-space-map
  [?\r] durand-evil-ret-map
  [?\d] 'durand-other-buffer
  ;; [?$] durand-evil-dollar-map
  [?$] 'evil-end-of-line
  [?i] 'evil-emacs-state
  [40] 'universal-argument
  [?x] 'amx
  [?\)] 'evil-forward-paragraph
  [?Q] 'quit-window)

;; normal mode

(evil-define-key nil evil-normal-state-map
  [home] 'evil-emacs-state
  [?x] 'amx
  [?X] (lambda ()
         (interactive)
         (pcase major-mode
           ('mu4e-headers-mode
            (mu4e-mark-execute-all t))
           (_
            (user-error "Not in mu4e-headers-mode"))))
  [backspace] 'durand-other-buffer
  [32] durand-evil-space-map
  [?\r] durand-evil-ret-map
  ;; [?\)] 'durand-end-of-line-or-block
  [?\)] 'evil-forward-paragraph
  [?s] 'durand-general-save-buffer
  [f10] 'durand-general-save-buffer
  [f9] (lambda () (interactive) (when (functionp durand-tex-action) (funcall durand-tex-action)))
  [?U] 'undo-tree-redo
  [?t] 'transpose-chars
  [?T] 'durand-show-current-time
  "&" 'transpose-chars-back-2
  "é" 'transpose-chars-back-3
  [?\"] 'evil-use-register
  [?'] 'fill-paragraph
  [?£] 'org-advance
  ;; [?$] durand-evil-dollar-map
  [?$] 'evil-end-of-line
  [?i] 'evil-insert-state
  [?j] 'evil-next-visual-line
  [?k] 'evil-previous-visual-line
  [?+] 'eval-expression
  "," 'evil-repeat-find-char-reverse
  ";" 'evil-repeat-find-char
  ;; [?,] (lambda () (interactive) (durand-buffer-scroll 'up))
  ;; [?\;] (lambda () (interactive) (durand-buffer-scroll 'down))
  [??] (lambda () (interactive) (durand-buffer-scroll 'up 1))
  [32 ?.] (lambda () (interactive) (durand-buffer-scroll 'down 1)) ; I like the repeat operator after all.
  [?.] 'evil-repeat
  [?§] (lambda () (interactive) (durand-buffer-scroll 'up nil t))
  [?è] (lambda () (interactive) (durand-buffer-scroll 'down nil t))
  [?!] (lambda () (interactive) (durand-buffer-scroll 'up 1 t))
  [?ç] (lambda () (interactive) (durand-buffer-scroll 'down 1 t))
  [?Q] 'quit-window
  [?p] 'evil-paste-after
  [?\s-m] 'durand-toggle-mode-line
  [?ù] 'evil-goto-mark
  [?S] 'cycle-spacing
  ;; [?z] 'downcase-region-or-word
  ;; [?Z] 'upcase-region-or-word
  [?z] durand-evil-dollar-map
  (kbd "C-SPC") 'set-mark-command
  [?=] 'evil-ex-search-forward
  [?/] 'counsel-grep-or-swiper
  [?<] 'er/expand-region
  [?>] 'er/contract-region
  (kbd "<S-backspace>") 'durand-next-real-buffer
  (kbd "SPC <S-backspace>") 'durand-previous-real-buffer
  [?-] (lambda () (interactive) (durand-buffer-scroll 'up))
  [?_] (lambda () (interactive) (durand-buffer-scroll 'down))
  [?\C--] (lambda () (interactive) (durand-buffer-scroll 'up 1))
  [?\C-_] (lambda () (interactive) (durand-buffer-scroll 'down 1))
  [?\t] (lambda ()
          "org-cycle or indent-for-tab-command"
          (interactive)
          (cond
           ((derived-mode-p 'org-mode)
            (org-cycle current-prefix-arg))
           (t
            (indent-for-tab-command current-prefix-arg))))
  [?J] 'evil-join
  [?\M-o] 'evil-jump-forward
  [?\C-a] 'evil-numbers/inc-at-pt
  [?\C-x] 'evil-numbers/dec-at-pt
  [?\(] 'universal-argument
  [?\à] 'durand-beginning-of-line-or-block)

;; universal argument mode
(define-key universal-argument-map [?\(] 'universal-argument-more)

(defun universal-argument--description ()
  (when prefix-arg
    (concat "("
            (pcase prefix-arg
              (`(-) " -")
              (`(,(and (pred integerp) n))
               (let ((str ""))
                 (while (and (> n 4) (= (mod n 4) 0))
                   (setq str (concat str " ("))
                   (setq n (/ n 4)))
                 (if (= n 4) str (format " %s" prefix-arg))))
              (_ (format " %s" prefix-arg))))))


;; end of visual line

(defun evil-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current visual line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (evil-end-of-visual-line)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-visual-line
                   vcount)))
  (evil-insert-state 1))

;; text objects

;; From https://stackoverflow.com/questions/18102004/

(defmacro define-and-bind-text-object (name key start-regex end-regex)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(defmacro define-and-bind-quote-text-object (name key quote-char)
  (let ((inner-name (make-symbol (concat "evil-inner-quote-" name)))
        (outer-name (make-symbol (concat "evil-outer-quote-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-quote ,quote-char beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-quote ,quote-char beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(define-and-bind-quote-text-object "dollar" "$" ?$)
(define-and-bind-text-object "slash" "/" "/" "/")
(define-and-bind-text-object "pipe" "|" "|" "|")
(define-and-bind-text-object "star" "*" "*" "*")
(define-and-bind-text-object "frenchquote" "«" "«" "»")
(define-and-bind-text-object "tikz" "z" "\\(\\\\bpi\\|\\\\tikzpicture\\).*$" "\\(\\\\epi\\|\\\\endtikzpicture\\)")
(define-and-bind-text-object "environment" "e" "\\\\begin{[^{}]+}$" "\\\\end{[^{}]+}")

(use-package org-noter
  :ensure t
  :config
  (setf org-noter-notes-search-path '("~/org/mes-notes")
        org-noter-notes-window-location 'vertical-split
        org-noter-auto-save-last-location t
        org-noter-doc-split-fraction '(0.5 . 0.65)))

(defun durand-narrow-dwim (arg)
  "Widen when narrowed, unless ARG is non-nil.
Quit org-edit-src, unless ARG is non-nil.
When region is active, narrow to that region.
In org-mode, if ARG is '(16), then execute `org-edit-special';
else try `org-edit-src-code', `org-narrow-to-block',
`org-narrow-to-subtree', and `org-edit-special' in this order.
Otherwise execute `narrow-to-defun'."
  (interactive "P")
  (cond
   ((and (buffer-narrowed-p) (not arg)) (widen))
   ((and (string-prefix-p "*Org Src" (buffer-name))
         (not arg))
    (org-edit-src-exit))
   ((region-active-p)
    (narrow-to-region (region-beginning) (region-end)))
   ((derived-mode-p 'org-mode)
    (cond
     ((equal arg '(16))
      (let ((current-prefix-arg nil))
        (ignore-errors (org-edit-special nil))))
     ((ignore-errors (org-edit-src-code) t)
      (delete-other-windows))
     ((ignore-errors (org-narrow-to-block) t))
     ((ignore-errors (org-narrow-to-subtree) t))
     ((let ((current-prefix-arg nil))
        (ignore-errors (org-edit-special nil) t)))
     (t (message "No pre-defined behaviour."))))
   (t
    (narrow-to-defun))))

;; (define-key ctl-x-map [?n] 'durand-narrow-dwim)

(use-package ledger-mode
  :ensure t
  :init
  (setf ledger-clear-whole-transactions 1
        ledger-complete-in-steps t)
  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode))

(use-package evil-ledger
  :ensure t
  :after ledger-mode
  :config
  (evil-define-key* 'visual evil-ledger-mode-map [?S] #'evil-ledger-sort)
  (add-hook 'ledger-mode-hook #'evil-ledger-mode)
  (add-hook 'ledger-mode-hook (lambda () (setq-local pcomplete-termination-string ""))))

;; (ivy-read "HI: " '("ffa" "ffb" "ffba" "ffaa")
;; 	  :unwind (lambda () (setq durand-changed nil)))

;; (setq ivy-display-function (lambda (str) (save-excursion
;; 					   (forward-line 1)
;; 					   (insert str))))
;; (setq ivy-display-function nil)

;; (setq durand-changed nil)

;; (defun durand-ivy-update-fn ()
;;   "test"
;;   (interactive)
;;   (setf (ivy-state-collection ivy-last)
;; 	(durand-ivy-cycle-collection
;; 	 ivy--index))
;;   (ivy--reset-state ivy-last)
;;   (ivy-beginning-of-buffer)
;;   (when (not durand-changed)
;;     (setq durand-changed t)))

;; (defun durand-ivy-update-fn ()
;;   "test"
;;   (interactive)
;;   (cond
;;    ((eq this-command 'ivy-next-line)
;;     (setf (ivy-state-collection ivy-last)
;; 	  (durand-ivy-cycle-collection 0)))
;;    ((eq this-command 'ivy-previous-line)
;;     (setf (ivy-state-collection ivy-last)
;; 	  (durand-ivy-cycle-collection -2)))
;;    (t
;;     nil))
;;   (when durand-changed
;;     (ivy--reset-state ivy-last))
;;   (when (not durand-changed)
;;     (setq durand-changed t)))

;; (defun durand-ivy-cycle-collection (arg)
;;   (let* ((arg (or arg 1))
;; 	 (col (ivy-state-collection ivy-last))
;; 	 (len (length col))
;; 	 (off-set (if durand-changed 0 1))
;; 	 new-li)
;;     (dotimes (ind len new-li)
;;       (push
;;        (nth (mod (- len ind off-set arg) len) col)
;;        new-li))))

;; (require 'midnight)
;; (add-to-list 'clean-buffer-list-kill-never-regexps "elfeed")
;; (add-to-list 'clean-buffer-list-kill-never-regexps "mu4e")
;; (add-to-list 'clean-buffer-list-kill-never-regexps "\\.tex")
;; (add-to-list 'clean-buffer-list-kill-never-buffer-names "setting.org")
;; (add-to-list 'clean-buffer-list-kill-never-buffer-names "*server*")
;; (add-to-list 'clean-buffer-list-kill-buffer-names "*Calculator*")
;; (setq clean-buffer-list-delay-general 0.04)

;; (use-package js2-mode
;;   :ensure t
;;   :config
;;   (require 'js2-mode)
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;;   (define-key js2-mode-map (vector ?\M-.) nil)
;;   (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;;   (require 'js2-refactor)
;;   (add-hook 'js2-mode-hook #'js2-refactor-mode)
;;   (js2r-add-keybindings-with-prefix "C-c C-r"))

;; (use-package js2-refactor
;;   :ensure t)

;; (use-package xref-js2
;;   :ensure t
;;   :config
;;   (require 'js2-mode)
;;   (require 'xref-js2)
;;   (add-hook 'js2-mode-hook
;;             (lambda ()
;;               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

;; (use-package emmet-mode
;;   :ensure t
;;   :config
;;   (add-hook 'mhtml-mode-hook 'emmet-mode)
;;   (add-hook 'css-mode-hook 'emmet-mode))

;; (use-package frog-jump-buffer
;;   :ensure t
;;   :init
;;   (setf frog-jump-buffer-max-buffers 20))
