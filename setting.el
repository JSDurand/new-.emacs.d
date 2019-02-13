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
  (let ((inhibit-message t))
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

;; (use-package company
;;   :ensure t
;;   :defer 10
;;   :config
;;   (global-company-mode -1)
;;   ;; (global-set-key [tab] 'company-complete)
;;   (global-set-key [tab] 'indent-for-tab-command)
;;   (setq company-require-match nil)
;;   (setq company-tooltip-align-annotations t)
;;   (company-flx-mode 1)
;;   (setq company-flx-limit 200)		; flx can be slow
;;   (define-key company-active-map [?\C-n] 'company-select-next) ; just use c-n/p to select
;;   (define-key company-active-map [?\C-p] 'company-select-previous-or-abort)
;;   (add-to-list 'completion-styles 'initials) ; initials completion style is handy.
;;   ;; Do not activate company mode in emacs lisp mode as it causes some crashes in the past!
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (interactive) (company-mode -1)))
;;   (setq company-show-numbers t))

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
    (setq projectile-completion-system 'ivy)
    (define-key projectile-mode-map [?\s-d] 'projectile-command-map)))

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
(set-face-attribute 'mode-line-buffer-id nil :background "gray10" :foreground "DarkOrange1")
;; (set-face-font 'mode-line-buffer-id "DejaVu Sans Mono for Powerline")
;; (set-face-attribute 'mode-line-buffer-id nil :height 1.1)
(set-face-attribute 'mode-line-highlight nil :box nil :background "deep sky blue")
(set-face-attribute 'mode-line-inactive  nil :background "gray10" :foreground "gray50")

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

(defun my-position ()
  "My function of mode-line-position"
  (cond
   ((string= major-mode "pdf-view-mode")
    mode-line-position)
   ((string= major-mode "org-agenda-mode")
    (org-agenda-show-blocks-number))))

(defun propertized-buffer-identification (fmt)
  "Return a list suitable for `mode-line-buffer-identification'.
FMT is a format specifier such as \"%12b\".  This function adds
text properties for face, help-echo, and local-map to it."
  (list (propertize fmt
                    'face 'mode-line-buffer-id
                    'help-echo
                    (purecopy "Nom du tampon
souris-1: Dernier tampon\nsouris-3: Prochain tampon")
                    'mouse-face 'mode-line-highlight
                    'local-map mode-line-buffer-identification-keymap)))

(setq-default mode-line-buffer-identification
              (propertized-buffer-identification " %b "))

(defun my-mode-line-modified ()
  (propertize
   (concat
    (if (buffer-modified-p)
        (propertize "M " 'face '(:foreground "DarkGoldenrod2"))
      " ")
    (if (not (buffer-file-name))
        (propertize "N " 'face '(:foreground "LightSkyBlue3"))
      " ")
    (if buffer-read-only
        (propertize "R " 'face '(:foreground "IndianRed1"))
      " "))
   'help-echo "M: modifié 
N: C'est peut-être pas un fichier
R: seulement pour lire"))

(defvar durand-custom-modeline ""
  "A custom variable to set for customisation")

(defface durand-mode-line-client-face '((t . (:foreground "CadetBlue2")))
  "Face for mode line client construct")

(defun durand-mode-line-buffer-name ()
  "trimmed buffer name"
  (let ((orig (format-mode-line (propertized-buffer-identification "%b")))
        (max 35))
    (if (> (length orig) max)
        (truncate-string-to-width orig max nil nil t)
      orig)))

(defun durand-mode-line-client ()
  "Custom mode line client construct"
  (propertize
   (if (frame-parameter nil 'client)
       "@" "")
   'face 'durand-mode-line-client-face
   'help-echo "un client d'emacs"))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;; mode-line-mule-info -- I'm always on utf-8
                (:eval (durand-mode-line-client))
                (:eval (my-mode-line-modified))
                ;; mode-line-remote -- no need to indicate this specially
                ;; mode-line-frame-identification -- this is for text-mode emacs only
                (:eval (propertize durand-custom-modeline 'face 'durand-custom-mode-face))
                " "
                (:eval (durand-mode-line-buffer-name))
                " "
                ;; mode-line-position
                (:eval (my-position))
                ;;(vc-mode vc-mode)  -- I use magit
                ;; (flycheck-mode flycheck-mode-line) -- I don't have this
                " %[ "
                mode-name
                " %] "
                ;; Only major mode
                " %n "
                mode-line-misc-info
                mode-line-end-spaces))
                ;; mode-line-modes -- I don't want all those minor modes information
                ;; " %I "


(setq durand-default-mode-line-format mode-line-format)

(defvar durand-mode-line-toggled nil
  "Determine if mode line is toggled")

(make-variable-buffer-local 'durand-mode-line-toggled)

(defun durand-toggle-mode-line (&optional arg)
  "
If ARG is nil, then toggle the mode-line.
If ARG is a positive integer, then set the mode-line-format to the default one.
If ARG is a negative integer, then set the mode-line-format to NIL."
  (interactive "P")
  (pcase arg
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
    ((and (pred integerp)
          (pred (lambda (num) (<= num 0))))
     (setq mode-line-format nil)
     (force-mode-line-update))
    (_
     (message "ARG should be either NIL, or an integer, but got %s" arg))))

(global-set-key [?\s-m] 'durand-toggle-mode-line)

(add-hook 'pdf-view-mode-hook (lambda () (durand-toggle-mode-line -1)))

(set-face-attribute 'mode-line nil
                    :background "gray10" :foreground "white" :height 1.3)

;; (defface durand-custom-mode-face '((t (:foreground "red" :inherit mode-line)))
;;   "Face used for displaying hydra presence")
(defface durand-custom-mode-face '((t (:foreground "red")))
  "Face used for displaying hydra presence")

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
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "s-s") 'isearch-forward)
  (define-key pdf-view-mode-map [?j] (lambda () (interactive) (pdf-view-scroll-up-or-next-page 1)))
  (define-key pdf-view-mode-map [?k] (lambda () (interactive) (pdf-view-scroll-down-or-previous-page 1))))

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
        ("buffer" "β")
        ("dired" "δ")
        ("messages" "M")
        ("fundamental" "φ")
        ("mu4e" "μ")
        ("main" "Μ")
        ("update" "υ")
        ("headers" "tête")
        ("help" "H")
        ("pdf" "Π")
        ("view" "v")
        ("durand" "Δ")
        ("interaction" "int")))
(cyphejor-mode 1)

;; (advice-remove 'mu4e-headers-search-bookmark  (lambda (&rest args) (cyphejor-mode 1)))
;; (advice-remove 'mu4e-headers-search  (lambda (&rest args) (cyphejor-mode 1)))
;; (advice-remove 'mu4e~headers-jump-to-maildir  (lambda (&rest args) (cyphejor-mode 1)))

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

(use-package js2-mode
  :ensure t
  :config
  (require 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (define-key js2-mode-map (vector ?\M-.) nil)
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (require 'js2-refactor)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package js2-refactor
  :ensure t)

(use-package xref-js2
  :ensure t
  :config
  (require 'js2-mode)
  (require 'xref-js2)
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'mhtml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))
