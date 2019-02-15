;; Custom miscellaneous settings

(fset 'yes-or-no-p 'y-or-n-p)
(setf large-file-warning-threshold nil)
(setf default-directory "~/.emacs.d/")
(setq enable-recursive-minibuffers t)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq words-include-escapes nil)
(setq sentence-end-double-space nil)
(put 'dired-find-alternate-file 'disabled nil)
(setq-default dired-listing-switches "-lah")
(setq save-interprogram-paste-before-kill t)
(temp-buffer-resize-mode 1)
(setq temp-buffer-max-height (lambda (buffer)
                               (round
                                (if (and (display-graphic-p)
                                         (eq (selected-window)
                                             (frame-root-window)))
                                    (* (/ (x-display-pixel-height) (frame-char-height) 12.0) 5)
                                  (* 5 (/ (- (frame-height) 2) 12.0))))))
;; By default we are in insert hydra.
(set-frame-parameter nil 'cursor-type 'bar)
(toggle-truncate-lines -1)
(recentf-mode 1)
(auto-fill-mode 1)
(add-to-list 'recentf-exclude "alias")
(add-to-list 'recentf-exclude "bookmark")
(set-frame-parameter nil 'alpha '(90 . 90))
(define-key global-map [?\s-q] nil)
(define-key global-map [?\s-g] (lambda () (interactive) (message "Spécifique")))
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; make the frame semi-fullscreen
;; (add-hook 'window-setup-hook 'toggle-frame-fullscreen)

(ignore-errors (set-face-foreground 'tex-math "DarkGoldenrod3"))

;; some common abbreviations
(define-abbrev global-abbrev-table "adn" "and")
(define-abbrev global-abbrev-table "tihs" "this")
(define-abbrev global-abbrev-table "si" "is")

(global-set-key [?\C-c ?s ?m] (lambda () (interactive) (and (or current-input-method
								(progn
								  (set-input-method "chinese-zozy")
								  nil))
							    (deactivate-input-method))))
(global-set-key (kbd "C-c h") 'beginning-of-buffer)
(global-set-key (kbd "C-c g") 'end-of-buffer)
(global-set-key (kbd "C-c j") 'delete-indentation)
(global-set-key [?\C-\M-d] 'down-list)
(global-set-key [?\C-\M-j] 'ispell-complete-word)
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-b") 'backward-word)
(global-set-key (kbd "M-F") 'forward-word)
(global-set-key (kbd "s-(") 'backward-paragraph)
(global-set-key (kbd "s-)") 'forward-paragraph)
(global-set-key (kbd "<f10>") 'save-buffer)
(global-set-key (kbd "s-b") '(lambda ()
			       "switch to the most recent buffer"
			       (interactive)
			       (switch-to-buffer (other-buffer))))
(global-set-key (kbd "s-B") 'switch-to-buffer)
(global-set-key (kbd "s-k") '(lambda ()
			       "kill recent buffer"
			       (interactive)
			       (kill-buffer (current-buffer))))
(global-set-key (kbd "s-K") 'kill-buffer)
(global-set-key [?\s-j] '(lambda ()
			   "join line backwards, as I am more used to that
				   behaviour."
			   (interactive)
			   (join-line -1)))
(global-set-key [?\M-&] 'query-replace-regexp)
(global-set-key [f5] (lambda ()
		       "Show the file size in the echo area."
		       (interactive) (message (file-size-human-readable (buffer-size)))))
(global-set-key [?\H-e] 'eshell)
(global-set-key [?\H-v] 'view-mode)

(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(define-key global-map [?\s-t] 'durand-show-current-time)
(define-key global-map [?\H-t] 'transpose-chars-back-2)
(define-key global-map [?\H-\C-t] 'transpose-chars-back-3)
(define-key global-map [?\C-\H-\M-t] 'transpose-chars-back-4)
(define-key global-map [?\H-\M-t] 'transpose-chars-back-N)

(define-key global-map [?\C-c tab] 'durand-forward-link)
(define-key global-map [?\C-c S-tab] 'durand-backward-link)

(define-key global-map [?\C-c ?J] 'jump-to-other-window-link)
(define-key emacs-lisp-mode-map [?\C-c tab] 'elisp-jump-to-definition)

;;;###autoload
(defun durand-show-current-time ()
  "Show the current time. With prefix arg, show in a separate window."
  (interactive)
  (let ((time-string (format-time-string "%A %e %B %H:%M:%S")))
    (if current-prefix-arg
        (with-temp-buffer-window
         "*current time*" nil nil
         (prin1 time-string))
      (message time-string))))

;; Record the link types that I know until now.
(defvar durand-link-types '(shr-url htmlize-link button mu4e-url)
  "Link types that I know until now.")

(put (intern "durand-forward-link") 'function-documentation (concat
                                                             "Forward to "
                                                             (mapconcat #'prin1-to-string
                                                                        (reverse (cdr (reverse durand-link-types)))
                                                                        ", ")
                                                             (format ", or %s" (-last #'identity durand-link-types))
                                                             " changes."))

;; Find the next link
;;;###autoload
(defun durand-find-next-link (&optional pos func)
  "Find the next link. POS defaults to `(point)' and FUNC defaults to `next-single-property-change'."
  (let ((pos (or pos (point)))
        (func (or func 'next-single-property-change))
        res)
    (dolist (le_type durand-link-types res)
      (setq res (or res (funcall func pos le_type))))))

;;;###autoload
(defun durand-find-previous-link (&optional pos func)
  "Find the previous link. POS defaults to `(point)' and FUNC defaults to `previous-single-property-change'."
  (let ((pos (or pos (point)))
        (func (or func 'previous-single-property-change))
        res)
    (dolist (le_type durand-link-types res)
      (setq res (or res (funcall func pos le_type))))))

;;;###autoload
(defun durand-forward-link ()
  (interactive)
  (let* ((next-change (durand-find-next-link))
         (next-url (when next-change (durand-find-next-link next-change 'get-text-property)))
         (final-change (if next-url next-change (durand-find-next-link next-change))))
    (if final-change
        (goto-char final-change)
      (message "No links found!"))))

;;;###autoload
(defun durand-backward-link ()
  (interactive)
  (let* ((next-change (durand-find-previous-link))
         (next-url (when next-change (durand-find-previous-link next-change 'get-text-property)))
         (final-change (if next-url next-change (durand-find-previous-link next-change))))
    (if final-change
        (goto-char final-change)
      (message "No links found!"))))

;;;###autoload
(defun elisp-jump-to-definition ()
  "Jump to the definition of the sexp at point. If the sexp
contains space, do nothing but message."
  (interactive)
  (let ((sexp (thing-at-point 'sexp t)))
    (if (and sexp
             (stringp sexp)
             (not (s-contains-p " " sexp)))
        (progn
          (setf sexp
                (if (string-match "'" sexp)
                    (replace-match "" nil nil sexp)
                  sexp))
          (swiper (concat "defun " sexp)))
      (message "invalid position"))))

;; (require 'comint)
;; (define-key comint-mode-map (vector ?q) 'bury-buffer)
;; I ended up with `set-process-sentinel'.

;; ptt
;;;###autoload
(defun ptt ()
  "PTT"
  (interactive)
  (ansi-term "/usr/local/bin/zsh")
  (term-line-mode)
  (insert "luit -encoding big5 telnet ptt.cc")
  (term-send-input)
  (term-char-mode)
  (message "Enter JSDurand and enter, and then press `C-c l' and enter again."))

(defun ptt-send-account-and-passwd ()
  "account and passwd"
  (interactive)
  ;; (term-line-mode)
  ;; (insert "JSDurand")
  ;; (term-char-mode)
  ;; (term-send-raw)
  (term-line-mode)
  (insert "ngFwyveyccyyu4u")
  (term-char-mode)
  (term-send-raw))

;;(define-key term-raw-map (vector ?\C-c ?l) 'ptt-send-account-and-passwd)

;; sign in
;;;###autoload
(defun durand-sign-sentinel (proc state)
  "Change to special mode if state is finished"
  (message state)
  (when (string-match "finished" state)
    (make-process
     :name "view"
     :command '("open" "/Users/durand/Desktop/Centre/Python/Hard Worker.png")
     :buffer nil)
    (kill-buffer (process-buffer proc))))

;;;###autoload
(defun durand-sign ()
  "Sign in or out or just check"
  (interactive)
  (let* ((buf-name "*sign*")
         (default-directory "/Users/durand/Desktop/Centre/Python")
         (buf (make-comint-in-buffer
               "sign"
               buf-name
               "python3"
               nil
               "/Users/durand/Desktop/Centre/Python/automatic_sign_in.py")))
    (switch-to-buffer buf)
    (set-process-sentinel (get-buffer-process buf-name) 'durand-sign-sentinel)))

;;;###autoload
(defun jump-to-other-window-link ()
  "Jump to the first link after point on the other window,
or the current window if there is only one window."
  (interactive)
  (other-window 1)
  (forward-button 1)
  (ignore-errors (push-button))
  (delete-other-windows)
  (recenter 0))

;;;###autoload
(defun bury-other-buffer ()
  "Bury the other buffer"
  (interactive)
  (save-selected-window
    (other-window 1)
    (bury-buffer)))

;;;###autoload
(defun kill-other-buffer-window (&optional arg)
  "Kill the other buffer and delete other windows.
If ARG is non-nil, then don't delete other windows"
  (interactive "P")
  (save-selected-window
    (other-window 1)
    (kill-buffer))
  (unless arg
    (delete-other-windows)))

(define-key global-map [?\C-c ?b] 'bury-other-buffer)
(define-key global-map [?\C-c ?k] 'kill-other-buffer-window)

;;;###autoload
(defun window-half-height ()
  "Return half the height of the selected window"
  (max 1 (/ (1- (window-height (selected-window))) 2)))

;;;###autoload
;; (defun transpose-chars-back-one ()
;;   "Go back one char and transpose characters"
;;   (interactive)
;;   (save-excursion
;;     (backward-char 1)
;;     (transpose-chars 1)))

;;;###autoload
(defun transpose-chars-back-N (n)
  "Transpose chars back N if possible"
  (interactive "N")
  (if (< (- (point) (point-min)) n)
      (user-error "No character at %d before the point" n)
    (save-excursion
      (transpose-regions (- (point) n) (- (point) n -1)
			 (- (point) n -1) (- (point) n -2)))))

;;;###autoload
(defun transpose-chars-back-2 ()
  "Transpose back 2 chars"
  (interactive)
  (transpose-chars-back-N 2))

;;;###autoload
(defun transpose-chars-back-3 ()
  "Go back two characters and transpose charcaters"
  (interactive)
  (transpose-chars-back-N 3))

;;;###autoload
(defun transpose-chars-back-4 ()
  "Go back three characters and transpose charcaters"
  (interactive)
  (transpose-chars-back-N 4))

;;;###autoload
(defun open-line-below ()
  "o in vim"
  (interactive)
  (progn
    (end-of-line)
    (open-line 1)
    (forward-line)
    (indent-according-to-mode)))
(global-set-key [?\C-o] 'open-line-below)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; wdired mode binding
;; (define-key dired-mode-map (vector ?, ?w) 'wdired-change-to-wdired-mode)
;; (define-key wdired-mode-map (vector ?, ?w) 'wdired-change-to-dired-mode) this is not needed.

(setq make-backup-files nil)
;;;###autoload
(defun eval-rep ()
  "my eval replace"
  (interactive)
  (kill-sexp -1)
  (insert (format "%S" (eval (read (current-kill 0))))))
(global-set-key [?\M-\s-ê] 'eval-rep)
(add-hook 'lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'lisp-interaction-mode-hook 'show-paren-mode)
;; (global-set-key [?\C-c ?v] 'view-mode)
;; (add-hook 'doc-view-mode-hook 'auto-revert-mode)
(global-set-key [?\C-x ?r ?s] 'bookmark-save)
(global-set-key [?\M-Z] 'zap-up-to-char)
(setq flyspell-issue-message-flag nil)

;;;###autoload
(defun cycle-spacing-advice (&rest args)
  "Deletes newlines as well!"
  (interactive)
  (cond
   ((and args (consp args))
    (append (list (- (prefix-numeric-value current-prefix-arg))) (cdr args)))
   (t
    args)))

(advice-add 'cycle-spacing :filter-args 'cycle-spacing-advice)

(define-key global-map (kbd "M-SPC") 'cycle-spacing)

;;;###autoload
(defun ask-before-quit-advice (orig-func &rest args)
  (if (not (frame-parameter nil 'client))
      (let ((answer (y-or-n-p "Do you really want to quit? ")))
	(if answer
	    (apply orig-func args)
	  (message "You're welcomed!")))
    (apply orig-func args)))

(advice-add 'save-buffers-kill-terminal :around 'ask-before-quit-advice)

;;;###autoload
(defun ask-before-send-message-advice (orig-func &rest args)
  "Confirm if the user really wants to send the message"
  (let ((answer (y-or-n-p "Do you really want to send the mail? ")))
    (if answer
	(apply orig-func args)
      (message "You're welcomed!"))))
(advice-add 'message-send-and-exit :around 'ask-before-send-message-advice)

(setq initial-frame-alist '((width . 118)
			    (alpha (90 . 90))))
(set-frame-width nil 118)
(add-to-list 'default-frame-alist '(width . 118))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline 20"))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (setq ns-auto-hide-menu-bar t)
(setq frame-resize-pixelwise t)
;; (add-hook 'window-setup-hook (lambda ()
;; 			       (setq ns-auto-hide-menu-bar t)
;; 			       (set-frame-position nil 0 -24)
;; 			       (set-frame-size nil (display-pixel-width) (display-pixel-height) t)))
(set-face-background 'header-line "gray10")
(setq revert-without-query '(".*"))
(global-set-key [?\C-*] 'clean-up-buffers)
;;;###autoload
(defun clean-up-buffers ()
  "
Clean up some buffers that I oft do not need to keep around;
If the buffer has a running process, then do not kill it."
  (interactive)
  (cl-loop for buffer being the buffers
	   do (and (is-not-needed-buffer buffer)
		   (kill-buffer (buffer-name buffer)))))

;;;###autoload
(defun clean-up-reentf-list (&optional regex)
  "Clean up recentf-list that I do not want to keep around based on REGEX"
  (interactive (let ((str (read-string "Enter regexp to filter out recentf-list: ")))
		 (list str)))
  (let ((to-delete (durand-filter-recent-list regex)))
    (when (get-buffer "*clear-recentf-list*")
      (progn
	(delete-windows-on "*clear-recentf-list*")
	(kill-buffer "*clear-recentf-list*")))
    (and to-delete
	 (dolist (del to-delete)
	   (durand-kill-from-recentf (file-name-nondirectory del))))))

;;;###autoload
(defun durand-filter-recent-list (regex)
  "Filter recentf-list based upon REGEX"
  (let (result)
    (dolist (recent recentf-list result)
      (when (string-match regex (file-name-nondirectory recent))
	(push recent result)))
    (when (progn
	    (with-temp-buffer-window
	     "*clear-recentf-list*" nil nil
	     (prin1 (mapconcat #'file-name-nondirectory result "\n")))
	    (y-or-n-p "Sure you want to delete these from recentf-list? "))
      result)))

;;;###autoload
(defun clean-up-buffers-regex (&optional regex)
  "Clean up buffers that I do not want to keep around based on REGEX"
  (interactive (let ((str (read-string "Enter regexp to filter out buffers: ")))
		 (list str)))
  (let ((to-delete (durand-filter-buffers regex)))
    (when (get-buffer "*clear-buffer-list*")
      (progn
	(delete-windows-on "*clear-buffer-list*")
	(kill-buffer "*clear-buffer-list*")))
    (and to-delete
	 (dolist (del to-delete)
	   (and (get-buffer del)
		(kill-buffer del))))))

;;;###autoload
(defun durand-filter-buffers (regex)
  "Filter recentf-list based upon REGEX"
  (let ((result (cl-loop for buffer being the buffers
		  if (string-match regex (buffer-name buffer))
		  collect (buffer-name buffer) into result
		  finally return result)))
    (when (progn
	    (with-temp-buffer-window
	     "*clear-buffer-list*" nil nil
	     (prin1 (mapconcat #'identity result "\n")))
	    (y-or-n-p "Sure you want to kill these buffers? "))
      result)))

;;;###autoload
(defun is-not-needed-buffer (buf)
  "Match some buffers I do not want to keep around"
  (let ((name (buffer-name buf)))
    (and
     (or (and (= ?* (aref name 0))
	      (not (string-match "^\\*scratch\\*$" name))
	      (not (string-match "^\\*Messages\\*$" name)))
	 (string-match "^magit" name))
     (null (get-buffer-process name)))))

(setq set-mark-command-repeat-pop t)

(define-key Info-mode-map [?\(] 'Info-backward-node)
(define-key Info-mode-map [?\)] 'Info-forward-node)
(require 'view)
(define-key view-mode-map [?j] 'View-scroll-line-forward)
(define-key view-mode-map [?k] 'View-scroll-line-backward)
(define-key view-mode-map [?l] 'scroll-left)
(define-key view-mode-map [?h] 'scroll-right)

;; org-agenda-ql by alphapapa
(add-to-list 'load-path "~/.emacs.d/my_packages/org-ql-master/")
(add-to-list 'load-path "~/.emacs.d/my_packages/org-sidebar-master/")

(use-package dash-functional
  :ensure t)

(require 'org-ql)
(require 'org-sidebar)			; a great package by alphapapa

;; (org-sidebar-ql '(and (regexp query)
;; 			(tags "maobaobao"))
;; 		  '("~/org/wiki.org"))

;; Custom sidebar functions.
;; (defun chercher-francais (query)
;;   "Rechercher dans la liste des mots français."
;;   (interactive (list (read-string "Rechercher: ")))
;;   (my-org-sidebar-ql `(and (regexp ,query)
;; 			   (tags "liste" "mots"))
;; 		     '("~/org/wiki.org")
;; 		     nil
;; 		     nil
;; 		     nil
;; 		     (format "Mot: %s" query)))

(defun chercher-français (query)
  "Rechercher un mot dans la liste des mots français dans le fichier wiki.org"
  (interactive (list (read-string "Question: ")))
  (let* ((route_du_fichier "~/org/français.org")
         (nom_du_fichier "français.org")
         (a_tuer (not (get-buffer nom_du_fichier)))
         (chose (mapconcat #'identity
                           (org-ql--query route_du_fichier `(and (regexp ,query) (tags "mots"))
                             :action (lambda ()
                                       (let ((element (cadr (org-element-headline-parser (line-end-position)))))
                                         (concat
                                          (plist-get element :raw-value)
                                          ": "
                                          (plist-get element :MEANING)))))
                           "\n")))
    (if (and a_tuer (get-buffer nom_du_fichier))
        (kill-buffer nom_du_fichier))
    (if (/= (length chose) 0)
        (with-current-buffer-window "*mots*" nil nil
                                    (insert chose))
      (define-word query 'wordreference))))

(define-key global-map (vector ?\C-c ?q) 'chercher-français)

;; Rechercher les fichiers pdf
(defvar pdf-dir-list '("/Users/durand/Downloads/" "/Users/durand/Desktop/Centre/Documents partout"
                       "/Users/durand/Desktop/Centre/Je veux lire/" "/Users/durand/Desktop/Centre/LaTeX temporaire/"
                       "/Users/durand/Desktop/Centre/MaoBaoBao/Autres/PDF/"
                       "/Users/durand/Desktop/Centre/MaoBaoBao/Mao Problems/"
                       "/Users/durand/Desktop/Centre/Mes notes/" "/Users/durand/Desktop/Centre/PDF/"
                       "/Users/durand/Desktop/Centre/Pour thèse/" "/Users/durand/Desktop/Centre/TeX/"
                       "/Users/durand/Desktop/Centre/Œuvres de professeur/"
                       "/Users/durand/Desktop/Centre/方便與智慧無二/")
  "La liste des dossiers où je peux chercher les fichiers pdf quand j'ai besoin.")

(defface durand-pdf-dir-face '((t :foreground "orange2"))
  "Face for directory in durand-pdf-mode")

(defface durand-pdf-nom-face '((t :foreground "SkyBlue1"))
  "Face for file name in durand-pdf-mode")

;;;###autoload
(defun durand-modify-pdf-buffer ()
  "Changer le tampon"
  (goto-char (point-min))
  (save-excursion
    (while (/= (point) (point-max))
      (put-text-property (point) (line-end-position) 'face 'durand-pdf-nom-face)
      (forward-line)))
  (let* ((chemin (buffer-substring-no-properties
                  (point) (line-end-position)))
         (dossier (file-name-directory chemin))
         (nom (file-name-nondirectory chemin))
         (inhibit-read-only t))
    (insert (propertize (concat dossier "\n")
                        'face 'durand-pdf-dir-face
                        'chemin dossier))
    (put-text-property (point) (line-end-position) 'display nom)
    (put-text-property (point) (line-end-position) 'chemin chemin)
    (save-excursion
      (while (/= (point) (point-max))
        (let* ((nouveau-chemin (buffer-substring-no-properties
                                (point) (line-end-position)))
               (nouveau-dossier (file-name-directory nouveau-chemin))
               (nouveau-nom (file-name-nondirectory nouveau-chemin)))
          (when (not (string-equal nouveau-dossier dossier))
            (setf dossier nouveau-dossier)
            (insert (propertize (concat "\n" nouveau-dossier "\n")
                                'face 'durand-pdf-dir-face
                                'chemin nouveau-dossier)))
          (put-text-property (point) (line-end-position) 'display nouveau-nom)
          (put-text-property (point) (line-end-position) 'chemin nouveau-chemin)
          (forward-line)))))

  (goto-char (point-min)))

(define-derived-mode durand-pdf-mode special-mode "Durand PDF"
  "chercher pdf"
  (face-remap-add-relative 'default '(:foreground "orange2"))
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (save-excursion
      (while (re-search-forward "\\([^p]..$\\|p[^d].$\\|pd[^f]$\\)" nil t)
        (delete-region (max (1- (line-beginning-position))
                            (point-min))
                       (line-end-position))))
    (durand-modify-pdf-buffer)))

(define-key durand-pdf-mode-map (vector ?n) 'forward-line)
(define-key durand-pdf-mode-map (vector ?N) 'durand-pdf-next-pdf-line)
(define-key durand-pdf-mode-map (vector ?p) (lambda () (interactive) (forward-line -1)))
(define-key durand-pdf-mode-map (vector ?P) 'durand-pdf-previous-pdf-line)

(define-key durand-pdf-mode-map [return] 'durand-pdf-open-pdf)
(define-key durand-pdf-mode-map [32] 'durand-pdf-open-or-scroll-up)
(define-key durand-pdf-mode-map [backspace] 'durand-pdf-open-or-scroll-down)
(define-key durand-pdf-mode-map [?o] 'kill-other-buffer-window)
(define-key durand-pdf-mode-map [?k] (lambda () (interactive) (kill-buffer (current-buffer))))

(defun durand-pdf-open-pdf ()
  "Open pdf or directory under point"
  (interactive)
  (let ((inhibit-read-only t)
        (chemin (get-text-property (point) 'chemin)))
    (if (and chemin (file-readable-p chemin))
        (find-file chemin)
      (message "Command not allowed on this line"))))

(defun durand-pdf-open-or-scroll-up ()
  "Open pdf or directory under point in another window"
  (interactive)
  (let* ((inhibit-read-only t)
         (chemin (get-text-property (point) 'chemin))
         (nom (file-name-nondirectory chemin)))
    (cond
     ((get-buffer nom)
      (save-selected-window
        (other-window 1)
        (pdf-view-scroll-up-or-next-page)))
     ((and chemin (file-readable-p chemin))
      (save-selected-window
        (find-file-other-window chemin)))
     (t
      (message "Command not allowed on this line")))))

(defun durand-pdf-open-or-scroll-down ()
  "Scroll down pdf"
  (interactive)
  (let* ((inhibit-read-only t)
         (chemin (get-text-property (point) 'chemin))
         (nom (file-name-nondirectory chemin)))
    (cond
     ((get-buffer nom)
      (save-selected-window
        (other-window 1)
        (pdf-view-scroll-down-or-previous-page)))
     ((and chemin (file-readable-p chemin))
      (save-selected-window
        (find-file-other-window chemin)))
     (t
      (message "Command not allowed on this line")))))

(defun durand-pdf-next-pdf-line ()
  "Next PDF line"
  (interactive)
  (let ((orig (point)))
    (end-of-line)
    (if (re-search-forward ".pdf$" nil t)
        (beginning-of-line)
      (goto-char orig)
      (message "No next pdf line"))))

(defun durand-pdf-previous-pdf-line ()
  "Next PDF line"
  (interactive)
  (let ((orig (point)))
    (beginning-of-line)
    (if (re-search-forward ".pdf$" nil t -1)
        (beginning-of-line)
      (goto-char orig)
      (message "No previous pdf line"))))

(defun durand-chercher-pdf (nom)
  "Chercher les fichers pdf par NOM"
  (interactive (list (read-string "Chercher: ")))
  (let ((chercher-buffer "*chercher pdf*")
        (nom (concat "*" nom "*")))
    (when (get-buffer chercher-buffer)
      (kill-buffer chercher-buffer))
    (pop-to-buffer-same-window chercher-buffer)
    (delete-region (point-min) (point-max))
    (dolist (dir pdf-dir-list)
      (let ((pro (make-process
                  :name "chercher"
                  :buffer chercher-buffer
                  :sentinel 'ignore
                  :command `("rg" "--files" "--no-messages" "--follow" "--iglob" ,nom ,dir))))
        (accept-process-output pro)))
    (durand-pdf-mode))
  (message "Chercher %s" nom))

;; manage window configurations in a simple way
(defvar window-snapshots '()
  "An alist for storing window configurations")

;;;###autoload
(defun save-window-snapshot ()
  "Save the current window configuration into `window-snapshots' alist."
  (interactive)
  (let ((key (read-string "Enter a name for the snapshot: ")))
    (setf (alist-get key window-snapshots) (current-window-configuration))
    (message "`%s' window snapshot saved!" key)))

;;;###autoload
(defun get-window-snapshot (key)
  "Given a KEY return the saved value in `window-snapshots' alist."
  (alist-get key window-snapshots nil nil 'equal))

;;;###autoload
(defun restore-window-snapshot ()
  "Restore a window snapshot from the `window-snapshots' alist."
  (interactive)
  (let* ((snapshot-name (ivy-read "Choose snapshot:" (mapcar #'car window-snapshots)))
         (snapshot (get-window-snapshot snapshot-name)))
    (if snapshot
        (set-window-configuration snapshot)
      (message "Snapshot `%s' not found" snapshot-name))))

;;;###autoload
(defun remove-window-snapshot ()
  "Remove a window snapshot from the `window-snapshots' alist."
  (interactive)
  (let* ((snapshot-name (ivy-read "Choose snapshot to remove:" (mapcar #'car window-snapshots)))
         (snapshot (assoc snapshot-name window-snapshots 'equal)))
    (setf window-snapshots (delq snapshot window-snapshots))
    (message "`%s' window snapshot removed!" snapshot-name)))

;; (defun my-org-sidebar-ql (query &optional buffers-files narrow group sort header)
;;   "Display a sidebar for `org-ql' QUERY.
;; Interactively, with prefix, prompt for these variables:


;; BUFFERS-FILES: A list of buffers and/
;; BUFFERS-FILES: A list of buffers and/or files to search.



;; GROUP: A text-property symbol present in each item (e.g. when
;; items are formatted by `org-ql-agenda--format-element', it might
;; be `priority' or `todo-state').

;; NARROW: Don't widen buffers before searching.

;; SORT: One or a list of `org-ql' sorting functions, like `date' or `priority'."
;;   (interactive (progn
;;                  (unless (or (equal current-prefix-arg '(4))
;;                              (derived-mode-p 'org-mode))
;;                    (user-error "Not an Org buffer: %s" (buffer-name)))
;;                  (list (read-minibuffer "Query: ")
;;                        (if (equal current-prefix-arg '(4))

;;                            (--if-let (read-from-minibuffer "Buffers/
;;                            (--if-let (read-from-minibuffer "Buffers/Files (blank for current buffer): ")


;;                                (pcase it
;;                                  ("" (current-buffer))
;;                                  ((rx bos "(") (-flatten (eval (read it))))
;;                                  (_ (s-split (rx (1+ space)) it)))
;;                              (current-buffer))
;;                          (current-buffer))
;;                        (not (eq current-prefix-arg '(4)))
;;                        (when (equal current-prefix-arg '(4))
;;                          (pcase (completing-read "Group by: "
;;                                                  (list "Don't group"
;;                                                        "priority"
;;                                                        "todo-state"))
;;                            ("Don't group" nil)
;;                            (property (intern (concat ":" property)))))
;;                        (when (equal current-prefix-arg '(4))
;;                          (pcase (completing-read "Sort by: "
;;                                                  (list "Don't sort"
;;                                                        "date"
;;                                                        "deadline"
;;                                                        "priority"
;;                                                        "scheduled"
;;                                                        "todo"))
;;                            ("Don't sort" nil)
;;                            (sort (intern sort)))))))
;;   (org-sidebar :header (or header (prin1-to-string query))
;;                :fns (list (cl-function
;;                            (lambda (&key group)
;;                              (--> (eval `(org-ql ',buffers-files
;;                                            ,query
;;                                            :narrow ,narrow
;;                                            :markers t
;;                                            :sort ,sort))
;;                                   (if group
;;                                       (--> (--group-by (org-element-property group it) it)
;;                                            (-sort (-on #'string<
;;                                                        (lambda (item)
;;                                                          (--> (car item)
;;                                                               (pcase group
;;                                                                 (:priority (char-to-string it))
;;                                                                 ((pred numberp) (number-to-string it))
;;                                                                 ((pred null) "None")
;;                                                                 ((pred stringp) it)))))
;;                                                   it))
;;                                     ;; Not grouping
;;                                     it)))))
;;                :group group
;; 	       :side-width 35))

;; (cl-defun org-sidebar (&key (fns '(org-sidebar--agenda-items org-sidebar--to-do-items))
;;                             (group nil group-passed)
;;                             super-groups
;;                             header
;; 			    side-width)
;;   "This package presents a helpful sidebar view for Org buffers.
;; At the top is a chronological list of scheduled and deadlined
;; tasks in the current buffer, and below that is a list of all
;; other non-done to-do items.  If the buffer is narrowed, the
;; sidebar only shows items in the narrowed portion; this allows
;; seeing an overview of tasks in a subtree.

;; FNS is a list of functions that return Org headline elements (as
;; returned by `org-element-headline-parser').  Such functions
;; should take a keyword argument `group' which causes them to
;; return elements grouped with `-group-by' (or they may omit
;; grouping, in which case the GROUP argument to this function must
;; not be used).  Elements returned by each function are formatted
;; with `org-sidebar-format-fn'.

;; GROUP specifies to call each function in FNS with its group
;; keyword argument non-nil.  SUPER-GROUPS may be set instead, which
;; specifies groups to be passed to `org-super-agenda'.

;; HEADER specifies a string to use as the header line.  If not
;; specified, it will be set automatically."
;;   (interactive)
;;   (let ((source-buffer (if org-sidebar-updating
;;                            org-sidebar-source-buffer
;;                          (current-buffer)))
;;         (slot 0)
;;         (fns (or org-sidebar-fns
;;                  fns))
;;         (group (cond (group-passed group)
;;                      (org-sidebar-updating org-sidebar-group)
;;                      (t org-sidebar-group-items)))
;;         (super-groups (or org-sidebar-super-groups
;;                           super-groups))
;;         (header (or org-sidebar-header
;;                     header))
;;         (inhibit-read-only t))
;;     (--each fns
;;       (when-let ((items (with-current-buffer source-buffer
;;                           (if group
;;                               (funcall it :group group)
;;                             (funcall it)))))
;;         (with-current-buffer (get-buffer-create (format " *org-sidebar: %s*" slot))
;;           (setq org-sidebar-source-buffer source-buffer
;;                 org-sidebar-group group
;;                 org-sidebar-super-groups super-groups
;;                 org-sidebar-fns fns
;;                 org-sidebar-header header)
;;           (setq-local org-sidebar-format-fn org-sidebar-format-fn)
;;           (org-sidebar--prepare-buffer (or header (buffer-name source-buffer)))
;;           (insert (cond (group (org-sidebar--format-grouped-items items))
;;                         ;; FIXME: Document super-groups in readme
;;                         (super-groups (let ((org-super-agenda-groups super-groups))
;;                                         (s-join "\n" (org-super-agenda--group-items
;;                                                       (mapcar org-sidebar-format-fn items)))))
;;                         (t (s-join "\n" (mapcar org-sidebar-format-fn items)))))
;;           (goto-char (point-min))
;;           (display-buffer-in-side-window (current-buffer)
;;                                          (list (cons 'side org-sidebar-side)
;;                                                (cons 'slot slot)
;;                                                (cons 'window-parameters (list (cons 'no-delete-other-windows t)))
;; 					       (cons 'window-width side-width)))
;;           (cl-incf slot))))))
