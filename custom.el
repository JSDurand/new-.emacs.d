;; Custom miscellaneous settings

(fset 'yes-or-no-p 'y-or-n-p)
(setq enable-recursive-minibuffers t)
(setq tab-always-indent 'complete)
(setq words-include-escapes t)
(setq sentence-end-double-space nil)
(put 'dired-find-alternate-file 'disabled nil)
(setq-default dired-listing-switches "-lah")
(setq save-interprogram-paste-before-kill t)
(temp-buffer-resize-mode 1)
(toggle-truncate-lines -1)
(recentf-mode 1)
(set-frame-parameter nil 'alpha '(90 . 90))

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

(define-key global-map [?\s-t] (lambda ()
				 (interactive)
				 (display-time-mode (or (and display-time-mode
							     -1)
							1))))
(define-key global-map [?\H-t] 'transpose-chars-back-one)
(define-key global-map [?\H-\C-t] 'transpose-chars-back-two)

;;;###autoload
(defun transpose-chars-back-one ()
  "Go back one char and transpose characters"
  (interactive)
  (save-excursion
    (backward-char 1)
    (transpose-chars 1)))

;;;###autoload
(defun transpose-chars-back-two ()
  "Go back two characters and transpose charcaters"
  (interactive)
  (save-excursion
    (backward-char 2)
    (transpose-chars 1)))

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
(defun ask-before-quit-advice (orig-func &rest args)
  (if (not (frame-parameter nil 'client))
      (let ((answer (read-char-choice "Do you really want to quit? " '(?y ?n))))
	(if (char-equal answer ?y)
	    (apply orig-func args)
	  (message "You're welcomed!")))
    (apply orig-func args)))

(advice-add 'save-buffers-kill-terminal :around 'ask-before-quit-advice)

(defun ask-before-send-message-advice (orig-func &rest args)
  "Confirm if the user really wants to send the message"
  (let ((answer (read-char-choice "Do you really want to send the mail? " '(?y ?n))))
    (cond
     ((char-equal ?y answer)
      (apply orig-func args))
     (t
      (message "You're welcomed!")))))
(advice-add 'message-send-and-exit :around 'ask-before-send-message-advice)

(setq initial-frame-alist '((width . 118)
			    (alpha (90 . 90))))
(set-frame-width nil 118)
(add-to-list 'default-frame-alist '(width . 118))
(add-to-list 'default-frame-alist '(font . "Menlo 20"))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(setq revert-without-query '(".*"))
(global-set-key [?\C-*] 'clean-up-buffers)
;;;###autoload
(defun clean-up-buffers ()
  "Clean up some buffers that I oft do not need to keep around"
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
    (or (and (= ?* (aref name 0))
	     (not (string-match "^\\*scratch\\*$" name))
	     (not (string-match "^\\*Messages\\*$" name))
	     (not (string-match "elfeed" name)))
	(string-match "^magit" name))))
(setq set-mark-command-repeat-pop t)

(define-key Info-mode-map [?\(] 'Info-backward-node)
(define-key Info-mode-map [?\)] 'Info-forward-node)
(require 'view)
(define-key view-mode-map [?j] 'View-scroll-line-forward)
(define-key view-mode-map [?k] 'View-scroll-line-backward)
(define-key view-mode-map [?l] 'scroll-left)
(define-key view-mode-map [?h] 'scroll-right)
