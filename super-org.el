;; super-org.el

;; some custom org functionalities upon which I rely heavily.

(use-package org
  :ensure t
  :defer t
  :config
  ;; Necessary since org-mode 9.2
  (require 'org-tempo)
  (setq org-todo-keywords '((sequence "TODO(t)" "START(s)" "WORKING(w)" "HARD-WORKING(h)" "ALMOST(a)" "|" "DONE(d)")
			    (sequence "TO-THINK(c)" "PENDING(p)" "HARD(r)" "IMPOSSIBLE(i)" "|" "SOLVED(v)")))
  (setq org-agenda-files '("~/org/agenda.org" "~/org/notes.org" "~/org/aujourdhui.org"))
  ;; (setq org-log-state-notes-insert-after-drawers nil)
  (setq org-log-into-drawer t)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?E)
  (setq org-default-priority ?B)
  (setq org-agenda-deadline-faces '((0.5 . org-warning)
				    (0.4 . org-upcoming-deadline)
				    (0.0 . default)))
  (setq org-agenda-block-separator ?\—)
  (add-hook 'org-mode-hook '(lambda ()
			      (define-key org-mode-map [?\ù] 'org-advance)
			      (define-key org-mode-map [?\ç] 'org-retreat)))
  ;; (add-hook 'org-archive-hook 'org-archive-kill-archive-file)
  (require 'org-agenda)
  (require 'org-super-agenda)
  (define-key org-mode-map [?\C-c tab] 'durand-forward-link)
  (define-key org-mode-map [?\C-c \S-tab] 'find-previous-link-in-buffer)
  (define-key org-mode-map [f8] 'org-account-prefix-map)
  (define-key org-mode-map [?\C-c ?\C-j] 'counsel-org-goto)
  (define-key org-agenda-mode-map [?\M-n] 'org-super-agenda-next-group)
  (define-key org-agenda-mode-map [?\M-p] 'org-super-agenda-previous-group)
  (define-key org-agenda-mode-map [f8] 'durand-org-account-prefix-map)
  (define-key org-agenda-mode-map [?c] 'org-agenda)
  (define-key org-agenda-mode-map [?V] 'orgy-view)
  (define-key org-agenda-mode-map [?\)] 'org-agenda-next-block)
  (define-key org-agenda-mode-map [?-] 'org-agenda-previous-block)
  (define-key org-agenda-mode-map [?$] 'org-agenda-go-to-block)
  (define-key org-agenda-mode-map [?j] 'org-agenda-jump-to-item)
  (define-key org-agenda-mode-map [?\M-j] 'org-agenda-open-novels)
  (define-key org-agenda-mode-map [?\H-b] 'org-open-bookmarks)
  (define-key org-agenda-mode-map [?\C-j] 'org-agenda-goto-date)
  (define-key org-agenda-mode-map [?n] 'org-agenda-next-item)
  (define-key org-agenda-mode-map [?N] 'org-agenda-next-line)
  (define-key org-agenda-mode-map [?p] 'org-agenda-previous-item)
  (define-key org-agenda-mode-map [?P] 'org-agenda-previous-line)
  (define-key org-agenda-mode-map (kbd "<backspace>") 'org-agenda-first-block)
  (define-key org-agenda-mode-map (kbd "à") 'org-agenda-last-block)
  (define-key org-agenda-mode-map (kbd "s-)") 'org-super-agenda-next-group)
  (define-key org-agenda-mode-map (kbd "s-(") 'org-super-agenda-previous-group)
  (define-key org-agenda-mode-map (kbd "s--") 'org-super-agenda-previous-group)
  (define-key org-agenda-mode-map (kbd "L") 'org-agenda-update-link)
  (define-key org-agenda-mode-map (kbd "<s-return>") '(lambda ()
                                                        "Delete other windows"
                                                        (interactive)
                                                        (org-agenda-open-link)
                                                        (delete-other-windows)))
  (define-key org-super-agenda-header-map [?\)] 'org-agenda-next-block)
  (define-key org-super-agenda-header-map [?-] 'org-agenda-previous-block)
  (define-key org-super-agenda-header-map (kbd "s-)") 'org-super-agenda-next-group)
  (define-key org-super-agenda-header-map (kbd "s-(") 'org-super-agenda-previous-group)
  (define-key org-super-agenda-header-map (kbd "s--") 'org-super-agenda-previous-group)
  (define-key org-super-agenda-header-map [f8] 'durand-org-account-prefix-map)
  (define-key org-super-agenda-header-map [?\M-n] 'org-super-agenda-next-group)
  (define-key org-super-agenda-header-map [?\M-p] 'org-super-agenda-previous-group)
  (define-key org-super-agenda-header-map [?n] 'org-agenda-next-item)
  (define-key org-super-agenda-header-map [?N] 'org-agenda-next-line)
  (define-key org-super-agenda-header-map [?p] 'org-agenda-previous-item)
  (define-key org-super-agenda-header-map [?P] 'org-agenda-previous-line)
  (advice-add 'org-edit-special :after '(lambda (&optional orig-fun)
					  "Make it full frame"
					  (delete-other-windows)))
  (set-face-attribute 'org-block nil :background "gray5" :foreground "DarkOrange1"))

(use-package org-habit
  :config
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-show-all-today t)
  (setq org-habit-graph-column 55)
  (setq org-habit-today-glyph ?|))

(use-package org
  :ensure org-plus-contrib)

;; org-drill
(require 'org-drill)

;; the original org-drill contains invalid calls to org-map-entries
;;;###autoload
(defun org-drill-hide-subheadings-if (test)
  "TEST is a function taking no arguments. TEST will be called for each
of the immediate subheadings of the current drill item, with the point
on the relevant subheading. TEST should return nil if the subheading is
to be revealed, non-nil if it is to be hidden.
Returns a list containing the position of each immediate subheading of
the current topic."
  (let ((drill-entry-level (org-current-level))
        (drill-sections nil))
    (org-show-subtree)
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (and (not (org-invisible-p))
                    (> (org-current-level) drill-entry-level))
           (when (or (/= (org-current-level) (1+ drill-entry-level))
                        (funcall test))
             (hide-subtree))
           (push (point) drill-sections)))
       nil 'tree))
    (reverse drill-sections)))

;; I only need basic functionality for french words here.
;;;###autoload
(defun org-drill-present-français ()
  "Hide the entry basically"
  (interactive)
  (outline-hide-subtree)
  (prog1 (org-drill-presentation-prompt)
    (org-drill-hide-subheadings-if 'org-drill-entry-p)))

;;;###autoload
(defun org-drill-present-français-answer (reschedule-fn)
  "Show the entry basically"
  (interactive)
  (outline-show-subtree)
  (funcall reschedule-fn))

(add-to-list 'org-drill-card-type-alist
             '("français" org-drill-present-français org-drill-present-français-answer))

;; delete scheduling information after drilling
(defun org-remove-schedule (&rest choses)
  "remove scheduling information"
  (interactive)
  (let ((inhibit-modification-hooks t)
        (inhibit-message t))
    (org-map-entries
     (lambda () (org--deadline-or-schedule '(4) 'scheduled nil))
     "drill"))
  (ignore-errors (save-buffer 0)))

(advice-add 'org-drill :after 'org-remove-schedule)

;; This can cause org mode to fontify todo items with priority cookies
;; wrong.
;; (setq org-drill-use-visible-cloze-face-p t)

;; Do a recenter afterwards
;; (advice-remove 'org-agenda-next-blo 'recenter-to-top)
;; (advice-remove 'org-agenda-previous- 'recenter-to-top)

;; do delete-other-windows
(advice-add 'org-agenda :after '(lambda (&rest params)
                                  "Full frame"
                                  (delete-other-windows)))

;; Recenter to top
;;;###autoload
(defun recenter-to-top (&rest some)
  "Recenter to top"
  (interactive)
  (recenter 0))

(setq org-use-speed-commands t)
(setq org-speed-commands-user '(("j" . counsel-org-goto)
				("P" . org-set-property)
				("a" . org-toggle-archive-tag)
				("U" . undo-tree-undo)
				("k" . (kill-buffer (current-buffer)))
				("S" . org-schedule)
				("v" . orgy-view)
                                ("§" . durand-org-hydra/body)))

(defun orgy-view ()
  "Recenter to top; if already there, return to previous position"
  (interactive)
  (let ((window-line (count-visible-lines (window-start) (point))))
    (if (= window-line (1+ scroll-margin))
        (recenter (or (1- (get 'orgy-recenter :line)) 0))
      (put 'orgy-recenter :line window-line)
      (recenter 0))))

(defun count-visible-lines (beg end)
  "Count visible lines between BEG and END"
  (interactive)
  (let ((line-num 1))
    (goto-char beg)
    (while (/= (line-number-at-pos (point)) (line-number-at-pos end))
      (forward-visible-line 1)
      (setq line-num (1+ line-num)))
    line-num))

(defhydra durand-org-hydra (org-mode-map "<f8>" :color blue)
  "
  Org speed key hydra
  "
  ("o" delete-other-windows "s-w")
  ("I" org-insert-todo-heading-respect-content "C-S-ret"))

;; (defun org-archive-kill-archive-file ()
;;   "Kill the archive file after archiving"
;;   (interactive)
;;   (with-current-buffer (file-name-nondirectory (org-extract-archive-file))
;;     (save-buffer 0)
;;     (kill-buffer)))

(set-face-attribute 'org-ellipsis nil :foreground nil)

(setq org-ellipsis " ⤵")

;; Not needed for now

;;;###autoload
;; (defun org-summary-todo (n-done n-not-done)
;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;   (org-todo (if (= n-not-done 0) "DONE" "TODO")))

;;;###autoload
;; (defun durand-org-repeat-item ()
;;   "Move the time of the current item forward one day"
;;   (interactive)
;;   (when (string= org-state "REPEATED")
;;     (let* ((appointment (org-entry-get (point) "APPOINTMENT"))
;; 	   (appointment-time (org-parse-time-string appointment))
;; 	   (appointment-year (nth 5 appointment-time))
;; 	   (appointment-month (nth 4 appointment-time))
;; 	   (appointment-day (nth 3 appointment-time))
;; 	   (new-appointment (format-time-string (org-time-stamp-format)
;; 						(encode-time 0 0 0
;; 							     (1+ appointment-day)
;; 							     appointment-month
;; 							     appointment-year))))
;;       (org-set-property "APPOINTMENT" new-appointment))))

;;;###autoload
(defun durand-org-back-to-repeat ()
  (interactive)
  (when (string= org-state "REPEATED")
    (org-set-property "LAST-REPEAT" (format-time-string (org-time-stamp-format t t) (current-time)))
    (org-add-log-setup 'state "REPEATED" "TO-REPEAT" 'time)
    (org-todo "TO-REPEAT")))

;; (remove-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-after-todo-state-change-hook 'durand-org-back-to-repeat)

(setq org-refile-targets nil)

;; org-capture and org-protocol
(require 'org-capture)
(require 'org-protocol)

;; The original org-protocol-convert handles youtube links wrong
(defun org-protocol-convert-query-to-plist (query)
  "Convert QUERY key=value pairs in the URL to a property list."
  (when query
    (apply 'append (mapcar (lambda (x)
			     (let ((c (split-string x "=")))
			       (list (intern (concat ":" (car c))) (mapconcat #'identity (cdr c) "="))))
			   (split-string query "&")))))


(setq org-capture-templates
      '(;; ("m" "Template for MaoBaoBao Notes" entry
	;;  (file+headline "~/org/wiki.org" "MaoBaoBao Notes")
	;;  "* day %U\n  %?")
	("m" "Account records" entry
	 (file+datetree "~/org/account/account.org")
	 "* %^{ITEM|breakfast|brunch|brunverage|lunch|dinner|beverage|snack|fruit}\n  :PROPERTIES:\n  :cost: %^{COST|0}\n  :RECORD_TIME: %U\n  :END:\n  %?\n\n  - "
	 :jump-to-captured t)
	("d" "Record Diaries" entry
	 (file+datetree "~/org/diary.org")
	 "* %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n"
	 :jump-to-captured t)
	("w" "Withdrawal records" entry
	 (file+headline "~/org/wiki.org" "Money Withdrawal")
	 "* WITHDRAW NTD %? %(org-insert-time-stamp (org-read-date nil t \"+0d\") nil nil)\n"
	 :kill-buffer t)
	("l" "Store links" entry
	 (file+headline "~/org/notes.org" "Links")
	 "* TO-THINK %? %(org-insert-time-stamp (org-read-date nil t \"+0d\") nil t)\n%a\n" :kill-buffer t)
        ("L" "for storing webpages" entry
         (file+headline "~/org/notes.org" "Links")
         "* PENDING %(org-filter-title) %(org-determine-tag)\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n  %(org-filtered-link)\n  %i\n  %?"
         :empty-lines 1
         :jump-to-captured t)
	("t" "TODO" entry
	 (file "~/org/aujourdhui.org")
	 "* TODO %? %^{Date to do:}t\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n"
	 :kill-buffer t)
	("b" "Blog posts" entry
	 (file+headline "~/org/notes.org" "Blog posts")
	 "* %? %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n")
	("a" "Abstractions" entry
	 (file+headline "~/org/wiki.org" "Abstractions")
	 "* ABSTRACT %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n\n")
	("A" "Agenda" entry
	 (file+headline "~/org/agenda.org" "Agenda")
	 "* TODO %?\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :DURATION: %^{Date: }t\n  :END:\n\n")
	("y" "YiFu" entry
	 (file+headline "~/org/wiki.org" "Yi Fu Tips")
	 "* MEMO %^{word}\n  :PROPERTIES:\n  :STORY: %\\2\n  :MEANING: %\\3\n  :END:\n** Yi Fu story\n   %^{story}\n** Meaning\n   %^{meaning}"
	 :kill-buffer t
	 :immediate-finish t)
	("p" "Piper" entry
	 (file+headline "~/org/wiki.org" "Piper Tips")
	 "* MEMO %^{title}\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :END:\n  %?"
	 :jump-to-captured t)
        ("c" "Chansons" entry
	 (file+headline "~/org/wiki.org" "Liste de Chansons")
	 "* MEMO %^{title}\n  :PROPERTIES:\n  :RECORD_TIME: %U\n  :LINK: %A\n  :END:\n  %?"
	 :jump-to-captured t)
        ("f" "français" entry
	 (file+headline "~/org/français.org" "Liste de mots français")
	 "* MEMO %^{mot} :drill:\n  :PROPERTIES:\n  :DRILL_CARD_TYPE: français\n  :RECORD_TIME: %U\n  :MEANING: %^{ce qu'il veut dire}\n  :END:\n\n  MEANING: %\\2\n%?"
	 :jump-to-captured t)))

;; filter out the title
;;;###autoload
(defun org-filter-title ()
  "Filter out some unnecessary parts of the link title"
  (let ((title (plist-get org-store-link-plist :description)))
    (cond
     ((string-match " - Mathematics Stack Exchange" title)
      (replace-match "" nil nil title))
     ((string-match " - YouTube" title)
      (replace-match "" nil nil title))
     (t
      title))))

;; filter title in the link
;;;###autoload
(defun org-filtered-link ()
  "Filter out some unnecessary parts in the link description"
  (let* ((link (plist-get org-store-link-plist :link))
         (title (plist-get org-store-link-plist :description))
         (filtered (cond ((string-match " - Mathematics Stack Exchange" title)
                          (replace-match "" nil nil title))
                         ((string-match " - YouTube" title)
                          (replace-match "" nil nil title))
                         (t
                          title))))
    (org-make-link-string link filtered)))


;; Determine tag based upon the URL
;;;###autoload
(defun org-determine-tag ()
  "Determine tag based upon the URL"
  (let ((link (plist-get org-store-link-plist :link)))
    (cond
     ((string-match "https?://www.youtube.com" link)
      ":youtube:")
     ((or
       (string-match "https?://math.stackexchange.com" link)
       (string-match "https?://mathoverflow.net/" link))
      ":stack:")
     (t
      ":web_link:"))))

;; Save the link in different locations based on the URL
;;;###autoload
;; (defun org-handle-link ()
;;   "Save the link in different locations based on the URL"
;;   (let ((link (plist-get org-store-link-plist :link)))
;;     (cond
;;      ((string-match "https?://www.youtube.com" link)
;;       (find-file "~/org/notes.org")
;;       (search-forward "Links")))))

;; automatically update account when capturing
;;;###autoload
(defun durand-capture-update-account ()
  "Run org-update-account if the template is \"m\""
  (interactive)
  (when (equal (plist-get org-capture-plist :key) "m")
    (with-current-buffer "account.org"
      (org-update-account)
      (ignore-errors (save-buffer 0))
      (org-columns))))

(add-hook 'org-capture-after-finalize-hook 'durand-capture-update-account)

;; offer completion of shops
;; I think I am better off using a predefined list...
(setq durand-frequent-shops '("新永泉"
                              "Four directions"
                              "Kebuke"
                              "Family mart"
                              "Sell Tzu Mo Noodle"
                              "Big pot"))
;; (defun org-complete-shop ()
;;   "Offer completion of shops."
;;   (interactive)
;;   (insert (ivy-read "Choose a shop" durand-frequent-shops)))

(setq account-history '())

(defun org-complete-shop ()
  "Offer completion of shops."
  (interactive)
  (if (s-suffix? "account.org" (buffer-name))
      (let (choice)
        (save-excursion
          (outline-previous-heading)
          ;; ensure level 4
          (if (= (car (org-heading-components)) 4)
              (let (res)
                ;; widen so that this can work in capture buffer
                (save-restriction
                  (widen)
                  (org-map-entries
                   (lambda ()
                     (let ((level (car (org-heading-components)))
                           contents)
                       (when (= level 4)
                         (let ((title (save-excursion
                                        (org-end-of-meta-data)
                                        (org-skip-whitespace)
                                        (buffer-substring-no-properties (point) (line-end-position)))))
                           (push title res)))))))
                (setq res (remove nil res))
                (setq res (cdr res))
                (setq res (nreverse res))
                (setq res (remove-duplicates
                           res
                           :test (lambda (x y)
                                   (and x y
                                        (string-equal
                                         (upcase x)
                                         (upcase y))))))
                (setq res (nreverse res))
                (setq choice (ivy-read "Choose one possible shop: " res
                                       :preselect "新永泉"
                                       :history account-history)))
            (user-error "Not at a heading of level 4!")))
        (insert choice))
    (user-error "This is not the account file, which is \"/Users/durand/org/account/account.org\"")))


;; offer completion for items in the same shop
(defun org-complete-item-same-shop ()
  "Offer completion for items in the same shop."
  (interactive)
  (if (s-suffix? "account.org" (buffer-name))
      (let (choice)
        (save-excursion
          (outline-previous-heading)
          ;; ensure level 4
          (if (= (car (org-heading-components)) 4)
              (let ((orig-title (save-excursion
                                  (org-end-of-meta-data)
                                  (org-skip-whitespace)
                                  (thing-at-point 'word)))
                    res)
                ;; widen so that this can work in capture buffer
                (save-restriction
                  (widen)
                  (org-map-entries
                   (lambda ()
                     (let ((level (car (org-heading-components)))
                           contents)
                       (when (= level 4)
                         (let ((title (save-excursion
                                        (org-end-of-meta-data)
                                        (org-skip-whitespace)
                                        (thing-at-point 'word)))
                               (limite (save-excursion
                                         (outline-next-heading)
                                         (point))))
                           (when (string-equal
                                  (and title (upcase title))
                                  (and orig-title (upcase orig-title)))
                             ;; (push (number-to-string (point)) contents)
                             ;; (push (number-to-string limite) contents)
                             (save-excursion
                               (while (search-forward "- " limite t)
                                 (push (buffer-substring-no-properties
                                        (point)
                                        (line-end-position))
                                       contents)))
                             (setq res (append res contents)))))))))
                (setq res (nreverse
                           (remove-duplicates
                            res
                            :test (lambda (x y)
                                    (string-equal (upcase x) (upcase y))))))
                (setq choice (ivy-read "Choose one possible item" res)))
            (user-error "Not at a heading of level 4!")))
        (insert choice))
    (user-error "This is not the account file, which is \"/Users/durand/org/account/account.org\"")))

;; smart completion
(defun org-smart-complete-item-or-shop-or-jump-to-next-item ()
  "When after \"- \" then complete item; otherwise complete shop"
  (interactive)
  (cond
   ((looking-back "- " (- (point) 2))
    (org-complete-item-same-shop))
   ((looking-back "^\\s-+" (line-beginning-position))
    (org-complete-shop))
   (t
    (org-account-jump-to-next-item))))

;; jump to next item
(defun org-account-jump-to-next-item ()
  "Jump to next item"
  (interactive)
  (end-of-line)
  (if (search-forward "-" nil t)
      (insert " ")
    (insert "\n")
    (indent-relative)
    (insert "- ")))

;; define a capture minor mode for this purpose
(setq account-mode-map (make-sparse-keymap))
(define-minor-mode account-mode "For completing in capturing accounts"
  nil
  "ACCOUNT"
  account-mode-map)

(add-hook 'org-capture-mode-hook (lambda ()
                                   "Activate account minor mode if in capturing accounts"
                                   (when (s-suffix? "account.org" (buffer-name))
                                     (account-mode))))

(define-key account-mode-map [tab] 'org-smart-complete-item-or-shop-or-jump-to-next-item)
;; (define-key account-mode-map (vector ?<) 'org-account-jump-to-next-item)
;; (define-key account-mode-map (vector ?\C-c ?x) 'org-complete-item-same-shop)

;; current money
;; (defvar durand-current-money 2215)

;; display current money
;;;###autoload
;; (defun durand-display-money ()
;;   "Display the current amount of money in the wallet"
;;   (interactive)
;;   (message "NTD %d" durand-current-money))

;; update current money
;; it turns out quite weird...
;;;###autoload
;; (defun durand-update-money ()
;;   "Update the current amount of money according to the record in wiki.org"
;;   (interactive)
;;   (with-current-buffer (or (get-buffer "wiki.org")
;; 			   (find-file "~/org/wiki.org"))
;;     (search-forward "** current money")
;;     (beginning-of-line)
;;     (let ((cur (org-entry-get (point) "CURRENT_MONEY"))
;; 	  (last-date (org-parse-time-string (org-entry-get (point) "LAST_UPDATE"))))
;;       (search-forward "** Money Withdrawal")
;;       (beginning-of-line)
;;       (let ((lim (save-excursion
;; 		   (outline-next-heading)
;; 		   (point)))
;; 	    (re "\\*\\*\\* WITHDRAW .* \\([[:alnum:]]+\\) .*\\(<.*>\\).*$"))
;; 	(while (re-search-forward re lim t)
;; 	  (when (time-less-p
;; 		 (apply 'encode-time last-date)
;; 		 (apply 'encode-time (org-parse-time-string (match-string 2))))))))))


(setq org-agenda-use-time-grid nil)
					; The time grid in agenda view is a little annoying
(setq org-agenda-start-with-follow-mode nil)
(setq org-agenda-start-with-log-mode nil)
(setq org-global-properties '((Effort_ALL . "0 0:10 0:20 0:30 0:40 0:50 1:00 1:30 2:00")))
(setq org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM %PRIORITY")
;; (remove-hook 'org-agenda-finalize-hook (lambda ()
;; 				      (let ((buffer-read-only nil))
;; 					(put-text-property (point-min) (point-max)
;; 							   'keymap 'org-agenda-mode-map)
;; 					(put-text-property (point-min) (point-max)
;; 							   'local-map 'org-agenda-mode-map))))

(setq org-agenda-custom-commands
      '(("o" "Custom"
	 ((agenda ""
                  ((org-super-agenda-groups
                    '((:name "Progress today"
                             :log t
                             :order -1)
                      (:name "Morning Tasks"
                             :log t
                             :tag "morning"
                             :order 1)
                      (:name "Afternoon Tasks"
                             :log t
                             :tag "afternoon"
                             :order 2)
                      (:name "Night Tasks"
                             :tag "night"
                             :log t
                             :order 3)
                      (:name "Deadlines" :deadline t)
                      ;; (:name "Daily items"
                      ;; 	    :heading-regexp "Martin\\|Midi\\|Après-Midi\\|Soir"
                      ;; 	    :order 3)
                      ;; (:name "Regular Habits"
                      ;;        :and (:tag "regular" :habit t))
                      (:name "Health Habits"
                             :and (:tag "habit" :tag "health")
                             :order 5
                             :log t)
                      (:name "MATH"
                             :tag "math"
                             :order -1)
                      (:name "Très Important"
                             :priority "A"
                             :order -1)
                      (:name "Scheduled"
                             :and (:scheduled t :not (:priority "A"))
                             :order 5
                             :log t)))
                   (org-agenda-span 'day)
                   (org-agenda-sorting-strategy '(priority-down time-up))))
          (todo "TO-THINK"
                ((org-super-agenda-groups
                  '((:name "À Voir" :tag "a_voir")
                    (:name "Mathématiques" :tag "math")
                    (:name "TeX" :tag "tex")
                    (:name "Question" :tag "question")))
                 (org-agenda-overriding-header "TO-THINK")))
          (todo "PENDING"
                ((org-super-agenda-groups
                  '((:name "YouTube" :tag "youtube")
                    (:name "Stack" :tag "stack")
                    (:name "Des Romans" :tag "roman")
                    (:name "Web Link" :tag "web_link")
                    (:name "PENDING" :anything t)))
                 (org-agenda-overriding-header "PENDING")))
          (tags "plan"
                ((org-agenda-files '("~/org/plan.org"))
                 (org-super-agenda-groups
                  '((:name "Début" :todo "DÉBUT")
                    (:name "Essaiyer" :todo "ESSAIYER")
                    (:name "Progresser" :todo "PROGRESSER")
                    (:name "Complété" :todo "COMPLÉTÉ")))
                 (org-agenda-overriding-header "PLAN"))))
         ((org-agenda-block-separator nil)))
        ("a" "Custom"
	 ((agenda ""
                  ((org-super-agenda-groups
                    '((:name "Progress today"
                             :log t
                             :order -1)
                      (:name "Morning Tasks"
                             :log t
                             :tag "morning"
                             :order 1)
                      (:name "Afternoon Tasks"
                             :log t
                             :tag "afternoon"
                             :order 2)
                      (:name "Night Tasks"
                             :tag "night"
                             :log t
                             :order 3)
                      (:name "Deadlines" :deadline t)
                      ;; (:name "Daily items"
                      ;; 	    :heading-regexp "Martin\\|Midi\\|Après-Midi\\|Soir"
                      ;; 	    :order 3)
                      ;; (:name "Regular Habits"
                      ;;        :and (:tag "regular" :habit t))
                      (:name "Health Habits"
                             :and (:tag "habit" :tag "health")
                             :order 5
                             :log t)
                      (:name "MATH"
                             :tag "math"
                             :order -1)
                      (:name "Très Important"
                             :priority "A"
                             :order -1)
                      (:name "Scheduled"
                             :and (:scheduled t :not (:priority "A"))
                             :order 5
                             :log t)))
                   (org-agenda-span 'day)
                   (org-agenda-sorting-strategy '(priority-down))))))
	;; ("A" todo "ACCOUNT"
	;;  ((org-agenda-files `(,org-account-file-name))
	;;   (org-agenda-text-search-extra-files nil)
	;;   (org-agenda-overriding-columns-format "%TIMESTAMP(DAY) %dashes(GRAPH) %total(TOTAL){+}")))
	("w" . "Waiting or withdrawal")
	("ww" "Waiting to be completed"
	 ((todo "TO-THINK")
	  (todo "PENDING"))
	 ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-tag-filter-preset '("-youtube"
                                          "-web_link"
                                          "-stack"))
	  (org-super-agenda-groups
	   '((:name "Articles to read or links"
		    :todo "TO-THINK")
	     (:name "Things to accomplish"
		    :todo "PENDING")))))
        ("wy" "YouTube links"
	 ((todo "PENDING"))
	 ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-tag-filter-preset '("+youtube"))))
        ("ws" "Stack Exchange links"
	 ((todo "PENDING"))
	 ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-tag-filter-preset '("+stack"))))
        ("wl" "Web Links"
	 ((todo "PENDING"))
	 ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-tag-filter-preset '("+web_link"))))
	("wm" "Money withdrawal record"
	 todo "WITHDRAW"
	 ((org-agenda-files '("~/org/wiki.org"))
	  (org-agenda-text-search-extra-files nil)
	  (org-agenda-overriding-columns-format "%item(DETAIL) %TIMESTAMP(DAY)")
	  (org-agenda-view-columns-initially t)
	  (org-super-agenda-groups
	   '((:name "Withdrawal Records"
		    :and (:todo "WITHDRAW" :tag "withdraw"))))))))
(setq org-account-file-name (format "~/org/account/account-%d-%d.org"
				    (caddr (calendar-current-date))
				    (car (calendar-current-date))))

;; use super agenda
(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode 1)
  (define-key org-super-agenda-header-map [?n] 'org-agenda-next-line)
  (define-key org-super-agenda-header-map [?N] 'org-agenda-next-item)
  (define-key org-super-agenda-header-map [?p] 'org-agenda-previous-line)
  (define-key org-super-agenda-header-map [?P] 'org-agenda-previous-item)
  (define-key org-super-agenda-header-map [?x] 'org-agenda-exit)
  (define-key org-super-agenda-header-map [?q] 'org-agenda-quit)
  (define-key org-super-agenda-header-map [?f] 'org-agenda-later)
  (define-key org-super-agenda-header-map [?b] 'org-agenda-earlier)
  (define-key org-super-agenda-header-map [?d] 'org-agenda-day-view)
  (define-key org-super-agenda-header-map [?w] 'org-agenda-week-view)
  (define-key org-super-agenda-header-map [?l] 'org-agenda-log-mode)
  (define-key org-super-agenda-header-map [?o] 'delete-other-windows))

;; The original function does not offer an option to select all.
;;;###autoload
(defun org-offer-links-in-entry (buffer marker &optional nth zero)
  "Offer links in the current entry and return the selected link.
If there is only one link, return it.
If NTH is an integer, return the NTH link found.
If ZERO is a string, check also this string for a link, and if
there is one, return it."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char marker)
     (let ((cnt ?0)
	   have-zero end links link c)
       (when (and (stringp zero) (string-match org-bracket-link-regexp zero))
	 (push (match-string 0 zero) links)
	 (setq cnt (1- cnt) have-zero t))
       (save-excursion
	 (org-back-to-heading t)
	 (setq end (save-excursion (outline-next-heading) (point)))
	 (while (re-search-forward org-any-link-re end t)
	   (push (match-string 0) links))
	 (setq links (org-uniquify (reverse links))))
       (cond
	((null links)
	 (message "No links"))
	((equal (length links) 1)
	 (setq link (car links)))
	((and (integerp nth) (>= nth 0) (>= (length links) (if have-zero (1+ nth) nth)))
	 (setq link (nth (if have-zero nth (1- nth)) links)))
        ((and (integerp nth) (< nth 0))
         (setq link links))
	(t				; we have to select a link
	 (save-excursion
	   (save-window-excursion
	     (delete-other-windows)
	     (with-output-to-temp-buffer "*Select Link*"
	       (dolist (l links)
		 (cond
		  ((not (string-match org-bracket-link-regexp l))
		   (princ (format "[%c]  %s\n" (cl-incf cnt)
				  (org-unbracket-string "<" ">" l))))
		  ((match-end 3)
		   (princ (format "[%c]  %s (%s)\n" (cl-incf cnt)
				  (match-string 3 l) (match-string 1 l))))
		  (t (princ (format "[%c]  %s\n" (cl-incf cnt)
				    (match-string 1 l)))))))
	     (org-fit-window-to-buffer (get-buffer-window "*Select Link*"))
	     (message "Select link to open, RET to open all:")
	     (setq c (read-char-exclusive))
	     (and (get-buffer "*Select Link*") (kill-buffer "*Select Link*"))))
	 (when (equal c ?q) (user-error "Abort"))
	 (if (equal c ?\C-m)
	     (setq link links)
	   (setq nth (- c ?0))
	   (when have-zero (setq nth (1+ nth)))
	   (unless (and (integerp nth) (>= (length links) nth))
	     (user-error "Invalid link selection"))
	   (setq link (nth (1- nth) links)))))
       (cons link end)))))

;;;###autoload
(defun org-super-agenda-next-group ()
  "Go to the next group in org super agenda buffer"
  (interactive)
  (if (save-match-data
	(save-excursion
	  (beginning-of-line)
	  (looking-at "^\\s-\\S-")))
      (progn
	(end-of-line)
	(re-search-forward "^\\s-\\S-" nil 'go))
    (re-search-forward "^\\s-\\S-" nil 'go))
  (beginning-of-line)
  (recenter))

;;;###autoload
(defun org-agenda-move-item-advice (old arg)
  "Apply move-block functions if cannot move item anymore"
  (while (> arg 0)
    (let ((ori (point)))
      (apply old (list 1))
      (cl-decf arg)
      (and (= ori (point))
           (let ((cur-bloc (let ((num 0))
                             (dolist (p org-agenda-block-seps num)
                               (when (>= (point) p)
                                 (setf num (1+ num))))))
                 (total-bloc (length org-agenda-block-seps)))
             (cond
              ((eq this-command 'org-agenda-next-item)
               (if (= total-bloc cur-bloc)
                   (message "Le dernier ligne")
                 (widen)
                 (goto-char (nth cur-bloc org-agenda-block-seps))
                 (org-agenda-narrow-block)
                 (org-agenda-next-item 1)))
              ((eq this-command 'org-agenda-previous-item)
               (if (= cur-bloc 1)
                   (message "Le premier ligne")
                 (widen)
                 (goto-char (nth (- cur-bloc 2) org-agenda-block-seps))
                 (org-agenda-narrow-block)
                 (goto-char (point-max))))
              (t
               (user-error "This advice is used in weird places: %s" this-command))))))))

(advice-add 'org-agenda-next-item :around 'org-agenda-move-item-advice)
(advice-add 'org-agenda-previous-item :around 'org-agenda-move-item-advice)

;; The original function uses the point (1+ (point-at-eol)), which will throw an error if that is bigger than
;; (point-max), so fix it.
;;;###autoload
(defun org-remove-subtree-entries-from-agenda (&optional buf beg end)
  "Remove all lines in the agenda that correspond to a given subtree.
The subtree is the one in buffer BUF, starting at BEG and ending at END.
If this information is not given, the function uses the tree at point."
  (let ((buf (or buf (current-buffer))) m p)
    (save-excursion
      (unless (and beg end)
	(org-back-to-heading t)
	(setq beg (point))
	(org-end-of-subtree t)
	(setq end (point)))
      (set-buffer (get-buffer org-agenda-buffer-name))
      (save-excursion
	(goto-char (point-max))
	(beginning-of-line 1)
	(while (not (bobp))
	  (when (and (setq m (org-get-at-bol 'org-marker))
		     (equal buf (marker-buffer m))
		     (setq p (marker-position m))
		     (>= p beg)
		     (< p end))
	    (let ((inhibit-read-only t))
	      (delete-region (point-at-bol) (min (point-max) (1+ (point-at-eol))))))
	  (beginning-of-line 0))))))

;;;###autoload
(cl-defun org-agenda-next-block (&optional (n 1))
  "Go to the next N-th block in org agenda buffer"
  (interactive)
  (let* ((cands (-filter (lambda (x)
                           (> x (point)))
                         org-agenda-block-seps))
         (res (if cands
                  (if (>= (length cands) n)
                      (nth (1- n) cands)
                    (org-agenda-last-block)
                    (user-error "Seulement %d blocs après, mais %d fois demandé" (length cands) n))
                (user-error "Le dernier bloc"))))
    (widen)
    (goto-char res)
    (org-agenda-narrow-block)))

(defvar org-agenda-total-blocks nil
  "Le nombre total de blocs dans org-agenda")

(defvar org-agenda-block-seps nil
  "Les séparateurs de blocs dans org-agenda comme une liste des positions")

(make-variable-buffer-local 'org-agenda-total-blocks)
(make-variable-buffer-local 'org-agenda-block-seps)

;;;###autoload
(defun org-agenda-count-blocks (&optional debut)
  "Count the total number of blocks in the agenda, starting from DEBUT, if non-nil"
  (save-restriction
    (save-excursion
      (save-match-data
        (widen)
        (goto-char (or debut (point-min)))
        (let* ((num 1)
               (res (list (point-min)))
               fin)
          (while (not fin)
            (let ((pos (next-single-property-change (line-end-position) 'org-agenda-structural-header)))
              (if pos
                  (progn
                    (setf num (1+ num)
                          res (append res (list pos)))
                    (goto-char pos))
                (setf fin t))))
          (setf
           org-agenda-total-blocks num
           org-agenda-block-seps res))))))

;;;###autoload
(defun org-agenda-show-blocks-number ()
  "Show the current position"
  (let ((num (length (-filter (lambda (x) (>= (point) x)) org-agenda-block-seps))))
    (and num (format "%d/%d" num org-agenda-total-blocks))))

;;;###autoload
(defun org-super-agenda-previous-group ()
  "Go to the next group in org super agenda buffer"
  (interactive)
  (if (save-match-data
	(save-excursion
	  (beginning-of-line)
	  (looking-at "^\\s-\\S-")))
      (progn
	(beginning-of-line)
	(re-search-forward "^\\s-\\S-" nil 'go -1))
    (re-search-forward "^\\s-\\S-" nil 'go -1))
  (beginning-of-line)
  (recenter))

;;;###autoload
(cl-defun org-agenda-previous-block (&optional (n 1))
  "Go to the next block in org agenda buffer"
  (interactive)
  (let* ((cands (-filter (lambda (x)
                           (< x (point)))
                         org-agenda-block-seps))
         (res (if cands
                  (if (>= (length cands) n)
                      (nth (1- n) (nreverse cands))
                    (org-agenda-first-block)
                    (user-error "Seulement %d blocs avant, mais %d fois demandé" (length cands) n))
                (user-error "Le premier bloc"))))
    (widen)
    (goto-char res)
    (org-agenda-narrow-block)))

;;;###autoload
(defun org-agenda-narrow-block ()
  "Narrow to one block"
  (interactive)
  (widen)
  (let* ((end (or
               (let ((chois (next-single-property-change (line-end-position) 'org-agenda-structural-header)))
                 (when chois
                   (1- chois)))
               (point-max)))
         (start (save-excursion
                  (goto-char
                   (or
                    (previous-single-property-change (line-end-position) 'org-agenda-structural-header)
                    (point-min)))
                  (line-beginning-position))))
    (narrow-to-region start end)
    (goto-char (point-min))))

(add-hook 'org-agenda-finalize-hook 'org-agenda-narrow-block)
(add-hook 'org-agenda-finalize-hook 'org-agenda-count-blocks)
;; (remove-hook 'org-agenda-finalize-hook 'durand-agenda-mode)

;; (define-derived-mode durand-agenda-mode org-agenda-mode "Durand-Agenda"
;;   "My agenda mode")

;; fix org-agenda-kill
(defun org-agenda-kill ()
  "Kill the entry or subtree belonging to the current agenda entry."
  (interactive)
  ;; use derived-mode-p instead of requiring strictly to be in org-agenda-mode
  (or (derived-mode-p 'org-agenda-mode) (error "Not in agenda"))
  (let* ((bufname-orig (buffer-name))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (type (org-get-at-bol 'type))
	 dbeg dend (n 0) conf)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(save-excursion
	  (goto-char pos)
	  (if (and (derived-mode-p 'org-mode) (not (member type '("sexp"))))
	      (setq dbeg (progn (org-back-to-heading t) (point))
		    dend (org-end-of-subtree t t))
	    (setq dbeg (point-at-bol)
		  dend (min (point-max) (1+ (point-at-eol)))))
	  (goto-char dbeg)
	  (while (re-search-forward "^[ \t]*\\S-" dend t) (setq n (1+ n)))))
      (setq conf (or (eq t org-agenda-confirm-kill)
		     (and (numberp org-agenda-confirm-kill)
			  (> n org-agenda-confirm-kill))))
      (and conf
	   (not (y-or-n-p
		 (format "Delete entry with %d lines in buffer \"%s\"? "
			 n (buffer-name buffer))))
	   (error "Abort"))
      (let ((org-agenda-buffer-name bufname-orig))
	(org-remove-subtree-entries-from-agenda buffer dbeg dend))
      (with-current-buffer buffer (delete-region dbeg dend))
      (message "Agenda item and source killed"))))

;; La fonction `org-agenda-set-mode-name' est carrément inutile!
;; La fonction originale est dans le fichier org-agenda.el
(defun org-agenda-set-mode-name ()
  "Cette fonction ne sert rien!")

;; redefine org-agenda-goto
(require 'org-agenda)
(defun org-agenda-goto (&optional highlight)
  "Go to the entry at point in the corresponding Org file."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen)
    (push-mark)
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (org-show-context 'agenda)
      (recenter 1)			; The change with the default function
      (org-back-to-heading t)
      (let ((case-fold-search nil))
	(when (re-search-forward org-complex-heading-regexp nil t)
	  (goto-char (match-beginning 4)))))
    (run-hooks 'org-agenda-after-show-hook)
    (and highlight (org-highlight (point-at-bol) (point-at-eol)))))

;; custom functionalities
;;;###autoload
(defun org-agenda-goto-with-fun (fun &optional highlight)
  "Go to the entry at point in the corresponding Org file, and execute FUN."
  (interactive)
  (save-window-excursion
    (let* ((marker (or (org-get-at-bol 'org-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   (pos (marker-position marker)))
      (switch-to-buffer-other-window buffer)
      (widen)
      (push-mark)
      (goto-char pos)
      (when (derived-mode-p 'org-mode)
	(org-show-context 'agenda)
	(recenter 1)
	(org-back-to-heading t)
	(let ((case-fold-search nil))
	  (when (re-search-forward org-complex-heading-regexp nil t)
	    (goto-char (match-beginning 4)))))
      (funcall fun))))

;; It turns out I do not need these functions now, as I directly go to the account file.

;;;###autoload
;; (defun durand-org-account-new ()
;;   "Make a new day from agenda buffer"
;;   (interactive)
;;   (org-agenda-goto-with-fun 'org-new-account))

;;;###autoload
;; (defun durand-org-account-insert ()
;;   "Insert a new entry and update from agenda buffer"
;;   (interactive)
;;   (org-agenda-goto-with-fun (lambda () (progn
;; 					 (call-interactively 'org-set-item-price-note)
;; 					 (org-update-account)))))

;;;###autoload
;; (defun durand-org-account-update ()
;;   "Update the account from agenda buffer"
;;   (interactive)
;;   (org-agenda-goto-with-fun 'org-update-account))

;;;###autoload
;; (defun durand-org-account-calc ()
;;   "Calculate the averages in agenda buffer"
;;   (interactive)
;;   (org-agenda-goto-with-fun 'org-calc-account))

;;;###autoload
;; (defun durand-org-account-modify ()
;;   "Modify the account field in agenda buffer"
;;   (interactive)
;;   (org-agenda-goto-with-fun 'org-modify-account))

(define-prefix-command 'durand-org-account-prefix-map)
;; (define-key durand-org-account-prefix-map [?n] 'durand-org-account-new)
;; (define-key durand-org-account-prefix-map [?i] 'durand-org-account-insert)
;; (define-key durand-org-account-prefix-map [?u] 'durand-org-account-update)
;; (define-key durand-org-account-prefix-map [?c] 'durand-org-account-calc)
;; (define-key durand-org-account-prefix-map [?m] 'durand-org-account-modify)
(define-key durand-org-account-prefix-map [?a] 'durand-org-agenda-append-text)
(define-key durand-org-account-prefix-map [?v] 'durand-org-agenda-goto-view-note)

;;;###autoload
(defun org-agenda-update-link (&optional arg)
  "Update the link in agenda buffer"
  (interactive "P")
  (if arg
      (org-agenda-goto-with-fun 'org-update-link)
    (org-agenda-goto-with-fun 'org-immediate-update-link)))

(defvar durand-before-obj nil
  "The position to return to when ivy-read ends")

;;;###autoload
(defun durand-cursor-follow-link ()
  "Place the cursor on the link in ivy-read; inspired by swiper"
  (with-ivy-window
    (swiper--cleanup)
    (let* ((cur (ivy-state-current ivy-last))
           (beg (and cur (cadr (assoc-default cur (caddr durand-before-obj)))))
           (end (and cur (caddr (assoc-default cur (caddr durand-before-obj)))))
           (wnd (ivy-state-window ivy-last)))
      (swiper--add-overlay beg end 'swiper-line-face wnd 0))))

;;;###autoload
(defun org-update-link (&optional link arg)
  "Update the link"
  (interactive)
  (let* ((next-heading-position (save-excursion
                                  (outline-next-heading)
                                  (point)))
         (current (let (res)
                    (save-excursion
                      (while (re-search-forward org-any-link-re
                                                next-heading-position
                                                t)
                        (push (list (match-string 2)
                                    (match-string 4)
                                    (match-beginning 0)
                                    (match-end 0))
                              res)))
                    (nreverse res)))
         (num_link (when current
                     (cond
                      (arg (substring-no-properties
                            (caar current)))
                      (t
                       (setf durand-before-obj (list (point) (buffer-name) current))
                       (ivy-read "Chois un lien à remplacer?"
                                 (mapcar (lambda (x)
                                           (substring-no-properties
                                            (car x)))
                                         current)
                                 :update-fn 'durand-cursor-follow-link
                                 :unwind (lambda ()
                                           (swiper--cleanup)
                                           (setf durand-before-obj nil)))))))
         (lien-courant (when (not (string= num_link ""))
                         (assoc num_link (mapcar (lambda (x)
                                                   (cons (car x) (cddr x)))
                                                 current))))
         (beg (and lien-courant (nth 1 lien-courant)))
         (end (and lien-courant (nth 2 lien-courant)))
         (collection (append (mapcar #'car current)
                             (mapcar #'car org-stored-links)
                             org--links-history))
         (link (when (not arg) (or link (ivy-read "Link: " collection))))
         (associate-current (when arg (assoc link current)))
         (associate (when (not arg) (assoc link org-stored-links)))
         (objet (if arg
                    (car org-stored-links)
                  (or associate-current associate (list link ""))))
         (lien (car objet))
         (desc (read-string "Desctiption: " (let ((default (cadr objet)))
                                              (when default
                                                (file-name-nondirectory default)))))
         (nouveau-lien (org-make-link-string lien desc)))
    (if lien-courant
        (setf (buffer-substring beg end)
              nouveau-lien)
      (save-excursion
        (outline-next-heading)
        (newline)
        (newline)
        (backward-char 2)
        (setf (buffer-substring (point) (1+ (point)))
              nouveau-lien)
        (indent-according-to-mode)))))

;;;###autoload
(defun org-immediate-update-link ()
  "Mettre à jour le lien immédiatement"
  (org-update-link nil t))

;;;###autoload
(defun org-append-text ()
  "Append text to the end of the entry before the next heading"
  (interactive)
  (outline-next-heading)
  (let ((beg (point))
	(text (read-string "Text: "))
	(fill-column 80))
    (insert (concat "\n" text "\n\n"))
    (indent-region beg (point))
    (fill-region beg (point))))

;;;###autoload
(defun durand-org-agenda-append-text ()
  "Append some text to the end of the entry at the point"
  (interactive)
  (org-agenda-goto-with-fun 'org-append-text))

(add-hook 'org-capture-mode-hook (lambda ()
				   "Activate fill-column in org capture"
				   (interactive)
				   (setq-local fill-column 80)
				   (auto-fill-mode 1)))
(add-hook 'org-log-buffer-setup-hook
	  (lambda ()
	    "Activate fill-column in org capture"
	    (interactive)
	    (setq-local fill-column 80)
	    (auto-fill-mode 1)))

;;;###autoload
(defun durand-org-get-notes ()
  "View the notes of the org-entry at point.
This returns a list of notes, where every element is a list whose `car' is the time,
and whose `caddr' is a list of strings, the content of the note."
  (interactive)
  (save-restriction
    (save-excursion
      (save-match-data
	(widen)
	(unless (looking-at org-heading-regexp)
	  (outline-back-to-heading t))
	(let ((limit (save-excursion (outline-next-heading) (point)))
	      res-list)
	  (while (re-search-forward org-note-regexp limit t)
	    (let ((indent-string (match-string 1))
		  (time-string (match-string 2))
		  (beg (1+ (point)))
		  res)
	      (forward-line)
	      (while (looking-at (concat "^" indent-string "\\s-\\{2\\}"))
		(push (buffer-substring-no-properties
		       (match-end 0)
		       (line-end-position))
		      res)
		(forward-line))
	      (push (list
		     (substring-no-properties time-string)
		     (list beg (1- (point)))
		     (mapconcat #'identity (nreverse res) "\n"))
		    res-list)))
	  (nreverse res-list))))))

(setq org-note-regexp (concat
		       "^\\([ \t]+\\)- Note taken on \\("
		       org-element--timestamp-regexp
		       "\\).*$"))

;;;###autoload
(defun durand-org-view-notes ()
  "View the notes entries in a separate buffer"
  (interactive)
  (let* ((notes (durand-org-get-notes))
	 (len (length notes)))
    (with-current-buffer-window
     "*durand-org-view-notes*"
     nil nil
     (goto-char (point-min))
     (insert "#+STARTUP: showall\n")
     (let ((times (mapcar #'car notes))
	   (metas (mapcar #'cadr notes))
	   (contents (mapcar #'caddr notes)))
       (if (/= 0 len)
	   (dotimes (i len)
	     (insert (concat
		      "\n"
		      (propertize "*" :note-meta (elt metas i))
		      " Note on "
		      (elt times i)
		      "\n"
		      (elt contents i)
		      "\n")))
	 (insert "No notes found!")))
     (goto-char (point-min))
     (org-mode))
    (message "%s note%s found" (if (= 0 len) "No" (number-to-string len))
	     (if (<= len 1) "" "s"))))

;;;###autoload
(defun durand-org-agenda-goto-view-note ()
  "Go to the corresponding file and view the notes from the agenda file."
  (interactive)
  (org-agenda-goto-with-fun 'durand-org-view-notes)
  (temp-buffer-window-show "*durand-org-view-notes*")
  (save-selected-window
    (pop-to-buffer "*durand-org-view-notes*")
    (goto-char (point-min))
    (fit-window-to-buffer nil temp-buffer-max-height)))

;; Go to the first block in block agenda view
;;;###autoload
(defun org-agenda-first-block ()
  "Go to the first block in block agenda view"
  (interactive)
  (widen)
  (goto-char (point-min))
  (org-agenda-narrow-block))

;;;###autoload
(defun org-agenda-go-to-block ()
  "Aller à un certain bloc"
  (interactive)
  (let* ((num (read-number "Quel bloc?" 1))
         (total (length org-agenda-block-seps)))
    (if (not (and (<= num total)
                  (>= num 1)))
        (message "Il n'y a que %d blocs." total)
      (widen)
      (goto-char (nth (1- num) org-agenda-block-seps))
      (org-agenda-narrow-block))))

;;;###autoload
(defun org-agenda-last-block ()
  "Go to the last block in block agenda view"
  (interactive)
  (widen)
  (if (and org-agenda-block-seps
           (consp org-agenda-block-seps))
      (goto-char (-last #'identity org-agenda-block-seps))
    (goto-char (point-max)))
  (org-agenda-narrow-block))

;; Modified tag group re to work in strings as well
(defvar durand-org-tag-group-re " +\\(:\\([[:alnum:]_@#%:]+\\):\\) *$"
  "Modified tag group re to work in strings as well")

;; Helper function that gets rid of todos, tags, and properties
;;;###autoload
(defun org-agenda-filter-extras (str)
  "Helper function that gets rid of todos, tags, and properties"
  (let* ((answer (substring-no-properties str))
         (case-fold-search nil)
         (org-todo-regexp "\\(ALMOST\\|DONE\\|HARD\\(?:-WORKING\\)?\\|IMPOSSIBLE\\|PENDING\\|S\\(?:OLVED\\|TART\\)\\|TO\\(?:-THINK\\|DO\\)\\|WORKING\\)")
         (remove-list `(,org-todo-regexp
                        ,durand-org-tag-group-re
                        ,org-priority-regexp
                        "^ +"
                        " +$")))
    (dolist (re remove-list answer)
      (when (string-match re answer)
        (setf answer (replace-match "" nil nil answer))))))

;; Collect all agenda items and jump to the selected one
;;;###autoload
(defun org-agenda-jump-to-item (&optional initial-input)
  "Collect all agenda items and jump to the selected one"
  (interactive)
  (let (items choice)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (next-single-property-change (point-at-eol) 'org-marker)
          (let ((item-pos (next-single-property-change (point-at-eol) 'org-marker)))
            (goto-char item-pos)
            (push (cons
                   (org-agenda-filter-extras (get-text-property (point) 'txt))
                   item-pos)
                  items)))
        (when items
          (setf items (nreverse items))
          (setf choice (ivy-read "Jump to:" (mapcar #'car items)
                                 :require-match t
                                 :initial-input initial-input
                                 :re-builder 'ivy--regex-plus)))))
    (when choice
      (goto-char (assoc-default choice items
                                (lambda (s r)
                                  (string-match (regexp-quote r) s)))))))

;;;###autoload
(defun org-agenda-open-novels ()
  "Open all novels; place the cursor at the first novel line."
  (interactive)
  (beginning-of-line)
  (while (get-text-property (point-at-bol) 'org-marker)
    (org-agenda-open-link -1)
    (forward-line 1)))

;;;###autoload
(defun org-open-bookmarks ()
  "Choose bookmarks to open."
  (interactive)
  (let* ((route_du_fichier "~/org/notes.org")
         (cands (org-ql--query route_du_fichier '(tags "bookmarks")
                  :action (lambda ()
                            (org-offer-links-in-entry (buffer-name) (point)))))
         (choice (ivy-read "Choose bookmarks: " (mapcar (lambda (x)
                                                          (string-match org-any-link-re (format "%s" (car x)))
                                                          (substring-no-properties
                                                           (match-string 3 (format "%s" (car x)))
                                                           1 -1))
                                                        cands))))
    (org-open-link-from-string
     (substring-no-properties
      (car (assoc* choice cands
                   :test (lambda (x y)
                           (string-match x y))))))))

;;;###autoload
(defun org-agenda-jump-to-novels ()
  "Jump to one of novels"
  (interactive)
  (org-agenda-jump-to-item "最新章"))

;;;###autoload
(defun durand-org-modify-note ()
  "Modify the note entry at point."
  (interactive)
  (assert (get-buffer "*durand-org-view-notes*") nil "No notes buffer alive")
  (let* ((note-choices
	  (with-current-buffer "*durand-org-view-notes*"
	    (org-map-entries (lambda ()
			       (list (buffer-substring-no-properties
				      (+ 2 (point))
				      (line-end-position))
				     (get-text-property (point) :note-meta))))))
	 (choice (ivy-read "Which note to modify? " note-choices
			   :require-match t
			   :caller 'durand-org-modify-note))
	 (choice-meta (cadr (assoc choice note-choices))))
    (goto-char (cadr choice-meta))))

;;;###autoload
(defun durand-org-delete-note ()
  "Delete the note entry at point"
  (interactive)
  (assert (get-buffer "*durand-org-view-notes*") nil "No notes buffer alive")
  (let* ((note-choices
	  (with-current-buffer "*durand-org-view-notes*"
	    (org-map-entries (lambda ()
			       (list (buffer-substring-no-properties
				      (+ 2 (point))
				      (line-end-position))
				     (get-text-property (point) :note-meta))))))
	 (choice (ivy-read "Which note to delete? " note-choices
			   :require-match t
			   :caller 'durand-org-delete-note))
	 (choice-meta (cadr (assoc choice note-choices)))
	 (beg (save-excursion
		(goto-char (car choice-meta))
		(forward-line -1)
		(point)))
	 (end (1+ (cadr choice-meta))))
    (kill-region beg end)))

(setq temp-buffer-max-height
      (/ (frame-height) 3))

(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

;; org clock
(setq org-clock-mode-line-total 'current)
;; (setq org-clock-persist t)
;; (org-clock-persistence-insinuate)

;; adjust org-goto styles
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)
(setq counsel-outline-face-style nil)
(setq counsel-org-goto-separator " ➜ ")

(define-prefix-command 'org-account-prefix-map)
(define-key org-account-prefix-map [?n] 'org-new-account)
(define-key org-account-prefix-map [?u] 'org-update-account)
(define-key org-account-prefix-map [?c] 'org-calc-account)
(define-key org-account-prefix-map [?r] 'org-run-src-block)
(define-key org-account-prefix-map [?i] 'org-set-item-price-note)
(define-key org-account-prefix-map [?m] 'org-modify-account)
(define-key org-account-prefix-map [?d] 'org-delete-account)
(define-key org-account-prefix-map [?g] 'org-account-go-to-day)
(define-key org-account-prefix-map [?G] 'org-account-go-to-last-day)
(define-key org-account-prefix-map [?q] 'org-clear-buffers)
(define-key org-account-prefix-map [tab] 'durand-forward-link)
(define-key org-account-prefix-map [S-tab] 'find-previous-link-in-buffer)
(define-key org-account-prefix-map [?v] 'durand-org-view-notes)

;;;###autoload
(defun org-clear-buffers ()
  "Clear all org mode buffers as well as /not needed/ buffers"
  (interactive)
  (progn
    (clean-up-buffers)
    (clean-up-buffers-regex "org$")))

(defmacro with-account (account-func)
  "Execute ACCOUNT-FUNC only when we are visiting an account file."
  (interactive)
  `(cond
    ((string-prefix-p "account" (buffer-name)) ,account-func)
    (t (user-error "\"%s\" is not an account file" (buffer-name)))))

;;;###autoload
(defun org-advance (x)
  (interactive "P")
  (when (buffer-narrowed-p)
    (beginning-of-buffer)
    (widen)
    (if (not x)
	(org-next-visible-heading 1)
      (org-forward-heading-same-level 1)))
  (org-narrow-to-subtree))

;;;###autoload
(defun org-retreat (x)
  (interactive "P")
  (when (buffer-narrowed-p)
    (beginning-of-buffer)
    (widen)
    (if (not x)
	(org-previous-visible-heading 1)
      (org-backward-heading-same-level 1)))
  (org-narrow-to-subtree))

;;;###autoload
;; (defun org-get-account-num ()
;;   "count how many days have been tagged 'account'"
;;   (interactive)
;;   (let ((org-use-tag-inheritance nil))
;;     (length (org-map-entries t "account"))))

;;;###autoload
;; (defun org-get-account-total ()
;;   "get the total value of the accuont values"
;;   (interactive)
;;   (let ((org-use-tag-inheritance nil))
;;     (apply '+ (mapcar 'string-to-number
;; 		      (org-map-entries (lambda ()
;; 					 (org-entry-get nil "total")) "account")))))

;;;###autoload
(defun org-get-account-entries (&optional day month year span)
  "Get all entries.
DAY, MONTH, YEAR can be specified to gather previous entries to the given date;
otherwise, use the current date."
  (let ((day (or day (cadr (calendar-current-date))))
	(month (or month (car (calendar-current-date))))
	(year (or year (caddr (calendar-current-date))))
	(span (if (equal span -1) nil span))
	res)
    (if (or (not (integerp day)) (not (integerp month)) (not (integerp year)))
	(message "%s %s %s" (integerp day) (integerp month) (integerp year))
      (dolist (jour (org-find-all-days) (nreverse res))
	(let ((date-spec (org-date-to-gregorian (car jour))))
	  (when (and (<= (cadr date-spec) day)
		     (or (null span) (< (- day (cadr date-spec)) span))
		     (= (car date-spec) month)
		     (= (caddr date-spec) year)
		     (< (- (cadr date-spec) day) 7))
	    (push jour res)))))))

;;;###autoload
(defun org-calc-account (&optional arg)
  "Sum up entries

Non-nil ARG asks for day;

`C-uC-u' asks for day, month, year, and span.

If ARG is a positive integer, it is the span.

If span is not specified or -1, then it calculates all entries in the
same month before the given date."
  (interactive "P")
  (with-account
   (let* ((day (and arg (read-number "Day: " (cadr (calendar-current-date)))))
	  (month (and (equal arg '(16)) (read-number "Month: " (car (calendar-current-date)))))
	  (year (and (equal arg '(16)) (read-number "Year: " (caddr (calendar-current-date)))))
	  (span (cond
		 ((and (integerp arg) (>= arg 0)) arg)
		 ((equal arg '(16)) (read-number "Span: " -1))
		 (t nil)))
	  (entries (org-get-account-entries day month year span))
	  (days (length entries))
	  (total (let ((cur 0))
		   (save-excursion
		     (dolist (entry entries cur)
		       (goto-char (cdr entry))
		       (re-search-forward org-date-tree-headline-regexp)
		       (setq cur (+ cur (string-to-number
					 (org-entry-get nil "total"))))))))
	  (ave (ignore-errors (/ total days))))
     (message (concat
	       (number-to-string days)
	       " days, spent "
	       (number-to-string total)
	       " with average "
	       (or
		(ignore-errors (number-to-string ave))
		"undefined"))))))

;;;###autoload
(defun org-find-all-days ()
  "Get all days information in TODO items
  The entry is supposed to be matched by `org-date-tree-headline-regexp`"
  (interactive)
  (with-account
   (let (res)
     (save-excursion
       (goto-char (point-min))
       (save-match-data
	 (while (re-search-forward org-date-tree-headline-regexp nil t)
	   (push (cons (match-string-no-properties 1)
		       (match-beginning 0))
		 res))))
     (nreverse res))))

(setq org-date-tree-headline-regexp "^\\*+\\s-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*$")

;;;###autoload
(defun org-find-last-day ()
  "Find the start position of last day entry"
  (interactive)
  (cdar (last (org-find-all-days))))

;;;###autoload
(defun org-find-pos-of-day (day)
  "Get the start position a specified day entry"
  (interactive)
  (let ((all-days (org-find-all-days)))
    (cdr (assoc day all-days))))

;;;###autoload
(defun org-update-account (&optional arg)
  "Update the last account.
With `C-u' prefix argument, update the chosen account;
with `C-uC-u' prefix argument, update all accounts."
  (interactive "P")
  (let* ((candidates (org-find-all-days))
	 (choice
	  (cond
	   ((null arg) (last candidates))
	   ((equal arg '(4)) (list (assoc
				    (ivy-read "Choose a day to update: " (mapcar #'car candidates)
					      :require-match t
					      :caller 'org-update-account)
				    candidates)))
	   ((equal arg '(16)) candidates)
	   (t 'wrong))))
      (if (eq choice 'wrong)
	  (message "Only prefix args of the form \"nil\", \"C-u\", or \"C-uC-u\" are allowed")
	(save-excursion
	  (dolist (date choice)
	    (let ((total 0))
	      (goto-char (cdr date))
	      (re-search-forward org-date-tree-headline-regexp nil t)
	      (org-map-tree (lambda ()
			      (setq total (+ total
					     (string-to-number
					      (or (org-entry-get (point) "cost")
						  "0"))))))
	      (org-set-property "total" (number-to-string total))
	      (org-set-property "dashes" (make-string (if (> total 600) 60 (/ total 10)) ?-))))))))

;;;###autoload
(defun org-get-account-fields ()
  "Find all items under the current day"
  (interactive)
  (with-account
   (save-excursion
     (save-match-data
       (beginning-of-line)
       (unless (looking-at org-date-tree-headline-regexp)
	 (re-search-forward org-date-tree-headline-regexp nil t -1))
       (re-search-forward org-date-tree-headline-regexp)
       (let (res pos)
	 (org-map-tree (lambda ()
			 (setq pos (point))
			 (skip-chars-forward "* ")
			 (push (cons
				(buffer-substring-no-properties (point) (line-end-position))
				(list (org-entry-get (point) "cost") pos))
			       res)))
	 (cdr (nreverse res)))))))

;;;###autoload
(defun org-modify-account ()
  "Modify the account entries under the current day"
  (interactive)
  (with-account
   (let* ((account-field-list (org-get-account-fields))
	  (target-entry (ivy-read "Choose one entry: "
				  (mapcar #'car account-field-list)
				  :require-match t
				  :caller 'org-modify-account))
	  (target-position (caddr (assoc target-entry account-field-list)))
	  (target-item (read-string "New item: " target-entry))
	  (target-price (read-string "New price: "
				     (cadr (assoc target-entry account-field-list))))
	  (note-change-p (y-or-n-p "Change note?")))
     (goto-char target-position)
     (skip-chars-forward "* ")
     (re-search-forward ".*$")
     (replace-match target-item)
     (org-set-property "cost" target-price)
     (when note-change-p
       (org-end-of-meta-data t)))))

;;;###autoload
(defun org-delete-account ()
  "Modify the account entries under the current day"
  (interactive)
  (with-account
   (let* ((account-field-list (org-get-account-fields))
	  (target-entry (ivy-read "Choose one entry: "
				  (mapcar #'car account-field-list)
				  :require-match t
				  :caller 'org-modify-account))
	  (target-position (caddr (assoc target-entry account-field-list))))
     (goto-char target-position)
     (skip-chars-forward "* ")
     (org-mark-subtree)
     (delete-region (region-beginning) (region-end)))))

;;;###autoload
(defun org-set-account-according-to-date (date &optional month year)
  "Update accounts tag according to DATE.
  DATE is an integer representing a date in month MONTH and year YEAR.
  MONTH and YEAR default to the current ones.
  This means if a date has the same quotient as DATE when
  divided by 7, then it will be tagged `account';
  otherwise it will have no tags."
  (with-account
   (save-excursion
     (outline-show-all)
     (dolist (running-day (org-find-all-days))
       (goto-char (cdr running-day))
       (re-search-forward org-date-tree-headline-regexp nil t)
       (let ((day (cadr (org-date-to-gregorian (car running-day)))))
	 (org-set-tags-to (cond ((and (<= day date)
				      (> (+ 7 day) date)) ":account:")
				(t nil)))
	 (org-set-tags t t)))))
  (outline-hide-body))

;;;###autoload
(defun org-day-format-transform (day &optional month year)
  "Take an integer DAY and transform it to a string.
  For example,
  (org-day-format-transform 1)
  when executed in August 2018 becomes
  => \"2018-08-01\""
  (let* ((day-string (pad-string-to (format "%d" day) 2))
	 (padded-date-string-list (mapcar (lambda (x) (pad-string-to (format "%d" x) 2))
					  (calendar-current-date)))
	 (month (or (and month (pad-string-to (format "%d" month) 2))
		    (car padded-date-string-list)))
	 (year (or (and year (pad-string-to (format "%d" year) 2))
		   (caddr padded-date-string-list))))
    (concat year
	    "-"
	    month
	    "-"
	    day-string)))

;;;###autoload
(defun pad-string-to (str num)
  "Pad a string STR to be of length greater than or equal to NUM with 0"
  (cond ((< (length str) num)
	 (concat (make-string (- num (length str)) ?0) str))
	(t
	 str)))

;;;###autoload
(defun org-run-src-block ()
  "Search for a src block and run it"
  (interactive)
  (with-account
   (save-excursion
     (re-search-forward "BEGIN_SRC")
     (org-babel-execute-src-block))))

;;;###autoload
(defun org-set-item-price-note (item-name item-price item-note)
  (interactive (let ((item (ivy-read "Enter item: "
				     '("breakfast" "brunch" "brunverage"
				       "lunch" "dinner" "beverage")
				     :caller 'org-set-item-price-note))
		     (price (read-number "Enter price: " 0))
		     (note (read-string "Enter note: " nil nil "todo")))
		 (list item price note)))
  (with-account
   (progn (end-of-buffer)
	  (outline-show-all)
	  (re-search-backward "tblfm")
	  (previous-line)
	  (org-table-insert-row 1)
	  (org-table-insert-hline)
	  (org-table-put (org-table-current-line) (org-table-current-column) item-name)
	  (org-table-put (org-table-current-line) (1+ (org-table-current-column)) (number-to-string item-price))
	  (org-table-put (org-table-current-line) (+ 2 (org-table-current-column)) item-note t)
	  (outline-hide-body))))

;;;###autoload
(defun org-delete-item-price-note (row-num &optional total-num)
  (interactive (let* ((total-num (save-excursion
				   (end-of-buffer)
				   (outline-show-all)
				   (re-search-backward "tblfm")
				   (previous-line 2)
				   (org-table-current-line)))
		      (num (ivy-read "Enter row number: "
				     (mapcar #'number-to-string (number-sequence 1 total-num))
				     :caller 'org-delete-item-price-note)))
		 (list (string-to-number num) total-num)))
  (with-account
   (progn
     (end-of-buffer)
     (outline-show-all)
     (re-search-backward "tblfm")
     (previous-line 2)
     (org-table-goto-line row-num)
     (kill-whole-line 2)
     (outline-hide-body))))

  ;;;###autoload
;; (defun org-account-go-to-day (day)
;;   "Go to the position of day DAY"
;;   (interactive (list (ivy-read "DAY: " (org-find-all-days))))
;;   (with-account
;;    (let* ((name (buffer-file-name))
;; 	  (first-dash (search "-" name))
;; 	  (last-dash (search "-" name :from-end t))
;; 	  (first-dot (search "." name))
;; 	  (year (string-to-number
;; 		 (substring name (1+ first-dash) last-dash)))
;; 	  (month (string-to-number
;; 		  (substring name (1+ last-dash) first-dot)))
;; 	  (day (string-to-number day))
;; 	  (date (org-day-format-transform day month year)))
;;      (progn
;;        (widen)
;;        (outline-hide-sublevels 2)
;;        (goto-char (org-find-pos-of-day date))
;;        (outline-show-entry)
;;        (recenter-top-bottom 0)
;;        (org-narrow-to-subtree)))))

;;;###autoload
(defun org-account-go-to-day (day &optional no-narrowp)
  "Go to the position of day DAY"
  (interactive (list (ivy-read "DAY: "
			       (if current-prefix-arg
				   (org-find-all-days)
				 (save-restriction
				   (widen)
				   (org-find-all-days)))
			       :re-builder 'ivy<span style="color: rgb(6,144,255)">--regex-ignore-order)
		     current-prefix-arg))
  (with-account
   (if no-narrowp
       (progn
	 (outline-hide-sublevels 2)
	 (goto-char (org-find-pos-of-day day))
	 (outline-show-children)
	 (recenter-top-bottom 0))
     (progn
       (widen)
       (outline-hide-sublevels 2)
       (goto-char (org-find-pos-of-day day))
       (recenter-top-bottom 0)
       (org-narrow-to-subtree)
       (outline-show-children)))))

;;;###autoload
(defun org-account-go-to-last-day (&optional arg)
  "Go to the position of day DAY"
  (interactive "P")
  (if arg
      (with-account
       (progn
	 (goto-char (org-find-last-day))
	 (outline-show-children)
	 (recenter-top-bottom 0)))
    (with-account
     (progn
       (widen)
       (outline-hide-sublevels 2)
       (goto-char (org-find-last-day))
       (outline-show-children)
       (recenter-top-bottom 0)
       (org-narrow-to-subtree)))))

;;;###autoload
(defun find-next-link-in-buffer (&optional arg)
  "
  Navigate to the links in the buffer \"without setting marks\";

  If ARG is nil, then go to the next link.
  If ARG is non-nil, then it is interpreted according to the interactive form \"p\""
  (interactive "p")
  (let ((search-count (or arg 1)))
    (re-search-forward "\\[\\[[^][]+]\\[[^][]+]]" nil t search-count)
    (backward-char 1)))

;;;###autoload
(defun find-previous-link-in-buffer (&optional arg)
  "
  Navigate to the links in the buffer \"without setting marks\";

  If ARG is nil, then go to the previous link.
  If ARG is non-nil, then it is interpreted according to the interactive form \"p\"

  This is a convenient variant of `find-next-link-in-buffer'"
  (interactive "p")
  (let ((search-count (or arg 1)))
    (re-search-backward "\\[\\[[^][]+]\\[[^][]+]]" nil t search-count)
    (forward-char 1)))

;; Last week day of the month
;; it's the last day of the month & it is a weekday
;; it's a friday, and it's the last-but-one or last-but-two day
;; of the month
(defun last-week-day-of-month-p (date)
  (let* ((day-of-week (calendar-day-of-week date))
	 (month (calendar-extract-month date))
	 (year (calendar-extract-year date))
	 (last-month-day (calendar-last-day-of-month month year))
	 (month-day (cadr date)))
    (or (and (eq month-day last-month-day)
	     (memq day-of-week '(1 2 3 4 5)))
	(and (eq day-of-week 5)
	     (or (eq month-day
		     (1- last-month-day))
		 (eq month-day
		     (- last-month-day 2)))))))

;; select week days
(defun selected-week-days (list-of-days date)
  "Return true if the date is in one of the days of week"
  (let ((day-of-week (calendar-day-of-week date)))
    (not (null
	  (memq day-of-week list-of-days)))))

;; just in case I need this
;; (defun org-retrieve-value ()
;;   "retrieve value from property drawer"
;;   (org-element-map (org-element-parse-buffer) 'property-drawer (lambda (hl)
;; 								 (nth 3 (nth 1 (assoc 'node-property hl))))))

;; (interactive (let ((c-day (cadr (calendar-current-date)))
;; 		     (c-month (car (calendar-current-date)))
;; 		     (c-year (caddr (calendar-current-date))))
;; 		 (cond
;; 		  ((null current-prefix-arg)
;; 		   (list c-day c-month c-year))
;; 		  ((equal current-prefix-arg '(4))
;; 		   (list (read-number "Day: " c-day) c-month c-year))
;; 		  ((equal current-prefix-arg '(16))
;; 		   (list
;; 		    (read-number "Day: " c-day)
;; 		    (read-number "Month: " c-month)
;; 		    (read-number "Year: " c-year)))
;; 		  (t
;; 		   (progn
;; 		     (message "Only \"C-u\" and \"C-uC-u\" and \"nil\" are accepted")
;; 		     (list 'wrong 'wrong 'wrong))))))

;; org-mime
(add-to-list 'load-path "~/.emacs.d/my_packages/org-mime/")
(require 'org-mime)

;; (defun org-mime-change-signature-color ()
;;   "Change the color of the signature of an email to rgb(6,144,255)"
;;   (goto-char (point-max))
;;   (when (search-backward "&#x2013;" nil t)
;;     (forward-line)
;;     (insert "<span style=\"color: rgb(6,144,255)\">")
;;     (forward-line)
;;     (insert "</span>")))

;; (remove-hook 'org-mime-html-hook 'org-mime-change-signature-color)

;; The process to attach color to signature in a mail:
;; In header, run (org-mu4e-compose-org-mode)
;; Then type the mail in the body
;; Then run (org-mime-htmlize)
;; Finally run (message-send-and-exit)
;; In theory the color of the signature will be correctly adjusted this way.

;; Now the signature is automatically blue-ish. To change it bak to
;; normal, just delete the export block and do not run
;; (org-mime-htmlize).