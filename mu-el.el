;; elfeed
;;;###autoload
(defun durand-lighten-name (name percent &optional digit)
  "Lighten color NAME by PERCENT.
     The defualt function gives four digits spec, not useful.
     So you can specify digits per component by DIGIT."
  (let ((digit (or digit 2)))
    (apply 'color-rgb-to-hex
	   (append
	    (apply 'color-hsl-to-rgb
		   (apply 'color-lighten-hsl
			  (append
			   (apply 'color-rgb-to-hsl
				  (color-name-to-rgb name))
			   (list percent))))
	    (list digit)))))

(use-package elfeed
  :ensure t
  :config
  (global-set-key [?\C-x ?w] 'elfeed))

;; Sometimes elfeed crashes emacs, and I cannot find the reason.
(setq elfeed-feeds
      '(("http://nullprogram.com/feed/"
	 program)
	("http://planet.emacsen.org/atom.xml"
	 emacs
	 blog)
	("https://lukesmith.xyz/rss.xml"
	 luke
	 blog)
	("https://stackexchange.com/feeds/tagsets/347224/favorite-tags?sort=active"
	 stackexchange
	 favorite)
	("https://stackexchange.com/feeds/tagsets/347226/real-love?sort=active"
	 real-love
	 interests)
	("https://www.reddit.com/r/emacs/.rss"
	 emacs
	 reddit)
	("https://math.stackexchange.com/feeds/tag?tagnames=number-theory&sort=newest"
	 interests
	 favorite)
	("https://mattbaker.blog/feed/"
	 interests
	 mattbaker
	 blog)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCTfRwznpxtbjQQQJ_15Fk2w"
	 youtube
	 3M)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw"
	 youtube
	 3blue1brown)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCyC_4jvPzLiSkJkLIkA7B8g"
	 youtube
	 music
	 lindsey)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCPRWWKG0VkBA0Pqa4Jr5j0Q"
	 youtube
	 joeman)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCjhwHd3mgmqm0ONm0bXKmng"
	 youtube
	 anju)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCcXhhVwCT6_WqjkEniejRJQ"
	 wintergarten
	 youtube)
	("https://math.stackexchange.com/feeds/question/2883754"
	 math
	 relation
	 important)
	("https://haskellweekly.news/haskell-weekly.atom"
	 haskell
	 relevant
	 blog)
	("https://themonadreader.wordpress.com/feed/"
	 monad-reader
	 blog
	 important)))
(with-eval-after-load 'elfeed
  (add-to-list 'elfeed-search-face-alist
	       '(emacs elfeed-emacs-face))
  (add-to-list 'elfeed-search-face-alist
	       '(relevant elfeed-relevant-face))
  (add-to-list 'elfeed-search-face-alist
	       '(important elfeed-math-face))
  (add-to-list 'elfeed-search-face-alist
	       '(youtube elfeed-youtube-face))
  ;; my primary interests
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :feed-url "math.*stackexchange"
				:feed-title "number\\|class\\|algebraic\\|field\\|elliptic\\|cohomology\\|group"
				:add '(math important)))
  ;; don't mark old ones as unread
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :before "1 week ago"
				:remove 'unread))
  ;; my secondary interests
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :feed-url "math.*stackexchange"
				:feed-title "geometry\\|topology\\|hodge\\|graph"
				:add '(math relevant)))
  (define-key elfeed-search-mode-map [?i] (lambda () (interactive)
					    (elfeed-search-set-filter "@1week-ago +important")))
  (define-key elfeed-search-mode-map [?l] (lambda () (interactive)
					    (elfeed-search-set-filter "@1week-ago +relevant")))
  (define-key elfeed-search-mode-map [?e] (lambda () (interactive)
					    (elfeed-search-set-filter "@1week-ago +emacs")))
  (define-key elfeed-search-mode-map [?b] 'elfeed-visit-or-play-with-mpv)
  (define-key elfeed-show-mode-map [?b] 'elfeed-visit-or-play-with-mpv))

(setq-default elfeed-search-filter "@1week-ago +unread")

(defface elfeed-math-face
  `((t . (:background "gray10" :foreground "deep sky blue")))
  "face for math feed")

(defface elfeed-relevant-face
  `((t . (:background "gray10" :foreground "light blue")))
  "face for relevant feed")

(defface elfeed-emacs-face
  `((t . (:background "gray10" :foreground "orange")))
  "face for relevant feed")
(defface elfeed-youtube-face
  `((t . (:background "gray10" :foreground "yellow")))
  "face for youtube feed")

;; play youtube video
;;;###autoload
(defun durand-play-with-mpv (quality)
  "
If currently visiting a youtube feed entry or if the cursor is on a youtube feed entry,
then play the video with mpv with QUALITY, else just inform this is not a youtube link."
  (interactive (list (durand-get-quality-val)))
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
		   elfeed-show-entry
		 (elfeed-search-selected t)))
	(quality-arg "")
	(quality-val quality)
	(fit-arg "--autofit=100%x100%"))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height ≤ %s with mpv..."
	     (elfeed-entry-link entry)
	     quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]"
				quality-val)))
    ;; (start-process "elfeed-mpv" nil "mpv" quality-arg fit-arg (elfeed-entry-link entry))
    (eshell)
    (insert (format "mpv --no-terminal %s %s %s &"
		    quality-arg
		    fit-arg
		    (elfeed-entry-link entry)))
    (eshell-send-input)))

(defvar elfeed-mpv-patterns
  '("youtu\\.?be")
  "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.
Default value is \"youtu\\.?be\"")

;;;###autoload
(defun elfeed-visit-or-play-with-mpv ()
  "Play in mpv if entry link matches `elfeed-mpv-patterns'; visit it otherwise.
See `durand-play-with-mpv' also."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
		   elfeed-show-entry
		 (elfeed-search-selected t)))
        (patterns elfeed-mpv-patterns))
    (while (and patterns
		(not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
        (call-interactively 'durand-play-with-mpv)
      (if (eq major-mode 'elfeed-search-mode)
          (elfeed-search-browse-url)
        (elfeed-show-visit)))))

;;;###autoload
(defun durand-get-quality-val ()
  "Let the user choose a quality format."
  (ivy-read "Max height resolution (0 for unlimited): "
	    '("0" "480" "720")
	    :caller 'durand-get-quality-val))

;; The code for clearing up data-base
;; (with-elfeed-db-visit (entry _)
;;   (setf (elfeed-entry-content entry) nil))

;; (elfeed-db-gc)

;; (defvar durand-pattern-alist '(("math" . 1)
;; 			       ("number.*theory" . 2)
;; 			       ("elliptic" . 2)
;; 			       ("cyclotomic" . 2)
;; 			       ("cohomology" . 2))
;;   "Patterns to match against an elfeed entry in auto-tagging, along with an attached score.")

;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu")
(require 'mu4e)
(global-set-key [?\C-x ?m] 'mu4e)

(setq mu4e-maildir (expand-file-name "~/mbsync"))
(setq mu4e-get-mail-command "mbsync gmail") ; mbsync works a lot better!
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-view-show-images t)
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-use-fancy-chars t)
(setq mu4e-update-interval nil)
(setq mu4e-completing-read-function 'ivy-completing-read)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
	("/archive" . ?a)))

(defun mu4e-view-attach-emacs (n)
  "Open the N-th attachment in emacs"
  (interactive (let ((num (read-number "Enter attachment number: " 1)))
		 (list num)))
  (mu4e-view-open-attachment-emacs (mu4e-message-at-point) n))

(define-key mu4e-view-mode-map [?o] 'mu4e-view-attach-emacs)

(add-to-list 'mu4e-view-actions
	     '("Browse this mail" . mu4e-action-view-in-browser))

(add-to-list 'mu4e-view-actions
	     '("view attachment in pdf-view" . mu4e-view-attach-emacs))

(remove '("view as pdf" . mu4e-action-view-as-pdf) mu4e-view-actions)


(setq mu4e-contexts
      `(,(make-mu4e-context
	  :name "BaoBao"
	  :enter-func (lambda () (mu4e-message "Entering 寶寶 context"))
	  :leave-func (lambda () (mu4e-message "Leaving 寶寶 context"))
	  ;; we match based on the contact-fields of the message
	  :match-func (lambda (msg)
			(when msg
			  (or
			   (mu4e-message-contact-field-matches msg :to "lingtingtsen@gmail.com")
			   (mu4e-message-contact-field-matches msg :from "lingtingtsen@gmail.com"))))
	  :vars '( ( user-mail-address	    . "mmemmew@gmail.com"  )
		   ( user-full-name	    . "大寶寶" )
		   ( mu4e-compose-signature .
					    "愛你的寶寶")
		   (mu4e-sent-folder . "/gmail/sent")
		   (smtpmail-smtp-user . "mmemmew")
		   (smtpmail-local-domain . "gmail.com")
		   (smtpmail-default-smtp-server . "smtp.gmail.com")
		   (smtpmail-smtp-server . "smtp.gmail.com")
		   (smtpmail-smtp-service . 587)))
	,(make-mu4e-context
	  :name "Awllower"
	  :enter-func (lambda () (mu4e-message "Entering Awllower context"))
	  :leave-func (lambda () (mu4e-message "Leaving Awllower context"))
	  ;; we match based on the contact-fields of the message
	  :match-func (lambda (msg)
			(when msg
			  (or
			   (mu4e-message-contact-field-matches msg :to "mmemmew@gmail.com")
			   (mu4e-message-contact-field-matches msg :from "mmemmew@gmail.com"))))
	  :vars '( ( user-mail-address	    . "mmemmew@gmail.com"  )
		   ( user-full-name	    . "Awllower" )
		   ( mu4e-compose-signature .
					    (concat
					     "Sincerely Yours,\n"
					     "Jean Sévère Durand"))
		   (mu4e-sent-folder . "/gmail/sent")
		   (smtpmail-smtp-user . "mmemmew")
		   (smtpmail-local-domain . "gmail.com")
		   (smtpmail-default-smtp-server . "smtp.gmail.com")
		   (smtpmail-smtp-server . "smtp.gmail.com")
		   (smtpmail-smtp-service . 587)))
	,(make-mu4e-context
	  :name "NCTS"
	  :enter-func (lambda () (mu4e-message "Switch to the NCTS context"))
	  :leave-func (lambda () (mu4e-message "Leave NCTS context"))
	  ;; no leave-func
	  ;; we match based on the maildir of the message
	  ;; this matches maildir /Arkham and its sub-directories
	  :match-func (lambda (msg)
			(when msg
			  (or
			   (mu4e-message-contact-field-matches msg :to "chunweilee@ncts.ntu.edu.tw")
			   (mu4e-message-contact-field-matches msg :from "chunweilee@ncts.ntu.edu.tw"))))
	  :vars '( ( user-mail-address	     . "chunweilee@ncts.ntu.edu.tw" )
		   ( user-full-name	     . "李俊緯" )
		   ( mu4e-compose-signature  .
					     (concat
					      "Sincerely Yours,\n"
					      "俊緯"))))))

;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
;; (setq mu4e-compose-context-policy nil)
