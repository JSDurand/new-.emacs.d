;; For fetching novels

(defvar novel-progress-alist '(("https://www.piaotian.com/html/7/7430/index.html" . 2168)
			       ("https://www.piaotian.com/html/9/9292/index.html" . 72))
  "Alist to track the current progress.")

;; the old value was '(("https://www.piaotian.com/html/7/7430/index.html" . 0) ("https://www.piaotian.com/html/8/8444/index.html" . 0) ("https://www.piaotian.com/html/3/3597/index.html" . 0)), but now the other novels are finished.

(defvar novel-pages nil
  "list to store page specific htmls")
(defvar current-url nil
  "list to store current url")
(defvar novel-title nil
  "current novel title")

;;;###autoload
(defun durand-update-novel-alist (url num)
  "Update the value associated with URL to NUM"
  (setf (cdr (assoc url novel-progress-alist)) num))

;;;###autoload
(defun durand-check-novel (url &optional ask)
  "Fetch URL and check if that is updated and print the information in echo area.

URL must be pre-setted, as the current number of entries depends upon variables.

If ASK is non-nil, ask before reading in emacs."
  (interactive (list (ivy-read "Select URL: "
			       (mapcar #'car novel-progress-alist)
			       :require-match t
			       :caller 'durand-check-novel)
		     current-prefix-arg))
  (setq novel-pages nil)
  (when (get-buffer "*novel*")
    (with-current-buffer "*novel*" (erase-buffer)))
  (call-process (expand-file-name "scripts/novel-checker" user-emacs-directory)
		nil
		"*novel*"
		nil
		url)
  (let* ((str (with-current-buffer "*novel*"
		(buffer-substring-no-properties (point-min)
						(progn
						  (beginning-of-buffer)
						  (end-of-line)
						  (point)))))
	 (num (string-to-number str))
	 (old-num (cdr (assoc url novel-progress-alist))))
    (if (/= old-num num)
	(progn
	  ;; (message "%d pages more!" (- num old-num))
	  (with-current-buffer "*novel*" (erase-buffer))
	  (call-process (expand-file-name "scripts/novel-viewer" user-emacs-directory)
			nil
			"*novel*"
			nil
			url
			(number-to-string (- num old-num)))
	  (let ((str (with-current-buffer "*novel*"
		       (buffer-substring-no-properties
			(point-min)
			(progn
			  (beginning-of-buffer)
			  (re-search-forward "html")
			  (point))))))
	    (and (or
		  (null ask)
		  (y-or-n-p "View in emacs? "))
		 (durand-read-novel (concat (file-name-directory url) str))))
	  (re-search-forward "\\s-" nil t)
	  (with-current-buffer "*novel*"
	    (while (save-excursion (re-search-forward "html" nil t))
	      (push (buffer-substring-no-properties
		     (point)
		     (progn
		       (re-search-forward "html")
		       (point)))
		    novel-pages)
	      (re-search-forward "\\s-" nil t)))
	  (setq novel-pages (mapcar
			     (lambda (s) (replace-regexp-in-string " " "" s))
			     (nreverse novel-pages)))
	  (kill-buffer "*novel*"))
      (progn
	(message "Nothing new!")
	(when (get-buffer "*novel*")
	  (kill-buffer "*novel*"))))))

;;;###autoload
(defun durand-read-previous-page ()
  "Read the previous page"
  (interactive)
  (let* ((current-novel (concat (file-name-directory current-url) "index.html"))
	 (current-page (cdr (assoc current-novel novel-progress-alist))))
    (if (<= current-page 1)
	(message "No previous page to read!")
      (durand-update-novel-alist current-novel (max 0 (- current-page 2)))
      (durand-check-novel current-novel))))

(defface novel-face
  '((t :background "#efe0d0"
       :foreground "black"
       :height 300))
  "Face used for reading novels in emacs")

(define-derived-mode novel-mode view-mode "NOVEL" "Major mode for reading novels in emacs"
  ;; (if (equal major-mode 'novel-mode)
  ;;     (setq durand-custom-modeline (format "%d" (length novel-pages)))
  ;;   (setq durand-custom-modeline nil))
  )

(define-key novel-mode-map [tab] (lambda ()
				   (interactive)
				   (if (not (null novel-pages))
				       (durand-read-novel
					(concat (file-name-directory current-url) (pop novel-pages)))
				     (message "No next page!"))))

(define-key novel-mode-map [S-tab] 'durand-read-previous-page)

(add-hook 'novel-mode-hook (lambda ()
			     "Some decorations for novel mode"
			     (interactive)
			     (progn
			       (toggle-truncate-lines -1)
			       (face-remap-add-relative 'default 'novel-face))))

;;;###autoload
(defun durand-read-novel (url &optional ask-update)
  "
Fetch novel content from the given URL, and then parse the
json file, and finally display the proper content in a dedicated buffer.
If called interactively, user will be prompted for the URL to use.
If ASK-UPDATE is non-nil, ask before updating alist."
  (interactive (list (read-string "Enter URL: ") current-prefix-arg))
  (message "Reading %s" url)
  (setq current-url url)
  (when (get-buffer "*Novel Reader*")
    (with-current-buffer "*Novel Reader*"
      (erase-buffer)))
  (let* ((root (concat (file-name-directory url) "index.html"))
	 (old-val (cdr (assoc root novel-progress-alist))))
    (and (or (not ask-update)
	     (y-or-n-p "Update alist by 1? "))
	 (durand-update-novel-alist root (1+ old-val))))
  (call-process (expand-file-name "scripts/novel-reader" user-emacs-directory)
		nil
		"*Novel Reader*"
		nil
		url)
  (find-file (expand-file-name "scripts/reader.html" user-emacs-directory))
  (fundamental-mode)
  (erase-buffer)
  (insert-buffer-substring "*Novel Reader*")
  (progn
    (advice-remove 'save-buffer 'save-tex-advice)
    (save-buffer 0))
  (with-current-buffer "*Novel Reader*"
    (erase-buffer))
  (call-process (expand-file-name "scripts/novel-title" user-emacs-directory)
		nil
		"*Novel Reader*"
		nil
		url)
  (with-current-buffer "*Novel Reader*"
    (setq novel-title (buffer-string)))
  (require 'json)
  (setq temp-var (json-read-file (buffer-file-name)))
  (erase-buffer)
  (let ((raw (cdr (elt (cdr (elt temp-var 0)) 2))))
    (insert (concat novel-title
		    "\n\n"
		    (mapconcat
		     #'identity
		     (split-string raw "。")
		     "\n"))))
  (goto-char (point-min))
  (novel-mode)
  (ignore-errors (save-buffer 0))
  (when (get-buffer "*Novel Reader*")
    (kill-buffer "*Novel Reader*"))
  (message "%s" (propertize
		 (format "%d page%s more"
			 (length novel-pages)
			 (if (> (length novel-pages) 1)
			     "s"
			   ""))
		 'face '(:foreground "light blue"))))


