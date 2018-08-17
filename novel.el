;; For fetching novels

(defvar novel-progress-alist '(("https://www.piaotian.com/html/7/7430/index.html" . 0)
			       ("https://www.piaotian.com/html/8/8444/index.html" . 0))
  "Alist to track the current progress.")

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
(defun durand-check-novel (url)
  "Fetch URL and check if that is updated and print the information in echo area.

URL must be pre-setted, as the current number of entries depends upon variables."
  (interactive (list (ivy-read "Select URL: "
			       '("https://www.piaotian.com/html/7/7430/index.html"
				 "https://www.piaotian.com/html/8/8444/index.html")
			       :require-match t
			       :caller 'durand-check-novel)))
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
	  (let ((y-or-n (y-or-n-p (format "New value: %d, update alist? " num))))
	    (and y-or-n
		 (durand-update-novel-alist url num)))
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
			  (point)))))
		(yn (y-or-n-p "View in emacs? ")))
	    (and yn
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
      (message "Nothing new!"))))

(defface novel-face
  '((t :background "#efe0d0"
       :foreground "black"
       :height 300))
  "Face used for reading novels in emacs")

;;;###autoload
(defun durand-read-novel (url)
  "
Fetch novel content from the given URL, and then parse the
json file, and finally display the proper conttet in a dedicated buffer."
  (interactive (list (read-string "Enter URL: ")))
  (setq current-url url)
  (when (get-buffer "*Novel Reader*")
    (with-current-buffer "*Novel Reader*"
      (erase-buffer)))
  (call-process (expand-file-name "scripts/novel-reader" user-emacs-directory)
		nil
		"*Novel Reader*"
		nil
		url)
  (find-file (expand-file-name "scripts/reader.html" user-emacs-directory))
  (fundamental-mode)
  (erase-buffer)
  (insert-buffer-substring "*Novel Reader*")
  (save-buffer 0)
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
  (toggle-truncate-lines -1)
  (face-remap-add-relative 'default 'novel-face)
  (local-set-key "\C-cn" (lambda ()
			   (interactive)
			   (if (not (null novel-pages))
			       (durand-read-novel
				(concat (file-name-directory current-url) (pop novel-pages)))
			     (message "No next page!"))))
  (view-mode 1)
  (save-buffer 0))


