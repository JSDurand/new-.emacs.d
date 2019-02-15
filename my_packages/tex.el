;;;###autoload
(defun tex ()
  "Command to compile TeX
If TEX-ROOT-FILE-NAME is non-nil, use it as the file name to compile,
else use the current file name.
I found using start-process better, as it is more minimal."
  (interactive)
  (unless (derived-mode-p 'tex-mode)
    (user-error "Not in a tex buffer"))
  (let* ((fnb (or tex-root-file-name
                  (file-name-base)))
         (bfn (buffer-file-name))
         (durand-tex-command (or (and (string= major-mode "plain-tex-mode") "xetex")
                                 "xelatex"))
         (output-bn "*durand-tex*")
         (default-directory (or (and bfn (file-name-directory bfn))
                                (error "Ce tampon n'est pas associé à un fichier")))
         (proc (progn
                 (when (get-buffer output-bn)
                   (kill-buffer output-bn))
                 (call-process "texfot" nil "*durand-tex*" t durand-tex-command fnb))))
    (setf tex-output-bn output-bn)
    (tex-display-or-kill-temp-buffer nil output-bn))
  (define-key plain-tex-mode-map [f9] 'tex-first-pdf)
  (setf durand-tex-action 'tex-first-pdf)
  (setf tex-changed t))

;;;###autoload
(defun tex-display-or-kill-temp-buffer (&optional arg bn)
  "display and fit to size the buffer BN
If ARG is non-nil, delete the buffer BN"
  (interactive (list current-prefix-arg nil))
  (let ((bn (or bn tex-output-bn)))
    (cond
     (arg
      (when (get-buffer bn)
        (delete-windows-on bn)
        (kill-buffer bn)))
     ((and bn
           (get-buffer bn)
           (buffer-live-p (get-buffer bn))
           (get-buffer-window bn))
      (delete-windows-on bn))
     ((and bn (get-buffer bn) (buffer-live-p (get-buffer bn)))
      (with-current-buffer bn
        (goto-char (point-min)))
      (display-buffer bn)
      (resize-temp-buffer-window (get-buffer-window bn)))
     ((null bn)
      (message "`tex-output-bn' is nil."))
     (t
      (message "No buffer named %s." bn)))))

;; using eshell is not very good.
;; (defun tex ()
;;   "Command to compile TeX
;; If TEX-ROOT-FILE-NAME is non-nil, use it as the file name to compile,
;; else use the current file name.
;; I found using eshell better, as it allows to interact with xetex directly,
;; and correct the result (temporarily) live in the shell."
;;   (interactive)
;;   (let ((fnb (or tex-root-file-name
;;                  (file-name-base)))
;;         (durand-tex-command (or (and (string= major-mode "plain-tex-mode")
;;                                      "texfot xetex")
;;                                 "xelatex")))
;;     (progn
;;       (eshell)
;;       (insert (format "%s %s" durand-tex-command (shell-quote-argument fnb)))
;;       (eshell-send-input)
;;       ;; (define-key eshell-mode-map [f9] (lambda ()
;;       ;; 					 (interactive)
;;       ;; 					 (tex-switcher (buffer-file-name (other-buffer)))))
;;       (define-key eshell-mode-map [f9] 'tex-action-eshell))))

(defvar tex-root-file-name nil
  "Root file for tex")
(defvar working-name nil
  "working file name for tex")
(defvar tex-changed nil
  "Non-nil if just used `tex'")
(defvar tex-output-bn nil
  "The name of the buffer used for displaying the output of tex process")

(make-variable-buffer-local 'tex-output-bn)
(make-variable-buffer-local 'tex-changed)

;;;###autoload
(defun tex-action-eshell ()
  "The function invoked when in eshell during a tex-working cycle"
  (interactive)
  (let* ((full-name (buffer-file-name (other-buffer)))
         (full-pdf-name (concat (or tex-root-file-name
                                    (file-name-sans-extension full-name))
                                ".pdf"))
         (pdf-name (or (and tex-root-file-name
                            (concat tex-root-file-name ".pdf"))
                       (file-name-nondirectory full-pdf-name))))
    (if (get-buffer pdf-name)
        (switch-to-buffer pdf-name)
      (find-file full-pdf-name))
    (tex-pdf-prepare)))

;;;###autoload
(defun tex-pdf-prepare ()
  "Preparative work needed in pdf buffer"
  (revert-buffer)
  (delete-other-windows)
  (define-key pdf-view-mode-map [f9] 'tex-action-pdf)
  (setf durand-tex-action 'tex-action-pdf))

;;;###autoload
(defun tex-action-pdf ()
  "Action invoked in pdf buffer in a tex working cycle"
  (interactive)
  (switch-to-buffer (or working-name
                        (concat (file-name-sans-extension (buffer-name)) ".tex")))
  (advice-add 'save-buffer :before 'save-tex-advice)
  (define-key plain-tex-mode-map [f9] 'tex-tex-go-to-pdf)
  (define-key latex-mode-map [f9] 'tex-tex-go-to-pdf)
  (setf durand-tex-action 'tex-pdf-go-to-tex-or-vice-versa))

;;;###autoload
(defun tex-first-pdf ()
  "First time switching to pdf"
  (interactive)
  (let ((corresponding-pdf (concat (or tex-root-file-name
                                      (file-name-sans-extension (buffer-name)))
                                  ".pdf")))
    (cond
     ((get-buffer corresponding-pdf)
      (switch-to-buffer corresponding-pdf)
      (tex-pdf-prepare))
     ((file-exists-p corresponding-pdf)
      (find-file corresponding-pdf)
      (tex-pdf-prepare))
     (t
      (message "Cannot find pdf named %s" corresponding-pdf)))))

;;;###autoload
(defun tex-tex-go-to-pdf ()
  "Go to the corresponding pdf file to the current tex file"
  (interactive)
  (let ((corresponding-pdf (concat (or tex-root-file-name
                                       (file-name-sans-extension (buffer-name)))
                                   ".pdf")))
    (cond
     ((and corresponding-pdf (get-buffer corresponding-pdf))
      (switch-to-buffer corresponding-pdf))
     ((file-exists-p corresponding-pdf)
      (find-file corresponding-pdf))
     (t
      (user-error "Cannot find pdf named %s" corresponding-pdf)))
    (define-key pdf-view-mode-map [f9] 'tex-pdf-go-to-tex)
    (setf durand-tex-action 'tex-pdf-go-to-tex-or-vice-versa)))

;;;###autoload
(defun tex-pdf-go-to-tex-or-vice-versa ()
  "Either go to pdf or go to tex"
  (interactive)
  (cond
   (current-prefix-arg
    (tex-display-or-kill-temp-buffer))
   ((derived-mode-p 'pdf-view-mode)
    (tex-pdf-go-to-tex))
   ((derived-mode-p 'tex-mode)
    (tex-tex-go-to-pdf))
   (t
    (user-error "Not in a pdf file or tex file."))))

;;;###autoload
(defun tex-pdf-go-to-tex ()
  "Go to the corresponding tex file to the current pdf file"
  (interactive)
  (switch-to-buffer (or working-name
                        (concat (file-name-sans-extension (buffer-name)) ".tex"))))

;;;###autoload
(defun tex-set-up-root (arg)
  "Set up TEX-ROOT-FILE-NAME using (file-name-base)"
  (interactive "P")
  (if (null arg)
      (progn (setq tex-root-file-name (file-name-base))
             (message (format "tex-root-file-name set to %s" tex-root-file-name)))
    (progn (setq tex-root-file-name nil)
           (message (format "tex-root-file-name set to %s" tex-root-file-name)))))

;;;###autoload
(defun tex-set-up-working (arg)
  "Set up WORKING-NAME using (buffer-name)"
  (interactive "P")
  (if (null arg)
      (progn (setq working-name (buffer-name))
             (message (format "working-name set to %s" working-name)))
    (progn (setq working-name nil)
           (message (format "working-name set to %s" working-name)))))

;;;###autoload
(defun save-tex-advice (&rest arg)
  (interactive)
  (cond ((string-equal major-mode "plain-tex-mode")
         (progn
           (define-key plain-tex-mode-map [f9] 'tex)
           (advice-remove 'save-buffer 'save-tex-advice)))
        ((string-equal major-mode "latex-mode")
         (progn
           (define-key latex-mode-map [f9] 'tex)
           (advice-remove 'save-buffer 'save-tex-advice)))))

(defvar tex-heading-list nil
  "The list of headings used in tex files")

(setq tex-heading-list '("heading"
                         "imp"
                         "thm"
                         "sec"
                         "secc"
                         "chap"
                         "tit"
                         "lem"))
;;;###autoload
;; (defun tex-re-build (head)
;;   "Build the regexp to match against HEAD"
;;   (concat "^\\\\" head " \\([^\n]+\\)$"))

;; The regex for headings is "^\\\\heading \\([^\n]+\\)$"
;; This is still experimental!
;;;###autoload
;; (defun tex-apply-special-font ()
;;   "Make some appearance changes"
;;   (interactive)
;;   (let ((inhibit-modification-hooks t))
;;     (save-excursion
;;       (beginning-of-buffer)
;;       (dolist (title tex-heading-list)
;; 	(while (re-search-forward (tex-re-build title) nil t)
;; 	  (put-text-property (match-beginning 1) (match-end 1)
;; 			     'face '(:height 2.0)))))))

(defface tex-big-face '((t (:height 1.5 :foreground "orange1")))
  "Highlight special heading in a big font!")

;; The special fontification should be done by `font-lock-add-keywords' instead.
(font-lock-add-keywords 'plain-tex-mode `((,(concat
                                             "^\\\\\\(?:"
                                             (mapconcat #'identity
                                                        tex-heading-list
                                                        "\\|")
                                             "\\) \\([^\n]+\\)$")
                                           1 'tex-big-face t))
                        'append)

(with-eval-after-load "tex-mode"
  (define-key plain-tex-mode-map [?\§] '(lambda () "remap to type escape key" (interactive) (insert "\\")))
  (define-key plain-tex-mode-map [f9] 'tex)
  (define-key plain-tex-mode-map [f11] 'tex-display-or-kill-temp-buffer)
  (define-key plain-tex-mode-map [f7] 'tex-set-up-root)
  (define-key plain-tex-mode-map [f8] 'tex-set-up-working)
  (define-key plain-tex-mode-map [?\)] 'end-exit-paren)
  (define-key plain-tex-mode-map [?ç] 'open-back-paren)
  (define-key plain-tex-mode-map [?\(] 'open-paren)
  (define-key plain-tex-mode-map [backspace] 'durand-delete-pair)
  (define-key plain-tex-mode-map [?\{] 'open-curly)
  (define-key plain-tex-mode-map [?\[] 'open-bracket)
  (define-key plain-tex-mode-map [?\C-c ?d] 'insert-def)
  (define-key plain-tex-mode-map [?\C-c ?o] 'one-def)
  (define-key plain-tex-mode-map [?\C-c ?t] 'two-def)
  (define-key plain-tex-mode-map [?\C-c ?\C-c] '(lambda () (interactive) (save-buffer 0) (tex)))
  (define-key plain-tex-mode-map [?\C-c ?r] 'read-tex-complete)
  (define-key plain-tex-mode-map [?\C-c ?\C-o] 'make-blank-space)
  (define-key plain-tex-mode-map [?\C-c ?\C-\S-o] '(lambda () (interactive) (make-blank-space 4)))
  (define-key plain-tex-mode-map [?\M-'] 'abbrev-prefix-mark)
  (define-key plain-tex-mode-map [?ù] abbrev-prefix-map)
  ;; (define-key plain-tex-mode-map [tab] 'completion-at-point)
  (define-key plain-tex-mode-map [tab] 'company-complete)
  
  (add-hook 'tex-mode-hook 'olivetti-mode)

  ;; (remove-hook 'tex-mode-hook
  ;;           (lambda ()
  ;;             (set-fill-column 90)
  ;;             (auto-fill-mode 1)))
  )

(with-eval-after-load "latex-mode"
  (define-key latex-mode-map [?\§] '(lambda () "remap to type escape key" (interactive) (insert "\\")))
  (define-key latex-mode-map [f9] 'tex)
  (define-key latex-mode-map [f7] 'tex-set-up-root)
  (define-key latex-mode-map [f8] 'tex-set-up-working)
  (define-key latex-mode-map [?\)] 'end-exit-paren)
  (define-key latex-mode-map [?ç] 'open-back-paren)
  (define-key latex-mode-map [?\(] 'open-paren)
  (define-key latex-mode-map [backspace] nil)
  (define-key latex-mode-map [?\{] 'open-curly)
  (define-key latex-mode-map [?\[] 'open-bracket)
  (define-key latex-mode-map [?\C-c ?d] 'insert-def)
  (define-key latex-mode-map [?\C-c ?o] 'one-def)
  (define-key latex-mode-map [?\C-c ?t] 'two-def)
  (define-key latex-mode-map [?\C-c ?r] 'read-tex-complete)
  (define-key latex-mode-map [?\C-c ?\C-o] 'make-blank-space)
  (define-key latex-mode-map [?\C-c ?\C-\S-o] '(lambda () (interactive) (make-blank-space 4)))
  (define-key latex-mode-map [?\M-'] 'abbrev-prefix-mark)
  (define-key latex-mode-map [?ù] abbrev-prefix-map)
  (define-key latex-mode-map [tab] 'completion-at-point))

;; I will take a two-character approach, but some of them are still
;; one-character expansion.

;; (setq abbrev-pairs
;;       (list
;;        '("a" "\\alpha")
;;        '("b" "\\beta")
;;        '("g" "\\gamma")
;;        '("d" "\\delta")
;;        '("D" "\\Delta")
;;        '("e" "\\epsilon")
;;        '("z" "\\zeta")
;;        '("h" "\\eta")
;;        '("j" "\\theta")
;;        '("k" "\\kappa")
;;        '("l" "\\lambda")
;;        '("m" "\\mu")
;;        '("n" "\\nu")
;;        '("x" "\\xi")
;;        '("p" "\\pi")
;;        '("r" "\\rho")
;;        '("s" "\\sigma")
;;        '("t" "\\tau")
;;        '("u" "\\upsilon")
;;        '("f" "\\phi")
;;        '("q" "\\chi")
;;        '("y" "\\psi")
;;        '("w" "\\omega")
;;        '("D" "\\Delta")
;;        '("G" "\\Gamma")
;;        '("J" "\\Theta")
;;        '("L" "\\Lambda")
;;        '("X" "\\Xi")
;;        '("P" "\\Pi")
;;        '("S" "\\Sigma")
;;        '("U" "\\Upsilon")
;;        '("F" "\\Phi")
;;        '("Y" "\\Psi")
;;        '("W" "\\Omega")
;;        '("ve" "\\varepsilon")
;;        '("vf" "\\varphi")
;;        '("vp" "\\varpi")
;;        '("<" "\\leq")
;;        '(">" "\\geq")
;;        '("==" "\\equiv")
;;        '("~=" "\\cong")
;;        '("." "\\cdot")
;;        '("pm" "\\pmod{}")
;;        '("A" "\\forall")
;;        '("i" "\\in")
;;        '("I" "\\infty")
;;        '("[" "\\subseteq")
;;        '("]" "\\supseteq")
;;        '("(" "\\subset")
;;        '(")" "\\supset")
;;        '("-" "\\setminus")))

;; This function needs to be modified; add some protection, and
;; distinguishes one and two character expansions.

;; (defun setup-abbrevs ()
;;   "My function to replace auctex math symbol expansion"
;;   (interactive)
;;   (let* ((key (read-string "Symbol: "))
;; 	 (str (cadr (assoc key abbrev-pairs))))
;;     (insert str)))

;; oft macros are close to each other, this makes it easy to distinguish them.
(with-eval-after-load 'plain-tex-mode
  (modify-syntax-entry ?\\ "_" plain-tex-mode-syntax-table))

(setq abbrev-prefix-map (make-sparse-keymap))
(define-key abbrev-prefix-map "a" (lambda () (interactive) (insert "\\alpha")))
(define-key abbrev-prefix-map "-" (lambda () (interactive) (insert "\\setminus")))
(define-key abbrev-prefix-map ")" (lambda () (interactive) (insert "\\supset")))
(define-key abbrev-prefix-map "(" (lambda () (interactive) (insert "\\subset")))
(define-key abbrev-prefix-map "]" (lambda () (interactive) (insert "\\supseteq")))
(define-key abbrev-prefix-map "[" (lambda () (interactive) (insert "\\subseteq")))
(define-key abbrev-prefix-map "{" (lambda () (interactive) (insert "\\left\\{\\right\\}") (backward-char 8)))
(define-key abbrev-prefix-map "I" (lambda () (interactive) (insert "\\infty")))
(define-key abbrev-prefix-map "i" (lambda () (interactive) (insert "\\in")))
(define-key abbrev-prefix-map "A" (lambda () (interactive) (insert "\\forall")))
(define-key abbrev-prefix-map "." (lambda () (interactive) (insert "\\cdot")))
(define-key abbrev-prefix-map "v." (lambda () (interactive) (insert "\\cdots")))
(define-key abbrev-prefix-map "~" (lambda () (interactive) (insert "\\cong")))
(define-key abbrev-prefix-map "=" (lambda () (interactive) (insert "\\equiv")))
(define-key abbrev-prefix-map ">" (lambda () (interactive) (insert "\\geq")))
(define-key abbrev-prefix-map "<" (lambda () (interactive) (insert "\\leq")))
(define-key abbrev-prefix-map "vp" (lambda () (interactive) (insert "\\varpi")))
(define-key abbrev-prefix-map "vf" (lambda () (interactive) (insert "\\varphi")))
(define-key abbrev-prefix-map "ve" (lambda () (interactive) (insert "\\varepsilon")))
(define-key abbrev-prefix-map "W" (lambda () (interactive) (insert "\\Omega")))
(define-key abbrev-prefix-map "Y" (lambda () (interactive) (insert "\\Psi")))
(define-key abbrev-prefix-map "F" (lambda () (interactive) (insert "\\Phi")))
(define-key abbrev-prefix-map "U" (lambda () (interactive) (insert "\\Upsilon")))
(define-key abbrev-prefix-map "S" (lambda () (interactive) (insert "\\Sigma")))
(define-key abbrev-prefix-map "P" (lambda () (interactive) (insert "\\Pi")))
(define-key abbrev-prefix-map "X" (lambda () (interactive) (insert "\\Xi")))
(define-key abbrev-prefix-map "L" (lambda () (interactive) (insert "\\Lambda")))
(define-key abbrev-prefix-map "J" (lambda () (interactive) (insert "\\Theta")))
(define-key abbrev-prefix-map "G" (lambda () (interactive) (insert "\\Gamma")))
(define-key abbrev-prefix-map "D" (lambda () (interactive) (insert "\\Delta")))
(define-key abbrev-prefix-map "w" (lambda () (interactive) (insert "\\omega")))
(define-key abbrev-prefix-map "y" (lambda () (interactive) (insert "\\psi")))
(define-key abbrev-prefix-map "q" (lambda () (interactive) (insert "\\chi")))
(define-key abbrev-prefix-map "f" (lambda () (interactive) (insert "\\phi")))
(define-key abbrev-prefix-map "u" (lambda () (interactive) (insert "\\upsilon")))
(define-key abbrev-prefix-map "t" (lambda () (interactive) (insert "\\tau")))
(define-key abbrev-prefix-map "s" (lambda () (interactive) (insert "\\sigma")))
(define-key abbrev-prefix-map "r" (lambda () (interactive) (insert "\\rho")))
(define-key abbrev-prefix-map "p" (lambda () (interactive) (insert "\\pi")))
(define-key abbrev-prefix-map "x" (lambda () (interactive) (insert "\\xi")))
(define-key abbrev-prefix-map "n" (lambda () (interactive) (insert "\\nu")))
(define-key abbrev-prefix-map "m" (lambda () (interactive) (insert "\\mu")))
(define-key abbrev-prefix-map "l" (lambda () (interactive) (insert "\\lambda")))
(define-key abbrev-prefix-map "k" (lambda () (interactive) (insert "\\kappa")))
(define-key abbrev-prefix-map "j" (lambda () (interactive) (insert "\\theta")))
(define-key abbrev-prefix-map "h" (lambda () (interactive) (insert "\\eta")))
(define-key abbrev-prefix-map "z" (lambda () (interactive) (insert "\\zeta")))
(define-key abbrev-prefix-map "e" (lambda () (interactive) (insert "\\epsilon")))
(define-key abbrev-prefix-map "E" (lambda () (interactive) (insert "\\exists")))
(define-key abbrev-prefix-map "D" (lambda () (interactive) (insert "\\Delta")))
(define-key abbrev-prefix-map "d" (lambda () (interactive) (insert "\\delta")))
(define-key abbrev-prefix-map "g" (lambda () (interactive) (insert "\\gamma")))
(define-key abbrev-prefix-map "b" (lambda () (interactive) (insert "\\beta")))
(define-key abbrev-prefix-map "+" (lambda () (interactive) (insert "\\sum")))
(define-key abbrev-prefix-map "0" (lambda () (interactive) (insert "\\circ")))
(define-key abbrev-prefix-map "c" 'read-tex-complete)

;; (defhydra abbrev-prefix-hydra (:color red)
;;   "insert"
;;   ("ù" nil :color blue)
;;   ("DEL" (delete-char -1) :color blue)
;;   ("1" (insert "1"))
;;   ("2" (insert "2"))
;;   ("3" (insert "3"))
;;   ("4" (insert "4"))
;;   ("5" (insert "5"))
;;   ("6" (insert "6"))
;;   ("7" (insert "7"))
;;   ("8" (insert "8"))
;;   ("9" (insert "9"))
;;   ("0" (insert "0"))
;;   ("c" (read-tex-complete) "custom")
;;   ("a" (insert "\\alpha"))
;;   ("-" (insert "\\setminus"))
;;   ("+" (insert "\\sum"))
;;   (")" (insert "\\supset"))
;;   ("(" (insert "\\subset"))
;;   ("]" (insert "\\supseteq"))
;;   ("[" (insert "\\subseteq"))
;;   ("I" (insert "\\infty"))
;;   ("i" (insert "\\in"))
;;   ("A" (insert "\\forall"))
;;   ("." (insert "\\cdot"))
;;   ("v." (insert "\\cdots"))
;;   ("~" (insert "\\cong"))
;;   ("=" (insert "\\equiv"))
;;   ("v(" (progn (insert "()") (backward-char)))
;;   ("v[" (progn (insert "[]") (backward-char)))
;;   ("v{" (progn (insert "{}") (backward-char)))
;;   (">" (insert "\\geq"))
;;   ("<" (insert "\\leq"))
;;   ("vp" (insert "\\varpi"))
;;   ("vf" (insert "\\varphi"))
;;   ("ve" (insert "\\varepsilon"))
;;   ("W" (insert "\\Omega"))
;;   ("Y" (insert "\\Psi"))
;;   ("F" (insert "\\Phi"))
;;   ("U" (insert "\\Upsilon"))
;;   ("S" (insert "\\Sigma"))
;;   ("P" (insert "\\Pi"))
;;   ("X" (insert "\\Xi"))
;;   ("L" (insert "\\Lambda"))
;;   ("J" (insert "\\Theta"))
;;   ("G" (insert "\\Gamma"))
;;   ("w" (insert "\\omega"))
;;   ("y" (insert "\\psi"))
;;   ("q" (insert "\\chi"))
;;   ("f" (insert "\\phi"))
;;   ("u" (insert "\\upsilon"))
;;   ("t" (insert "\\tau"))
;;   ("s" (insert "\\sigma"))
;;   ("r" (insert "\\rho"))
;;   ("p" (insert "\\pi"))
;;   ("x" (insert "\\xi"))
;;   ("n" (insert "\\nu"))
;;   ("m" (insert "\\mu"))
;;   ("l" (insert "\\lambda"))
;;   ("k" (insert "\\kappa"))
;;   ("j" (insert "\\theta"))
;;   ("h" (insert "\\eta"))
;;   ("z" (insert "\\zeta"))
;;   ("e" (insert "\\epsilon"))
;;   ("D" (insert "\\Delta"))
;;   ("d" (insert "\\delta"))
;;   ("g" (insert "\\gamma"))
;;   ("b" (insert "\\beta")))

;; (load-file (expand-file-name "my_packages/tex-complete.el" user-emacs-directory))

;;;###autoload
(defun durand-delete-pair ()
  "Delete the matching pair"
  (interactive)
  (cond (view-mode ; if in view-mode, then scroll down
         (View-scroll-page-backward))
        ((region-active-p) ; if the region is active, then do the original thing
         (delete-backward-char 1))
        ((memq (char-before) '(?\( ?\[ ?\{))
         (save-excursion
           (backward-char 1)
           (ignore-errors
             (forward-sexp 1)
             (delete-char -1)))
         (delete-char -1))
        (t
         (delete-char -1))))

;;;###autoload
(defun end-exit-paren ()
  "Use closing pasenthesis to exit the parenthesis"
  (interactive)
  (let ((ch (char-after nil))
        (ch-list '(?\) ?\} ?\] ?\$)))
    (cond ((memq ch ch-list) (forward-char))
          (t (insert ")")))))

;;;###autoload
(defun open-back-paren ()
  "Use closing pasenthesis to exit the parenthesis"
  (interactive)
  (let ((ch (char-before nil))
        (ch-list '(?\) ?\} ?\] ?\$)))
    (cond ((memq ch ch-list) (backward-char))
          (t (insert "ç")))))

;;;###autoload
(defun open-paren ()
  "open parenthesis inserts a matching pair"
  (interactive)
  (progn
    (insert "()")
    (backward-char)))

;;;###autoload
(defun open-curly ()
  "open curly inserts a matching pair"
  (interactive)
  (progn
    (insert "{}")
    (backward-char)))

;;;###autoload
(defun open-bracket ()
  "open bracket inserts a matching pair"
  (interactive)
  (progn
    (insert "[]")
    (backward-char)))

;;;###autoload
(defun insert-def ()
  "my function to insert defs of tex documents easily"
  (interactive)
  (let ((name (read-string "Enter macro name: "))
        (body (buffer-substring-no-properties (mark) (point))))
    (if (use-region-p)
        (progn (kill-region (region-beginning) (region-end))
               (insert (format "\\%s" name))
               (save-excursion
                 (goto-char (point-min))
                 (setq temp (search-forward-regexp "^\\\\def" nil t))
                 (when temp
                   (message "Macro inserted.")
                   (beginning-of-line)
                   (while (re-search-forward "^\\\\def" nil t)
                     (re-search-forward "{" nil t)
                     (backward-char 1)
                     (forward-sexp))
                   (open-line 1)
                   (forward-char 1)
                   (insert (format "\\def\\%s{%s}" name body))))
               (if (not temp)
                   (save-excursion (message "No defs found, insert in the above paragragh.")
                                   (backward-paragraph)
                                   (insert (format "\n\\def\\%s{%s}" name body)))))
      (message "Please activate region which contains the definiton before inserting the def"))))

;;;###autoload
(defun one-def ()
  "insert defonetext instead of def"
  (interactive)
  (let ((name (read-string "Enter macro name: ")))
    (progn (insert (format "\\%s" (downcase name)))
           (save-excursion
             (goto-char (point-min))
             (setq temp (search-forward-regexp "^\\\\def" nil t))
             (when temp
                   (message "Macro inserted.")
                   (beginning-of-line)
                   (while (re-search-forward "^\\\\def" nil t)
                     (re-search-forward "{" nil t)
                     (backward-char 1)
                     (forward-sexp))
                   (open-line 1)
                   (forward-char 1)
                   (insert (format "\\defonetext{%s}" name))))
           (if (not temp)
               (save-excursion (message "No defs found, insert in the above paragragh.")
                               (backward-paragraph)
                               (insert (format "\n\\defonetext{%s}" name)))))))

;;;###autoload
(defun two-def ()
  "insert deftwotext instead of def"
  (interactive)
  (let ((name (downcase (read-string "Enter macro name: ")))
        (body (buffer-substring-no-properties (mark) (point))))
    (if (use-region-p)
        (progn (kill-region (region-beginning) (region-end))
               (insert (format "\\%s" name))
               (save-excursion
                 (goto-char (point-min))
                 (setq temp (search-forward-regexp "^\\\\def" nil t))
                 (when temp
                   (message "Macro inserted.")
                   (beginning-of-line)
                   (while (re-search-forward "^\\\\def" nil t)
                     (re-search-forward "{" nil t)
                     (backward-char 1)
                     (forward-sexp))
                   (open-line 1)
                   (forward-char 1)
                   (insert (format "\\deftwotext{%s}{%s}" name body))))
               (if (not temp)
                   (save-excursion (message "No defs found, insert in the above paragragh.")
                                   (backward-paragraph)
                                   (insert (format "\n\\deftwotext{%s}{%s}" name body)))))
      (message "Please activate region which contains the definiton before inserting the def"))))

;;;###autoload
(defun get-defs ()
  "Collect all the defs in the tex document"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((res '()))
      (while (re-search-forward "^\\\\def" nil t)
        (let* ((start-pos (point))
               (s (cons
                   (buffer-substring-no-properties
                    (- (point) 4)
                    (progn
                      (re-search-forward "{" nil t)
                      (backward-char)
                      (cdr (bounds-of-thing-at-point 'sexp))))
                   start-pos)))
          (setq res (cons s res))))
      (nreverse res))))

;;;###autoload
(defun find-macro-name (x)
  "Find the name of a tex macro"
  (let* ((ind (string-match "{" x))
         (content (substring x 0 ind)))
    (cond
     ((string-equal content "\\defonetext")
      (concat "\\" (downcase (substring x (+ 1 ind) (string-match "}" x)))))
     ((string-equal content "\\deftwotext")
      (concat "\\" (downcase (substring x (+ 1 ind) (string-match "}" x)))))
     (t
      (substring x 4 ind)))))

;;;###autoload
(defun find-macro-content (x)
  "Find the content of a tex macro"
  (let* ((ind (string-match "{" x))
         (content (substring x 0 ind)))
    (cond
     ((string-equal content "\\defonetext")
      (substring x (1+ ind) -1))
     ((string-equal content "\\deftwotext")
      (substring x (1+ (string-match "{" x (1+ ind))) -1))
     (t
      (substring x (1+ ind) -1)))))

(setq tex-def-map (make-sparse-keymap))
(define-key tex-def-map [?\C-c ?f] #'tex-toggle-follow)

(defvar tex-follow-up-or-not nil
  "Variable to determine the tex follow mode")

;;;###autoload
(defun tex-toggle-follow ()
  "Toggle tex-follow-up-or-not"
  (interactive)
  (setq tex-follow-up-or-not (not tex-follow-up-or-not)))

;; I shall aggregate this into the prefix keymap and combine with headlone,
;; so that I can type at a great speed.
;;;###autoload
(defun tex-follow-up ()
  "Follow the definition in the tex file"
  (interactive)
  (when tex-follow-up-or-not
    (with-ivy-window
      (goto-char (cdr (assoc (ivy-state-current ivy-last) tex-def-alist))))))

;;;###autoload
(defun read-tex-complete ()
  "my function to find all defs and use ivy as backend to complete it,
assuming all defs come at the beginning of line"
  (interactive)
  (setq tex-follow-up-or-not nil)
  (setq tex-def-alist (get-defs))
  (setq tex-old-pos (point))
  (ivy-read "defs: " (mapcar #'car tex-def-alist)
            :action '(1
                      ("o" (lambda (x)
                             (interactive)
                             (insert (format "%s" (find-macro-name x))))
                       "Insert Macro Name"))
            :update-fn #'tex-follow-up
            :unwind (lambda ()
                      (goto-char tex-old-pos)
                      (setq tex-def-alist nil))
            :keymap tex-def-map))

(defvar tex-def-alist nil
  "An associative list to store the defs found in a tex file.")

(defvar tex-old-pos 0
  "The old position to go back to.")

;; code ends here

;;  (use-package auctex
;;    :defer t
;;    :ensure t
;;    :config
;;    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;    (add-hook 'TeX-mode-hook 'LaTeX-math-mode)
;;    (customize-set-variable 'LaTeX-math-abbrev-prefix (kbd "£"))
;;    (add-hook 'TeX-mode-hook '(lambda ()
;;				(define-key TeX-mode-map [?\§] '(lambda () "remap to type escape key" (interactive) (insert "\\")))
;;				(define-key TeX-mode-map [f9] 'tex)
;;				(define-key TeX-mode-map [?\)] 'end-exit-paren)
;;				(define-key TeX-mode-map [?\(] 'open-paren)
;;				(define-key TeX-mode-map [?\C-c ?d] 'insert-def)
;;				(define-key TeX-mode-map [?\C-c ?o] 'one-def)
;;				(define-key TeX-mode-map [?\C-c ?t] 'two-def)
;;				(define-key TeX-mode-map [?\C-c ?r] 'read-tex-complete))))
;; (call-process "/bin/bash" nil nil nil "-c" (format "xetex %s" (shell-quote-argument buffer-file-name)))
;; (let ((pdf-name (concat (file-name-base) ".pdf")))
;; (if (get-buffer pdf-name)
;;     (with-current-buffer pdf-name (revert-buffer))
;;   (find-file pdf-name)))

;;;###autoload
;; (defun tex-switcher (full-name)
;;   "Switch to pdf file"
;;   (interactive)
;;   (let* ((full-pdf-name (concat (or tex-root-file-name (file-name-sans-extension full-name)) ".pdf"))
;; 	 (pdf-name (if (null tex-root-file-name)
;; 		       (file-name-nondirectory full-pdf-name)
;; 		     (concat tex-root-file-name ".pdf"))))
;;     (if (get-buffer pdf-name)
;; 	(switch-to-buffer pdf-name)
;;       (find-file full-pdf-name))
;;     (revert-buffer)
;;     (define-key pdf-view-mode-map [f9] (lambda ()
;; 					 (interactive)
;; 					 (progn
;; 					   (switch-to-buffer
;; 					    (if (null working-name)
;; 						(concat (file-name-sans-extension (buffer-name)) ".tex")
;; 					      working-name))
;; 					   (advice-add 'save-buffer :before 'save-tex-advice)
;; 					   (define-key plain-tex-mode-map [f9]
;; 					     (lambda ()
;; 					       (interactive)
;; 					       (define-key plain-tex-mode-map [f9]
;; 						 (lambda ()
;; 						   (interactive)
;; 						   (switch-to-buffer
;; 						    (if (null tex-root-file-name)
;; 							(concat
;; 							 (file-name-sans-extension (buffer-name)) ".pdf")
;; 						      (concat
;; 						       tex-root-file-name ".pdf")))))
;; 					       (switch-to-buffer
;; 						(if (null tex-root-file-name)
;; 						    (concat (file-name-sans-extension (buffer-name)) ".pdf")
;; 						  (concat tex-root-file-name ".pdf")))
;; 					       (define-key pdf-view-mode-map [f9]
;; 						 (lambda ()
;; 						   (interactive)
;; 						   (switch-to-buffer
;; 						    (if (null working-name)
;; 							(concat (file-name-sans-extension (buffer-name)) ".tex")
;; 						      working-name))))))
;; 					   (define-key latex-mode-map [f9]
;; 					     (lambda ()
;; 					       (interactive)
;; 					       (define-key latex-mode-map [f9]
;; 						 (lambda ()
;; 						   (interactive)
;; 						   (switch-to-buffer
;; 						    (if (null tex-root-file-name)
;; 							(concat
;; 							 (file-name-sans-extension (buffer-name)) ".pdf")
;; 						      (concat
;; 						       tex-root-file-name ".pdf")))))
;; 					       (switch-to-buffer
;; 						(if (null tex-root-file-name)
;; 						    (concat (file-name-sans-extension (buffer-name)) ".pdf")
;; 						  (concat tex-root-file-name ".pdf")))
;; 					       (define-key pdf-view-mode-map [f9]
;; 						 (lambda ()
;; 						   (interactive)
;; 						   (switch-to-buffer
;; 						    (if (null working-name)
;; 							(concat (file-name-sans-extension (buffer-name)) ".tex")
;; 						      working-name)))))))))))

(defun make-blank-space (&optional down-p)
  "To make enough space to put something in. Default to up, with DOWN-P down"
  (interactive "P")
  (pcase down-p
    ((pred null)
     (beginning-of-line)
     (open-line 3)
     (forward-line)
     (indent-according-to-mode))
    (_
     (end-of-line)
     (open-line 3)
     (forward-line 2)
     (indent-according-to-mode))))

;; (define-derived-mode tex-org plain-tex-mode "TEX-ORG"
;;   "For writing tex documents in an org file.")

;; (defun make-blank-space (arg)
;;   "To make enough space to put something in. Default to up, with arg down"
;;   (interactive "P")
;;   (if (null arg)
;;       (progn
;; 	(beginning-of-line)
;; 	(open-line 3)
;; 	(forward-line)
;; 	(indent-according-to-mode))
;;     (progn
;;       (end-of-line)
;;       (open-line 3)
;;       (forward-line 2)
;;       (indent-according-to-mode))))

;; (define-derived-mode tex-org plain-tex-mode "TEX-ORG"
;;   "For writing tex documents in an org file.")
