;;; gme-rtf-mode.el --- A major mode for editing GME RTF scripts. -*- lexical-binding: t -*-

;; Modified from `scad-mode.el'.

;;; Code:

(require 'compat)
(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cl-lib))

(require 'cape-keyword)

(require 'smartparens)

(defgroup gme-rtf nil
  "A major mode for editing GME RTF scripts."
  :link '(url-link :tag "Homepage" "https://github.com/bohonghuang/gme-rtf-mode")
  :link '(emacs-library-link :tag "Library Source" "gme-rtf-mode.el")
  :group 'languages
  :prefix "gme-rtf-")

(defcustom gme-rtf-keywords
  '("pipe" "control" "event" "bind" "import")
  "GME RTF keywords."
  :type '(repeat string))

(defcustom gme-rtf-functions
  '("start")
  "GME RTF functions."
  :type '(repeat string))

(defcustom gme-rtf-modules
  '()
  "GME RTF modules."
  :type '(repeat string))

(defcustom gme-rtf-deprecated
  '()
  "GME RTF deprecated modules and functions."
  :type '(repeat string))

(defcustom gme-rtf-operators
  '("->")
  "GME RTF operators."
  :type '(repeat string))

(defvar-keymap gme-rtf-mode-map
  :doc "Keymap for `gme-rtf-mode'."
  :parent c-mode-base-map
  "TAB" #'indent-for-tab-command
  "M-TAB" #'completion-at-point)

(defvar gme-rtf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    st)
  "Syntax table for `gme-rtf-mode'.")

(defface gme-rtf-font-lock-keyword-face
  '((default :inherit font-lock-keyword-face))
  "Face for highlighting keywords in GME RTF mode.")

(cl-eval-when (:compile-toplevel :load-toplevel :execute)
  (put 'gme-rtf-mode 'c-mode-prefix "gme-rtf-"))

(defvar gme-rtf-font-lock-keywords
  `(("\\(pipe\\)[ \t]+\\(\\sw+\\)" . ((1 'gme-rtf-font-lock-keyword-face nil) (2 'font-lock-function-name-face nil t)))
    ("[^A-Za-z_]\\([0-9]+\\(?:\\.[0-9]+\\)?[fd]?\\)" . ((1 'font-lock-constant-face nil)))
    ("<\\(\\sw+\\)>" . ((1 'font-lock-variable-use-face nil)))
    ("^#[ \t]*import" . font-lock-preprocessor-face)
    ,@(c-lang-const c-complex-decl-matchers gme-rtf)
    (,(regexp-opt gme-rtf-keywords 'words)   . font-lock-keyword-face)
    (,(regexp-opt gme-rtf-modules 'words)    . font-lock-builtin-face)
    (,(regexp-opt gme-rtf-functions 'words)  . font-lock-function-name-face)
    (,(regexp-opt gme-rtf-deprecated 'words) . font-lock-warning-face)
    (,(regexp-opt gme-rtf-operators) . font-lock-operator-face))
  "Keyword highlighting specification for `gme-rtf-mode'.")

(defconst gme-rtf-font-lock-keywords-1 gme-rtf-font-lock-keywords)
(defconst gme-rtf-font-lock-keywords-2 gme-rtf-font-lock-keywords)
(defconst gme-rtf-font-lock-keywords-3 gme-rtf-font-lock-keywords)

(defvar gme-rtf-completions
  (append gme-rtf-keywords gme-rtf-functions gme-rtf-modules)
  "List of known words for completion.")

(setf (alist-get 'gme-rtf-mode cape-keyword-list) gme-rtf-completions)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rtf\\'" . gme-rtf-mode))

;;;###autoload
(define-derived-mode gme-rtf-mode prog-mode "GME RTF"
  "Major mode for editing GME RTF scripts."
  :group 'gme-rtf
  :after-hook (c-update-modeline)
  ;; (add-hook 'completion-at-point-functions
  ;;           #'gme-rtf-completion-at-point nil 'local)
  (c-initialize-cc-mode t)
  (c-init-language-vars gme-rtf-mode)
  (c-common-init 'gme-rtf-mode)
  (c-set-offset 'cpp-macro 0 nil)
  (c-run-mode-hooks 'c-mode-common-hook))

(defun gme-rtf-completion-at-point ()
  "Completion at point function."
  (when-let (bounds (bounds-of-thing-at-point 'word))
    (list (car bounds) (cdr bounds)
          gme-rtf-completions
          :exclusive 'no)))

(defun sp-gme-rtf-filter-angle-brackets (_id action context)
  "Non-nil if we should allow ID's ACTION in CONTEXT for angle brackets."
  ;; See the docstring for `sp-pair' for the possible values of ID,
  ;; ACTION and CONTEXT.
  (cond
   ;; Inside strings, don't do anything with < or >.
   ((eq context 'string)
    nil)
   ;; Don't do any smart pairing inside comments either.
   ((eq context 'comment)
    nil)
   ;; Otherwise, we're in code.
   ((eq context 'code)
    (let ((on-fn-return-type
           (looking-back (rx "->") nil))
          (on-match-branch
           (looking-back (rx "=>") nil)))
      (cond
       ;; Only insert a matching > if we're not looking at a
       ;; comparison.
       ((eq action 'insert)
        (and (not on-fn-return-type) (not on-match-branch)))
       ;; Always allow wrapping in a pair if the region is active.
       ((eq action 'wrap)
        (not on-match-branch))
       ;; When pressing >, autoskip if we're not looking at a
       ;; comparison.
       ((eq action 'autoskip)
        (and (not on-fn-return-type) (not on-match-branch)))
       ;; Allow navigation, highlighting and strictness checks if it's
       ;; not a comparison.
       ((eq action 'navigate)
        (and (not on-fn-return-type) (not on-match-branch))))))))

(defun sp-gme-rtf-skip-match-angle-bracket (_ms _mb me)
  "Non-nil if we should ignore the bracket as valid delimiter."
  (save-excursion
    (goto-char me)
    (let ((on-fn-return-type
           (sp--looking-back-p (rx "->") nil))
          (on-match-branch
           (sp--looking-back-p (rx "=>") nil)))
      (or on-fn-return-type on-match-branch))))

(sp-with-modes '(gme-rtf-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "<" ">"
                 :when '(sp-gme-rtf-filter-angle-brackets)
                 :skip-match 'sp-gme-rtf-skip-match-angle-bracket))

(provide 'gme-rtf-mode)
;;; gme-rtf-mode.el ends here
