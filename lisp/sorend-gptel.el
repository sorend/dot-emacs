;;; sorend-gptels.el --- Collection of tools for gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Soren A D
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (gptel "0.1.0") (project "0.9.0"))
;; Keywords: tools, files, workspace, project

;;; Code:

(require 'gptel)
(require 'project)

(defgroup sorend-gptel nil
  "Project-aware file operations tools for gptel."
  :group 'tools
  :prefix "sorend-gptel-")

(defcustom sorend-gptel-respect-ignores t
  "Whether to respect project ignore files (.gitignore, etc.) when listing files."
  :type 'boolean
  :group 'sorend-gptel)

(defcustom sorend-gptel-max-files 1000
  "Maximum number of files to return when listing project files."
  :type 'integer
  :group 'sorend-gptel)

(defun sorend/gptel--current-project ()
  "Get the current project or nil if not in a project."
  (project-current))

(defun sorend/gptel--project-root ()
  "Get the project root directory.
Returns project root if in a project, otherwise `default-directory'."
  (if-let ((project (sorend/gptel--current-project)))
      (project-root project)
    default-directory))

(defun sorend/gptel--resolve-path (path)
  "Resolve PATH relative to project root."
  (let ((project-root (sorend/gptel--project-root)))
    (expand-file-name path project-root)))

(defun sorend/gptel--validate-path (path)
  "Validate that PATH is safe to operate on within the project."
  (let* ((project-root (sorend/gptel--project-root))
         (resolved-path (sorend/gptel--resolve-path path)))
    (unless (file-in-directory-p resolved-path project-root)
      (error "Path %s is outside project boundaries" path))
    resolved-path))

(defun sorend/gptel--relative-path (file-path)
  "Convert FILE-PATH to relative path from project root."
  (file-relative-name file-path (sorend/gptel--project-root)))


(defun sorend/gptel-emacs-describe-variable (var)
  "Return documentation for VAR."
  (let ((symbol (intern var)))
    (if (boundp symbol)
        (prin1-to-string (symbol-value symbol))
      (format "Variable %s is not bound. This means the variable doesn't exist. Stop. Reassess what you're trying to do, examine the situation, and continue. " var))))

(defun sorend/gptel-emacs--describe-function (fun)
  "Return documentation for FUN."
  (let ((symbol (intern fun)))
    (if (fboundp symbol)
        (prin1-to-string (documentation symbol 'function))
      (format "Function %s is not defined." fun))))

(defun sorend/gptel-elisp--run-eval (command)
  "Return output of elisp eval COMMAND."
  (eval (if (stringp command)
            (car (read-from-string (format "(progn %s)" command)))
          command)))

(defun sorend/gptel-elisp--defun-region (function-name &optional buffer)
  "Return a cons cell (START . END) with points just before and after FUNCTION-NAME defun in BUFFER. If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp (concat "^(\\s-*defun\\s-+" (regexp-quote function-name) "\\_>") nil t)
          (let (start end)
            (beginning-of-defun)
            (setq start (point))
            (end-of-defun)
            (setq end (point))
            (cons start end))
        (message "Function `%s` not found in buffer %s" function-name (buffer-name))
        nil))))

(defun sorend/gptel-elisp--replace-defun-region (function-name new-string &optional buffer)
  "Replace the entire region of FUNCTION-NAME with NEW-STRING in BUFFER or current buffer if omitted."
  (let ((bounds (sorend/gptel-elisp--defun-region function-name buffer)))
    (if bounds
        (with-current-buffer (or buffer (current-buffer))
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert new-string)))
      (message "Function `%s` not found for replacement" function-name))))

(defun sorend/gptel-elisp--smerge-replace-defun-region (function-name new-string &optional buffer)
  "Insert smerge-style conflict markers for FUNCTION-NAME region with NEW-STRING in BUFFER or current buffer."
  (interactive "sFunction name: \nsNew function text: \nBBuffer (optional): ")
  (let ((buf (or buffer (current-buffer)))
        bounds old-text conflict-text)
    (setq bounds (sorend/gptel-elisp--defun-region function-name buf))
    (if (not bounds)
        (message "Function `%s` not found in buffer %s" function-name (buffer-name buf))
      (with-current-buffer buf
        (setq old-text (buffer-substring-no-properties (car bounds) (cdr bounds)))
        (setq conflict-text
              (concat "<<<<<<< FUNCTION BEFORE REPLACE\n"
                      old-text
                      "=======\n"
                      new-string
                      "\n>>>>>>> FUNCTION AFTER REPLACE\n"))
        (goto-char (car bounds))
        (delete-region (car bounds) (cdr bounds))
        (insert conflict-text)
        (smerge-mode 1)
        (message "Inserted smerge-style conflict for function `%s`" function-name)))))

(defun sorend/gptel-elisp-variable-doc (name)
  "Try to return documentation for variable NAME"
  (let ((var (intern-soft name)))
    (if (and var (boundp var))
        (documentation-property var 'variable-documentation)
      (format "No documentation found for variable: %s" name))))

(defun sorend/gptel-elisp-function-doc (name)
  "Try to return documentation for function NAME"
  (let ((func (intern-soft name)))
    (if (and func (fboundp func))
        (documentation func)
      (format "No documentation found for function: %s" name))))

(defun sorend/gptel-elisp-describe-symbol (name)
  "Return the source code for function or variable NAME as a string. If source isn't found, falls back to the Emacs Lisp object sexp."
  (let* ((sym (if (symbolp name) name (intern name)))
         (callable (or (functionp sym) (macrop sym)))
         (find-fn (if callable #'find-function-noselect #'find-variable-noselect)))
    (condition-case nil
        (let* ((res (funcall find-fn sym))
               (buf (car res))
               (pos (cdr res)))
          (with-current-buffer buf
            (save-excursion
              (goto-char pos)
              (buffer-substring-no-properties
               (point)
               (progn (end-of-defun) (point))))))
      (error
       (let ((obj (if callable
                      (symbol-function sym)
                    (symbol-value sym))))
         (pp-to-string obj))))))


(defun sorend/gptel-elisp-describe-symbol-fuzzy (name &optional limit)
  "List all functions and variables that fuzzily match the given name, along with a one-sentence summary, with an optional limit on the number of results."
  (let ((matches '()))
    ;; Search functions
    (mapatoms (lambda (sym)
                (when (and (fboundp sym) (string-match-p name (symbol-name sym)))
                  (push (format "Function: %s - %s"
                                (symbol-name sym)
                                (or (ignore-errors
                                      (documentation sym))
                                    "No documentation available."))
                        matches))))
    ;; Search variables
    (mapatoms (lambda (sym)
                (when (and (boundp sym) (string-match-p name (symbol-name sym)))
                  (push (format "Variable: %s - %s"
                                (symbol-name sym)
                                (or (ignore-errors
                                      (documentation-property sym 'variable-documentation))
                                    "No documentation available."))
                        matches))))
    ;; Apply limit if specified
    (when (and limit (> (length matches) limit))
      (setq matches (seq-take matches limit)))
    (if matches
        (string-join matches "
")
      (format "No matches found for name: %s" name))))


;;;###autoload
(defun workspace-read-file (file-path)
  "Read contents of FILE-PATH in the current project.
FILE-PATH should be relative to the project root.
Returns the file contents as a string."
  (interactive "fFile to read: ")
  (let ((resolved-path (sorend/gptel--validate-path file-path)))
    (if (file-exists-p resolved-path)
        (if (file-readable-p resolved-path)
            (with-temp-buffer
              (insert-file-contents resolved-path)
              (buffer-string))
          (error "File %s is not readable" file-path))
      (error "File %s does not exist" file-path))))

;;;###autoload
(defun workspace-write-file (file-path content &optional create-dirs)
  "Write CONTENT to FILE-PATH in the current project.
FILE-PATH should be relative to the project root.
If CREATE-DIRS is non-nil, create parent directories if they don't exist.
Returns the relative path of the written file."
  (interactive "FFile to write: \nsContent: \nP")
  (let ((resolved-path (sorend/gptel--validate-path file-path)))
    (when create-dirs
      (let ((dir (file-name-directory resolved-path)))
        (unless (file-exists-p dir)
          (make-directory dir t))))
    (with-temp-file resolved-path
      (insert content))
    (sorend/gptel--relative-path resolved-path)))

;;;###autoload
(defun workspace-edit-file (file-path content &optional mode)
  "Edit FILE-PATH in the current project with CONTENT.
FILE-PATH should be relative to the project root.
MODE can be:
- 'replace (default): Replace entire file content
- 'append: Append content to end of file
- 'prepend: Prepend content to beginning of file
Returns the relative path of the edited file."
  (interactive "fFile to edit: \nsContent: \ncMode (r/a/p): ")
  (let ((resolved-path (sorend/gptel--validate-path file-path))
        (edit-mode (or mode 'replace)))
    (unless (file-exists-p resolved-path)
      (error "File %s does not exist" file-path))
    (unless (file-writable-p resolved-path)
      (error "File %s is not writable" file-path))

    (pcase edit-mode
      ('replace
       (with-temp-file resolved-path
         (insert content)))
      ('append
       (with-temp-buffer
         (when (file-exists-p resolved-path)
           (insert-file-contents resolved-path))
         (goto-char (point-max))
         (insert content)
         (write-file resolved-path)))
      ('prepend
       (with-temp-buffer
         (insert content)
         (when (file-exists-p resolved-path)
           (insert-file-contents resolved-path))
         (write-file resolved-path)))
      (_ (error "Invalid edit mode: %s" edit-mode)))
    (sorend/gptel--relative-path resolved-path)))

;;;###autoload
(defun workspace-list-files (&optional directory pattern recursive include-dirs)
  "List files in the current project.
DIRECTORY is optional subdirectory within project (relative to project root).
PATTERN is an optional regex to filter filenames.
If RECURSIVE is non-nil, search subdirectories.
If INCLUDE-DIRS is non-nil, include directories in results.
Returns a list of relative file paths from project root."
  (interactive "DDirectory: \nsPattern (optional): \nP\nP")
  (let* ((project-root (sorend/gptel--project-root))
         (search-dir (if directory
                         (sorend/gptel--validate-path directory)
                       project-root))
         (project (sorend/gptel--current-project))
         (files '())
         (count 0))

    ;; Use project-files if available and no specific directory given
    (if (and project (not directory) sorend-gptel-respect-ignores)
        (progn
          (dolist (file (project-files project))
            (when (< count sorend-gptel-max-files)
              (let ((relative-path (sorend/gptel--relative-path file)))
                (when (or (not pattern)
                          (string-match-p pattern relative-path))
                  (push relative-path files)
                  (setq count (1+ count))))))
          (when include-dirs
            ;; Add directories from project files
            (let ((dirs (delete-dups
                        (mapcar #'file-name-directory files))))
              (dolist (dir dirs)
                (when (and dir (not (string= dir "./")))
                  (push (directory-file-name dir) files))))))

      ;; Fallback to directory-files
      (dolist (file (if recursive
                        (directory-files-recursively search-dir ".*" include-dirs)
                      (directory-files search-dir t "^[^.]" include-dirs)))
        (when (< count sorend-gptel-max-files)
          (let ((relative-path (sorend/gptel--relative-path file)))
            (when (or (not pattern)
                      (string-match-p pattern relative-path))
              (when (or include-dirs (not (file-directory-p file)))
                (push relative-path files)
                (setq count (1+ count))))))))

    (sort (delete-dups files) #'string<)))

;;;###autoload
(defun workspace-project-info ()
  "Get information about the current project.
Returns a plist with project details."
  (interactive)
  (let ((project (sorend/gptel--current-project)))
    (if project
        (list :has-project t
              :project-root (sorend/gptel--relative-path (project-root project))
              :project-type (when (fboundp 'project-try-vc)
                              (car (project-try-vc (project-root project))))
              :in-project-buffer (project-current t))
      (list :has-project nil
            :default-directory (sorend/gptel--relative-path default-directory)))))

;; gptel tool registration

;;;###autoload
(defun sorend-gptel-register-gptel-tools ()
  "Register project-aware workspace tools with gptel."
  (interactive)

  ;; Project info tool
  (gptel-make-tool
   :name "project-info"
   :description "Get information about the current Emacs project"
   :parameters '(:type object :properties () :required ())
   :function #'workspace-project-info
   :category "emacs")

  ;; Read file tool
  (gptel-make-tool
   :name "read-file"
   :description "Read the contents of a file in the current project"
   :parameters '(:type object
                 :properties (:file-path (:type string
                                          :description "Path to the file to read, relative to project root"))
                 :required (:file-path))
   :function #'workspace-read-file
   :category "emacs")

  ;; Write file tool
  (gptel-make-tool
   :name "write-file"
   :description "Write content to a file in the current project"
   :parameters '(:type object
                 :properties (:file-path (:type string
                                          :description "Path to the file to write, relative to project root")
                             :content (:type string
                                       :description "Content to write to the file")
                             :create-dirs (:type boolean
                                           :description "Create parent directories if they don't exist"
                                           :default nil))
                 :required (:file-path :content))
   :function (lambda (file-path content &optional create-dirs)
               (workspace-write-file file-path content create-dirs))
   :category "emacs")

  ;; Edit file tool
  (gptel-make-tool
   :name "edit-file"
   :description "Edit an existing file in the current project"
   :parameters '(:type object
                 :properties (:file-path (:type string
                                          :description "Path to the file to edit, relative to project root")
                             :content (:type string
                                       :description "Content to add/replace in the file")
                             :mode (:type string
                                    :enum ("replace" "append" "prepend")
                                    :description "Edit mode: replace entire content, append to end, or prepend to beginning"
                                    :default "replace"))
                 :required (:file-path :content))
   :function (lambda (file-path content &optional mode)
               (workspace-edit-file file-path content (intern (or mode "replace"))))
   :category "emacs")

  ;; List files tool
  (gptel-make-tool
   :name "list-files"
   :description "List files in the current project or a subdirectory"
   :parameters '(:type object
                       :properties (:directory (:type string :description "Directory to search within project (relative to project root, optional)")
                                               :pattern (:type string :description "Regex pattern to filter filenames (optional)")
                                               :recursive (:type boolean :description "Search subdirectories recursively" :default nil)
                                               :include-dirs (:type boolean :description "Include directories in results" :default nil))
                       :required ())
   :function (lambda (&optional directory pattern recursive include-dirs)
               (workspace-list-files directory pattern recursive include-dirs))
   :category "emacs")

  (gptel-make-tool
   :function #'sorend/gptel-emacs--describe-variable
   :name  "describe-variable"
   :description "Returns variable contents. After calling this tool, stop. Evaluate if the result helps. Then continue fulfilling user's request."
   :args (list '(:name "var"
                       :type string
                       :description "Variable name"))
   :category "elisp")

 (gptel-make-tool
  :function #'sorend/gptel-emacs--describe-function
  :name  "describe-function"
  :description "Returns function description. After calling this tool, stop. Evaluate if the result helps. Then continue fulfilling user's request."
  :args (list '(:name "fun"
                      :type string
                      :description "Function name"
                      :optional t))
  :category "elisp")

 (gptel-make-tool
  :function #'sorend/gptel-elisp--run-eval
  :name  "elisp-run-eval"
  :description "Use eval to evaluate elisp code. After calling this tool, stop. Then continue fulfilling user's request."
  :args (list '(:name "command"
                      :type string
                      :description "The elisp code to evaluate."))
  :category "elisp"
  :confirm t)

 (gptel-make-tool
  :function #'sorend/gptel-elisp--defun-region
  :name  "defun-region"
  :description "Read starting end end point of a function definition."
  :args (list '(:name "function"
                      :type string
                      :description "The name of the function to search.")
              '(:name "buffer"
                      :type string
                      :description "The buffer to search in"))
  :category "elisp")

 (gptel-make-tool
  :function #'sorend/gptel-elisp--smerge-replace-defun-region
  :name  "smerge-replace-defun-region"
  :description "Search for a function name, and replace the complete defun with a diff block. After calling this tool, stop. Then continue fulfilling user's request."
  :args (list '(:name "function-name" :type string :description "The elisp code to evaluate.")
              '(:name "new-string" :type string :description "The new function definition")
              '(:name "buffer" :type string :description "The buffer to perform the replacement"))
  :category "elisp")

 (gptel-make-tool
  :function #'sorend/gptel-elisp-variable-doc
  :name "elisp-variable-doc"
  :category "elisp"
  :description "Retrieve the documentation for an Elisp variable."
  :args (list '(:name "name" :type "string" :description "The name of the variable to retrieve documentation for.")))

 (gptel-make-tool
  :function #'sorend/gptel-elisp-function-doc
  :name "elisp-function-doc"
  :category "elisp"
  :description "Retrieve the documentation for an Elisp function."
  :args (list '(:name "name" :type "string" :description "The name of the function to retrieve documentation for.")))

 (gptel-make-tool
  :function #'sorend/gptel-elisp-describe-symbol
  :name "elisp-describe-symbol"
  :category "elisp"
  :description "Return source code for function or variable."
  :args (list '(:name "name" :type "string" :description "The name of the symbol to receive sources for.")))

 (gptel-make-tool
  :function #'sorend/gptel-elisp-describe-symbol-fuzzy
  :name "elisp-fuzzy-match"
  :category "elisp"
  :description "List all functions and variables that fuzzily match the given name, along with a one-sentence summary, with an optional limit on the number of results."
  :args (list '(:name "name"
                      :type "string"
                      :description "The name to match against available functions and variables.")
              '(:name "limit"
                      :type "integer"
                      :optional t
                      :description "The maximum number of results to return. If nil, returns all matches.")))

 (message "Project-aware workspace tools registered with gptel"))



;;;###autoload
(defun sorend-gptel-unregister-gptel-tools ()
  "Unregister workspace tools from gptel."
  (interactive)
  (dolist (tool-name '("project-info" "read-file" "write-file" "edit-file" "list-files" "describe-variable" "describe-function" "elisp-run-eval" "defun-region" "smerge-replace-defun-region" "elisp-variable-doc" "elisp-function-doc" "elisp-describe-symbol" "elisp-fuzzy-match"))
    (gptel-remove-tool tool-name))
  (message "Workspace tools unregistered from gptel"))

(provide 'sorend-gptel)

;;; sorend-gptel.el ends here
