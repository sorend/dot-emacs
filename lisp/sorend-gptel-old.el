
(require 'gptel)

;; get absolute list of files in the current project
(defun sorend/list-project-files ()
  (let* ((project (project-current))
         (root (and project (project-root project))))
    (when root
      (mapcar (lambda (file)
                (expand-file-name file root))
              (project-files project)))))

(defun sorend/search-project-files (search-str)
  "Return a list of full paths to files in the current project containing SEARCH-STR."
  (let* ((project (project-current t))
         (root (project-root project))
         (files (project-files project))
         (matching-files
          (cl-remove-if-not
           (lambda (file)
             (let ((fullpath (expand-file-name file root)))
               (when (file-readable-p fullpath)
                 (with-temp-buffer
                   (insert-file-contents fullpath)
                   (search-forward search-str nil t)))))
           files)))
    (mapcar (lambda (file) (expand-file-name file root)) matching-files)))

(gptel-make-tool
 :function #'sorend/search-project-files
 :name "search-project-files"
 :description "Search files in the current project that contain a string. Returns list of filepaths that can be read with 'read_file'."
 :category "project"
 :include t)

(gptel-make-tool
 :function #'sorend/list-project-files
 :name "list-project-files"
 :description "List all files in the current project. Returns a list of filepaths that can be read with 'read_file'."
 :category "project"
 :include t)

(gptel-make-tool
 :function (lambda (filepath)
             (with-temp-buffer
               (insert-file-contents (expand-file-name filepath))
               (buffer-string)))
 :name "read_file"
 :description "Read the contents of a file."
 :args (list '(:name "filepath"
                     :type string
                     :description "Path to the file to read. Supports relative paths and ~."))
 :category "filesystem"
 :include t)

(provide 'sorend-gptel-old)
