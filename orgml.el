;;; orgml.el --- JSON to Org document converter

;;; Commentary:
;; This package provides functionality to convert JSON strings into
;; Org document representation following specific formatting rules.

;;; Code:

(require 'json)
(require 'cl-lib)

(defun orgml-json-to-org (json-string)
  "Convert JSON-STRING to org document representation.
The conversion follows these rules:
- Numbers become headings with :number: tag
- Strings become headings with :string: tag
- Booleans become headings with :bool: tag
- Null becomes heading with :null: tag
- Arrays become ~array~ heading with sub-items
- Objects become ~object~ heading with key-value pairs

Returns org document string."
  (let ((data (json-read-from-string json-string)))
    (orgml--convert-value data 1)))

(defun orgml--convert-value (value level)
  "Convert VALUE to org format at heading LEVEL."
  (cond
   ((numberp value)
    (orgml--make-heading level (number-to-string value) "number"))

   ((stringp value)
    (orgml--make-heading level value "string"))

   ((eq value t)
    (orgml--make-heading level "t" "bool"))

   ((eq value :json-false)
    (orgml--make-heading level "false" "bool"))

   ((eq value json-null)
    (orgml--make-heading level "null" "null"))

   ((vectorp value)
    (orgml--convert-array value level))

   ((hash-table-p value)
    (orgml--convert-object value level))

   ((listp value)
    ;; Handle alist (key-value pairs)
    (if (orgml--is-alist value)
        (orgml--convert-alist value level)
      (orgml--convert-list value level)))

   (t
    (orgml--make-heading level (format "%S" value) "unknown"))))

(defun orgml--convert-array (array level)
  "Convert ARRAY (vector) to org format at LEVEL."
  (let* ((array-length (length array))
         (result (orgml--make-heading level (orgml--format-array-heading array-length) nil)))
    (dotimes (i array-length)
      (setq result (concat result "\n"
                           (orgml--convert-value (aref array i) (+ level 1)))))
    result))

(defun orgml--convert-list (list level)
  "Convert LIST to org format at LEVEL."
  (let* ((list-length (length list))
         (result (orgml--make-heading level (orgml--format-array-heading list-length) nil)))
    (dolist (item list)
      (setq result (concat result "\n"
                           (orgml--convert-value item (+ level 1)))))
    result))

(defun orgml--convert-object (hash-table level)
  "Convert HASH-TABLE (object) to org format at LEVEL."
  (let* ((object-size (hash-table-count hash-table))
         (result (orgml--make-heading level (orgml--format-object-heading object-size) nil)))
    (maphash (lambda (key value)
               (let ((key-str (if (symbolp key) (symbol-name key) (format "%s" key))))
                 (setq result (concat result "\n"
                                      (orgml--make-key-value-heading
                                       (+ level 1) key-str value)))))
             hash-table)
    result))

(defun orgml--convert-alist (alist level)
  "Convert ALIST (association list) to org format at LEVEL."
  (let* ((alist-length (length alist))
         (result (orgml--make-heading level (orgml--format-object-heading alist-length) nil)))
    (dolist (pair alist)
      (let ((key (car pair))
            (value (cdr pair)))
        (let ((key-str (if (symbolp key) (symbol-name key) (format "%s" key))))
          (setq result (concat result "\n"
                               (orgml--make-key-value-heading
                                (+ level 1) key-str value))))))
    result))

(defun orgml--make-heading (level text tag)
  "Create org heading at LEVEL with TEXT and optional TAG."
  (let ((stars (make-string level ?*)))
    (if tag
        (format "%s %s :%s:" stars text tag)
      (format "%s %s" stars text))))

(defun orgml--make-key-value-heading (level key value)
  "Create org heading at LEVEL for KEY-VALUE pair."
  (let ((stars (make-string level ?*)))
    (cond
     ((or (vectorp value) (and (listp value) (not (eq value json-null)) (not (orgml--is-alist value))))
      ;; For arrays, create heading with ~array~ and expand items
      (let* ((array-length (if (vectorp value) (length value) (length value)))
             (result (format "%s =%s=: %s" stars key (orgml--format-array-heading array-length))))
        (if (vectorp value)
            (dotimes (i (length value))
              (setq result (concat result "\n"
                                   (orgml--convert-value (aref value i) (+ level 1)))))
          (dolist (item value)
            (setq result (concat result "\n"
                                 (orgml--convert-value item (+ level 1))))))
        result))
     ((or (hash-table-p value) (orgml--is-alist value))
      ;; For objects, create heading with ~object~ and expand key-value pairs
      (let* ((object-length (if (hash-table-p value) (hash-table-count value) (length value)))
             (result (format "%s =%s=: %s" stars key (orgml--format-object-heading object-length))))
        (if (hash-table-p value)
            (maphash (lambda (k v)
                       (let ((key-str (if (symbolp k) (symbol-name k) (format "%s" k))))
                         (setq result (concat result "\n"
                                              (orgml--make-key-value-heading
                                               (+ level 1) key-str v)))))
                     value)
          (dolist (pair value)
            (let ((k (car pair))
                  (v (cdr pair)))
              (let ((key-str (if (symbolp k) (symbol-name k) (format "%s" k))))
                (setq result (concat result "\n"
                                     (orgml--make-key-value-heading
                                      (+ level 1) key-str v)))))))
        result))
     (t
      ;; For primitive values, use inline format
      (let ((value-str (orgml--value-to-string value))
            (tag (orgml--get-value-tag value)))
        (format "%s =%s=: %s :%s:" stars key value-str tag))))))

(defun orgml--value-to-string (value)
  "Convert VALUE to string representation."
  (cond
   ((numberp value) (number-to-string value))
   ((stringp value) value)
   ((eq value t) "true")
   ((eq value :json-false) "false")
   ((eq value json-null) "null")
   (t (format "%S" value))))

(defun orgml--get-value-tag (value)
  "Get the appropriate tag for VALUE."
  (cond
   ((numberp value) "number")
   ((stringp value) "string")
   ((eq value t) "bool")
   ((eq value :json-false) "bool")
   ((eq value json-null) "null")
   ((vectorp value) "array")
   ((hash-table-p value) "object")
   ((listp value) "array")
   (t "unknown")))

(defun orgml--is-alist (list)
  "Check if LIST is an association list (list of cons cells)."
  (and (listp list)
       (not (null list))
       (cl-every (lambda (x) (consp x)) list)))

(defun orgml--format-array-heading (length)
  "Format array heading with LENGTH information."
  (cond
   ((= length 0) "~[empty]~")
   ((= length 1) "~[1 item]~")
   (t (format "~[%d items]~" length))))

(defun orgml--format-object-heading (length)
  "Format object heading with LENGTH information."
  (cond
   ((= length 0) "~{empty}~")
   ((= length 1) "~{1 item}~")
   (t (format "~{%d items}~" length))))

(defun orgml-new-buffer ()
  "Convert JSON from active region (or entire buffer) to org format in new readonly buffer.
Creates a new window with a readonly buffer displaying the converted org content."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (json-text (buffer-substring-no-properties start end))
         (org-content (condition-case err
                          (orgml-json-to-org json-text)
                        (error (format "Error converting JSON: %s" (error-message-string err)))))
         (buffer-name (generate-new-buffer-name "*JSON to Org*")))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert org-content)
      (org-mode)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (display-buffer (current-buffer) '((display-buffer-pop-up-window))))
    (message "Converted JSON to org format in buffer: %s" buffer-name)))

(provide 'orgml)

;;; orgml.el ends here
