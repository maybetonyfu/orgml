;;; orgml.el --- JSON to Org document converter -*- lexical-binding: t;-*-

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
  (let ((json-object-type 'hash-table)
        (json-array-type 'vector)
        (json-key-type 'string))
    (let ((data (json-read-from-string json-string)))
      (orgml--convert-value data 1))))

(defun orgml--convert-value (value level)
  "Convert VALUE to org format at heading LEVEL."
  (cond
   ((numberp value)
    (orgml--make-heading level (number-to-string value) "number"))

   ((stringp value)
    (orgml--make-heading level value "string"))

   ((eq value t)
    (orgml--make-heading level "true" "bool"))

   ((eq value :json-false)
    (orgml--make-heading level "false" "bool"))

   ((eq value json-null)
    (orgml--make-heading level "null" "null"))

   ((vectorp value)
    (orgml--convert-array value level))

   ((hash-table-p value)
    (orgml--convert-object value level))

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
     ((vectorp value)
      ;; For arrays, create heading with ~array~ and expand items
      (let* ((array-length (length value))
             (result (format "%s =%s=: %s" stars key (orgml--format-array-heading array-length))))
        (dotimes (i (length value))
          (setq result (concat result "\n"
                               (orgml--convert-value (aref value i) (+ level 1)))))
        result))
     ((hash-table-p value)
      ;; For objects, create heading with ~object~ and expand key-value pairs
      (let* ((object-length (hash-table-count value))
             (result (format "%s =%s=: %s" stars key (orgml--format-object-heading object-length))))
        (maphash (lambda (k v)
                   (let ((key-str (if (symbolp k) (symbol-name k) (format "%s" k))))
                     (setq result (concat result "\n"
                                          (orgml--make-key-value-heading
                                           (+ level 1) key-str v)))))
                 value)
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
   (t "unknown")))


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

(defun orgml-quit-window ()
  "Close the current window and kill the buffer."
  (interactive)
  (quit-window t))

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
      (org-indent-mode)
      (setq buffer-read-only t)
      ;; Set up local keymap to handle 'q' for closing window
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key (kbd "q") 'orgml-quit-window)
      (goto-char (point-min))
      (display-buffer (current-buffer) '((display-buffer-pop-up-window))))
    (message "Converted JSON to org format in buffer: %s" buffer-name)))

(provide 'orgml)

;;; orgml.el ends here
