;;; test-orgml.el --- Tests for orgml.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for orgml.el using ERT (Emacs Lisp Regression Testing).
;; This file tests all public and internal functions with various edge cases.

;;; Code:

(require 'ert)
(require 'orgml)
(require 'json)

;;; Test Data Setup

(defvar orgml-test-simple-json
  "{\"name\": \"John\", \"age\": 30, \"active\": true}"
  "Simple JSON object for testing.")

(defvar orgml-test-complex-json
  "{\"users\": [{\"id\": 1, \"name\": \"Alice\"}, {\"id\": 2, \"name\": \"Bob\"}], \"config\": {\"debug\": false, \"timeout\": null}}"
  "Complex nested JSON for testing.")

(defvar orgml-test-array-json
  "[1, 2, 3, \"hello\", true, null]"
  "Array JSON for testing.")

(defvar orgml-test-empty-json
  "{\"empty_array\": [], \"empty_object\": {}}"
  "JSON with empty containers for testing.")

;;; Core Function Tests

(ert-deftest orgml-test-json-to-org-simple-object ()
  "Test conversion of simple JSON object."
  (let ((result (orgml-json-to-org orgml-test-simple-json)))
    (should (stringp result))
    (should (string-match-p "\\* ~{3 items}~" result))
    (should (string-match-p "\\*\\* =name=: John :string:" result))
    (should (string-match-p "\\*\\* =age=: 30 :number:" result))
    (should (string-match-p "\\*\\* =active=: true :bool:" result))))

(ert-deftest orgml-test-json-to-org-array ()
  "Test conversion of JSON array."
  (let ((result (orgml-json-to-org orgml-test-array-json)))
    (should (stringp result))
    (should (string-match-p "\\* ~\\[6 items\\]~" result))
    (should (string-match-p "\\*\\* 1 :number:" result))
    (should (string-match-p "\\*\\* hello :string:" result))
    (should (string-match-p "\\*\\* true :bool:" result))
    (should (string-match-p "\\*\\* null :null:" result))))

(ert-deftest orgml-test-json-to-org-complex ()
  "Test conversion of complex nested JSON."
  (let ((result (orgml-json-to-org orgml-test-complex-json)))
    (should (stringp result))
    (should (string-match-p "\\* ~{2 items}~" result))
    (should (string-match-p "\\*\\* =users=: ~\\[2 items\\]~" result))
    (should (string-match-p "\\*\\*\\* ~{2 items}~" result))
    (should (string-match-p "\\*\\*\\*\\* =id=: 1 :number:" result))
    (should (string-match-p "\\*\\*\\*\\* =name=: Alice :string:" result))))

(ert-deftest orgml-test-json-to-org-empty-containers ()
  "Test conversion of empty arrays and objects."
  (let ((result (orgml-json-to-org orgml-test-empty-json)))
    (should (stringp result))
    (should (string-match-p "\\*\\* =empty_array=: ~\\[empty\\]~" result))
    (should (string-match-p "\\*\\* =empty_object=: ~{empty}~" result))))

(ert-deftest orgml-test-json-to-org-invalid-json ()
  "Test error handling for invalid JSON."
  (should-error (orgml-json-to-org "{invalid json")))

;;; Value Conversion Tests

(ert-deftest orgml-test-convert-value-number ()
  "Test conversion of number values."
  (should (equal (orgml--convert-value 42 1) "* 42 :number:"))
  (should (equal (orgml--convert-value 3.14 2) "** 3.14 :number:"))
  (should (equal (orgml--convert-value 0 1) "* 0 :number:"))
  (should (equal (orgml--convert-value -5 1) "* -5 :number:")))

(ert-deftest orgml-test-convert-value-string ()
  "Test conversion of string values."
  (should (equal (orgml--convert-value "hello" 1) "* hello :string:"))
  (should (equal (orgml--convert-value "" 2) "**  :string:"))
  (should (equal (orgml--convert-value "with spaces" 1) "* with spaces :string:")))

(ert-deftest orgml-test-convert-value-boolean ()
  "Test conversion of boolean values."
  (should (equal (orgml--convert-value t 1) "* true :bool:"))
  (should (equal (orgml--convert-value :json-false 1) "* false :bool:")))

(ert-deftest orgml-test-convert-value-null ()
  "Test conversion of null value."
  (should (equal (orgml--convert-value json-null 1) "* null :null:")))

(ert-deftest orgml-test-convert-value-vector ()
  "Test conversion of vector (array) values."
  (let ((vec [1 2 3]))
    (let ((result (orgml--convert-value vec 1)))
      (should (string-match-p "\\* ~\\[3 items\\]~" result))
      (should (string-match-p "\\*\\* 1 :number:" result))
      (should (string-match-p "\\*\\* 2 :number:" result))
      (should (string-match-p "\\*\\* 3 :number:" result)))))

(ert-deftest orgml-test-convert-value-empty-vector ()
  "Test conversion of empty vector."
  (let ((vec []))
    (let ((result (orgml--convert-value vec 1)))
      (should (equal result "* ~[empty]~")))))

;;; Array and List Conversion Tests

(ert-deftest orgml-test-convert-array ()
  "Test array conversion function."
  (let ((array [1 "test" t]))
    (let ((result (orgml--convert-array array 1)))
      (should (string-match-p "\\* ~\\[3 items\\]~" result))
      (should (string-match-p "\\*\\* 1 :number:" result))
      (should (string-match-p "\\*\\* test :string:" result))
      (should (string-match-p "\\*\\* true :bool:" result)))))


;;; Object and Hash Table Tests

(ert-deftest orgml-test-convert-object ()
  "Test hash table (object) conversion."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "name" "John" hash)
    (puthash "age" 30 hash)
    (let ((result (orgml--convert-object hash 1)))
      (should (string-match-p "\\* ~{2 items}~" result))
      (should (string-match-p "\\*\\* =name=: John :string:" result))
      (should (string-match-p "\\*\\* =age=: 30 :number:" result)))))


;;; Utility Function Tests

(ert-deftest orgml-test-make-heading ()
  "Test heading creation function."
  (should (equal (orgml--make-heading 1 "test" "tag") "* test :tag:"))
  (should (equal (orgml--make-heading 2 "test" nil) "** test"))
  (should (equal (orgml--make-heading 3 "test" "string") "*** test :string:"))
  (should (equal (orgml--make-heading 1 "" "empty") "*  :empty:")))

(ert-deftest orgml-test-make-key-value-heading-primitive ()
  "Test key-value heading for primitive values."
  (should (equal (orgml--make-key-value-heading 1 "name" "John")
                 "* =name=: John :string:"))
  (should (equal (orgml--make-key-value-heading 2 "age" 30)
                 "** =age=: 30 :number:"))
  (should (equal (orgml--make-key-value-heading 1 "active" t)
                 "* =active=: true :bool:"))
  (should (equal (orgml--make-key-value-heading 1 "value" json-null)
                 "* =value=: null :null:")))

(ert-deftest orgml-test-make-key-value-heading-array ()
  "Test key-value heading for array values."
  (let ((result (orgml--make-key-value-heading 1 "items" [1 2])))
    (should (string-match-p "\\* =items=: ~\\[2 items\\]~" result))
    (should (string-match-p "\\*\\* 1 :number:" result))
    (should (string-match-p "\\*\\* 2 :number:" result))))

(ert-deftest orgml-test-value-to-string ()
  "Test value to string conversion."
  (should (equal (orgml--value-to-string 42) "42"))
  (should (equal (orgml--value-to-string "hello") "hello"))
  (should (equal (orgml--value-to-string t) "true"))
  (should (equal (orgml--value-to-string :json-false) "false"))
  (should (equal (orgml--value-to-string json-null) "null")))

(ert-deftest orgml-test-get-value-tag ()
  "Test value tag determination."
  (should (equal (orgml--get-value-tag 42) "number"))
  (should (equal (orgml--get-value-tag "hello") "string"))
  (should (equal (orgml--get-value-tag t) "bool"))
  (should (equal (orgml--get-value-tag :json-false) "bool"))
  (should (equal (orgml--get-value-tag json-null) "null"))
  (should (equal (orgml--get-value-tag [1 2 3]) "array"))
  (should (equal (orgml--get-value-tag (make-hash-table)) "object")))


(ert-deftest orgml-test-format-array-heading ()
  "Test array heading formatting."
  (should (equal (orgml--format-array-heading 0) "~[empty]~"))
  (should (equal (orgml--format-array-heading 1) "~[1 item]~"))
  (should (equal (orgml--format-array-heading 5) "~[5 items]~")))

(ert-deftest orgml-test-format-object-heading ()
  "Test object heading formatting."
  (should (equal (orgml--format-object-heading 0) "~{empty}~"))
  (should (equal (orgml--format-object-heading 1) "~{1 item}~"))
  (should (equal (orgml--format-object-heading 3) "~{3 items}~")))

;;; Edge Cases and Error Handling

(ert-deftest orgml-test-deeply-nested-structure ()
  "Test deeply nested JSON structures."
  (let ((nested-json "{\"level1\": {\"level2\": {\"level3\": {\"value\": \"deep\"}}}}"))
    (let ((result (orgml-json-to-org nested-json)))
      (should (string-match-p "\\* ~{1 item}~" result))
      (should (string-match-p "\\*\\* =level1=: ~{1 item}~" result))
      (should (string-match-p "\\*\\*\\* =level2=: ~{1 item}~" result))
      (should (string-match-p "\\*\\*\\*\\* =level3=: ~{1 item}~" result))
      (should (string-match-p "\\*\\*\\*\\*\\* =value=: deep :string:" result)))))

(ert-deftest orgml-test-mixed-types-array ()
  "Test array with mixed data types."
  (let ((mixed-json "[42, \"string\", true, false, null, [], {}]"))
    (let ((result (orgml-json-to-org mixed-json)))
      (should (string-match-p "\\* ~\\[7 items\\]~" result))
      (should (string-match-p "\\*\\* 42 :number:" result))
      (should (string-match-p "\\*\\* string :string:" result))
      (should (string-match-p "\\*\\* true :bool:" result))
      (should (string-match-p "\\*\\* false :bool:" result))
      (should (string-match-p "\\*\\* null :null:" result))
      (should (string-match-p "\\*\\* ~\\[empty\\]~" result))
      (should (string-match-p "\\*\\* ~{empty}~" result)))))

(ert-deftest orgml-test-special-characters-in-strings ()
  "Test strings with special characters."
  (let ((special-json "{\"key with spaces\": \"value\\nwith\\nnewlines\", \"symbols\": \"@#$%^&*()\"}"))
    (let ((result (orgml-json-to-org special-json)))
      (should (stringp result))
      (should (string-match-p "=key with spaces=" result))
      (should (string-match-p "=symbols=" result)))))

(ert-deftest orgml-test-numeric-edge-cases ()
  "Test various numeric formats."
  (let ((numeric-json "{\"integer\": 42, \"float\": 3.14159, \"negative\": -100, \"zero\": 0, \"scientific\": 1e5}"))
    (let ((result (orgml-json-to-org numeric-json)))
      (should (string-match-p "=integer=: 42 :number:" result))
      (should (string-match-p "=float=: 3.14159 :number:" result))
      (should (string-match-p "=negative=: -100 :number:" result))
      (should (string-match-p "=zero=: 0 :number:" result))
      (should (string-match-p "=scientific=: 100000.0 :number:" result)))))

;;; Integration Tests

(ert-deftest orgml-test-real-world-json ()
  "Test with realistic JSON structure."
  (let ((real-json "{
    \"user\": {
      \"id\": 123,
      \"name\": \"Alice Smith\",
      \"email\": \"alice@example.com\",
      \"preferences\": {
        \"theme\": \"dark\",
        \"notifications\": true
      },
      \"roles\": [\"user\", \"admin\"]
    },
    \"metadata\": {
      \"created\": \"2023-01-01\",
      \"updated\": null,
      \"version\": 1.2
    }
  }"))
    (let ((result (orgml-json-to-org real-json)))
      (should (stringp result))
      (should (> (length result) 100))
      (should (string-match-p "\\* ~{2 items}~" result))
      (should (string-match-p "=user=" result))
      (should (string-match-p "=metadata=" result))
      (should (string-match-p "=preferences=" result))
      (should (string-match-p "=roles=: ~\\[2 items\\]~" result)))))

;;; Interactive Function Tests

(ert-deftest orgml-test-new-buffer-creation ()
  "Test buffer creation for interactive function (mock test)."
  ;; This is a simplified test since the interactive function involves
  ;; buffer manipulation and display which is hard to test in batch mode
  (let ((json-string "{\"test\": \"value\"}"))
    (with-temp-buffer
      (insert json-string)
      (let ((converted (orgml-json-to-org json-string)))
        (should (stringp converted))
        (should (string-match-p "\\* ~{1 item}~" converted))
        (should (string-match-p "\\*\\* =test=: value :string:" converted))))))

;;; Performance and Stress Tests

(ert-deftest orgml-test-large-array ()
  "Test performance with larger arrays."
  (let ((large-array-json (concat "[" (mapconcat #'number-to-string (number-sequence 1 100) ", ") "]")))
    (let ((result (orgml-json-to-org large-array-json)))
      (should (string-match-p "\\* ~\\[100 items\\]~" result))
      (should (string-match-p "\\*\\* 1 :number:" result))
      (should (string-match-p "\\*\\* 100 :number:" result)))))

;;; Test Runner Helper

(defun orgml-run-all-tests ()
  "Run all orgml tests and display results."
  (interactive)
  (ert-run-tests-interactively "orgml-test"))

(provide 'test-orgml)

;;; test-orgml.el ends here