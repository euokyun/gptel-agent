;;; gptel-agent-test-tools.el --- Unit tests for gptel-agent filesystem tools -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; ERT tests for the filesystem tools in gptel-agent-tools.el: "Read",
;; "Grep", "Glob" and "Edit".  These call the tool implementations
;; directly (`gptel-agent--read-file-lines', `gptel-agent--grep',
;; `gptel-agent--glob' and `gptel-agent--edit-files'), not via the LLM.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)

(defmacro gptel-agent-test-with-temp-dir (&rest body)
  "Run BODY with `default-directory' bound to a fresh temporary directory.
The directory and its contents are deleted afterwards.  Within BODY,
TEMP-DIR is bound to the directory path."
  (declare (indent 0))
  `(let ((temp-dir (make-temp-file "gptel-agent-test-" t)))
     (unwind-protect
         (let ((default-directory temp-dir))
           ,@body)
       (delete-directory temp-dir 'recursive))))

(defmacro gptel-agent-test-with-file (name content &rest body)
  "Write CONTENT to file NAME in a temp dir and run BODY.
`default-directory' is bound to the temporary directory for BODY, so
NAME resolves relative to it.  Within BODY, FILE is bound to the
absolute path of NAME.  The directory is deleted afterwards."
  (declare (indent 2))
  `(gptel-agent-test-with-temp-dir
     (let ((file (expand-file-name ,name)))
       (write-region ,content nil file)
       ,@body)))

(defun gptel-agent-test-file-content (file)
  "Return the text contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;; Read tool

(ert-deftest gptel-agent-test-read-full-file ()
  "Test reading an entire file with no line range."
  (gptel-agent-test-with-file "lines.txt" "one\ntwo\nthree\n"
    (should (equal (gptel-agent--read-file-lines file nil nil)
                   "one\ntwo\nthree\n"))))

(ert-deftest gptel-agent-test-read-line-range ()
  "Test reading lines 2 to 4, inclusive of both ends."
  (gptel-agent-test-with-file "lines.txt" "1\n2\n3\n4\n5\n"
    (should (equal (gptel-agent--read-file-lines file 2 4)
                   "2\n3\n4\n"))))

(ert-deftest gptel-agent-test-read-from-start-line-to-eof ()
  "Test reading from START-LINE to the end when END-LINE is nil."
  (gptel-agent-test-with-file "lines.txt" "1\n2\n3\n4\n5\n"
    (should (equal (gptel-agent--read-file-lines file 3 nil)
                   "3\n4\n5\n"))
    (should (equal (gptel-agent--read-file-lines file 2 nil)
                   "2\n3\n4\n5\n"))
    (should (equal (gptel-agent--read-file-lines file 5 nil)
                   "5\n"))))

(ert-deftest gptel-agent-test-read-from-bof-to-end-line ()
  "Test reading from the beginning when START-LINE is nil."
  (gptel-agent-test-with-file "lines.txt" "1\n2\n3\n4\n5\n"
    (should (equal (gptel-agent--read-file-lines file nil 3)
                   "1\n2\n3\n"))))

(ert-deftest gptel-agent-test-read-without-trailing-newline ()
  "Test reading a file whose last line has no trailing newline."
  (gptel-agent-test-with-file "lines.txt" "1\n2\n3"
    (should (equal (gptel-agent--read-file-lines file nil nil)
                   "1\n2\n3"))
    (should (equal (gptel-agent--read-file-lines file 2 3) "2\n3"))))

(ert-deftest gptel-agent-test-read-past-end-of-file ()
  "Test that a range extending past EOF returns everything available."
  (gptel-agent-test-with-file "lines.txt" "1\n2\n3\n"
    (should (equal (gptel-agent--read-file-lines file 2 100)
                   "2\n3\n"))))

(ert-deftest gptel-agent-test-read-single-line ()
  "Test reading a single line."
  (gptel-agent-test-with-file "lines.txt" "1\n2\n3\n"
    (should (equal (gptel-agent--read-file-lines file 2 2) "2\n"))))

(ert-deftest gptel-agent-test-read-refuses-directory ()
  "Test that reading a directory signals an error."
  (gptel-agent-test-with-temp-dir
    (should-error (gptel-agent--read-file-lines temp-dir nil nil)
                  :type 'error)))

(ert-deftest gptel-agent-test-read-missing-file ()
  "Test that reading a nonexistent file signals an error."
  (gptel-agent-test-with-temp-dir
    (should-error (gptel-agent--read-file-lines file nil nil)
                  :type 'error)))

(ert-deftest gptel-agent-test-read-file-too-large ()
  "Test that reading a large file without a line range errors.
Below the size threshold in KB, the file is read as usual."
  (gptel-agent-test-with-file "big.txt" (concat (make-string 3071 ?x) "\n")
    ;; A 3 KB file: readable with default threshold (400 KB), too big at 1 KB.
    (should (equal (gptel-agent--read-file-lines file nil nil)
                   (concat (make-string 3071 ?x) "\n")))
    (let ((gptel-agent-read-file-size-threshold 1))
      (let ((err (should-error (gptel-agent--read-file-lines file nil nil))))
        (should (string-match-p "too large" (error-message-string err))))
      ;; With an explicit line range, large files are still readable.
      (should (equal (gptel-agent--read-file-lines file 1 1)
                     (concat (make-string 3071 ?x) "\n"))))))

(ert-deftest gptel-agent-test-read-symlink ()
  "Test reading through a symlink to another file."
  (gptel-agent-test-with-temp-dir
    (let ((target (expand-file-name "target.txt")))
      (write-region "one\ntwo\n" nil target)
      (make-symbolic-link target (expand-file-name "link.txt"))
      (should (equal (gptel-agent--read-file-lines
                      (expand-file-name "link.txt") 1 2)
                     "one\ntwo\n")))))

(ert-deftest gptel-agent-test-read-binary-file ()
  "Test that binary content is rejected with an informative error."
  (gptel-agent-test-with-file "binary.bin" (string #xfe #xff 0 1)
    (let ((err (should-error (gptel-agent--read-file-lines file nil nil))))
      (should (string-match-p "binary file" (error-message-string err))))))

;;;; Grep tool

(ert-deftest gptel-agent-test-grep-basic ()
  "Test basic grep over a directory: files sorted, line numbers shown."
  (gptel-agent-test-with-temp-dir
    (write-region "alpha\nneedle here\n" nil (expand-file-name "a.txt"))
    (write-region "needle\nbeta\n" nil (expand-file-name "b.txt"))
    (let ((result (gptel-agent--grep "needle" default-directory)))
      (should (string-match-p "a\.txt\n2:needle here" result))
      (should (string-match-p "b\.txt\n1:needle" result)))))

(ert-deftest gptel-agent-test-grep-single-file ()
  "Test grepping a single file instead of a directory."
  (gptel-agent-test-with-file "a.txt" "alpha\nneedle\n"
    (let ((result (gptel-agent--grep "needle" file)))
      (should (string-match-p "2:needle" result)))))

(ert-deftest gptel-agent-test-grep-no-match ()
  "Test that grep with no matches returns a no-results notice."
  (gptel-agent-test-with-file "a.txt" "alpha\nbeta\n"
    (should (string-match-p "search failed with exit-code 1"
                            (gptel-agent--grep "nonexistent-pattern" file)))))

(ert-deftest gptel-agent-test-grep-glob-filter ()
  "Test that the glob argument restricts searched files."
  (gptel-agent-test-with-temp-dir
    (write-region "needle in .el\n" nil (expand-file-name "code.el"))
    (write-region "needle in .md\n" nil (expand-file-name "doc.md"))
    (let ((result (gptel-agent--grep "needle" default-directory "*.md")))
      (should (string-match-p "doc\.md\n1:needle in \.md" result))
      (should-not (string-match-p "code\.el" result)))))

(ert-deftest gptel-agent-test-grep-context-lines ()
  "Test that context lines surround matches."
  (gptel-agent-test-with-file "a.txt" "one\ntwo\nthree\nfour\n"
    (let ((result (gptel-agent--grep "three" file nil 1)))
      (should (string-match-p "3:three" result))
      (should (string-match-p "^2-two$" result))
      (should (string-match-p "^4-four$" result)))))

(ert-deftest gptel-agent-test-grep-regex ()
  "Test that PCRE-style regexes are honored."
  (gptel-agent-test-with-file "a.txt" "foo123\nbar\n"
    (let ((result (gptel-agent--grep "foo\\d+" file)))
      (should (string-match-p "1:foo123" result))
      (should-not (string-match-p "bar" result)))))

(ert-deftest gptel-agent-test-grep-subdirectory ()
  "Test that grep recurses into subdirectories."
  (gptel-agent-test-with-temp-dir
    (make-directory "sub")
    (write-region "needle here\n" nil (expand-file-name "sub/deep.txt"))
    (should (string-match-p "deep\.txt\n1:needle here"
                            (gptel-agent--grep "needle" default-directory)))))

(ert-deftest gptel-agent-test-grep-unreadable-path ()
  "Test that grepping a nonexistent path signals an error."
  (gptel-agent-test-with-temp-dir
    (should-error (gptel-agent--grep "x" file) :type 'error)))

;;;; Glob tool

(defvar gptel-agent-test-glob-force-find nil
  "If non-nil, `gptel-agent-test-glob-invoke' forces the find branch.")

(defun gptel-agent-test-glob-invoke (pattern &optional depth path)
  "Call `gptel-agent--glob' with PATTERN, DEPTH and PATH.
PATH defaults to `default-directory'.  If
`gptel-agent-test-glob-force-find' is non-nil, stub `executable-find'
so that \"tree\" resolves to nil, forcing the find code path."
  (let ((path (or path default-directory)))
    (if (and gptel-agent-test-glob-force-find
             (executable-find "find" t))
        (let ((find-path (executable-find "find" t)))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (command &optional _remote)
                       (and (string= command "find") find-path))))
            (gptel-agent--glob pattern path depth)))
      (gptel-agent--glob pattern path depth))))

(defmacro gptel-agent-test-deftest-glob (name docstring &rest body)
  "Define ERT tests NAME-tree and NAME-find, both running BODY.
Each variant skips itself when the corresponding executable is
missing; the find variant additionally stubs `executable-find' to
force the find branch of `gptel-agent--glob' even when `tree' is
installed."
  (declare (indent 1) (doc-string 2))
  `(progn
     (ert-deftest ,(intern (concat (symbol-name name) "-tree")) ()
       ,docstring
       (skip-unless (executable-find "tree"))
       ,@body)
     (ert-deftest ,(intern (concat (symbol-name name) "-find")) ()
       ,(concat docstring "  Exercises the `find' branch.")
       (skip-unless (executable-find "find"))
       (let ((gptel-agent-test-glob-force-find t))
         ,@body))))

(defun gptel-agent-test-glob-fixture ()
  "Populate `default-directory' with files for glob tests.
Creates a.txt, b.md, README.md, .hidden.txt, config, old.txt and
new.txt (old.txt older than new.txt), sub/deep.txt,
.hiddendir/h.txt and .git/config."
  (dolist (f '("a.txt" "b.md" "README.md" ".hidden.txt"
               "config" "old.txt" "new.txt"))
    (write-region "" nil (expand-file-name f)))
  (make-directory "sub")
  (write-region "" nil (expand-file-name "sub/deep.txt"))
  (make-directory ".hiddendir")
  (write-region "" nil (expand-file-name ".hiddendir/h.txt"))
  (make-directory ".git")
  (write-region "" nil (expand-file-name ".git/config"))
  ;; Explicit, well-separated mtimes so that ordering assertions do not
  ;; depend on file creation timing.
  (set-file-times (expand-file-name "old.txt") (seconds-to-time 1700000000))
  (set-file-times (expand-file-name "new.txt") (seconds-to-time 1900000000)))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-matches-files
  "Test that glob matches patterns and returns full paths."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let ((result (gptel-agent-test-glob-invoke "*.md")))
      (should (string-match-p (regexp-quote (expand-file-name "b.md")) result))
      (should (string-match-p (regexp-quote (expand-file-name "README.md")) result))
      (should-not (string-match-p "a\.txt" result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-includes-hidden
  "Test that hidden files and directories are searched."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let ((result (gptel-agent-test-glob-invoke "*.txt")))
      (should (string-match-p (regexp-quote (expand-file-name ".hidden.txt")) result))
      (should (string-match-p (regexp-quote (expand-file-name ".hiddendir/h.txt"))
                              result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-excludes-git
  "Test that .git directories are excluded from results."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let ((result (gptel-agent-test-glob-invoke "config")))
      (should (string-match-p (regexp-quote (expand-file-name "config")) result))
      (should-not (string-match-p "\.git/config" result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-case-insensitive
  "Test that patterns match case-insensitively."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let ((result (gptel-agent-test-glob-invoke "*.MD")))
      (should (string-match-p (regexp-quote (expand-file-name "b.md")) result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-depth-limits-recursion
  "Test that DEPTH prevents recursion into subdirectories."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let ((result (gptel-agent-test-glob-invoke "*.txt" 1)))
      (should (string-match-p (regexp-quote (expand-file-name "a.txt")) result))
      (should-not (string-match-p "deep\.txt" result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-sorts-newest-first
  "Test that results are sorted by mtime, newest first."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let* ((result (gptel-agent-test-glob-invoke "*.txt"))
           (new-pos (string-match (regexp-quote (expand-file-name "new.txt")) result))
           (old-pos (string-match (regexp-quote (expand-file-name "old.txt")) result)))
      (should new-pos)
      (should old-pos)
      (should (< new-pos old-pos)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-no-match
  "Test that a non-matching pattern lists no files."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let ((result (gptel-agent-test-glob-invoke "zzznomatch")))
      (should-not (string-match-p "a\.txt" result))
      (should-not (string-match-p "Error:" result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-empty-pattern
  "Test that an empty pattern signals an error."
  (gptel-agent-test-with-temp-dir
    (should-error (gptel-agent-test-glob-invoke "") :type 'error)))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-unreadable-path
  "Test that an unreadable path signals an error.
The path argument is rejected before any external program runs."
  (gptel-agent-test-with-temp-dir
    (should-error (gptel-agent-test-glob-invoke
                   "*" nil (expand-file-name "nonexistent")) :type 'error)))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-truncates-large-output
  "Test that output over 20000 chars is truncated with a notice."
  (gptel-agent-test-with-temp-dir
    (dotimes (i 80)
      (write-region "" nil (expand-file-name
                            (format "%s%d.txt" (make-string 240 ?a) i))))
    (let ((result (gptel-agent-test-glob-invoke "*")))
      (should (string-match-p "results too large" result))
      (should (string-match-p "Stored in:" result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-sorts-across-directories
  "Test that results are sorted by mtime across directory boundaries.
The `tree' backend sorts within each directory only, so this guards
against a per-directory ordering leaking out."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (set-file-times (expand-file-name "sub/deep.txt") (seconds-to-time 2000000000))
    (set-file-times (expand-file-name "new.txt") (seconds-to-time 1900000000))
    (let* ((result (gptel-agent-test-glob-invoke "*.txt"))
           (deep-pos (string-match (regexp-quote (expand-file-name "sub/deep.txt"))
                                   result))
           (new-pos (string-match (regexp-quote (expand-file-name "new.txt"))
                                  result))
           (old-pos (string-match (regexp-quote (expand-file-name "old.txt"))
                                  result)))
      (should (< deep-pos new-pos))
      (should (< new-pos old-pos)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-symlinked-file
  "Test that symlinks are listed as paths without the \" -> \" decoration."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (make-symbolic-link "new.txt" (expand-file-name "link.txt"))
    (let ((result (gptel-agent-test-glob-invoke "*.txt")))
      (should (string-match-p (regexp-quote (expand-file-name "link.txt")) result))
      (should-not (string-match-p " -> " result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-notices-no-match
  "Test that a non-matching pattern returns an explicit notice."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let ((result (gptel-agent-test-glob-invoke "zzznomatch")))
      (should (string-match-p "No files matching" result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-rejects-invalid-depth
  "Test that a non-positive or non-numeric depth signals an error."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (should-error (gptel-agent-test-glob-invoke "*.txt" 0) :type 'error)
    (should-error (gptel-agent-test-glob-invoke "*.txt" -1) :type 'error)
    (should-error (gptel-agent-test-glob-invoke "*.txt" "nonsense") :type 'error)))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-depth-as-string
  "Test that a string depth is accepted and limits recursion."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let ((result (gptel-agent-test-glob-invoke "*.txt" "1")))
      (should (string-match-p (regexp-quote (expand-file-name "a.txt")) result))
      (should-not (string-match-p "deep\.txt" result)))))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-path-is-file
  "Test that passing a file as the search path signals an error."
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (should-error (gptel-agent-test-glob-invoke
                   "*" nil (expand-file-name "a.txt")) :type 'error)))

(gptel-agent-test-deftest-glob gptel-agent-test-glob-warns-on-unreadable-dir
  "Test that unreadable directories produce a warning instead of silence."
  (skip-unless (not (zerop (user-uid))))
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (make-directory "locked")
    (write-region "" nil (expand-file-name "locked/x.txt"))
    (set-file-modes "locked" #o000)
    (unwind-protect
        (let ((result (gptel-agent-test-glob-invoke "*.txt")))
          (should (string-match-p "Glob warning" result))
          (should (string-match-p "locked" result))
          (should (string-match-p (regexp-quote (expand-file-name "a.txt")) result)))
      (set-file-modes "locked" #o755))))

(ert-deftest gptel-agent-test-glob-branches-agree ()
  "Test that the tree and find branches return the same file set."
  (skip-unless (and (executable-find "tree") (executable-find "find")))
  (gptel-agent-test-with-temp-dir
    (gptel-agent-test-glob-fixture)
    (let* ((tree-result (gptel-agent-test-glob-invoke "*.txt"))
           (find-result (let ((gptel-agent-test-glob-force-find t))
                          (gptel-agent-test-glob-invoke "*.txt")))
           (paths
            (lambda (result)
              (sort
               (delq nil (mapcar
                          (lambda (line)
                            (and (string-prefix-p temp-dir line)
                                 (file-regular-p line)
                                 line))
                          (split-string result "\n")))
               #'string<))))
      (should (equal (funcall paths tree-result)
                     (funcall paths find-result))))))

;;;; Edit tool

(ert-deftest gptel-agent-test-edit-string-replacement ()
  "Test replacing a unique string occurrence."
  (gptel-agent-test-with-file "a.txt" "foo bar baz\n"
    (let ((result (gptel-agent--edit-files file "bar" "qux")))
      (should (string-match-p "Successfully replaced" result))
      (should (equal (gptel-agent-test-file-content file) "foo qux baz\n")))))

(ert-deftest gptel-agent-test-edit-multiline-replacement ()
  "Test replacing a multiline string."
  (gptel-agent-test-with-file "a.txt" "line1\nline2\nline3\n"
    (gptel-agent--edit-files file "line2\nline3" "two\nthree")
    (should (equal (gptel-agent-test-file-content file)
                   "line1\ntwo\nthree\n"))))

(ert-deftest gptel-agent-test-edit-replacement-only-first-match ()
  "Test that only the unique first match is replaced.
A non-unique OLD-STR must not be replaced at all."
  (gptel-agent-test-with-file "a.txt" "a a\n"
    (let ((err (should-error (gptel-agent--edit-files file "a" "b"))))
      (should (string-match-p "not unique" (error-message-string err)))
      (should (equal (gptel-agent-test-file-content file) "a a\n")))))

(ert-deftest gptel-agent-test-edit-string-not-found ()
  "Test that a missing OLD-STR signals an error."
  (gptel-agent-test-with-file "a.txt" "foo\n"
    (let ((err (should-error (gptel-agent--edit-files file "nope" "x"))))
      (should (string-match-p "Could not find" (error-message-string err))))))

(ert-deftest gptel-agent-test-edit-missing-new-str ()
  "Test that a missing NEW-STR signals an error."
  (gptel-agent-test-with-file "a.txt" "foo\n"
    (should-error (gptel-agent--edit-files file "foo" nil) :type 'error)))

(ert-deftest gptel-agent-test-edit-json-false-is-string-mode ()
  "Test that DIFFP set to :json-false still uses string replacement."
  (gptel-agent-test-with-file "a.txt" "foo bar\n"
    (gptel-agent--edit-files file "bar" "qux" :json-false)
    (should (equal (gptel-agent-test-file-content file) "foo qux\n"))))

(ert-deftest gptel-agent-test-edit-string-replacement-rejects-directory ()
  "Test that string replacement on a directory signals an error."
  (gptel-agent-test-with-temp-dir
    (should-error (gptel-agent--edit-files temp-dir "a" "b") :type 'error)))

(ert-deftest gptel-agent-test-edit-unreadable-path ()
  "Test that editing a nonexistent file signals an error."
  (gptel-agent-test-with-temp-dir
    (should-error (gptel-agent--edit-files file "a" "b") :type 'error)))

(ert-deftest gptel-agent-test-edit-apply-diff ()
  "Test applying a unified diff, including with code fences."
  (gptel-agent-test-with-file "a.txt" "alpha\nbeta\ngamma\n"
    (let ((diff "--- a/a.txt\n+++ b/a.txt\n@@ -1,3 +1,3 @@\n alpha\n-beta\n+BETA\n gamma\n")
          (diff-fenced "```diff\n--- a/a.txt\n+++ b/a.txt\n@@ -1,3 +1,3 @@\n alpha\n-beta\n+BETA\n gamma\n```\n"))
      ;; Unfenced
      (gptel-agent--edit-files file nil diff t)
      (should (equal (gptel-agent-test-file-content file)
                     "alpha\nBETA\ngamma\n"))
      ;; Reverting with a fenced diff; --forward ignores applied patches,
      ;; so re-write the original content first.
      (write-region "alpha\nbeta\ngamma\n" nil file)
      (gptel-agent--edit-files file nil diff-fenced t)
      (should (equal (gptel-agent-test-file-content file)
                     "alpha\nBETA\ngamma\n")))))

(ert-deftest gptel-agent-test-edit-apply-diff-bad-hunk ()
  "Test that a failing patch signals an error and changes nothing."
  (gptel-agent-test-with-file "a.txt" "alpha\nbeta\ngamma\n"
    (should-error (gptel-agent--edit-files
                   file nil
                   "--- a/a.txt\n+++ b/a.txt\n@@ -1,2 +1,2 @@\n-zeta\n+yeta\n" t)
                  :type 'error)
    (should (equal (gptel-agent-test-file-content file)
                   "alpha\nbeta\ngamma\n"))))

;;;; TodoWrite tool: cross-turn task list injection

(defconst gptel-agent-test-todos
  [(:content "Write the package" :status "completed"
    :activeForm "Writing the package")
   (:content "Write the module" :status "in_progress"
    :activeForm "Writing the module")
   (:content "Run tests" :status "pending"
    :activeForm "Running tests")]
  "Sample task list, a vector as returned by JSON decoding.")

(ert-deftest gptel-agent-test-todos-state-string ()
  "Test that the injected block lists every item and its status."
  (let ((str (gptel-agent--todos-state-string gptel-agent-test-todos 0)))
    (should (string-match-p "<task-list" str))
    (should (string-match-p "1/3 completed" str))
    (should (string-match-p "- \\[x\\] Write the package" str))
    (should (string-match-p "- \\[~\\] Write the module" str))
    (should (string-match-p "- \\[ \\] Run tests" str))
    (should-not (string-match-p "STALE" str))))

(ert-deftest gptel-agent-test-todos-state-string-nil ()
  "Test that an empty task list injects nothing."
  (should-not (gptel-agent--todos-state-string nil 0))
  (should-not (gptel-agent--todos-state-string [] 0)))

(ert-deftest gptel-agent-test-todos-state-string-stale ()
  "Test that the block is marked stale only after the threshold."
  (let ((gptel-agent-todos-stale-after 3))
    (should-not (string-match-p
                 "STALE" (gptel-agent--todos-state-string
                          gptel-agent-test-todos 2)))
    (let ((str (gptel-agent--todos-state-string gptel-agent-test-todos 3)))
      (should (string-match-p "STALE" str))
      ;; The nudge names the in-progress item so the model can judge it.
      (should (string-match-p "Writing the module" str)))
    ;; Staleness marking can be turned off.
    (let ((gptel-agent-todos-stale-after nil))
      (should-not (string-match-p
                   "STALE" (gptel-agent--todos-state-string
                            gptel-agent-test-todos 100))))))

(ert-deftest gptel-agent-test-todos-state-string-truncated ()
  "Test that long task lists are truncated with a note."
  (let* ((gptel-agent-todos-max-injected 2)
         (str (gptel-agent--todos-state-string gptel-agent-test-todos 0)))
    (should (string-match-p "Write the package" str))
    (should (string-match-p "Write the module" str))
    (should-not (string-match-p "Run tests" str))
    (should (string-match-p "+1 more items" str))))

(ert-deftest gptel-agent-test-todo-tool-enabled-p ()
  "Test that injection is gated on the TodoWrite tool being present."
  (let ((todo (gptel-get-tool "TodoWrite"))
        (other (gptel-get-tool "Read"))
        (gptel-use-tools t))
    (should (gptel-agent--todo-tool-enabled-p (list todo)))
    (should (gptel-agent--todo-tool-enabled-p (list other todo)))
    ;; Tool names are accepted too, as presets specify them.
    (should (gptel-agent--todo-tool-enabled-p '("TodoWrite")))
    (should-not (gptel-agent--todo-tool-enabled-p (list other)))
    (should-not (gptel-agent--todo-tool-enabled-p nil))
    ;; No tools are sent at all when tool use is off.
    (let ((gptel-use-tools nil))
      (should-not (gptel-agent--todo-tool-enabled-p (list todo))))))

(ert-deftest gptel-agent-test-inject-todos-appends-to-prompt ()
  "Test that the task list is appended to the prompt, not the buffer."
  (with-temp-buffer
    (setq-local gptel-tools (list (gptel-get-tool "TodoWrite"))
                gptel-agent--todos gptel-agent-test-todos)
    (let* ((gptel-buffer (current-buffer))
           (gptel-agent--todos-stale-turns 2)
           (fsm (gptel-make-fsm
                 :info (list :buffer gptel-buffer)))
           (prompt-buffer (generate-new-buffer " *test-prompt*")))
      (unwind-protect
          (progn
            (with-current-buffer prompt-buffer
              (insert "user question\n")
              (setq-local gptel-tools (list (gptel-get-tool "TodoWrite")))
              (gptel-agent--inject-todos fsm))
            ;; Injected at the end of the prompt...
            (should (string-match-p
                     "\\`user question\n\n<task-list"
                     (with-current-buffer prompt-buffer (buffer-string))))
            ;; ...and the buffer itself is untouched.
            (should (equal (buffer-string) ""))
            ;; The staleness counter advanced.
            (should (= gptel-agent--todos-stale-turns 3)))
        (kill-buffer prompt-buffer)))))

(ert-deftest gptel-agent-test-inject-todos-no-tool ()
  "Test that nothing is injected when TodoWrite is not in use."
  (with-temp-buffer
    (setq-local gptel-tools (list (gptel-get-tool "Read"))
                gptel-agent--todos gptel-agent-test-todos)
    (let* ((buf (current-buffer))
           (gptel-agent--todos-stale-turns 0)
           (fsm (gptel-make-fsm :info (list :buffer buf)))
           (prompt-buffer (generate-new-buffer " *test-prompt*")))
      (unwind-protect
          (progn
            (with-current-buffer prompt-buffer
              (insert "user question\n")
              (setq-local gptel-tools (list (gptel-get-tool "Read")))
              (gptel-agent--inject-todos fsm))
            (should-not (string-match-p
                         "task-list"
                         (with-current-buffer prompt-buffer (buffer-string))))
            (should (= gptel-agent--todos-stale-turns 0)))
        (kill-buffer prompt-buffer)))))

(ert-deftest gptel-agent-test-write-todo-resets-staleness ()
  "Test that a TodoWrite call refreshes the overlay and the stale counter.
Also covers both shapes the tool can receive: JSON decoding yields a
vector, but callers may pass a plain list."
  (dolist (todos (list gptel-agent-test-todos
                       (append gptel-agent-test-todos nil)))
    (with-temp-buffer
      (insert "some response text")
      (let* ((gptel--fsm-last
              (gptel-make-fsm :info (list :position (point-max-marker))))
             (gptel-mode t)
             (gptel-use-header-line t)
             (header-line-format '("" "" ""))
             (gptel-agent--todos-stale-turns 7))
        (gptel-agent--write-todo todos)
        (should (= gptel-agent--todos-stale-turns 0))
        (should (equal gptel-agent--todos todos))
        (should (gptel-agent--todo-overlay))
        ;; The header line names the in-progress item.
        (should (string-match-p
                 "Writing the module"
                 (substring-no-properties (nth 2 header-line-format))))
        ;; And the overlay shows the staleness-free list.
        (should-not (string-match-p
                     "not updated"
                     (substring-no-properties
                      (overlay-get (gptel-agent--todo-overlay) 'after-string))))))))

(provide 'gptel-agent-test-tools)

;;; gptel-agent-test-tools.el ends here
