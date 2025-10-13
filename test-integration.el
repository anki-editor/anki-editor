;;; test-integration.el --- Integration tests for conflict detection  -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for external modification detection.
;; Requires test_server.py running on port 28765.

;;; Code:

(require 'anki-editor)

(defvar test-integration-results nil
  "List of test results.")

(defun test-integration-log (message &rest args)
  "Log a test MESSAGE with ARGS."
  (let ((msg (apply #'format message args)))
    (message "%s" msg)
    (push msg test-integration-results)))

(defun test-integration-assert (condition description)
  "Assert CONDITION is true, logging DESCRIPTION."
  (if condition
      (test-integration-log "✓ %s" description)
    (test-integration-log "✗ FAILED: %s" description)
    (error "Test failed: %s" description)))

(defun test-integration-cleanup ()
  "Clean up test file."
  (let ((file "/tmp/anki-test.org"))
    (when (file-exists-p file)
      (delete-file file))))

(defun test-integration-run ()
  "Run integration tests for conflict detection."
  (interactive)
  (setq test-integration-results nil)

  (test-integration-log "\n=== Starting Integration Tests ===\n")

  ;; Setup
  (test-integration-cleanup)
  (let ((test-file "/tmp/anki-test.org")
        (anki-editor-api-port "28765")
        note-id remote-hash-1)

    ;; Create test org file
    (with-temp-file test-file
      (insert "#+PROPERTY: ANKI_DECK Test\n\n")
      (insert "* Test Note\n")
      (insert ":PROPERTIES:\n")
      (insert ":ANKI_NOTE_TYPE: Basic\n")
      (insert ":END:\n")
      (insert "** Front\n")
      (insert "What is 2+2?\n")
      (insert "** Back\n")
      (insert "Four\n"))

    (test-integration-log "Created test file: %s" test-file)

    ;; Test 1: Create note and verify remote hash stored
    (test-integration-log "\n--- Test 1: Create Note ---")
    (with-current-buffer (find-file-noselect test-file)
      (goto-char (point-min))
      (re-search-forward "^\\* Test Note")

      ;; Push note
      (anki-editor-push-note-at-point)

      ;; Verify ANKI_NOTE_ID was set
      (setq note-id (org-entry-get nil "ANKI_NOTE_ID"))
      (test-integration-assert note-id "Note ID was set after creation")
      (test-integration-log "Created note ID: %s" note-id)

      ;; Verify ANKI_REMOTE_HASH was set
      (setq remote-hash-1 (org-entry-get nil "ANKI_REMOTE_HASH"))
      (test-integration-assert remote-hash-1 "Remote hash was set after creation")
      (test-integration-log "Initial remote hash: %s" remote-hash-1)

      ;; Test 2: Update note without external changes (should succeed)
      (test-integration-log "\n--- Test 2: Update Without External Changes ---")
      (re-search-forward "^\\*\\* Back")
      (forward-line 1)
      (insert "The answer is four.\n")

      (re-search-backward "^\\* Test Note")
      (anki-editor-push-note-at-point)

      (let ((remote-hash-2 (org-entry-get nil "ANKI_REMOTE_HASH")))
        (test-integration-assert remote-hash-2 "Remote hash updated after successful push")
        (test-integration-assert (not (string= remote-hash-1 remote-hash-2))
                                "Remote hash changed after update")
        (test-integration-log "New remote hash: %s" remote-hash-2)
        (setq remote-hash-1 remote-hash-2))

      ;; Test 3: Simulate external modification and detect conflict
      (test-integration-log "\n--- Test 3: Detect External Modification ---")

      ;; Directly modify note in test server via API
      (let* ((update-response
              (anki-editor-api-call-result
               'updateNoteFields
               :note (list :id (string-to-number note-id)
                          :fields (list :Back "EXTERNALLY MODIFIED")))))
        (test-integration-log "Simulated external modification via API"))

      ;; Try to push local changes with conflict policy set to 'skip
      (let ((anki-editor-conflict-policy 'skip))
        (re-search-forward "^\\*\\* Back")
        (end-of-line)
        (insert " More local changes.")

        (re-search-backward "^\\* Test Note")
        (anki-editor-push-note-at-point)

        ;; Check that remote hash was NOT updated (push was skipped)
        (let ((remote-hash-3 (org-entry-get nil "ANKI_REMOTE_HASH")))
          (test-integration-assert (string= remote-hash-1 remote-hash-3)
                                  "Remote hash unchanged after skipped conflict")
          (test-integration-log "Conflict was detected and note was skipped")))

      ;; Test 4: Overwrite policy
      (test-integration-log "\n--- Test 4: Overwrite Policy ---")
      (let ((anki-editor-conflict-policy 'overwrite))
        (anki-editor-push-note-at-point)

        (let ((remote-hash-4 (org-entry-get nil "ANKI_REMOTE_HASH")))
          (test-integration-assert (not (string= remote-hash-1 remote-hash-4))
                                  "Remote hash updated after overwrite")
          (test-integration-log "Note was overwritten successfully")))

      (kill-buffer))

    (test-integration-cleanup)
    (test-integration-log "\n=== All Tests Completed ===\n"))

  ;; Display results
  (with-current-buffer (get-buffer-create "*Integration Test Results*")
    (erase-buffer)
    (dolist (result (reverse test-integration-results))
      (insert result "\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer)))

  (message "Integration tests completed. See *Integration Test Results* buffer."))

(provide 'test-integration)
;;; test-integration.el ends here
