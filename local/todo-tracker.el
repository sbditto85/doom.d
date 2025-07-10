;;; todo-tracker.el --- A tool for tracking TODO items -*- lexical-binding: t -*-

;; Author:
;; Maintainer:
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage:


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; A tool for tracking TODO items with hierarchical organization,
;; completion status, and current work item designation.

;;; Code:

(defgroup todo-tracker nil
  "TODO tracking functionality."
  :prefix "todo-tracker-"
  :group 'applications)

(defconst todo-tracker-mode-name "Todo-Tracker")

(defcustom todo-tracker-indentation 2
  "The amount of indentation of each deeper level."
  :group 'todo-tracker)

(defcustom todo-tracker-file "~/Documents/emacs/org/todos.org"
  "File to save TODO items in."
  :group 'todo-tracker)

(defcustom todo-tracker-header "TODO Items"
  "Header under which to store TODO items.

This header is located in the file stored in the variable
`todo-tracker-file'."
  :group 'todo-tracker)

(defcustom todo-tracker-message-on-action t
  "When non-nil, message the most recent tree when running a command."
  :group 'todo-tracker)

(defface todo-tracker-current-item-face
  '((t (:inherit highlight)))
  "`todo-tracker-mode' face used for showing current work item."
  :group 'todo-tracker)

(defface todo-tracker-completed-face
  '((t (:inherit shadow :strike-through t)))
  "`todo-tracker-mode' face used for completed items."
  :group 'todo-tracker)

(defun todo-tracker--narrow-to-subtree ()
  "Helper function to find subtree and narrow to it."
  (widen)
  (goto-char (point-min))
  (let ((header-found-p (search-forward (concat "* " todo-tracker-header) nil t)))
    (unless header-found-p
      (error "unable to find TODO tracker header"))
    (org-narrow-to-subtree)
    (goto-char (point-min))))

(defun todo-tracker--fontify-items ()
  "Fontify items in the todo-tracker buffer."
  (with-current-buffer "*todo-tracker*"
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "- " nil t)
        (let ((start (point))
              (end (line-end-position))
              (line-text (buffer-substring start end)))
          (cond
           ((string-match "^\\(.*?\\) \\[COMPLETED\\]" line-text)
            (add-text-properties start end '(face todo-tracker-completed-face)))
           ((string-match "^\\(.*?\\) \\[CURRENT\\]" line-text)
            (add-text-properties start end '(face todo-tracker-current-item-face)))))))))

(defun todo-tracker--update-buffer ()
  "Refresh the contents of the todo-tracker buffer."
  (let ((todo-tracker-text nil))
    (save-excursion
      (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
      (save-restriction
        (todo-tracker--narrow-to-subtree)
        (end-of-line)
        (let ((start (point)))
          (goto-char (point-max))
          (setq todo-tracker-text (string-trim (buffer-substring start (point)))))))
    (with-current-buffer (get-buffer-create "*todo-tracker*")
      (unless (equal mode-name todo-tracker-mode-name)
        (todo-tracker-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert todo-tracker-text)
        (todo-tracker--fontify-items))
      (goto-char (point-max)))))

(defun todo-tracker--get-tree ()
  "Return the text of the current todo-tracker tree."
  (let ((todo-tracker-text nil))
    (save-excursion
      (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
      (save-restriction
        (todo-tracker--narrow-to-subtree)
        (end-of-line)
        (let ((start (point)))
          (goto-char (point-max))
          (string-trim (buffer-substring start (point))))))))

(defun todo-tracker--display-message-p ()
  "Return non-nil if message should be displayed of task tree."
  (and todo-tracker-message-on-action
       (not (equal (buffer-name) "*todo-tracker*"))))

(defun todo-tracker--get-current-indentation ()
  "Get the indentation level of the last item, or 0 if none."
  (save-excursion
    (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
    (save-restriction
      (todo-tracker--narrow-to-subtree)
      (goto-char (point-max))
      (let ((found-item-p (search-backward "- " nil t)))
        (if found-item-p
            (let ((at-col (current-column)))
              (when (= 1 at-col)
                (setq at-col 0))
              at-col)
          0)))))

(defun todo-tracker-add-item (item)
  "Add ITEM as a new TODO item at the same level as the last item."
  (interactive "sName of new TODO item:")
  (save-excursion
    (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
    (save-restriction
      (todo-tracker--narrow-to-subtree)
      (goto-char (point-max))
      (let ((indent-level (todo-tracker--get-current-indentation)))
        (insert "\n"
                (make-string indent-level ?\s)
                "- "
                item))))
  (todo-tracker--update-buffer)
  (when (todo-tracker--display-message-p)
    (message "%s" (todo-tracker--get-tree))))

(defun todo-tracker-add-sub-item (item)
  "Add ITEM as a new TODO sub-item (deeper level)."
  (interactive "sName of new sub-item:")
  (save-excursion
    (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
    (save-restriction
      (todo-tracker--narrow-to-subtree)
      (goto-char (point-min))
      (let ((target-line nil)
            (target-indent 0))
        ;; First try to find the current item
        (while (and (not target-line)
                    (search-forward "[CURRENT]" nil t))
          (let ((line-start (line-beginning-position)))
            (when (save-excursion
                    (goto-char line-start)
                    (looking-at ".*- "))
              (setq target-line line-start)
              (setq target-indent (current-column)))))
        ;; If no current item found, use the last item
        (unless target-line
          (goto-char (point-max))
          (let ((found-item-p (search-backward "- " nil t)))
            (when found-item-p
              (setq target-line (line-beginning-position))
              (setq target-indent (current-column)))))
        ;; Add the sub-item after the target line
        (when target-line
          (goto-char target-line)
          (end-of-line)
          (insert "\n"
                  (make-string (+ target-indent todo-tracker-indentation) ?\s)
                  "- "
                  item)))))
  (todo-tracker--update-buffer)
  (when (todo-tracker--display-message-p)
    (message "%s" (todo-tracker--get-tree))))

(defun todo-tracker-mark-complete ()
  "Mark the current TODO item as complete."
  (interactive)
  (save-excursion
    (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
    (save-restriction
      (todo-tracker--narrow-to-subtree)
      (goto-char (point-max))
      (let ((found-item-p (search-backward "- " nil t)))
        (unless found-item-p
          (error "No TODO item to mark as complete."))
        (let ((item (save-excursion
                      (skip-chars-forward "- ")
                      (buffer-substring (point) (line-end-position)))))
          (delete-region (point) (line-end-position))
          (insert item " [COMPLETED]")
          (message "Marked as complete: %s" item))))
    (todo-tracker--update-buffer)))

(defun todo-tracker-mark-current ()
  "Mark the current TODO item as the current work item."
  (interactive)
  (save-excursion
    (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
    (save-restriction
      (todo-tracker--narrow-to-subtree)
      ;; First, remove [CURRENT] from any existing items
      (goto-char (point-min))
      (while (search-forward "[CURRENT]" nil t)
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (when (save-excursion
                  (goto-char line-start)
                  (looking-at ".*- "))
            (let ((line-text (buffer-substring line-start line-end)))
              (when (string-match "\\(.*?\\) \\[CURRENT\\]" line-text)
                (delete-region line-start line-end)
                (insert (match-string 1 line-text)))))))
      ;; Now mark the last item as current
      (goto-char (point-max))
      (let ((found-item-p (search-backward "- " nil t)))
        (unless found-item-p
          (error "No TODO item to mark as current."))
        (let ((item (save-excursion
                      (skip-chars-forward "- ")
                      (buffer-substring (point) (line-end-position)))))
          (delete-region (point) (line-end-position))
          (insert item " [CURRENT]")
          (message "Marked as current: %s" item))))
    (todo-tracker--update-buffer)))

(defun todo-tracker-remove-item ()
  "Remove the last TODO item."
  (interactive)
  (save-excursion
    (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
    (save-restriction
      (todo-tracker--narrow-to-subtree)
      (goto-char (point-max))
      (let ((found-item-p (search-backward "- " nil t)))
        (unless found-item-p
          (error "No TODO item to remove.")))
      (let ((item (save-excursion
                    (skip-chars-forward "- ")
                    (buffer-substring (point) (line-end-position)))))
        (delete-region (1- (line-beginning-position)) (line-end-position))
        (message "Removed: %s" item))
      (todo-tracker--update-buffer))))

(defun todo-tracker-remove-completed ()
  "Remove all completed TODO items."
  (interactive)
  (save-excursion
    (with-no-warnings (set-buffer (find-file-noselect todo-tracker-file)))
    (save-restriction
      (todo-tracker--narrow-to-subtree)
      (goto-char (point-min))
      (let ((removed-count 0))
        (while (search-forward "[COMPLETED]" nil t)
          (let ((line-start (line-beginning-position)))
            (delete-region line-start (1+ (line-end-position)))
            (setq removed-count (1+ removed-count))))
        (message "Removed %d completed items" removed-count)))
    (todo-tracker--update-buffer)))

(defun todo-tracker ()
  "Display todo-tracker buffer."
  (interactive)
  (todo-tracker--update-buffer)
  (switch-to-buffer "*todo-tracker*"))

(defconst todo-tracker-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "a") #'todo-tracker-add-item)
      (define-key map (kbd "s") #'todo-tracker-add-sub-item)
      (define-key map (kbd "c") #'todo-tracker-mark-complete)
      (define-key map (kbd "m") #'todo-tracker-mark-current)
      (define-key map (kbd "d") #'todo-tracker-remove-item)
      (define-key map (kbd "r") #'todo-tracker-remove-completed))))

(define-derived-mode todo-tracker-mode
  special-mode todo-tracker-mode-name
  "Major mode for interacting with todo-tracker buffer.

Key bindings:
  a - Add new TODO item
  s - Add new sub-item
  c - Mark item as complete
  m - Mark item as current work
  d - Remove last item
  r - Remove all completed items")

(provide 'todo-tracker)

;;; todo-tracker.el ends here
