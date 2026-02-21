;;; elfeed-ai.el --- AI-powered article summarization for elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Daniel Fleischer

;; Author: Daniel Fleischer
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (elfeed "3.4.1") (gptel "0.9"))
;; Keywords: comm, news
;; URL: https://github.com/danielfleischer/elfeed-ai

;; This program is free software; you can redistribute it and/or modify
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

;; Summarize elfeed articles using an LLM via gptel.
;;
;; Setup:
;;   (add-hook 'elfeed-search-mode-hook #'elfeed-ai-mode)
;;
;; Usage:
;;   1. In the elfeed search buffer, mark entries with `m' (unmark with `u')
;;   2. Press `S' to open the summarization menu
;;   3. Optionally press `g' to configure gptel (model, temperature, etc.)
;;   4. Press `s' to summarize
;;   5. Summaries appear asynchronously in the *elfeed-ai* Org buffer

;;; Code:

(require 'elfeed)
(require 'gptel)
(require 'gptel-transient)
(require 'shr)
(require 'transient)

(defgroup elfeed-ai nil
  "AI-powered summarization for elfeed."
  :group 'elfeed
  :prefix "elfeed-ai-")

(defcustom elfeed-ai-system-prompt
  "You are a helpful assistant. Summarize the following article concisely, highlighting the key points. Use org-mode formatting, starting with level 2."
  "System prompt sent to the LLM for article summarization."
  :type 'string
  :group 'elfeed-ai)

(defcustom elfeed-ai-buffer-name "*elfeed-ai*"
  "Name of the buffer used to display article summaries."
  :type 'string
  :group 'elfeed-ai)

(defvar gptel--system-message)

(defun elfeed-ai--register-directive ()
  "Register the elfeed-ai directive in `gptel-directives'."
  (unless (assq 'Elfeed-AI gptel-directives)
    (push (cons 'Elfeed-AI elfeed-ai-system-prompt) gptel-directives)))

;;; Marking

(defun elfeed-ai-mark ()
  "Mark the current elfeed entry for summarization.
Adds the `summarize' tag and moves to the next entry."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)
      (elfeed-tag entry 'summarize))
    (elfeed-search-update--force)
    (forward-line 1)))

(defun elfeed-ai-unmark ()
  "Unmark the current elfeed entry.
Removes the `summarize' tag and moves to the next entry."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)
      (elfeed-untag entry 'summarize))
    (elfeed-search-update--force)
    (forward-line 1)))

(defun elfeed-ai-unmark-all ()
  "Remove the `summarize' tag from all entries in the current search."
  (interactive)
  (dolist (entry elfeed-search-entries)
    (elfeed-untag entry 'summarize))
  (elfeed-search-update--force))

(defun elfeed-ai--marked-entries ()
  "Return all entries in the current search tagged with `summarize'."
  (seq-filter (lambda (entry) (elfeed-tagged-p 'summarize entry))
              elfeed-search-entries))

;;; Content extraction

(defvar shr-inhibit-images)
(defvar shr-use-fonts)

(defun elfeed-ai--entry-text (entry)
  "Extract plain text content from an elfeed ENTRY."
  (let ((content (elfeed-deref (elfeed-entry-content entry))))
    (if content
        (with-temp-buffer
          (insert content)
          (let ((shr-inhibit-images t)
                (shr-use-fonts nil))
            (shr-render-region (point-min) (point-max)))
          (buffer-string))
      "")))

;;; Output formatting

(defun elfeed-ai--format-entry-heading (entry)
  "Format an Org heading with metadata for ENTRY."
  (let ((title (elfeed-entry-title entry))
        (feed (elfeed-feed-title (elfeed-entry-feed entry)))
        (date (format-time-string "%Y-%m-%d" (elfeed-entry-date entry)))
        (url (elfeed-entry-link entry)))
    (concat "* " title "\n"
            ":PROPERTIES:\n"
            ":FEED: " (or feed "Unknown") "\n"
            ":DATE: " date "\n"
            ":URL: " (or url "") "\n"
            ":END:\n\n")))

;;; Summarization

(defun elfeed-ai--make-callback (entry total counter buffer)
  "Create a gptel callback for ENTRY.
TOTAL is the number of entries being summarized.
COUNTER is a cons cell whose car tracks completed requests.
BUFFER is the Org output buffer."
  (lambda (response info)
    (if (not response)
        (message "elfeed-ai: request failed for \"%s\" — %s"
                 (elfeed-entry-title entry)
                 (plist-get info :status))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert (elfeed-ai--format-entry-heading entry))
          (insert response "\n\n"))))
    (setcar counter (1+ (car counter)))
    (message "elfeed-ai: %d/%d summaries completed" (car counter) total)
    (when (and (= (car counter) total) (buffer-live-p buffer))
      (with-current-buffer buffer (goto-char (point-min)))
      (display-buffer buffer))))

;;;###autoload
(defun elfeed-ai-summarize ()
  "Summarize marked elfeed entries using an LLM via gptel.
Uses entries tagged with `summarize' (mark with \\[elfeed-ai-mark]).
Summaries are displayed in an Org buffer."
  (interactive)
  (let ((entries (elfeed-ai--marked-entries)))
    (unless entries
      (user-error "No marked entries. Use `m' to mark entries first"))
    (let ((buf (get-buffer-create elfeed-ai-buffer-name))
          (total (length entries))
          (counter (cons 0 nil))
          (model-name (format "%s" gptel-model))
          (backend-name (gptel-backend-name gptel-backend)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer))
        (org-mode)
        (insert "#+TITLE: Elfeed AI Summaries\n")
        (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert (format "#+MODEL: %s / %s\n\n" backend-name model-name)))
      (dolist (entry entries)
        (let ((text (elfeed-ai--entry-text entry)))
          (if (string-empty-p text)
              (progn
                (setcar counter (1+ (car counter)))
                (message "elfeed-ai: skipping \"%s\" (no content)"
                         (elfeed-entry-title entry)))
            (gptel-request text
              :system gptel--system-message
              :buffer buf
              :callback (elfeed-ai--make-callback entry total counter buf)))))
      ;; Remove summarize tags from processed entries
      (dolist (entry entries)
        (elfeed-untag entry 'summarize))
      (elfeed-search-update--force)
      (message "elfeed-ai: summarizing %d article%s via %s/%s..."
               total (if (= total 1) "" "s")
               backend-name model-name))))

;;; Transient menu

;;;###autoload
(transient-define-prefix elfeed-ai-menu ()
  "Elfeed AI summarization menu."
  [:description
   (lambda ()
     (let ((marked (length (elfeed-ai--marked-entries))))
       (format "Elfeed AI  [%d marked article%s]"
               marked (if (= marked 1) "" "s"))))
   ["Actions"
    ("s" "Summarize" elfeed-ai-summarize)
    ("g" "gptel settings..." gptel-menu)
    ("q" "Quit" transient-quit-one)]])

;;; Keymap and minor mode

(defvar elfeed-ai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'elfeed-ai-mark)
    (define-key map (kbd "u") #'elfeed-ai-unmark)
    (define-key map (kbd "U") #'elfeed-ai-unmark-all)
    (define-key map (kbd "S") #'elfeed-ai-menu)
    map)
  "Keymap for `elfeed-ai-mode'.")

;;;###autoload
(define-minor-mode elfeed-ai-mode
  "Minor mode for AI-powered summarization in elfeed.
Provides keybindings for marking entries and summarizing them.
\\{elfeed-ai-mode-map}"
  :lighter " AI"
  :keymap elfeed-ai-mode-map
  (when elfeed-ai-mode
    (elfeed-ai--register-directive)
    (setq-local gptel--system-message elfeed-ai-system-prompt)))

(provide 'elfeed-ai)
;;; elfeed-ai.el ends here
