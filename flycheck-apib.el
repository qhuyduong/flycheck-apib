;;; flycheck-apib.el --- API Blueprint flycheck integration -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Josh Benner

;; Author: Josh Benner <joshbenner@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((apib-mode "0.1") (dash "2.14.1") (dash-functional "1.2.0") (emacs "26.1") (flycheck "27") (s "1.12.0"))
;; Keywords: API Blueprint flycheck drafter
;; URL: https://github.com/joshbenner/flycheck-apib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configures Flycheck for apib-mode

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'flycheck)
(require 's)

(defconst flycheck-apib-level-message-regex "^\\(.*\\): (.*)\s+\\(.*\\)$")

(defconst flycheck-apib-line-column-regex "^line \\([0-9]*\\), column \\([0-9]*\\).*$")

(defun flycheck-apib-error-flatten (error)
  "Flatten the composite ERROR to a list of errors."
  (let ((pure-error (car (split-string error "; ")))
        (line-column-pairs (cdr (split-string error "; "))))
    (let ((level (nth 1 (s-match flycheck-apib-level-message-regex pure-error)))
          (error-message (nth 2 (s-match flycheck-apib-level-message-regex pure-error))))
      (-map (lambda (line-column-pair)
              (let ((line (nth 1 (s-match flycheck-apib-line-column-regex line-column-pair)))
                    (column (nth 2 (s-match flycheck-apib-line-column-regex line-column-pair))))
                (list level error-message line column)))
            line-column-pairs))))

(defun flycheck-apib-error-parser (output checker buffer)
  "Parse errors with OUTPUT CHECKER within BUFFER."
  (let ((errors (funcall (-compose (lambda (errors)
                                     (-map #'flycheck-apib-error-flatten errors))
                                   (lambda (errors)
                                     (-remove 'string-empty-p errors))
                                   (lambda (output)
                                     (split-string output "\n")))
                         output)))
    (-map (lambda (error)
            (flycheck-error-new :buffer buffer
                                :checker checker
                                :filename buffer-file-name
                                :level (intern (nth 0 error))
                                :message (nth 1 error)
                                :line (string-to-number (nth 2 error))
                                :column (string-to-number (nth 3 error))))
          (-flatten-n 1 errors))))

(flycheck-define-checker apib-drafter
  "A syntax checker for API Blueprint using drafter."
  :command ("drafter" "-ul" source)
  :error-parser flycheck-apib-error-parser
  :modes apib-mode)

;;;###autoload
(defun flycheck-apib-setup ()
  "Setup Flycheck for apib-mode."
  (add-to-list 'flycheck-checkers 'apib-drafter))

(provide 'flycheck-apib)

;;; flycheck-apib.el ends here
