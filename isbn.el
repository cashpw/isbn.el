;;; isbn.el --- ISBN utilities -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cash@cashpw.com>
;; Maintainer: Cash Prokop-Weaver <cash@cashpw.com>
;; Created: June 30, 2025
;; Modified: June 30, 2025
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashpw/isbn.el
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'cl-lib)

(cl-defun isbn-10-p (number)
  "Return non-nil if NUMBER is a valid ISBN-10."
  (cl-check-type number string)
  (let* ((number-without-hyphens (replace-regexp-in-string "-" "" number))
         (number-as-list
          (mapcar
           #'string-to-number
           (string-split number-without-hyphens "" 'omit-nulls))))
    (and
     (= 10 (length number-as-list))
     ;; Each of the first nine digits of the 10-digit ISBN—excluding the check
     ;; digit itself—is multiplied by its (integer) weight, descending from 10
     ;; to 2, and the sum of these nine products found. The value of the check
     ;; digit is simply the one number between 0 and 10 which, when added to
     ;; this sum, means the total is a multiple of 11. (Wikipedia)
     (= 0
        (mod
         ;; Addition-only algorithm from Wikipedia.
         (let ((a 0)
               (b 0))
           (dotimes (i 10)
             (cl-incf a (nth i number-as-list))
             (cl-incf b a))
           b)
         11)))))

(cl-defun isbn-13-p (number)
  "Return non-nil if NUMBER is a valid ISBN-13."
  (cl-check-type number string)
  (let* ((number-without-hyphens (replace-regexp-in-string "-" "" number))
         (number-as-list
          (mapcar
           #'string-to-number
           (string-split number-without-hyphens "" 'omit-nulls))))
    (and
     (= 13 (length number-as-list))
     ;; The ISBN-13 check digit, which is the last digit of the ISBN, must range
     ;; from 0 to 9 and must be such that the sum of all the thirteen digits,
     ;; each multiplied by its (integer) weight, alternating between 1 and 3, is
     ;; a multiple of 10. (Wikipedia)
     (= 0
        (mod
         (apply #'+
                (list
                 (nth 0 number-as-list)
                 (* 3 (nth 1 number-as-list))
                 (nth 2 number-as-list)
                 (* 3 (nth 3 number-as-list))
                 (nth 4 number-as-list)
                 (* 3 (nth 5 number-as-list))
                 (nth 6 number-as-list)
                 (* 3 (nth 7 number-as-list))
                 (nth 8 number-as-list)
                 (* 3 (nth 9 number-as-list))
                 (nth 10 number-as-list)
                 (* 3 (nth 11 number-as-list))
                 (nth 12 number-as-list)))
         10)))))

(defvar isbn--hyphenated-number-regexp
  (rx
   space
   (group (maximal-match (one-or-more (seq digit (zero-or-one "-")))))
   space)
  "Matches a chain of hyphenated(?) digits.")

(cl-defun isbn--get-all-candidates-in-buffer ()
  "Return list of ISBN-13 numbers in the current buffer."
  (let ((candidates '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward isbn--hyphenated-number-regexp nil t)
        (push (substring-no-properties (match-string 1)) candidates)))
    (nreverse candidates)))

(cl-defun isbn-13-get-all-in-buffer ()
  "Return list of ISBN-13 numbers in the current buffer."
  (seq-filter #'isbn-13-p (isbn--get-all-candidates-in-buffer)))

(cl-defun isbn-10-get-all-in-buffer ()
  "Return list of ISBN-10 numbers in the current buffer."
  (seq-filter #'isbn-10-p (isbn--get-all-candidates-in-buffer)))

(provide 'isbn)
;;; isbn.el ends here
