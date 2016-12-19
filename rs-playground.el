;;; rs-playground.el --- Local Rust playground for short code snippets.

;; Copyright (C) 2016  Alexander I.Grafov (axel)

;; Author: Alexander I.Grafov <grafov@gmail.com>
;; URL: https://github.com/grafov/rs-playground
;; Keywords: tools, rust
;; Package-Requires: ((emacs "24") (rust-mode "1.0.0"))

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

;; Local playground for the Rust programs similar to play.rust-lang.org.
;; `M-x rs-playground` and type you rust code then make&run it with `C-Return`.

;; Playground works in conjunction with `rust-mode` and requires
;; preconfigured environment for Rust language.

;; It is port of github.com/grafov/go-playground for Go language.

;; You may push code to play.rust-lang.org with rust-mode' function `rust-playpen-buffer`.

;;

;;; Code:

(require 'rust-mode)
(require 'compile)
(require 'time-stamp)

; I think it should be defined in rust-mode.
(defcustom rust-bin "rustc"
  "The ’rust’ command."
  :type 'string
  :group 'rust-mode)

(defgroup rs-playground nil
  "Options specific to Rust Playground."
  :group 'rust-mode)

(defcustom rs-playground-ask-file-name nil
  "Non-nil means we ask for a name for the snippet.

By default it will be created as snippet.go"
  :type 'boolean
  :group 'rs-playground)

(defcustom rs-playground-confirm-deletion t
  "Non-nil means you will be asked for confirmation on the snippet deletion with `rs-playground-rm'.

By default confirmation required."
  :type 'boolean
  :group 'rs-playground)

(defcustom rs-playground-basedir "~/rs-playground"
  "Base directory for playground snippets."
  :group 'rs-playground)

(defvar-local rs-playground-current-snippet-file "snippet.rs"
  "The current snippet file.")

(define-minor-mode rs-playground-mode
  "A place for playing with golang code and export it in short snippets."
  :init-value nil
  :lighter ""
  :keymap '(([C-return] . rs-playground-exec))
  (setq mode-name "Play(Rust)"))

(defun rs-playground-snippet-file-name(&optional snippet-name)
 (setq-local rs-playground-current-snippet-file (let ((file-name (cond (snippet-name)
                         (rs-playground-ask-file-name
                          (read-string "Rust Playground filename: "))
                         ("snippet"))))
    (concat (rs-playground-snippet-unique-dir file-name) "/" file-name ".rs"))))

(defun rs-playground-exec ()
  "Save the buffer then runs Rust compiler for executing the code."
  (interactive)
  (make-local-variable 'compile-command)
  (let ((snippet-file buffer-file-name))
	(save-buffer t)	
	(compile (concat rust-bin " " snippet-file " -o snippet && " (file-name-directory snippet-file) "snippet"))))

;;;###autoload
(defun rs-playground ()
  "Run playground for Rust language in a new buffer."
  (interactive)
  (let ((snippet-file-name (rs-playground-snippet-file-name)))
    (switch-to-buffer (create-file-buffer snippet-file-name))
	(rs-playground-insert-template-head "snippet of code")
(insert "fn main() {

    println!(\"Results:\")

}
")
    (backward-char 3)
    (rust-mode)
    (rs-playground-mode)
    (set-visited-file-name snippet-file-name t)))

(defun rs-playground-insert-template-head (description)
  (insert "// -*- mode:rust;mode:rs-playground -*-
// " description " @ " (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S") "

// === Rust Playground ===
// Execute the snippet with Ctl-Return
// Remove the snippet completely with its dir and all files M-x `rs-playground-rm`

"))

;;;###autoload
(defun rs-playground-rm ()  
  "Remove files of the current snippet together with directory of this snippet."
  (interactive)
  (if (string-match-p (file-truename rs-playground-basedir) (file-truename (buffer-file-name)))
      (if (or (not rs-playground-confirm-deletion)
	       (y-or-n-p (format "Do you want delete whole snippet dir %s? "
				 (file-name-directory (buffer-file-name)))))
		  (progn
			(save-buffer)
			(delete-directory (file-name-directory (buffer-file-name)) t t)
			(kill-buffer)))
	(message "Won't delete this! Because %s is not under the path %s. Remove the snippet manually!"
			 (buffer-file-name) rs-playground-basedir)))

;; ;;;###autoload
;; (defun rs-playground-download (url)
;;   "Download a paste from the play.golang.org and insert it in a new local playground buffer.
;; Tries to look for a URL at point."
;;   (interactive (list (read-from-minibuffer "Playground URL: " (ffap-url-p (ffap-string-at-point 'url)))))
;;   (with-current-buffer
;;       (let ((url-request-method "GET") url-request-data url-request-extra-headers)
;;         (url-retrieve-synchronously (concat url ".go")))
;;     (let* ((snippet-file-name (rs-playground-snippet-file-name)) (buffer (create-file-buffer snippet-file-name)))
;;       (goto-char (point-min))
;;       (re-search-forward "\n\n")
;;       (copy-to-buffer buffer (point) (point-max))
;;       (kill-buffer)
;;       (with-current-buffer buffer
;; 		(goto-char (point-min))
;; 		(rs-playground-insert-template-head (concat url " imported"))
;; 		(rust-mode)
;; 		(rs-playground-mode)
;; 		(set-visited-file-name snippet-file-name t)
;;         (switch-to-buffer buffer)))))

;; (defun rs-playground-upload ()
;;   "Upload the current buffer to play.golang.org and return the short URL of the playground."
;;   (interactive)
;;   (goto-char (point-min))
;;   (forward-line)
;;   (insert (rust-playpen-buffer)))

(defun rs-playground-snippet-unique-dir (prefix)
  "Get unique directory under `rs-playground-basedir`."
  (let ((dir-name (concat rs-playground-basedir "/"
                          (if (and prefix rs-playground-ask-file-name) (concat prefix "-"))
                          (time-stamp-string "at-%:y-%02m-%02d-%02H%02M%02S"))))
    (make-directory dir-name t)
    dir-name))

(provide 'rs-playground)
;;; rs-playground.el ends here
