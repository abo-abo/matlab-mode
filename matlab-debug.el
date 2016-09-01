;;; matlab-debug.el --- Debugger integration

;; Copyright (C) 2016 Oleh Krehel

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

;;* dbstop if error
(defconst matlab-dbstatus-header
  (propertize "dbstatus:" 'face 'font-lock-keyword-face))

(defun matlab-dbstatus ()
  "Return pretty-printed \"dbstatus\"."
  (interactive)
  (let ((strs (cdr (split-string (matlab-eval "dbstatus") "\n" t))))
    (unless strs
      (setq strs '("none")))
    (mapconcat #'identity (cons matlab-dbstatus-header strs)
               "\n    ")))

(defun matlab-dbstop-if-error ()
  "Toggle \"dbstop if error\"."
  (interactive)
  (let ((status (matlab-eval "dbstatus")))
    (if (string-match-p "^Stop if error" status)
        (matlab-eval "dbclear if error")
      (matlab-eval "dbstop if error")))
  (when (called-interactively-p 'any)
    (message
     (matlab-dbstatus))))

(defun matlab-dbstop-if-caught-error ()
  "Toggle \"dbstop if caught error\"."
  (interactive)
  (let ((status (matlab-eval "dbstatus")))
    (if (string-match-p "^Stop if caught error" status)
        (matlab-eval "dbclear if caught error")
      (matlab-eval "dbstop if caught error")))
  (when (called-interactively-p 'any)
    (message
     (matlab-dbstatus))))

(defhydra hydra-matlab-dbstatus (:color pink
                                 :columns 1)
  "
%s(matlab-dbstatus)
"
  ("e" (matlab-dbstop-if-error) "dbstop if error")
  ("c" (matlab-dbstop-if-caught-error) "dbstop if caught error")
  ("q" nil "quit"))

;;* Breakpoint in fringe
(defun matlab-dbg-breakpoint-add (point)
  (gdb-put-string nil point `(left-fringe breakpoint breakpoint-enabled))
  (let ((file (buffer-file-name))
        (line (line-number-at-pos)))
    (matlab-eval (format "dbstop in %s at %d" file line))))

(defun matlab-dbg-breakpoint-remove (posn)
  (with-selected-window (posn-window posn)
    (let ((pt (posn-point posn))
          (file (buffer-file-name))
          (line (line-number-at-pos)))
      (gdb-remove-strings pt (1+ pt))
      (matlab-eval (format "dbclear in %s at %d" file line)))))

(defun matlab-dbg-breakpoint-toggle (event)
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (with-selected-window (posn-window posn)
      (if (buffer-file-name)
          (if (numberp (posn-point posn))
              (save-excursion
                (goto-char (posn-point posn))
                (if (or (posn-object posn)
                        (memq (car (fringe-bitmaps-at-pos (posn-point posn)))
                              '(breakpoint)))
                    (matlab-dbg-breakpoint-remove posn)
                  (matlab-dbg-breakpoint-add (posn-point posn))))))
      (posn-set-point posn))))

;;* Misc debug
(defun matlab-debug-goto ()
  "When in debugger, to to the top of \"dbstack\"."
  (interactive)
  (let ((stack (matlab-eval "dbstack")))
    (when (string-match "In \\(.*\\) (line \\([0-9]+\\))$" stack)
      (let* ((file (match-string 1 stack))
             (line (match-string 2 stack))
             (full-file (matlab-eval (format "which('%s')" file))))
        (if (file-exists-p full-file)
            (progn
              (setq matlab-debug-current-file full-file)
              (find-file-other-window full-file)
              (goto-char (point-min))
              (forward-line (string-to-number line)))
          (error "MATLAB requested file %s but it does not exist" full-file))))))

(defun matlab-debug-dbstep (arg)
  "When in debugger, to to the top of \"dbstack\"."
  (interactive "p")
  (let* ((res (matlab-eval (format "dbstep %d" arg)))
         (line-number (read res)))
    (with-current-buffer (find-file-noselect matlab-debug-current-file)
      (goto-char (point-min))
      (forward-line (1- line-number)))))

(defun matlab-dbstep-in ()
  (interactive)
  (matlab-eval "dbstep in;dbhotlink(1);"))

(defun matlab-dbcont ()
  (interactive)
  (matlab-eval "dbcont"))

(defun matlab-dbquit ()
  (interactive)
  (matlab-eval "dbquit"))

(defun matlab-dbup ()
  (interactive)
  (matlab-eval "dbup"))

(defun matlab-dbdown ()
  (interactive)
  (matlab-eval "dbdown"))

(defun matlab-eval-variable ()
  (interactive)
  (let ((vars
         (delete
          "em_who"
          (split-string (matlab-eval "em_who") "\n" t))))
    (ivy-read "var: " vars
              :action
              (lambda (x)
                (message (matlab-eval x))))))

(defhydra hydra-matlab (:color pink
                        :exit t)
  "mat"
  ("e" hydra-matlab-dbstatus/body "on error")
  ("d" hydra-matlab-debug/body "debug")
  ("q" nil "quit"))

(defhydra hydra-matlab-debug (:color pink)
  "db"
  ("S" matlab-dbstatus "status")
  ("l" matlab-debug-goto "goto line")
  ("j" matlab-debug-dbstep "dbstep")
  ("e" matlab-eval-variable "eval")
  ("g" (matlab-eval "dbcont") "dbcont" :exit t)
  ("Q" (matlab-eval "dbquit") "dbquit" :exit t)
  ("q" nil "quit"))

(provide 'matlab-debug)
