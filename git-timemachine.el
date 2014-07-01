;;; -*- lexical-binding: t -*-

(require 'subr-x)
(require 'cl-lib)

(defun git-timemachine--revisions ()
 (split-string
  (shell-command-to-string
   (format "cd %s && git log --pretty=format:%s %s"
    (shell-quote-argument timemachine-git-directory)
    (shell-quote-argument "%h")
    (shell-quote-argument timemachine-file)))
  nil t "\s+"))

(defun git-timemachine-show-current-revision ()
 (interactive)
 (git-timemachine-show-revision (car (git-timemachine--revisions))))

(defun git-timemachine-show-previous-revision ()
 (interactive)
 (git-timemachine-show-revision (cadr (member timemachine-revision (git-timemachine--revisions)))))

(defun git-timemachine-show-next-revision ()
 (interactive)
 (git-timemachine-show-revision (cadr (member timemachine-revision (reverse (git-timemachine--revisions))))))

(defun git-timemachine-show-revision (revision)
 (when revision
  (let ((current-position (point)))
   (setq buffer-read-only nil)
   (erase-buffer)
   (insert
    (shell-command-to-string
     (format "cd %s && git show %s:%s"
      (shell-quote-argument timemachine-git-directory)
      (shell-quote-argument revision)
      (shell-quote-argument timemachine-file))))
   (setq buffer-read-only t)
   (set-buffer-modified-p nil)
   (let* ((revisions (git-timemachine--revisions))
          (n-of-m (format "(%d/%d)" (- (length revisions) (cl-position revision revisions :test 'equal)) (length revisions))))
    (setq mode-line-format (list "Commit: " revision " -- %b -- " n-of-m " -- [%p]")))
   (setq timemachine-revision revision)
   (goto-char current-position))))

(defun git-timemachine-quit ()
 (interactive)
 (kill-buffer))

(defun git-timemachine-kill-revision ()
 (interactive)
 (let ((this-revision timemachine-revision))
  (with-temp-buffer
   (insert this-revision)
   (kill-region (point-min) (point-max)))))

(defun git-timemachine-mode ()
 (interactive)
 (let* ((git-directory (concat (string-trim-right (shell-command-to-string "git rev-parse --show-toplevel")) "/"))
        (relative-file (string-remove-prefix git-directory (buffer-file-name)))
        (timemachine-buffer (format "timemachine:%s" (buffer-name))))
  (with-current-buffer (get-buffer-create timemachine-buffer)
   (setq buffer-file-name relative-file)
   (set-auto-mode)
   (setq-local timemachine-git-directory git-directory)
   (setq-local timemachine-file relative-file)
   (setq-local timemachine-revision nil)
   (local-set-key (kbd "p") 'git-timemachine-show-previous-revision)
   (local-set-key (kbd "n") 'git-timemachine-show-next-revision)
   (local-set-key (kbd "q") 'git-timemachine-quit)
   (local-set-key (kbd "w") 'git-timemachine-kill-revision)
   (git-timemachine-show-current-revision)
   (switch-to-buffer timemachine-buffer))))

(provide 'git-timemachine)
