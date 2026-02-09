;;; op-git.el --- git related functions required by org-page

;; Copyright (C) 2012, 2013, 2014 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: convenience
;; Homepage: https://github.com/kelvinh/org-page

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

;; git repository operation functions

;;; Code:

(require 'ox)
(require 'ht)
(require 'aog-util)
(require 'aog-vars)
(require 'dash)

(defun aog/git--run (repo-dir &rest args)
  "Run git ARGS in REPO-DIR and return stdout as a string."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory repo-dir)))
      (if (zerop (apply #'process-file "git" nil t nil args))
          (buffer-string)
        (error "git command failed: git %s" (mapconcat #'identity args " "))))))

(defun aog/git-all-files (repo-dir &optional branch)
  "This function will return a list contains all org files in git repository
presented by REPO-DIR, if optional BRANCH is offered, will check that branch
instead of pointer HEAD."
  (let* ((org-file-ext ".org")
         (output (aog/git--run repo-dir "ls-tree" "-r" "--name-only"
                               (or branch "HEAD"))))
    (--map (expand-file-name it repo-dir)
           (--filter (string-suffix-p org-file-ext it t)
                     (split-string output "\n")))))

(defun aog/git-branch-name (repo-dir)
  "Return name of current branch of git repository presented by REPO-DIR."
  (string-trim (aog/git--run repo-dir "rev-parse" "--abbrev-ref" "HEAD")))

(defun aog/git-new-branch (repo-dir branch-name)
  "This function will create a new branch with BRANCH-NAME, and checkout it."
  (let* ((branches (split-string (aog/git--run repo-dir "branch" "--list" branch-name)
                                 "\n" t)))
    (unless branches
      (aog/git--run repo-dir "branch" branch-name))
    (aog/git--run repo-dir "checkout" branch-name)))

(defun aog/git-change-branch (repo-dir branch-name)
  "This function will change branch to BRANCH-NAME of git repository presented
by REPO-DIR. Do nothing if it is current branch."
  (unless (string= (aog/git-branch-name repo-dir) branch-name)
    (aog/git--run repo-dir "checkout" branch-name)))

(defun aog/git-init-repo (repo-dir)
  "This function will initialize a new empty git repository. REPO-DIR is the
directory where repository will be initialized."
  (unless (file-directory-p repo-dir)
    (mkdir repo-dir t))
  (aog/git--run repo-dir "init"))

(defun aog/git-commit-changes (repo-dir message)
  "This function will commit uncommitted changes to git repository presented by
REPO-DIR, MESSAGE is the commit message."
  (aog/git--run repo-dir "add" "-A")
  (aog/git--run repo-dir "commit" "-m" message))

(defun aog/git-files-changed (repo-dir base-commit)
  "This function can get modified/deleted org files from git repository
presented by REPO-DIR, diff based on BASE-COMMIT. The return value is a
property list, property :update maps a list of updated/added files, property
:delete maps a list of deleted files.
For git, there are three types: Added, Modified, Deleted, but for org-page,
only two types will work well: need to publish or need to delete.
<TODO>: robust enhance, branch check, etc."
  (let* ((org-file-ext ".org")
         (git-repo (file-name-as-directory repo-dir))
         (output (aog/git--run repo-dir "diff" "--name-status" base-commit "HEAD"))
         upd-list del-list)

    (--each (split-string output "\n")
      (when (string-match "\\`[A|M]\t\\(.*\.org\\)\\'" it)
        (!cons (concat git-repo (match-string 1 it)) upd-list))
      (when (string-match "\\`D\t\\(.*\.org\\)\\'" it)
        (!cons (concat git-repo (match-string 1 it)) del-list)))

    (list :update upd-list :delete del-list)))

(defun aog/git-last-change-date (repo-dir filepath)
  "This function will return the last commit date of a file in git repository
presented by REPO-DIR, FILEPATH is the path of target file, can be absolute or
relative."
  (condition-case nil
      (let* ((output (aog/git--run repo-dir "log" "-1" "--format=%ci" "--" filepath)))
        (when (string-match "\\`\\([0-9]+-[0-9]+-[0-9]+\\) .*\\'" (string-trim output))
          (match-string 1 output)))
    (error nil)))

(defun aog/git-remote-name (repo-dir)
  "This function will return all remote repository names of git repository
presented by REPO-DIR, return nil if there is no remote repository."
  (split-string (aog/git--run repo-dir "remote") "\n" t))

(defun aog/git-push-remote (repo-dir remote-repo branch)
  "This function will push local branch to remote repository, REPO-DIR is the
local git repository, REMOTE-REPO is the remote repository, BRANCH is the name
of branch will be pushed (the branch name will be the same both in local and
remote repository), and if there is no branch named BRANCH in remote repository,
it will be created."
  (aog/git--run repo-dir "push" remote-repo branch))


(provide 'aog-git)

;;; op-git.el ends here
