;;; org-page.el --- static site generator based on org mode

;; Copyright (C) 2012, 2013, 2014 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
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

;; See documentation at https://github.com/kelvinh/org-page

;; Org-page is a static site generator based on org mode.

;; Org-page provides following features:

;; 1) org sources and html files managed by git
;; 2) incremental publication (according to =git diff= command)
;; 3) category support
;; 4) tags support (auto generated)
;; 5) RSS support (auto generated)
;; 6) search engine support (auto generated)
;; 7) a beautiful theme
;; 8) theme customization support
;; 9) commenting (implemented using disqus)
;; 10) site visiting tracking (implemented using google analytics)
;; 11) index/about page support (auto generated if no default provided)
;; 12) site preview
;; 13) highly customizable

;;; Code:

(require 'ox)
(require 'ht)
(require 'aog-util)
(require 'aog-vars)
(require 'aog-git)
(require 'aog-enhance)
(require 'aog-export)
(require 'simple-httpd nil t)

(defconst org-page-version "0.5")

(defun aog/do-publication (&optional force-all
                                    base-git-commit pub-base-dir
                                    auto-commit auto-push)
  "The main entrance of org-page. The entire procedure is:
1) verify configuration
2) read changed files on branch `aog/repository-org-branch' of repository
`aog/repository-directory', the definition of 'changed files' is:
   1. if FORCE-ALL is non-nil, then all files will be published
   2. if FORCE-ALL is nil, the changed files will be obtained based on
BASE-GIT-COMMIT
   3. if BASE-GIT-COMMIT is nil or omitted, the changed files will be obtained
based on previous commit
3) publish org files to html, if PUB-BASE-DIR is specified, use that directory
to store the generated html files, otherwise html files will be stored on branch
`aog/repository-html-branch' of repository `aog/repository-directory'
4) if PUB-BASE-DIR is nil, and AUTO-COMMIT is non-nil, then the changes stored
on branch `aog/repository-html-branch' will be automatically committed, but be
careful, this feature is NOT recommended, and a manual commit is much better
5) if PUB-BASE-DIR is nil, AUTO-COMMIT is non-nil, and AUTO-PUSH is non-nil,
then the branch `aog/repository-html-branch' will be pushed to remote repo."
  (interactive
   (let* ((f (y-or-n-p "Publish all org files? "))
          (b (unless f (read-string "Base git commit: " "HEAD~1")))
          (p (when (y-or-n-p
                    "Publish to a directory? (to original repo if not) ")
               (read-directory-name "Publication directory: ")))
          (a (when (not p)
               (y-or-n-p "Auto commit to repo? ")))
          (u (when (and a (not p))
               (y-or-n-p "Auto push to remote repo? "))))
     (list f b p a u)))
  (aog/verify-configuration)
  (setq aog/item-cache nil)
  (let* ((to-repo (not (stringp pub-base-dir)))
         (git-repo-p (file-directory-p
                      (expand-file-name ".git" aog/repository-directory)))
         (orig-branch (when git-repo-p
                        (aog/git-branch-name aog/repository-directory)))
         (store-dir (if to-repo "~/.op-tmp/" pub-base-dir)) ; TODO customization
         (store-dir-abs (file-name-as-directory (expand-file-name store-dir)))
         changed-files all-files remote-repos)
    (when (and to-repo (not git-repo-p))
      (error "Publishing to repository branch requires a git repository"))
    (when git-repo-p
      (aog/git-change-branch aog/repository-directory aog/repository-org-branch))
    (aog/prepare-theme store-dir)
    (setq all-files
          (cl-remove-if
           #'(lambda (file)
               (let ((root-dir (file-name-as-directory
                                (expand-file-name aog/repository-directory))))
                 (member t
                          (mapcar
                          #'(lambda (cat)
                              (string-prefix-p
                               cat
                               (file-relative-name file root-dir)))
                          aog/category-ignore-list))))
           (if git-repo-p
               (aog/git-all-files aog/repository-directory)
             (directory-files-recursively
              aog/repository-directory ".*\\.org\\'" nil))))
    (setq changed-files (if force-all
                            `(:update ,all-files :delete nil)
                          (if git-repo-p
                              (aog/git-files-changed aog/repository-directory
                                                     (or base-git-commit "HEAD~1"))
                            `(:update ,all-files :delete nil))))
    (aog/publish-changes all-files changed-files store-dir-abs)
    (when to-repo
      (aog/git-change-branch aog/repository-directory aog/repository-html-branch)
      (copy-directory store-dir aog/repository-directory t t t)
      (delete-directory store-dir t))
    (when (and to-repo auto-commit)
      (aog/git-commit-changes aog/repository-directory "Update published html \
files, committed by org-page.")
      (when auto-push
        (setq remote-repos (aog/git-remote-name aog/repository-directory))
        (if (not remote-repos)
            (message "No valid remote repository found.")
          (let (repo)
            (if (> (length remote-repos) 1)
                (setq repo (read-string
                            (format "Which repo to push %s: "
                                    (prin1-to-string remote-repos))
                            (car remote-repos)))
              (setq repo (car remote-repos)))
            (if (not (member repo remote-repos))
                (message "Invalid remote repository '%s'." repo)
              (aog/git-push-remote aog/repository-directory
                                  repo
                                  aog/repository-html-branch)))))
      (when git-repo-p
        (aog/git-change-branch aog/repository-directory orig-branch)))
    (if to-repo
        (message "Publication finished: on branch '%s' of repository '%s'."
                 aog/repository-html-branch aog/repository-directory)
      (message "Publication finished, output directory: %s." pub-base-dir))))

(defun aog/new-repository (repo-dir)
  "Generate a new git repository in directory REPO-DIR, which can be
perfectly manipulated by org-page."
  (interactive
   (list (read-directory-name
          "Specify a directory to become the repository: " nil nil nil)))
  (aog/git-init-repo repo-dir)
  (aog/generate-readme repo-dir)
  (aog/git-commit-changes repo-dir "initial commit")
  (aog/git-new-branch repo-dir aog/repository-org-branch)
  (aog/generate-index repo-dir)
  (aog/git-commit-changes repo-dir "add source index.org")
  (aog/generate-about repo-dir)
  (aog/git-commit-changes repo-dir "add source about.org")
  (mkdir (expand-file-name "blog/" repo-dir) t))

(defun aog/verify-configuration ()
  "Ensure all required configuration fields are properly configured, include:
`aog/repository-directory': <required>
`aog/site-domain': <required>
`aog/personal-disqus-shortname': <optional>
`aog/personal-duoshuo-shortname': <optional>
`aog/export-backend': [optional](default 'html)
`aog/repository-org-branch': [optional] (but customization recommended)
`aog/repository-html-branch': [optional] (but customization recommended)
`aog/site-main-title': [optional] (but customization recommanded)
`aog/site-sub-title': [optional] (but customization recommanded)
`aog/personal-github-link': [optional] (but customization recommended)
`aog/personal-google-analytics-id': [optional] (but customization recommended)
`aog/theme': [optional]
`aog/highlight-render': [optional](default 'js)"
  (unless (and aog/repository-directory
               (file-directory-p aog/repository-directory))
    (error "Directory `%s' is not properly configured."
           (symbol-name 'aog/repository-directory)))
  (unless (file-directory-p (aog/get-theme-dir))
    (error "Org-page cannot detect theme directory `%s' automatically, please \
help configure it manually, usually it should be <org-page directory>/themes/."
           (symbol-name 'aog/theme)))
  (unless aog/site-domain
    (error "Site domain `%s' is not properly configured."
           (symbol-name 'aog/site-domain)))

  (setq aog/repository-directory (expand-file-name aog/repository-directory))
  (unless (or (string-prefix-p "http://" aog/site-domain)
              (string-prefix-p "https://" aog/site-domain))
    (setq aog/site-domain (concat "http://" aog/site-domain)))
  (unless aog/theme
    (setq aog/theme 'mdo))
  (unless aog/highlight-render
    (setq aog/highlight-render 'js)))

(defun aog/generate-readme (save-dir)
  "Generate README for `aog/new-repository'. SAVE-DIR is the directory where to
save generated README."
  (string-to-file
   (concat
    (format "Personal site of %s, managed by emacs, org mode, git and org-page."
            (or user-full-name "[Author]"))
    "\n\n"
    "This git repository is generated by org-page \"aog/new-repository\" \
function, it is only used for demonstrating how the git branches and directory \
structure are organized by org-page.")
   (expand-file-name "README" save-dir)))

(defun aog/generate-index (save-dir)
  "Generate index.org for `aog/new-repository'. SAVE-DIR is the directory where
to save generated index.org."
  (string-to-file
   (concat "#+TITLE: Index" "\n\n"
           (format "This is the home page of %s."
                   (or user-full-name "[Author]")))
   (expand-file-name "index.org" save-dir)))

(defun aog/generate-about (save-dir)
  "Generate about.org for `aog/new-repository'. SAVE-DIR is the directory where
to save generated about.org."
  (string-to-file
   (concat "#+TITLE: About" "\n\n"
           (format "* About %s" (or user-full-name "[Author]")) "\n\n"
           "  This file is automatically generated by org-page.")
   (expand-file-name "about.org" save-dir)))

(defun aog/insert-options-template (&optional title uri
                                             keywords tags description)
  "Insert a template into current buffer with information for exporting.

TITLE: the title of this post
URI: the uri of this post, usually looks like: /2013/12/27/the-post-title,
the following parameters could be used:
    %y: to represent the year of creation date
    %m: to represent the month of creation date
    %d: to represent the day of creation date
KEYWORDS: the keywords of this post, used by search engine
TAGS: the tags of this post, should be separated by comma and space
DESCRIPTION: the description of this post, it will be displayed in RSS feed

Note that this function does not verify the input parameters, it is users'
responsibility to guarantee these parameters are valid."
  (interactive
   (let* ((i (read-string "Title: "))
          (u (read-string "URI(%y, %m and %d can be used to represent year, \
month and day): " (unless (string= i "")
                    (format-spec "/blog/%y/%m/%d/%t"
                                 `((?y . "%y")
                                   (?m . "%m")
                                   (?d . "%d")
                                   (?t . ,(encode-string-to-url i)))))))
          (k (read-string "Keywords(separated by comma and space [, ]): "))
          (a (read-string "Tags(separated by comma and space [, ]): "))
          (d (read-string "Description: ")))
     (list i u k a d)))
  (if (not (bolp)) (newline))
  (insert (format
           "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+URI:         %s
#+KEYWORDS:    %s
#+TAGS:        %s
#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
#+DESCRIPTION: %s
"
           (if (string= title "") (buffer-name) title)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1))
           (if (string= uri "") "<TODO: insert your uri here>" uri)
           (if (string= keywords "")
               "<TODO: insert your keywords here>"
             keywords)
           (if (string= tags "") "<TODO: insert your tags here>" tags)
           org-export-default-language
           org-export-headline-levels
           nil ;; org-export-with-section-numbers
           nil ;; org-export-with-toc
           org-export-preserve-breaks
           ;; org-export-html-expand
           org-export-with-fixed-width
           org-export-with-tables
           nil ;; org-export-with-sub-superscripts
           nil ;; org-export-with-special-strings
           org-export-with-footnotes
           org-export-with-emphasize
           org-export-with-timestamps
           (if (string= description "")
               "<TODO: insert your description here>"
             description))))

(defun aog/new-post (&optional category filename)
  "Setup a new post.

CATEGORY: this post belongs to
FILENAME: the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid."
  (interactive
   (let* ((c (read-string "Category: " "blog"))
          (f (read-string "filename: " "new-post.org")))
     (list c f)))
  (if (string= category "")
      (setq category "blog"))
  (if (string= filename "")
      (setq filename "new-post.org"))
  (unless (string-suffix-p ".org" filename)
    (setq filename (concat filename ".org")))
  (let* ((dir (concat (file-name-as-directory aog/repository-directory)
                      (file-name-as-directory category)))
         (path (concat dir filename)))
    (aog/git-change-branch aog/repository-directory aog/repository-org-branch)
    (if (file-exists-p path)
        (error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (if (called-interactively-p 'any)
        (call-interactively 'aog/insert-options-template)
      (aog/insert-options-template "<Insert Your Title Here>"
                                  "/%y/%m/%d/%t/"
                                  "add, keywords, here"
                                  "add, tags, here"
                                  "add description here"))
    (save-buffer)))

(defun aog/do-publication-and-preview-site (path)
  "Do publication in PATH and preview the site in browser with simple-httpd.
When invoked without prefix argument then PATH defaults to
`aog/site-preview-directory'."
  (interactive
   (if current-prefix-arg
       (list (read-directory-name "Path: "))
       (list aog/site-preview-directory)))
  (aog/do-publication t nil path)
  (unless (get-process "httpd")
    (httpd-serve-directory path))
  (browse-url (format "http://%s:%d" "localhost" httpd-port)))


(provide 'aog)

;;; org-page.el ends here
