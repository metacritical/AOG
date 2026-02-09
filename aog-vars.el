;;; op-vars.el --- Variable configurations required by org-page

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

;; op-vars.el contains almost all variable definitions and configurations.

;;; Code:

(require 'ox)
(require 'ht)


(defgroup org-page nil
  "Options for generating static pages using org-page."
  :tag "Org static page generator" :group 'org)

(defconst aog/temp-buffer-name "*Org Page Output*"
  "Name of the temporary buffer used by org-page.")

(defconst aog/load-directory
  (cond
   (load-file-name (file-name-directory load-file-name))
   ((symbol-file 'aog/temp-buffer-name)
    (file-name-directory (symbol-file 'aog/temp-buffer-name)))
   ((string= (file-name-nondirectory buffer-file-name) "op-vars.el")
    (file-name-directory buffer-file-name))
   (t nil))
  "The directory where org-page is loaded from.")

(defcustom aog/repository-directory nil
  "The git repository directory, where org files stored on branch
`aog/repository-org-branch', and generated html files stored on branch
`aog/repository-html-branch'."
  :group 'org-page :type 'string)

(defcustom aog/export-backend 'html
  "The org-export backend used for page generation"
  :group 'org-page :type 'symbol)

(defcustom aog/site-domain nil
  "The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned."
  :group 'org-page :type 'string)

(defcustom aog/site-main-title "org-page"
  "The main title of entire site."
  :group 'org-page :type 'string)

(defcustom aog/site-sub-title "static site generator"
  "The subtitle of entire site."
  :group 'org-page :type 'string)

(defcustom aog/repository-org-branch "source"
  "The branch where org files stored on, it is within repository presented by
`aog/repository-directory'."
  :group 'org-page :type 'string)

(defcustom aog/repository-html-branch "master"
  "The branch where generated html files stored on, it is within repository
presented by `aog/repository-directory'."
  :group 'org-page :type 'string)

(defcustom aog/theme-root-directory
  (concat aog/load-directory "themes/")
  "The root directory that stores themes for page rendering. By default, it
points to the directory `themes' in org-page installation directory."
  :group 'org-page :type 'string)

(defcustom aog/theme 'mdo
  "The theme used for page generation."
  :group 'org-page :type 'symbol)

(defcustom aog/highlight-render 'js
  "Code highlight render."
  :group 'org-page :type 'symbol)


(defcustom aog/personal-github-link "https://github.com/kelvinh/org-page"
  "The personal github link."
  :group 'org-page :type 'string)

(defcustom aog/personal-avatar nil
  "The link to an avatar image."
  :group 'org-page :type 'string)

(defcustom aog/personal-disqus-shortname nil
  "The personal disqus shortname."
  :group 'org-page :type 'string)

(defcustom aog/personal-duoshuo-shortname nil
  "The personal duoshuo shortname."
  :group 'org-page :type 'string)

(defcustom aog/personal-google-analytics-id nil
  "Personal google analytics id."
  :group 'org-page :type 'string)

(defcustom aog/template-directory nil
  "The directory stores templates for page rendering. By default, org-page uses
`aog/theme' and `aog/theme-root-directory' to determine the template directory.
DON'T set this variable unless you know what you are doing!"
  :group 'org-page :type 'string)

(defcustom aog/confound-email t
  "This variable is used to determine whether email addresses should be
confounded or not."
  :group 'org-page :type 'boolean)

(defcustom aog/tag-rss nil
  "This variable is used to determine whether a rss.xml will be generated for
each tag."
  :group 'org-page :type 'boolean)

(defcustom aog/organization nil
  "This variable is used to determine whether the site is used by organization or not"
  :group 'org-page :type 'boolean)

(defcustom aog/retrieve-category-function 'aog/get-file-category
  "The function used to retrieve an org file's category, its parameter is the
org file's path, if parameter is nil, it should return all categories, the
default value is `aog/get-file-category'."
  :group 'org-page :type 'function)

(defcustom aog/site-preview-directory "~/.op-tmp/"
  "Temporary directory path for site preview."
  :group 'org-page :type 'string)

(defvar aog/category-config-alist
  '(("blog" ;; this is the default configuration
    :show-meta t
    :show-comment t
    :uri-generator aog/generate-uri
    :uri-template "/blog/%y/%m/%d/%t/"
    :sort-by :date     ;; how to sort the posts
    :category-index t) ;; generate category index or not
   ("index"
    :show-meta nil
    :show-comment nil
    :uri-generator aog/generate-uri
    :uri-template "/"
    :sort-by :date
    :category-index nil)
   ("about"
    :show-meta nil
    :show-comment nil
    :uri-generator aog/generate-uri
    :uri-template "/about/"
    :sort-by :date
    :category-index nil))
  "Configurations for different categories, can and should be customized.")

(defvar aog/category-ignore-list
  '("themes" "assets")
  "Ignore these subdirs/categories for navigation")

;;; this variable is deprecated
(defvar aog/default-template-parameters
  (ht ("blog-uri" "/blog/")
      ("wiki-uri" "/wiki/")
      ("tags-uri" "/tags/")
      ("about-uri" "/about/")
      ("site-main-title" aog/site-main-title)
      ("site-sub-title" aog/site-sub-title)
      ("avatar" aog/personal-avatar)
      ("github" aog/personal-github-link)
      ("site-domain" (if (and aog/site-domain
                              (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                            aog/site-domain))
                         (match-string 1 aog/site-domain)
                       aog/site-domain))
      ("disqus-shortname" aog/personal-disqus-shortname)
      ("disqus-comment" (if aog/personal-disqus-shortname t nil))
      ("duoshuo-shortname" aog/personal-duoshuo-shortname)
      ("duoshuo-comment" (if aog/personal-duoshuo-shortname t nil))
      ("google-analytics-id" aog/personal-google-analytics-id)
      ("google-analytics" (if aog/personal-google-analytics-id t nil))
      ("creator-info" org-html-creator-string))
  "Default template rendering parameters.")

(defvar aog/item-cache nil
  "The cache for general purpose.")

(defconst aog/rss-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>{{title}}</title>
    <link>{{link}}</link>
    <description>{{description}}</description>
    <pubDate>{{date}}</pubDate>
    <lastBuildDate>{{date}}</lastBuildDate>
    <docs>http://www.rssboard.org/rss-specification</docs>
    <generator>Org-page static site generator \
(https://github.com/kelvinh/org-page)</generator>
    {{#items}}
    <item>
      <title>{{item-title}}</title>
      <link>{{item-link}}</link>
      <description><![CDATA[{{& item-description}}]]></description>
      <pubDate>{{item-update-date}}</pubDate>
      <guid>{{item-link}}</guid>
    </item>
    {{/items}}
  </channel>
</rss>"
  "Template for RSS rendering.")

(defcustom aog/html-creator-string
  (format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
 (<a href=\"http://orgmode.org\">Org mode</a> %s)"
          (format "%s.x" emacs-major-version)
          (if (fboundp 'org-version)
              (replace-regexp-in-string "\\..*" ".x" (org-version))
            "Unknown Version"))
  "Information about the creator of the HTML document."
  :group 'org-page
  :type 'string)

(defcustom aog/date-final-format
  (lambda (datestr) datestr)
  "final date formatting function. Org-page format dates as
aaaa-mm-dd by default during the generation of category index
pages. This function allows the user to change the format.

The provided function should accept one string argument which is
the date in the aaaa-mm-dd format. It should return a string
representing the date in its new format."
  :group 'org-page :type 'function)

(defcustom aog/hashover-comments nil
  "use hashover commenting system"
  :group 'org-page
  :type 'boolean)


(provide 'aog-vars)

;;; aog-vars.el ends here
