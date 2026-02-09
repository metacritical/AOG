;;; op-template.el --- templating system based on mustache, required by org-page

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

;; templating system based on mustache.el, to replace `format-spec'.

;;; Code:

(require 'ox)
(require 'cl-lib)
;; (require 'mustache)
(autoload 'mustache-render "mustache")
(require 'aog-util)
(require 'aog-vars)
(require 'aog-git)


(defun aog/get-template-dir ()
  "Return the template directory, it is determined by variable
`aog/theme-root-directory' with `aog/theme' or `aog/template-directory'."
  (or aog/template-directory
      (file-name-as-directory
       (expand-file-name
        (format "%s/templates" (symbol-name aog/theme))
        aog/theme-root-directory))))

(defun aog/get-cache-item (key)
  "Get the item associated with KEY in `aog/item-cache', if `aog/item-cache' is
nil or there is no item associated with KEY in it, return nil."
  (and aog/item-cache
       (plist-get aog/item-cache key)))

(defun aog/update-cache-item (key value)
  "Update the item associated with KEY in `aog/item-cache', if `aog/item-cache' is
nil, initialize it."
  (if aog/item-cache
      (plist-put aog/item-cache key value)
    (setq aog/item-cache `(,key ,value)))
  value)

(defmacro aog/get-cache-create (key &rest body)
  "Firstly get item from `aog/item-cache' with KEY, if item not found, evaluate
BODY and push the result into cache and return it."
  `(or (aog/get-cache-item ,key)
       (aog/update-cache-item ,key (funcall (lambda () ,@body)))))

(defun aog/get-category-name (category)
  "Return the name of the CATEGORY based on aog/category-config-alist :label property. 
Default to capitalized CATEGORY name if no :label property found."
  (let* ((config (cdr (or (assoc category aog/category-config-alist)
                          (assoc "blog" aog/category-config-alist)))))
    (or (plist-get config :label)
        (capitalize category))))

(defun aog/render-header (&optional param-table)
  "Render the header on each page. PARAM-TABLE is the hash table from mustache
to render the template. If it is not set or nil, this function will try to build
a hash table accordint to current buffer."
  (mustache-render
   (aog/get-cache-create
    :header-template
    (message "Read header.mustache from file")
    (file-to-string (concat (aog/get-template-dir) "header.mustache")))
   (or param-table
       (ht ("page-title" (concat (or (aog/read-org-option "TITLE") "Untitled")
                                 " - " aog/site-main-title))
           ("author" (or (aog/read-org-option "AUTHOR")
                         user-full-name "Unknown Author"))
           ("description" (aog/read-org-option "DESCRIPTION"))
           ("keywords" (aog/read-org-option "KEYWORDS"))))))

(defun aog/render-navigation-bar (&optional param-table)
  "Render the navigation bar on each page. it will be read firstly from
`aog/item-cache', if there is no cached content, it will be rendered
and pushed into cache from template. PARAM-TABLE is the hash table for mustache
to render the template. If it is not set or nil, this function will try to
render from a default hash table."
  (aog/get-cache-create
   :nav-bar-html
   (message "Render navigation bar from template")
   (mustache-render
    (aog/get-cache-create
     :nav-bar-template
     (message "Read nav.mustache from file")
     (file-to-string (concat (aog/get-template-dir) "nav.mustache")))
    (or param-table
        (ht-merge (ht ("site-main-title" aog/site-main-title)
                      ("site-sub-title" aog/site-sub-title)
                      ("nav-categories"
                       (mapcar
                        #'(lambda (cat)
                            (ht ("category-uri"
                                 (concat "/" (encode-string-to-url cat) "/"))
                                ("category-name" (aog/get-category-name cat))))
                        (sort (cl-remove-if
                               #'(lambda (cat)
                                   (or (string= cat "index")
                                       (string= cat "about")))
                               (aog/get-file-category nil))
                              'string-lessp)))
                      ("github" aog/personal-github-link)
                      ("avatar" aog/personal-avatar)
                      ("site-domain" (if (string-match
                                          "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                          aog/site-domain)
                                         (match-string 1 aog/site-domain)
                                       aog/site-domain)))
                  (if aog/organization (ht ("authors-li" t)) (ht ("avatar" aog/personal-avatar))))))))

(defun aog/render-content (&optional template param-table)
  "Render the content on each page. TEMPLATE is the template name for rendering,
if it is not set of nil, will use default post.mustache instead. PARAM-TABLE is
similar to `aog/render-header'. `aog/highlight-render' is `js' or `htmlize'."
  (mustache-render
   (aog/get-cache-create
    (if template
        (intern (replace-regexp-in-string "\\.mustache$" "-template" template))
      :post-template)
    (message (concat "Read " (or template "post.mustache") " from file"))
    (file-to-string (concat (aog/get-template-dir)
                            (or template "post.mustache"))))
   (or param-table
       (ht ("title" (or (aog/read-org-option "TITLE") "Untitled"))
           ("content"
            (cond ((eq aog/highlight-render 'js)
                   (progn
                     (cl-letf (((symbol-function'org-html-fontify-code)
                                #'(lambda (code lang)
                                    (when code
                                      (org-html-encode-plain-text code)))))
                       (org-export-as aog/export-backend nil nil t nil))))
                  ((eq aog/highlight-render 'htmlize)
                   (org-export-as aog/export-backend nil nil t nil))))))))

(defun aog/render-footer (&optional param-table)
  "Render the footer on each page. PARAM-TABLE is similar to
`aog/render-header'."
  (mustache-render
   (aog/get-cache-create
    :footer-template
    (message "Read footer.mustache from file")
    (file-to-string (concat (aog/get-template-dir) "footer.mustache")))
   (or param-table
       (let* ((filename (buffer-file-name))
              (title (or (aog/read-org-option "TITLE") "Untitled"))
              (date (fix-timestamp-string
                     (or (aog/read-org-option "DATE")
                         (format-time-string "%Y-%m-%d"))))
              (tags (aog/read-org-option "TAGS"))
              (tags (if tags
                        (mapcar
                         #'(lambda (tag-name)
                             (ht ("link" (aog/generate-tag-uri tag-name))
                                 ("name" tag-name)))
                         (delete "" (mapcar 'trim-string (split-string tags "[:,]+" t))))))
              (category (funcall (or aog/retrieve-category-function
                                     #'aog/get-file-category)
                                 filename))
              (config (cdr (or (assoc category aog/category-config-alist)
                               (assoc "blog" aog/category-config-alist))))
              (uri (funcall (plist-get config :uri-generator)
                            (plist-get config :uri-template) date title)))
         (ht ("show-meta" (plist-get config :show-meta))
             ("show-comment" (plist-get config :show-comment))
             ("date" (funcall aog/date-final-format date))
             ("mod-date" (funcall
			  aog/date-final-format
			  (if (not filename)
			      (format-time-string "%Y-%m-%d")
			    (or (aog/git-last-change-date
				 aog/repository-directory
				 filename)
				(format-time-string
				 "%Y-%m-%d"
				 (nth 5 (file-attributes filename)))))))
             ("tags" tags)
             ("tag-links" (if (not tags) "N/A"
                            (mapconcat
                             #'(lambda (tag)
                                 (mustache-render
                                  "<a href=\"{{link}}\">{{name}}</a>" tag))
                             tags ", ")))
             ("author" (or (aog/read-org-option "AUTHOR")
                           user-full-name
                           "Unknown Author"))
	     ("hashover-comment" (and (boundp 'aog/hashover-comments)
				      aog/hashover-comments))
             ("disqus-id" uri)
             ("disqus-url" (get-full-url uri))
             ("disqus-comment" (and (boundp 'aog/personal-disqus-shortname)
                                    aog/personal-disqus-shortname))
             ("disqus-shortname" aog/personal-disqus-shortname)
             ("duoshuo-comment" (and (boundp 'aog/personal-duoshuo-shortname)
                                     aog/personal-duoshuo-shortname))
             ("duoshuo-shortname" aog/personal-duoshuo-shortname)
             ("google-analytics" (and (boundp 'aog/personal-google-analytics-id)
                                      aog/personal-google-analytics-id))
             ("google-analytics-id" aog/personal-google-analytics-id)
             ("creator-info" aog/html-creator-string)
             ("email" (confound-email (or (aog/read-org-option "EMAIL")
                                          user-mail-address
                                          "Unknown Email"))))))))

;;; this function is deprecated
(defun aog/update-default-template-parameters ()
  "Update the default template parameters. It is only needed when user did some
customization to relevant variables."
  (ht-update
   aog/default-template-parameters
   (ht ("site-main-title" aog/site-main-title)
       ("site-sub-title" aog/site-sub-title)
       ("github" aog/personal-github-link)
       ("site-domain" (if (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                        aog/site-domain)
                          (match-string 1 aog/site-domain)
                        aog/site-domain))
       ("disqus-shortname" aog/personal-disqus-shortname)
       ("disqus-comment" (if aog/personal-disqus-shortname t nil))
       ("duoshuo-shortname" aog/personal-duoshuo-shortname)
       ("duoshuo-comment" (if aog/personal-duoshuo-shortname t nil))
       ("google-analytics-id" aog/personal-google-analytics-id)
       ("google-analytics" (if aog/personal-google-analytics-id t nil))))
  aog/default-template-parameters)

;;; this function is deprecated
(defun aog/compose-template-parameters (attr-plist content)
  "Compose parameters for org file represented in current buffer.
ATTR-PLIST is the attribute plist of the buffer, retrieved by the combination of
`org-export--get-inbuffer-options' and `aog/get-inbuffer-extra-options'."
  (let* ((info
          (org-combine-plists
           (org-export--get-global-options 'html)
           attr-plist))
         (title (org-element-interpret-data (plist-get info :title)))
         (author (org-element-interpret-data
                  (or (plist-get info :author) user-full-name)))
         (email (confound-email (or (plist-get info :email)
                                    user-mail-address)))
         (description (or (plist-get info :description) nil))
         (keywords (or (plist-get info :keywords) nil))
         (category (plist-get info :category))
         (show-meta-info (and (not (eq category 'index))
                              (not (eq category 'about))
                              (not (eq category 'none))))
         (creation-date (if (plist-get info :date)
                            (fix-timestamp-string
                             (org-element-interpret-data
                              (plist-get info :date)))
                          "N/A"))
         (mod-date (or (plist-get info :mod-date) "N/A"))
         (tag-links (mapconcat
                     #'(lambda (tag-name)
                         (mustache-render
                          "<a href=\"{{link}}\">{{name}}</a>"
                          (ht ("link" (aog/generate-tag-uri tag-name))
                              ("name" tag-name))))
                     (plist-get info :tags) ", "))
         (show-comment (eq category 'blog))
         (disqus-id (plist-get info :uri))
         (disqus-url (get-full-url disqus-id))
         (param-table (ht-create)))
    (ht-update param-table aog/default-template-parameters)
    (ht-update
     param-table
     (ht ("page-title"        (concat title " - " aog/site-main-title))
         ("author"            author)
         ("description"       description)
         ("keywords"          keywords)
         ("title"             title)
         ("content"           content)
         ("show-meta-info"    show-meta-info)
         ("creation-date"     creation-date)
         ("modification-date" mod-date)
         ("tags"              tag-links)
         ("show-comment"      show-comment)
         ("disqus-id"         disqus-id)
         ("disqus-url"        disqus-url)
         ("email"             email)))
    param-table))


(provide 'aog-template)

;;; op-template.el ends here
