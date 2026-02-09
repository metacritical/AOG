;;; op-enhance.el --- HTML page customization required by org-page

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

;; Improve generated html page display effect

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'aog-util)
(require 'aog-vars)


(defun aog/get-theme-dir ()
  "Return the resource storage directory, it is determined by variable
`aog/theme-root-directory' and `aog/theme'."
  (file-name-as-directory
   (expand-file-name
    (format "%s/resources" (symbol-name aog/theme))
    aog/theme-root-directory)))

(defun aog/prepare-theme (pub-root-dir)
  "Copy theme files to PUB-ROOT-DIR."
  (let ((pub-theme-dir (expand-file-name "media/" pub-root-dir))
        (theme-dir (aog/get-theme-dir)))
    (unless (file-directory-p theme-dir)
      (message "Theme %s not found, use default theme `mdo' instead."
               (symbol-name aog/theme))
      (setq aog/theme-root-directory (concat aog/load-directory "themes/"))
      (setq aog/theme 'mdo)
      (setq theme-dir (aog/get-theme-dir)))
    (when (file-directory-p pub-theme-dir)
      (delete-directory pub-theme-dir t))
    (copy-directory theme-dir pub-theme-dir t t t)))


(provide 'aog-enhance)

;;; op-enhance.el ends here
