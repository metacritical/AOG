
将该文件夹解压后放至 org-page包位置\theme 文件夹下即可

用 org-page 管理多个站点的简单方法：

(require 'org-page)

(defun aog/custom-org-page (&optional site)
  "choose the org-page's main repository"
  (interactive (let ((sitet (read-string "静态站点名：")))
                 (list sitet)))
  (cond         ;;emacs-china
        ((equal site "emacs-china")
         (setq aog/repository-directory "~/github/emacs-china.github.io"
               aog/site-domain "emacs-china.github.io"
               aog/theme 'emacs_love
               aog/personal-github-link "https://github.com/emacs-china"
               aog/personal-disqus-shortname "emacs-china"
               aog/personal-duoshuo-shortname "emacs-china"
               aog/site-main-title "EMACS-CHINA"
               aog/site-sub-title "=============>集思广益")
         )
                ;;xxxx
        ((equal site "xxxx")
         (setq aog/repository-directory "xxxx"
               aog/site-domain "xxxx"
               aog/theme 'mdo
               aog/personal-github-link "xxxx"
               aog/personal-disqus-shortname "xxxx"
               aog/personal-duoshuo-shortname "xxxx"
               aog/site-main-title "xxxx"
               aog/site-sub-title "xxxx")
         )
        (t nil)))
