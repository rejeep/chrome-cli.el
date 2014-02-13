(require 'commander)
(require 'shut-up)

(shut-up-silence-emacs)

(defvar cc-test/windows
  '((:id "38"   :name "rejeep/chrome-cli.el")
    (:id "1641" :name "Tuxicity - Blog"))
  "")

(defvar cc-test/tabs
  '((:id "1642" :window-id "1641" :title "Tuxicity - Blog"       :url "http://tuxicity.se/")
    (:id "1813" :window-id "38"   :title "rejeep/chrome-cli.el"  :url "https://github.com/rejeep/chrome-cli.el")
    (:id "1895" :window-id "38"   :title "prasmussen/chrome-cli" :url "https://github.com/prasmussen/chrome-cli"))
  "")


(defvar cc-test-window-id nil)

(defun cc-test/window-id (window-id)
  (setq cc-test-window-id window-id))

(defun cc-test/print (&rest args)
  (princ (concat (s-join "\n" args) "\n")))

(defun cc-test/list (what)
  (let ((tabs (if cc-test-window-id
                  (-select
                   (lambda (tab)
                     (equal cc-test-window-id (plist-get tab :window-id)))
                   cc-test/tabs)
                cc-test/tabs)))
    (cond ((string= what "windows")
           (apply 'cc-test/print
                  (-map
                   (lambda (window)
                     (format "[%s] %s"
                             (plist-get window :id)
                             (plist-get window :name))
                     )
                   cc-test/windows
                   )
                  )
           )
          ((string= what "tabs")
           (apply 'cc-test/print
                  (-map
                   (lambda (tab)
                     (format "[%s:%s] %s"
                             (plist-get tab :window-id)
                             (plist-get tab :id)
                             (plist-get tab :title))
                     )
                   tabs
                   )
                  )
           )
          ((string= what "links")
           (apply 'cc-test/print
                  (-map
                   (lambda (tab)
                     (format "[%s:%s] %s"
                             (plist-get tab :window-id)
                             (plist-get tab :id)
                             (plist-get tab :url)
                             )
                     )
                   tabs
                   )
                  )
           )
          ))
  )

;;   (cond ((string= what "windows")
;;          (cc/print
;;           "[1641] Tuxicity - Blog"
;;           "[38] rejeep/chrome-cli.el"
;;           )
;;          )
;;         ((string= what "tabs")
;;          (if window-id
;;              (cc/print
;;               "[1642] Tuxicity - Blog"
;;               "[1813] rejeep/chrome-cli.el"
;;               "[1895] prasmussen/chrome-cli")
;;            )
;;          )
;;         )
;;   )

(commander
 (command "list <what>" cc-test/list)

 (option "-w <window-id>" cc-test/window-id)
 )
