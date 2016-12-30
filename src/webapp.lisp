;(ql:quickload :drakma)
;(ql:quickload :xmls)

(in-package :cl-user)
(defpackage :feedme.webapp
  (:use :common-lisp :clack :ningle :cl-markup :sxql)
  (:import-from :feedme.model
                :unread-items
                :add-feed
                :feeds
                :update-feeds)
  (:export :reload-webapp
           :start-webapp
           :stop-webapp))

(in-package :feedme.webapp)

(defvar *app* (make-instance 'ningle:<app>))
(defvar *handler* nil)

(defun get-member (member-name l)
  (second (member member-name l)))

(defun style ()
  "body {
    margin: 0;
    padding: 0;
    background: #222222;
    color: #bdc3c7;
    font-family: 'Garamond', 'Georgia', serif;
    font-weight: normal;
    font-size: 80%;
   }
   h1, h2, h3, h4, h5 { font-weight: normal; }
   h2 { font-size: 160%; }
   a { color: #3498db; text-decoration: none; }
   hr { border: 1px solid #444444; }
   input { border: 1px solid #444444; background: #222222; margin: 0 8px; color: #bdc3c7; }
   #wrapper { clear: both; max-width: 800px; margin-left: auto; margin-right: auto; }
   #items > ul > li { list-style-type: none; }
   .item { overflow: hidden; }
   .item img { max-width: 600px; }
   .item-title a { color: #1abc9c; }
   .item p { clear: both; }
   .clear { clear: both; }
   .feed-name { padding: 0; margin: -1.9em 0 0; font-size: 0.8em; }
   #top_bar { margin: 0; padding: 4px; background-color: #111111; }
   #top_bar .menu { margin: 0; }
   #top_bar .menu li { float: left; list-style-type: none; margin-left: 20px; }
   #top_bar .unread_items { float: right; padding-right: 24px; }
   "
)

(defun html-base (content)
  (let ((*auto-escape* nil))
    (html5 (:head (:style (style)))
           (:body
             (:div :id "top_bar"
                   (:ul :class "menu"
                     (:li (:a :href "/" "home"))
                     (:li (:a :href "/feeds" "feeds"))
                     (:li (:a :href "/fetch" "fetch")))
                   (:div :class "unread_items"
                         (format nil "unread items: ~a"
                                 (second
                                   (first (crane:query
                                            (sxql:select
                                              (sxql:fields (:as (:count :id) :num-of-items))
                                              (sxql:from :item)
                                              (sxql:where (:= :archived 0))))))))

                   (:div :class "clear" ""))
             (:div :id "wrapper"
                   content)))))

(defun render-items (items)
  (let ((*auto-escape* nil)
        (archive-html (if (= (length items) 0)
              (markup (:div (:h4 "hurray, no unread items")))
              (markup (:div (:a :href (concatenate 'string "/archive?max_id="
                                           (princ-to-string
                                             (get-member ':|id| (car (last items))))))
                        "archive this view")))))
    (markup (:div
             :id "items"
             (:ul (loop for i in items
                        collect (markup (:li
                                         (:div
                                          :class "item"
                                          (:h2
                                           :class "item-title" (:a
                                                                :href (get-member ':|url| i)
                                                                (get-member ':|title| i)))
                                          (:div
                                           :class "feed-name"
                                           (get-member ':|name| i))
                                          (:p (get-member ':|content| i))
                                          (:hr)))))))
            (:p archive-html))))

(setf (ningle:route *app* "/")
  #'(lambda (params)
      (declare (ignore params))
      (let* ((*auto-escape* nil)
             (items (unread-items))
             (html (html-base (render-items items))))
        `(200
          (:content-type "text/html; charset=utf-8")
          (,html)))))

(setf (ningle:route *app* "/archive")
  #'(lambda (params)
      (let ((last-item-id (cdr (assoc "max_id" params :test #'string=))))
        (handler-case
          (let ((int-last-item-id (parse-integer last-item-id)))
            (crane:query (sxql:update :item
                                       (sxql:set= :archived 1)
                                       (sxql:where (:and (:<= :id int-last-item-id)
                                                         (:<= :archived 0))))))
          (error (e) (progn (format t "archive error: ~a ~%" e) t)))
        '(302
          (:location "/")
          ("")))))

(setf (ningle:route *app* "/feeds")
  #'(lambda (params)
      (let ((feed_name (cdr (assoc "feed_name" params :test #'string=)))
            (feed_url (cdr (assoc "feed_url" params :test #'string=))))
        (if (and feed_name feed_url) (add-feed feed_name feed_url)))
      (let ((html (html-base (markup
                               (:h2 "New feed")
                               (:div (:form :method "get" :action "/feeds"
                                            (:input :type "text" :name "feed_name" :placeholder "name")
                                            (:input :type "text" :name "feed_url" :placeholder "url")
                                            (:input :type "submit" :value "ok")))
                               (:h2 "Feeds")
                               (:ul (loop for f in (feeds)
                                          collect (let ((name (slot-value f 'feedme.model::name))
                                                        (url (slot-value f 'feedme.model::url)))
                                                    (markup (:li (:a :href url name))))))))))
      `(200
        (:content-type "text/html; charset=utf-8")
        (,html)))))

(setf (ningle:route *app* "/fetch")
  #'(lambda (params)
      (declare (ignore params))
      (update-feeds)
      `(302
        (:location "/")
        (""))))

(defun is-running? ()
  (if (eq *handler* nil)
    nil
    t))

(defun start-webapp ()
  (if (not (is-running?))
    (setf *handler* (clack:clackup *app*
                                   :server :woo
                                   :use-thread t))))

(defun stop-webapp ()
  (if (is-running?)
    (progn
      (clack:stop *handler*)
      (setf *handler* nil))))

(defun reload-webapp ()
  (stop-webapp)
  (start-webapp))
