(in-package :cl-user)
(defpackage :feedme.fetch
  (:use :common-lisp :drakma :xmls)
  (:export :fetch-items))

(in-package :feedme.fetch)

(setq drakma:*text-content-types* (cons '("application" . "rss+xml")
    drakma:*text-content-types*))

(defun get-children (dom selectors)
  (let ((selector (car selectors))
        (remaining_selectors (cdr selectors)))
    (if remaining_selectors
      (get-children (xmls:xmlrep-find-child-tag selector dom) remaining_selectors)
      (xmls:xmlrep-find-child-tags selector dom))))

(defun extract-rss-item (dom)
  (loop for item in (get-children dom '("channel" "item"))
        collect (list :title (third (xmls:xmlrep-find-child-tag "title" item))
                      :content (third (xmls:xmlrep-find-child-tag "description" item))
                      :url (third (xmls:xmlrep-find-child-tag "link" item)))))

(defun extract-atom-item (dom)
  (loop for item in (get-children dom '("entry"))
        collect (list :title (third (xmls:xmlrep-find-child-tag "title" item))
                      :content (third (xmls:xmlrep-find-child-tag "content" item))
                      :url (xmls:xmlrep-attrib-value "href" (xmls:xmlrep-find-child-tag "link" item)))))

(defun fetch-items (url)
  (let ((response (drakma:http-request url :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format response) :utf-8)
    (let ((dom (xmls:parse response)))
      (if (xmls:xmlrep-find-child-tags "channel" dom)
        (extract-rss-item dom)
        (extract-atom-item dom)))))
