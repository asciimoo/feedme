;(ql:quickload :crane :silent t)

;; woo -> clack -> ningle
(in-package :cl-user)
(defpackage :feedme
  (:use :common-lisp)
  (:import-from :feedme.model
                :item
                :feed
                :add-item
                :add-feed
                :feeds
                :update-feeds
                :unread-items)
  (:import-from :feedme.webapp
                :start-webapp
                :reload-webapp
                :stop-webapp)
  (:export :add-feed
           :item
           :feed
           :start-webapp
           :reload-webapp
           :stop-webapp
           :update-feeds
           :unread-items))

(in-package :feedme)
