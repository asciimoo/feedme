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
                :update-feeds)
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
           :update-feeds))

(in-package :feedme)
