(in-package :cl-user)

(defpackage :feedme.model
  (:use :common-lisp :crane)
  (:import-from :feedme.fetch
                :fetch-items)
  (:export :add-item
           :add-feed
           :unread-items
           :item
           :feed
           :feeds))

(in-package :feedme.model)
(crane:setup
  :migrations-directory "migrations/"
  :databases
  '(:main
    (:type :sqlite3
     :name "feedme.sqlite3")))

(crane:connect)

(crane:deftable feed ()
  (name :type text :uniquep t)
  (url :type text :uniquep t)
  (etag :type text))

(defun add-feed (name url &optional (etag "-"))
  (let ((feed (crane:create 'feed
			      :name name
			      :url url
			      :etag etag)))
    (crane:save feed)))

(crane:deftable item ()
  (title :type text :uniquep t)
  (url :type text :uniquep t)
  (content :type text)
  (archived :type integer)
  (feed :type integer :foreign feed :nullp nil))

(defun add-item (title url content feed-id &optional (archived 0))
  (let ((feed-item (crane:create 'item
				 :title title
				 :url url
				 :content content
				 :feed feed-id
				 :archived archived)))
    (format t "[!] item added (~a,~a): ~a~%" feed-id archived title)
    (crane:save feed-item)))

(defun feeds ()
  (crane:filter 'feed))

(defun unread-items ()
  (crane:filter 'item (:= :archived 0)))

(defun update-feed (feed-id url)
  (loop for feed-item in (fetch-items url) do
    (let ((title (getf feed-item :title))
	  ;(pubdate (item-content 'pubDate feed-item))
          (item-url (getf feed-item :url))
          (content (getf feed-item :content)))
      (crane:with-transaction ()
        (if (not (crane:filter 'item :url item-url))
          (add-item title item-url content feed-id))))))

(defun update-feeds ()
  (loop for feed in (feeds) do
        (format t "fetching feed: ~a~%" (slot-value feed 'name))
        (handler-case
          (update-feed (slot-value feed 'id) (slot-value feed 'url))
          (error (e) (progn
                       (format t "cannot fetch feed '~a': ~a~%" (slot-value feed 'name) e)
                       t))))
  t)
