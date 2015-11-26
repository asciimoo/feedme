(in-package :cl-user)
(defpackage feedme-asd
  (:use :cl :asdf))

(in-package :feedme-asd)

(defsystem feedme
  :version "0.1.0"
  :author "Adam Tauber"
  :license "AGPLv3+"
  :depends-on (:cl-markup
               :clack
               :crane
               :drakma
               :ningle
               :woo
               :xmls)
  :components ((:module "src"
                :components
                  ((:file "feedme" :depends-on ("model" "webapp"))
                   (:file "webapp" :depends-on ("model"))
                   (:file "model" :depends-on ("fetch"))
                   (:file "fetch"))))
  :description "Super micro feed reader"
  :long-description "Super micro feed reader")
