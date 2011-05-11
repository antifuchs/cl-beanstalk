(asdf:defsystem cl-beanstalk
  :description "An interface to the beanstalk queue server"
  :depends-on (:usocket :flexi-streams :split-sequence)
  :components ((:file "package")
               (:file "beanstalk" :depends-on ("package"))))
