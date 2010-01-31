(asdf:defsystem cl-beanstalk
  :depends-on (:usocket :flexi-streams :split-sequence)
  :components ((:file "package")
               (:file "beanstalk" :depends-on ("package"))))