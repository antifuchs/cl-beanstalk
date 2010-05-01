(defvar *port* cl-user::*beanstalk-port*)

(After
 (when (var :connection)
   (beanstalk:disconnect (var :connection))))
