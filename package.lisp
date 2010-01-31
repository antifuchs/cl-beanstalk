(defpackage :beanstalk
  (:use)
  (:export #:with-beanstalk-connection #:quit #:connect #:disconnect
           ;; Conditions:
           #:beanstalk-error #:bad-reply #:beanstalkd-out-of-memory #:buried-job #:beanstalkd-draining
           #:beanstalkd-internal-error #:bad-message-format #:expected-crlf #:unknown-command
           #:deadline-soon #:yaml-parsing-failed
           ;; Protocol commands:
           #:use #:put #:reserve #:delete #:release #:bury #:touch #:watch #:ignore
           #:peek #:peek-ready #:peek-delayed #:peek-buried #:kick #:list-tube-used
           #:stats-job #:stats-tube #:stats
           #:list-tubes #:list-tubes-watched))

(defpackage :beanstalk-internal
  (:use :cl))