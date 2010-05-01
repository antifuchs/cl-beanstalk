(Given* #?"^the cl-beanstalk client is started$" ()
  (setf (var :connection) (beanstalk:connect "localhost" *port*)))
