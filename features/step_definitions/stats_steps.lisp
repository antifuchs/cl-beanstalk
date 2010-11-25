(When* #?{^I request queue stats for the "([^"]*)" queue$} (tube)
  (setf (var :stats) (beanstalk:stats-tube (var :connection) tube)))

(Then* #?{^the stats should have ([^ ]*) (.*)$} (arg value)
  (let ((property (read-from-string arg))
        (expected-value (read-from-string value))
        (actual-value (getf (var :stats) (read-from-string arg))))
    (assert (equal expected-value actual-value)
            ()
            "Stats for ~s are not ~s as expected, but ~s.~%Stats are: ~s"
            property expected-value actual-value (var :stats))))

(When* #?{^I request a queue list$} ()
  (setf (var :queues) (beanstalk:list-tubes (var :connection))))

(Then* #?{^the queue list should contain "([^"]*)"$} (queue)
  (assert (member queue (var :queues) :test #'string=)))

(When* #?{^I request statistics for that job$} ()
  (setf (var :stats)
        (beanstalk:stats-job (var :connection) (var :injected-job-id))))
