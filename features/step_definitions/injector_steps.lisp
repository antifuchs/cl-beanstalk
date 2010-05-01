(defun inject-job (queue body &key (pri 0) (delay 0))
  (beanstalk:use (var :connection) queue)
  (beanstalk:put (var :connection) pri delay 90 body))

(When* #?"^I inject a job \"([^\"]*)\" into the \"([^\"]*)\" queue$" (body queue)
  (inject-job queue body))

(When* #?"^I inject a job \"([^\"]*)\" into the \"([^\"]*)\" queue at priority (\\d+)$" (body queue pri)
  (inject-job queue body :pri (parse-integer pri)))

(When* #?"^I inject a job \"([^\"]*)\" into the \"([^\"]*)\" queue with (\\d+)s delay$" (body queue delay)
  (inject-job queue body :delay (parse-integer delay)))


