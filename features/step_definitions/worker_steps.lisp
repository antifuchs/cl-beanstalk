(Given* #?"^I am listening on the \"([^\"]*)\" queue$" (queue)
  (beanstalk:watch (var :connection) queue))

(defun reserve-job (&key timeout)
  (multiple-value-bind (body id status) (beanstalk:reserve (var :connection) :timeout timeout)
    (setf (var :body) body
          (var :id) id
          (var :status) status)))

(When* #?"^I reserve a job$" ()
  (reserve-job))

(Then* #?"^the status should be (:.*)$" (expected)
 (assert (eql (read-from-string expected) (var :status))
         nil
         "Status is ~S, should be ~S" (var :status) expected))

(When* #?"^I reserve a job with (\\d+)s timeout$" (timeout)
  (reserve-job :timeout (parse-integer timeout)))

(Then* #?"^the cl-beanstalk job body should be \"([^\"]*)\"$" (body)
  (assert (string= body (var :body))
          nil
          "Job's body is ~S, should be ~S" (var :body) body))

(When* #?"^I delete the job$" ()
  (assert (beanstalk:delete (var :connection) (var :id))))
