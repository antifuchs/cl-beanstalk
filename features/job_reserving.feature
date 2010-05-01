Feature: Reserving a job with cl-beanstalk

  As a beanstalk worker
  I want to be able to get units of work
  So that I can be a reliable worker node.

Scenario: Reserving a job from the default queue

  Given the cl-beanstalk client is started
  Given I am listening on the "default" queue

  When a client injects a job "foo" into the "default" queue
  And I reserve a job
  Then the status should be :reserved
  And the cl-beanstalk job body should be "foo"
  
  When I delete the job
  And I reserve a job with 0s timeout
  Then the status should be :timed-out
