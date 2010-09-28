Feature: Injecting jobs with cl-beanstalk

  As a beanstalk client
  I want to inject jobs into the beanstalk server
  So that my workers can get units of work, in the right order.

Background:
  Given the cl-beanstalk client is started

Scenario: Injecting a job into the default queue.

  Given there is a worker listening on the "default" queue
  When I inject a job "foo" into the "default" queue
  And the worker reserves a job

  Then the job body should be "foo"

Scenario: Injecting a job into another queue

  When I inject a job "foo" into the "some-other" queue
  When there is a worker listening on the "some-other" queue
  And the worker reserves a job

  Then the job body should be "foo"

Scenario: Injecting jobs with a priority
  
  Given there is a worker listening on the "default" queue

  When I inject a job "not urgent" into the "default" queue at priority 900
  And I inject a job "very urgent" into the "default" queue at priority 9

  When the worker reserves a job
  Then the job body should be "very urgent"

  When the worker reserves a job
  Then the job body should be "not urgent"

@wip @slow
Scenario: Injecting jobs with a delay

  Given there is a worker listening on the "default" queue

  When I inject a job "delayed" into the "default" queue with 1s delay
  And the worker reserves a job with 0s timeout
  Then the reserve should have timed out

  When the worker reserves a job with 2s timeout
  Then the job body should be "delayed"
