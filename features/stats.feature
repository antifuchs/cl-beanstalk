Feature: Statistics

  As a user of cl-beanstalk, 
  I want to see job and queue statistics
  So that I can know what my work load is.

Background:
  Given the cl-beanstalk client is started

Scenario: Queue statistics
  Given there is a worker listening on the "something" queue
  When I request queue stats for the "something" queue
  And the stats should have :current-watching 1

Scenario: Listing queues
  Given there is a worker listening on the "something" queue
  When I request a queue list
  Then the queue list should contain "default"
  And the queue list should contain "something"

Scenario: Job statistics
  Given there is a worker listening on the "default" queue
  Given I inject a job "foo" into the "default" queue

  When I request statistics for that job
  Then the stats should have :state "ready"
  And the stats should have :tube "default"
  And the stats should have :reserves 0
  
  When I reserve a job
  And I request statistics for that job
  Then the stats should have :state "reserved"
