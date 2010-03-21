Given /^the cl\-beanstalk client is started$/ do
  startup
  reconnect
end

Transform /^priority \d+$/ do |prio_spec|
  prio_spec.match(/\d+$/)[0].to_i
end

Transform /^\d+s (?:delay|timeout)$/ do |time_spec|
  time_spec.match(/^(\d+)s/)[1].to_i
end


When /^I inject a job "([^\"]*)" into the "([^\"]*)" queue(?: at (priority \d+))?(?: with (\d+s delay))?$/ do |body, queue, pri, delay|
  send_lisp_code(<<-LISP, 3)
    (beanstalk:use *connection* "#{queue}")
  LISP
  pri ||= 1
  delay ||= 0
  send_lisp_code(<<-LISP, 3)
    (beanstalk:put *connection* #{pri} #{delay} 90 "#{body}")
  LISP
end

Given /^I am listening on the "([^\"]*)" queue$/ do |queue|
  send_lisp_code "(beanstalk:watch *connection* \"#{queue}\")"
end

When /^I reserve a job$/ do
  send_lisp_code "(nth-value 0 (beanstalk:reserve *connection*))" do |output|
    body = output.join('').match(/"([^"]+)"/)[1]
    @job = OpenStruct.new(:body => body)
  end
end
