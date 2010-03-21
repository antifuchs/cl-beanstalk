# Reserving:

Given /^there is a worker listening on the "([^\"]*)" queue$/ do |queue|
  @worker = Beanstalk::Pool.new([BEANSTALK_CONNSPEC])
  @worker.watch(queue)
end

When /^the worker reserves a job$/ do 
  @timed_out = false
  @job = @worker.reserve(5)
end

When /^the worker reserves a job with (\d+s timeout)$/ do |timeout|
  begin
    @timed_out = false
    @job = @worker.reserve(timeout)
  rescue Beanstalk::TimedOut
    @timed_out = true
  end  
end

Then /^the reserve should have timed out$/ do
  @timed_out.should be_true
end

# Injection:

When /^a client injects a job "([^\"]*)" into the "([^\"]*)" queue$/ do |text, queue|
  @client = Beanstalk::Pool.new([BEANSTALK_CONNSPEC])
  @client.use(queue)
  @client.put(text)
end
