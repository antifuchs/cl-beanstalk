Then /^the job body should be "([^\"]*)"$/ do |text|
  @job.body.should == text
end
