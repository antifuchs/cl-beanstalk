Then /^the reserve should have timed out$/ do
  @timed_out.should be_true
end

Then /^the job body should be "([^\"]*)"$/ do |text|
  @job.body.should == text
end
