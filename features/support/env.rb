require 'cucumber'
require 'spec'

require 'beanstalk-client'
require 'clucumber'

# Constants:
PORT = 11317
BEANSTALK_CONNSPEC = "localhost:#{PORT}"

# Programs we need:

unless File.exist?(File.expand_path("../step_definitions/clucumber_override.wire", File.dirname(__FILE__)))
  begin
    @main_clucumber = ClucumberSubprocess.new(File.expand_path("../", File.dirname(__FILE__)),
                                              :port => 42428)
    at_exit do
      @main_clucumber.kill
    end

    @main_clucumber.start <<-LISP
      (defvar cl-user::*beanstalk-port* #{PORT})
      (load #p"#{File.expand_path("../../cl-beanstalk.asd", File.dirname(__FILE__))}")
  LISP
  rescue PTY::ChildExited
    puts(@main_clucumber && @main_clucumber.output)
  end
end


at_exit {
  system "kill #{@beanstalk.pid}" rescue nil
}

@beanstalk = IO.popen("beanstalkd -p #{PORT}")


module CLBeanstalkWorld
end

World(CLBeanstalkWorld)
