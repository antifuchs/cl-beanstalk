require 'cucumber'
require 'spec'

require 'singleton'
require 'pty'
require 'expect'

require 'beanstalk-client'


# Constants:
PORT = 11317
BEANSTALK_CONNSPEC = "localhost:#{PORT}"

LISP = ENV['LISP'] || 'sbcl --no-siteinit --no-userinit --disable-debugger'

Dir.chdir(File.join(File.dirname(__FILE__), '..', '..'))

# Programs we need:

class CLAdapter
  include Singleton
  
  TIMEOUT=1
  
  def startup
    return if @startup_done
    @startup_done = true
    
    at_exit {
      system "kill #{@beanstalk.pid}" rescue nil
      system "kill #{@pid}" rescue nil
    }

    @beanstalk = IO.popen("beanstalkd -p #{PORT}")
    @output, @input, @pid = PTY.spawn("#{LISP}")

    @input.sync = true
    send "(require :asdf)"
    send '(load "cl-beanstalk.asd")'
    send "(handler-case (asdf:oos 'asdf:load-op :cl-beanstalk) (error (e) (print e) (exit)))"
    send "(defpackage :cl-beanstalk-test (:use :cl))"
    send "(in-package :cl-beanstalk-test)"
    send "(defvar *connection* nil)"
    @output.expect "* ", TIMEOUT
  end

  def reconnect
    send_lisp_code "(when *connection* (beanstalk:disconnect *connection*) (setf *connection* nil))"
    send_lisp_code "(setf *connection* (beanstalk:connect \"localhost\" #{PORT}))"
  end
  
  def send_lisp_code(code, timeout=TIMEOUT)
    @input.puts "nil"
    send(code)
    @output.expect(code.strip, timeout) { |output|
      raise "Huh, didn't see my own input" unless output
      @output.expect("* ", timeout) {|output|
        raise "Lisp probably raised an error." unless output
        yield(output) if block_given?
      }
    }
  end
  
  def send(string, timeout=TIMEOUT)
    @output.expect("* ", timeout) {|output|
      raise "Inferior lisp didn't output a prompt" unless output
      @input.puts string
    }
  end
end

module CLBeanstalkWorld
  def adapter
    CLAdapter.instance
  end
  
  def startup
    adapter.startup
  end
  def reconnect
    adapter.reconnect
  end
  def send(code, timeout=nil)
    if timeout
      adapter.send(code, timeout)
    else
      adapter.send(code)
    end
  end
  def send_lisp_code(code, timeout=nil, &block)
    if timeout
      adapter.send_lisp_code(code, timeout, &block)
    else
      adapter.send_lisp_code(code, &block)
    end
  end
end

Before('@debug') do
  $expect_verbose = true
end

After('@debug') do
  $expect_verbose = false
end

World(CLBeanstalkWorld)
