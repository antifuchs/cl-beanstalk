# CL-BEANSTALK, an Implementation of the beanstalk v1.4.2 protocol in Common Lisp

CL-BEANSTALK implements the version 1.4.2 of the Beanstalk queuing service protocol. It allows splitting programs into "worker" parts that execute units of work asynchronously, by fetching them from the beanstalk daemon.

## Installing CL-BEANSTALK

CL-BEANSTALK requires three CL libraries to run:

1. [usocket](http://common-lisp.net/project/usocket)
2. [flexi-streams](http://weitz.de/flexi-streams/)
3. [split-sequence](http://www.cliki.net/SPLIT-SEQUENCE)

All of these libraries are available via [quicklisp](http://quicklisp.org/).

### Getting cl-beanstalk from quicklisp

On the lisp REPL, with quicklisp loaded, run:

        (ql:quickload :cl-beanstalk)

And you're done!

### Getting cl-beanstalk from source

	git clone git://github.com/antifuchs/cl-beanstalk.git

Then, add the directory to your asdf system definition directory. After this, asdf:load-system should be able to find and load cl-beanstalk.
        
## Using CL-BEANSTALK

First, all protocol functions reside in the **BEANSTALK:** package. Their names are the same as in the [beanstalk protocol documentation](http://github.com/kr/beanstalkd/blob/v1.4.2/doc/protocol.txt?raw=true).

Second, there are several error conditions specified in the protocol document, and all of these map to a specific lisp condition. All error conditions are subtypes of `BEANSTALK:BEANSTALK-ERROR`. There are several such error conditions, for an exhaustive list see the file [package.lisp](package.lisp). 

### Connecting to the queue server and a little example

To start using CL-BEANSTALK, you need to connect to a beanstalk daemon. To do this, it provides functions `BEANSTALK:CONNECT` and `BEANSTALK:DISCONNECT`, and the macro `BEANSTALK:WITH-BEANSTALK-CONNECTION`:

	(beanstalk:with-beanstalk-connection (conn "localhost" 11300)
	  ;; Put jobs into a specific tube:
	  (beanstalk:use conn "URLs-to-fetch")
	  ;; Put two pieces of work into the tube, both at priority 100, 
	  ;; no delay, with 180 seconds maximum running time:
	  (beanstalk:put conn 100 0 180 "http://boinkor.net/")
	  (beanstalk:put conn 100 0 180 "http://planet.lisp.org/"))

### Getting work from the queue server

Workers need to get their units of work from somewhere, and they need to tell the queue server that they have performed (or failed to perform that work). Here's a simple (meaning, untested) worker implementation that fetches URLs from the tube in the last example and handles them in some way:

	(beanstalk:with-beanstalk-connection (conn "localhost" 11300)
		  ;; Get jobs from a specific tube:
		  (beanstalk:watch conn "URLs-to-fetch")
		  (loop 
		    ;; reserve the next job:
		    (multiple-value-bind (url id status) (beanstalk:reserve conn :timeout 1)
	              ;; If no new jobs arrived within 1 second, quit:
	              (when (eql status :timed-out) (return))
	              (handle-response (drakma:http-request url))
	              ;; The job is done, remove it from the queue:
	              (beanstalk:delete conn id))))

Note that this code can run on several worker processes (indeed, worker machines) at once, and perform several thousands units of work in parallel.

For a complete reference, see the [beanstalk protocol documentation](http://github.com/kr/beanstalkd/blob/v1.4.2/doc/protocol.txt?raw=true).

### Conditions and errors

Whenever it makes sense to do so, a function signals a non-error condition to highlight an untypical event: For example, if a job is buried instead of put on the "ready" queue because the server can't reverve enough memory, that results in a condition being signaled via WARN or SIGNAL.

These functions will signal out-of-band information from the queue server:

* `BEANSTALK:PUT` and `BEANSTALK:RELEASE` will signal `beanstalk:buried-job` if the queue server ran out of memory while allocating a bigger priority queue. This (along with the third return value, :buried) indicates that the job was not put on the "ready" queue, but is now buried.
* `BEANSTALK:RESERVE` will signal `beanstalk:deadline-soon` indicating that a job previously reserved in this worker process (a job that was neither deleted nor released) job is within 1 second of its allocated time to run deadline. The worker can choose to extend the deadline by applying `BEANSTALK:TOUCH` to the reserved job ID.