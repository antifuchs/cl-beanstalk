(in-package :beanstalk-internal)

(defclass beanstalk-connection ()
     ((socket :initarg :socket :accessor socket-of)
      (stream :initarg :stream :accessor stream-of)
      (name :initarg :name :accessor name-of :initform nil)))

(defmethod print-object ((o beanstalk-connection) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~A" (name-of o))))

;;; Connecting to a beanstalk server:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *debug-protocol* nil))

(defun proto-debug* (datum &rest args)
  (apply #'format *trace-output* datum args))

(defmacro proto-debug (datum &rest args)
  (when *debug-protocol*
    `(proto-debug* ,datum ,@args)))

(defun beanstalk:connect (host port &rest initargs)
  "Connect to a beanstalk daemon running on HOST:PORT."
  (let* ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
         (stream (flexi-streams:make-flexi-stream (usocket:socket-stream socket)
                                                  :external-format (flexi-streams:make-external-format
                                                                    :us-ascii :eol-style :crlf))))
    (apply #'make-instance 'beanstalk-connection :socket socket :stream stream initargs)))

(defun beanstalk:disconnect (connection)
  "Disconnect the beanstalk connection."
  (usocket:socket-close (socket-of connection)))

(defun beanstalk:quit (connection)
  "Alias for BEANSTALK:DISCONNECT."
  (beanstalk:disconnect connection))

(defmacro beanstalk:with-beanstalk-connection ((connection host port &rest initargs) &body body)
  "Open a beanstalk connection to HOST:PORT for as long as control is inside BODY."
  `(let ((,connection (beanstalk:connect ,host ,port ,@initargs)))
     (unwind-protect (progn ,@body)
       (beanstalk:disconnect ,connection))))

;;; Communications:

(defun send-command (connection name &rest args)
  (let ((stream (stream-of connection)))
    (proto-debug "<== ~A~{ ~A~}~%" (string-downcase name) args)
    (format stream "~A~{ ~A~}~%" (string-downcase name) args)
    (force-output stream)))

(defun read-reply (connection)
  (let* ((line (read-line (stream-of connection)))
         (reply (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)))
    (proto-debug "==>(~S) ~S~%" line reply)
    `(,(intern (string-upcase (first reply)) :keyword) ,@(rest reply))))

(defun converse (connection command data-producer &rest args)
  (apply #'send-command connection command args)
  (when data-producer
    (funcall data-producer (stream-of connection)))
  (read-reply connection))

(define-condition beanstalk-error () ())
(define-condition bad-reply ()
  ((reply :initarg :reply :reader reply))
  (:report (lambda (o s)
             (format s "Received unknown reply ~S from server."
                     (reply o)))))
(define-condition beanstalkd-out-of-memory (beanstalk-error) ()
  (:report (lambda (o s)
             (declare (ignore o))
             (format s "Beanstalkd is out of memory - retry later."))))
(define-condition buried-job (warning)
  ((id :initarg :id :reader buried-job-id))
  (:report (lambda (o s)
             (format s "Beanstalkd ran out of memory growing the priority queue: Job #~A buried."
                     (buried-job-id o)))))
(define-condition beanstalkd-draining (beanstalk-error) ()
  (:report (lambda (o s)
             (declare (ignore o))
             (format s "Beanstalkd is draining jobs - try another server, or disconnect and retry."))))
(define-condition beanstalkd-internal-error (beanstalk-error) ()
  (:report (lambda (o s)
             (declare (ignore o))
             (format s "Internal error in the beanstalkd server. Please report at <http://groups.google.com/group/beanstalk-talk>."))))
(define-condition bad-message-format (beanstalk-error) ()
  (:report (lambda (o s)
             (declare (ignore o))
             (format s "Server reports a malformed protocol message: This is a bug in cl-beanstalk. Please report to the authors."))))
(define-condition expected-crlf (bad-message-format) ()
  (:report (lambda (o s)
             (declare (ignore o))
             (format s "Server reports that there is a CRLF missing at the end of data. This is a bug in cl-beanstalk. Please report it to the authors."))))
(define-condition unknown-command (beanstalk-error)
  ((unknown-command :initarg command :reader unknown-command))
  (:report (lambda (o s)
             (format s "Server doesn't understand the command that this client sent (~A). This could indicate a protocol mismatch."
                     (unknown-command o)))))

(define-condition deadline-soon (warning) ()
  (:report (lambda (o s)
             (declare (ignore o))
             (format s "Deadline for a previously reserved job expires in less than 1 second."))))

(defmacro command-reply-case ((reply connection command data-producer &rest args) &body clauses)
  `(catch 'done
     (loop
       (catch 'retry
         (let ((,reply (converse ,connection ,command ,data-producer ,@args)))
           (throw 'done
             (case (first ,reply)
               ,@(loop for clause in clauses
                       collect `(,(first clause) (apply (lambda ,@(rest clause)) (rest ,reply))))
               (:out_of_memory (cerror "Retry" 'beanstalkd-out-of-memory)
                               (throw 'retry nil))
               (:internal_error (error 'beanstalk:beanstalkd-internal-error))
               (:draining (error 'beanstalk:beanstalkd-draining))
               (:bad_format (error 'beanstalk:bad-message-format))
               (:unknown_command (error 'beanstalk:unknown-command :command ,command))
               (otherwise (error 'beanstalk:bad-reply :reply ,reply)))))))))

;;; Communication types:

(defun length-below-200 (string)
  (<= (length string) 200))

(deftype tube-name () (and '(vector base-char *)
                           '(satisfies length-below-200)))
(deftype pri () '(integer 0 4294967295))
(deftype time-spec () '(integer 0))

;;; Protocol functions:

;;; * Producer:

(defun beanstalk:use (connection name)
  (check-type name tube-name)
  (command-reply-case (reply connection :use nil name)
    (:using (tube) tube)))

(defgeneric convert-message-out (message connection external-format)
  (:method ((message string) conn external-format)
    (declare (ignore conn))
    (flexi-streams:string-to-octets message :external-format external-format))
  (:method ((message vector) conn external-format)
    (declare (ignore conn external-format))
    message))

(defun beanstalk:put (connection pri delay ttr message &key (external-format :utf-8))
  (check-type pri pri)
  (check-type delay time-spec)
  (check-type ttr time-spec)
  (let ((message (convert-message-out message connection external-format)))
   (labels ((write-message (stream)
              (proto-debug "~A~%" message)
              (write-sequence message stream)
              (terpri stream)
              (force-output stream)))
     (command-reply-case (reply connection :put #'write-message pri delay ttr (length message))
       (:inserted (id) (values (parse-integer id) :inserted))
       (:buried (id)
         (let ((id (parse-integer id)))
           (warn 'beanstalk:buried-job :id id)
           (values id :buried)))
       (:expected_crlf () (error 'beanstalk:expected-crlf))))))

;;; * Worker:

(defun read-data-reply (bytes-as-string stream external-format)
  (let* ((bytes (parse-integer bytes-as-string))
         (message (make-array bytes :element-type '(unsigned-byte 8) :initial-element 0)))
    (read-sequence message stream)
    ;; read the terminating \r\n:
    (read-line stream)
    (flexi-streams:octets-to-string message :external-format external-format)))

(defun beanstalk:reserve (connection &key timeout (external-format :utf-8))
  (macrolet ((reservoid (command &rest args)
               `(command-reply-case (reply connection ,command nil ,@args)
                  (:deadline_soon ()
                    (warn 'beanstalk:deadline-soon)
                    (values nil nil :deadline-soon))
                  (:timed_out ()
                    (values nil nil :timed-out))
                  (:reserved (id bytes)
                    (values (read-data-reply bytes (stream-of connection) external-format) (parse-integer id)
                            :reserved)))))
    (if timeout
        (reservoid :reserve-with-timeout timeout)
        (reservoid :reserve))))

(defun beanstalk:touch (connection id)
  (command-reply-case (reply connection :touch nil id)
    (:not_found () nil)
    (:touched () t)))

(defun beanstalk:delete (connection id) ; *
  (command-reply-case (reply connection :delete nil id)
    (:not_found () nil)
    (:deleted () t)))

(defun beanstalk:release (connection id pri delay)
  (check-type pri pri)
  (check-type delay time-spec)
  (command-reply-case (reply connection :release nil id pri delay)
    (:not_found () (values nil :not-found))
    (:buried ()
      (warn 'beanstalk:buried-job :id id)
      (values nil :buried))
    (:released () t)))

(defun beanstalk:bury (connection id pri)
  (check-type pri pri)
  (command-reply-case (reply connection :bury nil id pri)
    (:not_found () (values nil :not-found))
    (:buried () (values t :buried))))

(defun beanstalk:watch (connection tube)
  (check-type tube tube-name)
  (command-reply-case (reply connection :watch nil tube)
    (:watching (count) (parse-integer count))))

(defun beanstalk:ignore (connection tube)    ; *
  (check-type tube tube-name)
  (command-reply-case (reply connection :ignore nil tube)
    (:watching (count) (parse-integer count))
    (:not_ignored () nil)))

;;; * Other commands:

(macrolet ((define-peeker (name &rest args)
               `(defun ,name (connection ,@args &key (external-format :utf-8))
                  (command-reply-case (reply connection (string ',name) nil ,@args)
                    (:not_found () (values nil nil :not-found))
                    (:found (id bytes)
                            (values (read-data-reply bytes (stream-of connection) external-format)
                                    (parse-integer id)
                                    :found))))))
  (define-peeker beanstalk:peek id)
  (define-peeker beanstalk:peek-ready)
  (define-peeker beanstalk:peek-delayed)
  (define-peeker beanstalk:peek-buried))

(defun beanstalk:kick (connection bound)
  (command-reply-case (reply connection :kick nil bound)
    (:kicked (count) (parse-integer count))))

(defun beanstalk:list-tube-used (connection)
  (command-reply-case (reply connection :list-tube-used nil)
    (:using (tube) tube)))

;;; TODO: Implement "Other commands" from http://github.com/kr/beanstalkd/blob/v1.1/doc/protocol.txt?raw=true
;;; These require an at least quarter-assed YAML implementation. I'm just not in the mood for this right now.
;; stats-job
;; stats-tube
;; stats
;; list-tubes
;; list-tube-used
;; list-tubes-watched