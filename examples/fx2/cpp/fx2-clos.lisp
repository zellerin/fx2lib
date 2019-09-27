(defpackage fx2
  (:use #:cl)
  (:shadow cl:open cl:close))

(load "fx2.lisp")

(in-package fx2)

(defvar *vid* #x04b4)
(defvar *pid* #x8613)

(defclass fx2()
  ((ff-pointer :reader ff-pointer)))

(defmethod initialize-instance :after ((obj fx2) &key)
  (setf (slot-value obj 'ff-pointer) (new_fx2)))

(defun open (self &key (vid) (pid) (idx))
  (cond ((and pid idx vid)
	 (fx2_open_0 (ff-pointer self) vid pid idx))
	((and pid vid)
	 (fx2_open_1 (ff-pointer self) vid pid))
	(vid
	 (fx2_open_2 (ff-pointer self) vid))
	(t
	 (fx2_open_3 (ff-pointer self)))))

(defmethod set-interface ((self fx2) (interface cl:integer) (alt_setting cl:integer))
  (fx2_set_interface (ff-pointer self) interface alt_setting))

(defmethod isopen ((self fx2))
  (fx2_isopen (ff-pointer self)))

(defmethod close ((self fx2))
  (fx2_close (ff-pointer self)))

(defmethod set-debug-level ((self fx2) (n cl:integer))
  (fx2_set_debug_level (ff-pointer self) n))

(defun do-usb-command (self buf size type request
			  value
			  index length
		       &key (timeout 1000))
  (if timeout
    (fx2_do_usb_command_0 (ff-pointer self) buf size type request value index length timeout)))

(defmethod clear-halt ((self fx2) (ep cl:character))
  (fx2_clear_halt (ff-pointer self) ep))

(defmethod reset ((self fx2))
  (fx2_reset (ff-pointer self)))

(defmethod set-configuration ((self fx2) (config cl:integer))
  (fx2_set_configuration (ff-pointer self) config))

(defun ep-bulk (self buf size
		ep
		timeout)
  (let ((foreign (cffi:foreign-alloc :unsigned-char :initial-contents buf)))
    (fx2_ep_bulk (ff-pointer self)
		 foreign size ep timeout)
    (prog1 (loop for i from 0 below size
		 collect (cffi:mem-ref foreign :char i))
      (cffi:foreign-free foreign))))



(defvar *default-fx2*)

(defun write-ram (addr data &key (length (length data)) (fx2 *default-fx2*))
  (loop with transferred = 0
	while (< transferred length)
	for chunk-size = (min 1024 (- length transferred))
	for buf = (subseq data transferred (+ transferred chunk-size))
	for buffer = (cffi:foreign-alloc
		      :unsigned-char :initial-contents buf)
	for ret = (do-usb-command fx2 buffer (length buf) #x40 #xa0 (+ addr transferred) 0 chunk-size)
	do (cffi:foreign-free buffer)
	if (plusp ret)
	  do
	     (signal "Sent ~d bytes" ret)
	     (incf transferred ret)
	else
	  do (error "Return size: ~d" ret)))

(defun reset-device (reset)
  "Put device in reset or to run"
  (signal (if reset "Put device to reset" "Set device to run"))
  (write-ram #xe600 (if reset  #(1) #(0))))

(defun reset-bix (path)
  "Use this function to reset your firmware.  You'll need to reopen the device afterward."
  (reset-device t)
  (with-open-file (f path :element-type '(unsigned-byte 8))
    (let* ((l (file-length f))
	   (data (make-array l :element-type '(unsigned-byte 8))))
      (assert (= l (read-sequence data f)))
      (signal "Loading bix files, size ~d" l)
      (write-ram 0 data)
      (reset-device nil))))

(defmacro with-open-fx (pars &body body)
  `(let* ((*default-fx2* (make-instance 'fx2::fx2)))
     (fx2::open *default-fx2* ,@pars)
     (unwind-protect
	  (progn
	    ,@body)
       (fx2::close *default-fx2*))))

(defun do-bulk ()
  "Test bulk transfer example."
  (with-open-fx (:pid  #x1004 :vid #x4b4)
    (let ((buff #(1 2 3 4 5 6 7 8))
	  (ret (make-array 10 :element-type 'unsigned-byte)))
      (print (ep-bulk *default-fx2* buff (length buff) #x02 1000))
      (print (ep-bulk *default-fx2* ret (length ret) #x86 1000))
      ret)))

#+example (fx2::with-open-fx (:pid  #x8613 :vid #x4b4) (fx2::reset-bix "/opt/compressed/git/fx2lib/examples/bulkloop/build/bulkloop.bix"))
