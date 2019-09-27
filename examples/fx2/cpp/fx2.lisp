;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 3.0.12
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.


(in-package fx2)

(cffi:defcfun ("_wrap_cdata" cdata) :pointer
  (ptr :pointer)
  (nelements :int))

(cffi:defcfun ("_wrap_memmove" memmove) :void
  (data :pointer)
  (indata :pointer)
  (inlen :int))

(cffi:defcfun ("_wrap_new_fx2" new_fx2) :pointer)

(cffi:defcfun ("_wrap_fx2_open" _fx2_open) :void
  (self :pointer)
  (vid :int)
  (pid :int)
  (idx :int))

(cffi:defcfun ("_wrap_fx2_set_interface" fx2_set_interface) :void
  (self :pointer)
  (interface :int)
  (alt_setting :int))

(cffi:defcfun ("_wrap_fx2_isopen" fx2_isopen) :pointer
  (self :pointer))

(cffi:defcfun ("_wrap_fx2_close" fx2_close) :void
  (self :pointer))

(cffi:defcfun ("_wrap_fx2_set_debug_level" fx2_set_debug_level) :void
  (self :pointer)
  (n :int))

(cffi:defcfun ("_wrap_fx2_do_usb_command" fx2_do_usb_command) :int
  (self :pointer)
  (buf :string)
  (size :int)
  (type :unsigned-char)
  (request :unsigned-char)
  (value :unsigned-short)
  (index :unsigned-short)
  (length :unsigned-short)
  (timeout :int))

(cffi:defcfun ("_wrap_fx2_clear_halt" fx2_clear_halt) :int
  (self :pointer)
  (ep :char))

(cffi:defcfun ("_wrap_fx2_reset" fx2_reset) :int
  (self :pointer))

(cffi:defcfun ("_wrap_fx2_set_configuration" fx2_set_configuration) :int
  (self :pointer)
  (config :int))

(cffi:defcfun ("_wrap_fx2_ep_bulk" fx2_ep_bulk) :pointer
  (self :pointer)
  (buf :string)
  (size :int)
  (ep :unsigned-char)
  (timeout :int))