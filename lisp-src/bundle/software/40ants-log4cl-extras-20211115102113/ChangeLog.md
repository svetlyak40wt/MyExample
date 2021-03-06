<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E6-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.0 (2021-10-03)

* Now `:PLAIN` and `:JSON` logger will output logger's category, filename and a callable name.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E5-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.1 (2021-03-02)

* Fixed fail during logging error with `(setf some-func)` in the backtrace.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E5-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.0 (2021-01-24)

* Function `TRACEBACK-TO-STRING` was removed and
  replaced with [`log4cl-extras/error:print-backtrace`][6a57] which is now
  a part of public `API`.

* Added ability to filter secret and sensitive values.
  Read documentation, to lear more.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E4-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.2 (2020-11-26)

<a id="fixed"></a>

### Fixed

* Fixed [`with-log-unhandled`][3fd6] for cases when some function argument's print-object signaled the error.

Because of this nasty error, sometimes [`with-log-unhandled`][3fd6] didn't log "Unandled error".

<a id="added"></a>

### Added

* Now [`log4cl-extras/error:with-log-unhandled`][3fd6] macro accepts key argument `DEPTH` which is 10 by default.

This argument can be overriden by setting [`log4cl-extras/error:*max-traceback-depth*`][c93b].

* Also another variable [`log4cl-extras/error:*max-call-length*`][6d41] can be set to control
  how long function or method name can be. By default it is 100, but methods are logged along
  with their specialized arguments and can be longer.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E4-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.1 (2019-03-05)

<a id="fixed"></a>

### Fixed

* Added missing dependency from `CL-STRINGS` system.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0 (2019-03-04)

<a id="improved"></a>

### Improved

Now [`log4cl-extras/config:setup`][74de] sets appender into a mode when it prints log in a human
readable way if it its called from the `SLY`'s `REPL`. All logger fields are
printed as well, including a traceback.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0 (2019-01-07)

<a id="improved"></a>

### Improved

* Now condition's description is added to the end of the backtrace.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E2-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.2 (2018-12-08)

<a id="fixed"></a>

### Fixed

* Fixed system's loading in environments with `C` locale.

  This closes issue reported along with pull request #1.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E2-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.1 (2018-11-24)

<a id="fixed"></a>

### Fixed

* Previously, macros [`log4cl-extras/error:with-log-unhandled`][3fd6] catched every signal,
  not only signals derived from `ERROR`. Because of that,
  it logged traceback for non error signals like that:

`lisp
  (log4cl-json/error:with-log-unhandled ()
      (signal "foo"))
`

  Now this bad behavior was fixed and only `errors` are logged.

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2017-08-29)

<a id="new"></a>

### New

* Added ability to log tracebacks using [`log4cl-extras/error:with-log-unhandled`][3fd6].

<a id="x-28LOG4CL-EXTRAS-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0 (2017-01-23)

* Initial version.


[74de]: https://40ants.com/log4cl-extras/#x-28LOG4CL-EXTRAS-2FCONFIG-3ASETUP-20FUNCTION-29
[6d41]: https://40ants.com/log4cl-extras/#x-28LOG4CL-EXTRAS-2FERROR-3A-2AMAX-CALL-LENGTH-2A-20-28VARIABLE-29-29
[c93b]: https://40ants.com/log4cl-extras/#x-28LOG4CL-EXTRAS-2FERROR-3A-2AMAX-TRACEBACK-DEPTH-2A-20-28VARIABLE-29-29
[6a57]: https://40ants.com/log4cl-extras/#x-28LOG4CL-EXTRAS-2FERROR-3APRINT-BACKTRACE-20FUNCTION-29
[3fd6]: https://40ants.com/log4cl-extras/#x-28LOG4CL-EXTRAS-2FERROR-3AWITH-LOG-UNHANDLED-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
