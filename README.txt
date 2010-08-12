Introduction

This system provides gzip and gunzip functions and a Gray-streams
implementation, both built on a set of lower-level zlib functions.

Functions are provided for inflating and deflating to and from Lisp
octet vectors and Lisp octet streams, which may be tuned using the
Zlib tuning parameters described in the Zlib C function deflateInit2.

In addition, a basic implementation of the data structure described in
RFC1952 is included, allowing a hybrid approach to reading gzip data,
using native Lisp streams and Zlib inflate/deflate.

Measuring execution times of the gz:gzip and gz:gunzip functions shows
deoxybyte-gzip to be fractionally slower than gzip, but over 3x slower
than gunzip.

Relative execution time (SBCL 1.0.40 X86-64):

                  gzip   deoxybyte-gzip     gzip-stream
  Compression      1.0             1.1             1.1
Decompression      1.0             3.1             3.5

Timings were taken compressing and decompressing a 1.8 Gb text file
using gzip/gunzip command line programs, the gz:gzip/gz:gunzip
functions (which use an internal buffer of length 2^16 -1) and the
Gray streams classes using
gz:stream-read-sequence/gz:stream-write-sequence methods. A zlib
compression level of 6 was used.

This system is named with a deoxybyte- prefix because there are
several existing Common Lisp packages using the gzip name and I don't
want to clash with them.


Installation

deoxybyte-gzip uses ASDF for system definition. Copy or symlink
deoxybyte-gzip.asd (and optionally deoxybyte-gzip-test.asd) to your
asdf:*central-registry* and load deoxybyte-io with the asdf:operate
function:

 (asdf:operate 'asdf:load-op :deoxybyte-gzip)

or with the equivalent deoxybyte-systems:load-system function:

 (dxs:load-system :deoxybyte-gzip)


Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests with the asdf:operate function:

 (asdf:operate 'asdf:test-op :deoxybyte-gzip)

or with the equivalent deoxybyte-systems:test-system function:

 (dxs:test-system :deoxybyte-gzip)


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

 (dxs:document-system :deoxybyte-gzip)

at the REPL, provided that CLDOC is installed.


Dependencies

deoxybyte-systems       git://github.com/keithj/deoxybyte-systems.git
deoxybyte-io            git://github.com/keithj/deoxybyte-io.git


Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
