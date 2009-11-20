Introduction

This system provides gzip and gunzip functions and a Gray-streams
implementation, both built on a set of lower-level zlib functions.

Measuring execution times of the gz:gzip and gz:gunzip functions shows
deozybyte-gzip to be fractionally slower than GNU gzip/gunzip, but not
significantly so.

Relative execution time:

                  gzip   deozybyte-gzip
  Compression      1.0             1.05
Decompression      1.0             1.03

Timings were taken compressing and decompressing a 1.4 Gb text file
using GNU gzip/gunzip command line programs and the gz:gzip/gz:gunzip
functions (which use an internal buffer of length 2^16 -1).

The Gray-streams implementation is currently considerably slower
because it does not yet contain any optimized code for the more
efficient sequence types (e.g. (simple-array (unsigned-byte 8))).

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
