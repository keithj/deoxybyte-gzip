Introduction

I needed a small set of zlib functions and the currently available
pure Common Lisp ones that I know of are incomplete (with respect to
reading and writing) or too slow.

In time I will build a Gray-streams implementation on top of this
low-level code.

Performance decompressing a 1.4Gb text file, compared to gunzip:

(time (gunzip "test.fasta.gz" "out.fasta"))
Evaluation took:
  29.672 seconds of real time
  28.261766 seconds of total run time (25.101568 user, 3.160198 system)
  95.25% CPU
  71,021,005,209 processor cycles
  3,134,368 bytes consed

time gunzip -c test.fasta.gz > out.fasta
real    0m29.027s
user    0m9.325s
sys     0m2.992s


No particular optimization has been done yet. This code has only been
tested on the REPL. This system is named with a deoxybyte- prefix
because there are several existing Common Lisp packages using the gzip
name and I don't want to clash with them.


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
deoxybyte-unix          git://github.com/keithj/deoxybyte-unix.git


Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
