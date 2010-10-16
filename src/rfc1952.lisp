;;;
;;; Copyright (C) 2009-2010 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-gzip.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :uk.co.deoxybyte-gzip)

;;; Gzip member format constants from RFC1952.
(defconstant +id1+ #x1f "ID1 (IDentification 1)")
(defconstant +id2+ #x8b "ID2 (IDentification 2)")

(defconstant +cm-deflate+ 8 "CM (Compression Method)")

(defconstant +flag-text+ 0 "FLG (FLaGs) FTEXT")
(defconstant +flag-fhcrc+ (ash 1 1) "FLG (FLaGs) FHCRC")
(defconstant +flag-extra+ (ash 1 2) "FLG (FLaGs) FEXTRA")
(defconstant +flag-name+ (ash 1 3) "FLG (FLaGs) FNAME")
(defconstant +flag-comment+ (ash 1 4) "FLG (FLaGs) FCOMMENT")

(defconstant +xfl-slowest+ 2
  "XFL (eXtra FLags) compressor used maximum compression, slowest algorithm")
(defconstant +xfl-fastest+ 4
  "XFL (eXtra FLags) compressor used fastest algorithm")

(defconstant +os-fat-filesystem+ 0)
(defconstant +os-amiga+ 1)
(defconstant +os-vms+ 2)
(defconstant +os-unix+ 3)
(defconstant +os-vm/cms+ 4)
(defconstant +os-atari-tos+ 5)
(defconstant +os-hpfs-filesystem+ 6)
(defconstant +os-macintosh+ 7)
(defconstant +os-z-system+ 8)
(defconstant +os-cp/m+ 9)
(defconstant +os-tops-20+ 10)
(defconstant +os-ntfs-filesystem+ 11)
(defconstant +os-qdos+ 12)
(defconstant +os-acorn-riscos+ 13)
(defconstant +os-unknown+ 255)

(defstruct gz-member
  "A gzip member as defined by RFC1952.

- id1: IDentification 1.
- id2: IDentification 2.
- cm: Compression Method.
- flg: FLaGs.
- mtime: Modification TIME.
- xfl: eXtra FLags.
- os: Operating System.
- xlen: eXtra LENgth.
- isize: Input SIZE.
- crc32: CRC-32.
- cdata: Compressed DATA.
- cend: Compressed data END. The length of compressed data in cdata,
  starting at index 0, if cdata is only partially filled."
  (id1 +id1+ :type uint8 :read-only t)
  (id2 +id2+ :type uint8 :read-only t)
  (cm +cm-deflate+ :type uint8 :read-only t)
  (flg +flag-extra+ :type uint8)
  (mtime 0 :type uint32)
  (xfl 0 :type uint8)
  (os +os-unknown+ :type uint8)
  (xlen 0 :type uint16)
  (isize 0 :type uint32)
  (crc32 0 :type uint32)
  (cdata (make-array 0 :element-type 'octet) :type simple-octet-vector)
  (cend 0 :type uint32))
