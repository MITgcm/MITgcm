C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_copydate.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_CopyDate(
     I                         indate,
     O                         outdate,
     I                         mythid
     &                       )

c     ==================================================================
c     SUBROUTINE cal_CopyDate
c     ==================================================================
c
c     o Copy a date array or a time interval array to another date array
c       or time interval array.
c              
c     started: Christian Eckert eckert@mit.edu  03-Jan-1999
c
c              - Introduced to calendar version 0.1.1
c
c     changed: Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c     ==================================================================
c     SUBROUTINE cal_CopyDate
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer indate(4)
      integer outdate(4)
      integer mythid

c     == local variables ==

c     == end of interface ==

      outdate(1) = indate(1)
      outdate(2) = indate(2)
      outdate(3) = indate(3)
      outdate(4) = indate(4)

      return
      end
