C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_intyears.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      integer function cal_IntYears(
     I                               mythid
     &                             )

c     ==================================================================
c     FUNCTION cal_IntYears
c     ==================================================================
c
c     o Return the number of calendar years that are affected by the 
c       current model integration.
c
c              
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
c
c              - restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c     ==================================================================
c     FUNCTION cal_IntYears
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer mythid

c     == end of interface ==

      cal_IntYears = ( modelenddate(1)  /10000 -
     &             modelstartdate(1)/10000   ) + 1

      if ( ( modelenddate(2) .eq. 0) .and.
     &     ( mod(modelenddate(1),100) .eq. 1 ) .and.
     &       mod(modelenddate(1)/100,100) .eq. 1 ) then
        cal_IntYears = cal_IntYears -1
      endif

      return
      end
