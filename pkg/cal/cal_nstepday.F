C $Header: /u/gcmpack/MITgcm/pkg/cal/Attic/cal_nstepday.F,v 1.1 2001/05/14 22:07:26 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      integer function cal_nStepDay(
     I                               mythid
     &                             )

c     ==================================================================
c     FUNCTION cal_nStepDay
c     ==================================================================
c
c     o Return the number of time steps that can be performed during one
c       calendar day.
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
c     FUNCTION cal_nStepDay
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

c     == end of interface ==

c     See the remarks on assumptions in the documentation for this piece
c     of code.
      cal_nStepDay = secondsperday/nint(modelstep)

      return
      end
