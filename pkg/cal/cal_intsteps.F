C $Header: /u/gcmpack/MITgcm/pkg/cal/Attic/cal_intsteps.F,v 1.1 2001/05/14 22:07:26 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      integer function cal_IntSteps(
     I                               mythid
     &                             )

c     ==================================================================
c     FUNCTION cal_IntSteps
c     ==================================================================
c
c     o Return the number of model timesteps are to be made by the 
c       current model integration.
c
c              
c     started: Christian Eckert eckert@mit.edu  01-May-2000
c
c     changed: 
c
c     ==================================================================
c     FUNCTION cal_IntSteps
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

c     == end of interface ==

      cal_IntSteps = modelintsteps

      return
      end
