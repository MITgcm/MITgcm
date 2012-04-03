C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_stepsperday.F,v 1.3 2012/04/03 15:20:29 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_StepsPerDay(
     I                            myiter,
     I                            mytime,
     O                            numsteps,
     I                            mythid
     &                          )

c     ==================================================================
c     SUBROUTINE cal_StepsPerDay
c     ==================================================================
c
c     o Given the iteration number and/or the current time of
c       integration return the number of steps left for the current
c       calendar day including the step belonging to iter/currenttime.
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
c     SUBROUTINE cal_StepsPerDay
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer numsteps
      integer mythid

c     == local variables ==

      integer currentdate(4)
      integer present
      integer presentsecs
      integer final
      integer finalsecs
      integer ierr
      integer modelstepsperday

c     == end of interface ==

      call cal_GetDate( myiter, mytime, currentdate, mythid )

      present     = currentdate(1)
      presentsecs = (currentdate(2)/10000)*secondsperhour +
     &              mod(currentdate(2)/100,100)*secondsperminute +
     &              mod(currentdate(2),100)
      final       = modelenddate(1)

      if (present .lt. final) then
C       modelstepsperday = cal_nStepDay(mythid)
C-jmc: inline Function "cal_nStepDay":
        modelstepsperday = secondsperday/nint(modelstep)
        numsteps = modelstepsperday -
     &             int(presentsecs/modelstep)
      else if (present .eq. final) then
        finalsecs   = (modelenddate(2)/10000)*secondsperhour +
     &                mod(modelenddate(2)/100,100)*secondsperminute +
     &                mod(modelenddate(2),100)
        numsteps    = int((finalsecs - presentsecs)/modelstep)
      else

        ierr = 1101
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_StepsPerDay.'

      endif

      return
      end
