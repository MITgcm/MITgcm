#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"
C
      SUBROUTINE CALC_FILELOAD( curTime, iloop, aloop,
     &                        wght0, wght1,
     &                        intime0, intime1,
     &                        ifTime, myThid)

C     *==========================================================*
C     | Determines weights to be used for external files.        |
C     *==========================================================*

      IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     curTime - Simulation time (s) starting from year 0
C     iloop - loop counter for main loop (coupled periods)
C     aloop - loop counter for atm time steps (within a coupled per.)
C     wght0, wght1 - weights of the two months to average
C     intime0 - first of the two month to average
C     intime1 - latter of the two months to average
C     iftime - return true if time to do a file re-load
C     myThid - Thread no. that called this routine.
      _RL  curTime   
      INTEGER iloop
      INTEGER aloop
      _RL  wght0
      _RL  wght1
      INTEGER intime0
      INTEGER intime1
      LOGICAL ifTime
      INTEGER myThid


C     !LOCAL VARIABLES

      _RL  dmonth(0:13)
      DATA dmonth /-1339200.D0, 1339200.D0, 3888000.D0, 6436800.D0,
     &          9072000.D0, 11707200.D0, 14342400.D0, 16977600.D0,
     &         19656000.D0, 22291200.D0, 24926400.D0, 27561600.D0,
     &         30196800.D0, 32875200.D0/
C               /-15.5D0*86400.D0, 15.5D0*86400.D0, 45.0D0*86400.D0,
C     &           74.5D0*86400.D0, 105.0D0*86400.D0, 135.5D0*86400.D0,
C     &          166.0D0*86400.D0, 196.5D0*86400.D0, 227.5D0*86400.D0,
C     &          258.0D0*86400.D0, 288.5D0*86400.D0, 319.0D0*86400.D0,
C     &          349.5D0*86400.D0, 380.5D0*86400.D0/
      _RL secYr
      DATA secYr /31536000.D0/   !ignore externForcingCycle from data
      _RL  ifcyc     ! time in current year, in seconds
      INTEGER mn     ! loop counter

      iftime=.FALSE.
      ifcyc=mod(curTime,secYr)
  
      DO mn=1,13

        IF ((ifcyc.GT.dmonth(mn-1)).AND.(ifcyc.LE.dmonth(mn))) 
     &      intime1=mn

      ENDDO

C     hence if curTime falls exactly mid-month, it is dmonth(intime1)
C     although this should never happen if passed curTime is a mid-step time
    
      intime0=intime1-1
      wght1=(ifcyc - dmonth(intime0)) / 
     &     (dmonth(intime1) - dmonth(intime0))
      wght0= 1. _d 0-wght1

      IF (ifcyc.LE.dmonth(intime0) + dtatmo) ifTime=.TRUE.
      IF ((iloop.EQ.1).AND.(aloop.EQ.1)) ifTime= .TRUE.

      IF (intime0.EQ.0) intime0=12
      IF (intime1.EQ.13) intime1=1
      
      RETURN
      END
