#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"
C     !INTERFACE:
      SUBROUTINE ATM2OCN_MAIN( iloop, aloop, inMonth,
     &                                 myIter, myThid )
C     *==========================================================*
C     | Main call after atmos call; calls routine to convert/get |
C     | the atmos fluxes, do ther derivative term calculation,   |
C     | then add restoring and fixed fluxes, if any.             |
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmos/Ocean/Seaice Interface Variables ===
#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     iloop - loop counter for main loop (coupled periods)
C     aloop - loop counter for atm time steps (within a coupled per.)
C     inMonth - current month
C     myThid - Thread no. that called this routine.
      INTEGER iloop    
      INTEGER aloop
      INTEGER inMonth
      INTEGER myIter
      INTEGER myThid

C     LOCAL VARIABLES:
      _RL wght0,wght1
      INTEGER intime0,intime1
      LOGICAL iftime ! comes back true if time to do a file re-load
      _RL  curTime   ! current time (s) of simulation starting from 0.0

      curTime = float(iloop-1)*dtcouplo + (float(aloop)-0.5)*dtatmo

      PRINT *,'Executing atm2ocn main',curTime
      CALL CALC_FILELOAD( curTime, iloop, aloop, wght0, wght1,
     &                    intime0, intime1, ifTime, myThid)

      CALL READ_ATMOS( inMonth,myThid)

      IF ( (atmosTauuFile.NE.' ').OR.
     &   (atmosTauvFile.NE.' ').OR.(atmosWindFile.NE.' ')) THEN     
        CALL SUBTRACT_MEANS( wght0, wght1,
     &                       intime0, intime1, myThid)
      ENDIF

      CALL CALC_1DTO2D( myThid)

      CALL FIXED_FLUX_ADD( wght0,wght1,
     &              intime0,intime1,ifTime,myIter,myThid)

      CALL RELAX_ADD( wght0,wght1,
     &              intime0,intime1,ifTime,myIter,myThid)
 
      RETURN
      END
