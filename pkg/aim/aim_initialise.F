C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_initialise.F,v 1.1 2002/10/09 01:01:24 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE AIM_INITIALISE( myThid )
C     *==================================================================*
C     | S/R AIM_INITIALISE
C     *==================================================================*
C     | Initialisation of AIM atmospheric physics package :
C     | 1) call iniphys (=> set parameters to default value)
C     | 2) read AIM parameters 
C     | 3) initialisation of AIM arrays (time-ave)
C     *==================================================================*
      IMPLICIT NONE

C     -------------- Global variables ------------------------------------
#include "SIZE.h" 
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "AIM_DIAGS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance
      INTEGER myThid

#ifdef ALLOW_AIM
C     == Local variables ==
C     FSG  - Cell mid-point in vertical
C     HSG  - Cell face in vertical
C     RLAT - Call mid-point latitude
C     pGround - Lower boundary pressure
C     J, K, bi,bj  - Loop counters
      _RL FSG( Nr)
      _RL HSG(0:Nr)
      _RL RLAT(sNy)
      _RL pGround
      INTEGER J, K
      INTEGER Katm
      INTEGER bi,bj

      pground = 1. _d 5
      DO K=1,Nr
       Katm = _KD2KA( K )
       FSG(Katm) = rC(K)/pGround
       HSG(Katm) = rF(K)/pGround
      ENDDO
       K=Nr+1
       Katm = _KD2KA( K )
       HSG(Katm) = rF(K)/pGround

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         RLAT(J) = yC(1,J,bi,bj)*deg2rad
        ENDDO
        CALL INPHYS( FSG, HSG, RLAT, myThid )
       ENDDO
      ENDDO

      CALL AIM_READ_PHYSPARMS( myThid )

#ifdef ALLOW_TIMEAVE 
C     Initialise diagnostic counters ( these are cleared on model starti i.e. not
C     loaded from history file for now ).
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_RESET(USTRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(VSTRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(TSRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(OLRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(SSRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(SLRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(SHFtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(EVAPtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(PRECNVtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(PRECLStave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(CLOUDCtave, 1, bi, bj, myThid)
        AIM_TimeAve(1,bi,bj) = 0.
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_AIM */

      RETURN
      END
