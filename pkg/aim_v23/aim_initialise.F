C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_initialise.F,v 1.1 2002/11/22 17:17:03 jmc Exp $
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
C     HSG  - Cell face in vertical
C     pGround - Lower boundary pressure
C     I, J, K, bi,bj  - Loop counters
      _RL HSG(0:Nr)
      _RL pGround
      INTEGER K, bi, bj
      INTEGER Katm

      pGround = atm_po
      DO K=1,Nr
       Katm = _KD2KA( K )
       HSG(Katm) = rF(K)/pGround
      ENDDO
       K=Nr+1
       Katm = _KD2KA( K )
       HSG(Katm) = rF(K)/pGround

c     DO bj = myByLo(myThid), myByHi(myThid)
c      DO bi = myBxLo(myThid), myBxHi(myThid)

C--   set default value for all atmos. physics parameter:
        CALL INPHYS( HSG, myThid )

c      ENDDO
c     ENDDO

C--   set energy fractions in LW bands as a function of temperature:
C     initialize common block RADFIX (originally called from FORDATE in SPEEDY)
      _BEGIN_MASTER(myThid)  
       CALL RADSET( myThid)
      _END_MASTER ( myThid)  

      CALL AIM_READPARMS( myThid )

#ifdef ALLOW_TIMEAVE 
C     Initialise diagnostic counters (these are cleared on model start
C      i.e. not loaded from history file for now ).
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
