C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_do_inphys.F,v 1.5 2001/05/29 19:28:53 cnh Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE AIM_INIT( myThid )
C     /==================================================================\
C     | S/R AIM_INIT                                                     |
C     |==================================================================|
C     | Interface between AIM atmospheric physics package and the        |
C     | dynamical model for initialisation.                              |
C     \==================================================================/
      IMPLICIT rEAL*8 (A-H,O-Z)

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
      REAL FSG( Nr)
      REAL HSG( Nr+1)
      REAL RLAT(sNy)
      INTEGER J, K
      INTEGER Katm
      INTEGER bi,bj

      pground = 1. _d 5
      DO K=1,Nr
       Katm = _KD2KA( K )
       FSG(Katm  ) = rC(K)/pGround
       HSG(Katm+1) = rF(K)/pGround
      ENDDO
      HSG(1) = rF(Nr+1)/pGround

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         RLAT(J) = yC(1,J,bi,bj)*deg2rad
        ENDDO
        CALL INPHYS( FSG, HSG, RLAT, myThid )
       ENDDO
      ENDDO

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
