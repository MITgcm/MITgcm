C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_do_inphys.F,v 1.2 2001/02/02 21:36:29 adcroft Exp $
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
C     J, K - Loop counters
      REAL FSG( Nr)
      REAL HSG( Nr+1)
      REAL RLAT(sNy)
      INTEGER J, K
      INTEGER Katm

      pground = 1. _d 5
      DO K=1,Nr
       Katm = _KD2KA( K )
       FSG(Katm  ) = rC(K)/pGround
       HSG(Katm+1) = rF(K)/pGround
      ENDDO
      HSG(1) = rF(Nr+1)/pGround

      DO J=1,sNy
       RLAT(J) = yC(1,J,1,1)*deg2rad
      ENDDO

      CALL INPHYS( FSG, HSG, RLAT )

#ifdef INCLUDE_DIAGNOSTICS_INTERFACE_CODE 
C     Initialise diagnostic counters ( these are cleared on model starti i.e. not
C     loaded from history file for now ).
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVER_INI_XY(USTRtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(VSTRtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(TSRtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(OLRtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(SSRtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(SLRtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(SHFtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(EVAPtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(PRECNVtave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(PRECLStave, bi, bj, myThid)
        CALL TIMEAVER_INI_XY(CLOUDCtave, bi, bj, myThid)
        AIM_TimeAve(1,bi,bj) = 0.
       ENDDO
      ENDDO
#endif /* INCLUDE_DIAGNOSTICS_INTERFACE_CODE */

#endif /* ALLOW_AIM */

      RETURN
      END
