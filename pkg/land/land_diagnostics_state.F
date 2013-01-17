C $Header: /u/gcmpack/MITgcm/pkg/land/land_diagnostics_state.F,v 1.2 2013/01/17 23:09:49 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_DIAGNOSTICS_STATE
C     !INTERFACE:
      SUBROUTINE LAND_DIAGNOSTICS_STATE(
     I                            myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_DIAGNOSTICS_STATE
C     | o fill-in diagnostics array for LAND state variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Land package :
#include "LAND_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: my Thread number Id
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     == Local variables ==
C     bi,bj    :: Tile indices
c     INTEGER bi, bj
c     INTEGER i,j
c     _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     LOGICAL  DIAGNOSTICS_IS_ON
c     EXTERNAL DIAGNOSTICS_IS_ON

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( useDiagnostics ) THEN

        CALL DIAGNOSTICS_FILL(land_skinT,'GrdSurfT',0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(land_groundT,'GrdTemp ',0,land_nLev,
     &                                                  0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(land_enthalp,'GrdEnth ',0,land_nLev,
     &                                                  0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(land_groundW,'GrdWater',0,land_nLev,
     &                                                  0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(land_hSnow,'LdSnowH ',0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(land_snowAge,'LdSnwAge',0,1,
     &                                                  0,1,1,myThid)

      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
