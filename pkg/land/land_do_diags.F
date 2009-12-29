C $Header: /u/gcmpack/MITgcm/pkg/land/land_do_diags.F,v 1.10 2009/12/29 23:05:52 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_DO_DIAGS
C     !INTERFACE:
      SUBROUTINE LAND_DO_DIAGS(
     I                land_frc, bi,bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_DO_DIAGS
C     | o Calculate Land diagnostics
C     *==========================================================*
C     | note: snap-shot output have been moved to a better place
C     |       and are now written from LAND_OUTPUT
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
#include "LAND_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     bi,bj    :: Tile index
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: Number of this instance of the routine
      _RS land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj, myIter, myThid
      _RL     myTime
CEOP

#ifdef ALLOW_LAND

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Local Variables ==

#ifdef ALLOW_LAND_TAVE

      IF (land_taveFreq.GT.0.) THEN

C--   Sum ground Temp, soil moisture and Run-Off:
        CALL TIMEAVE_CUMULATE(land_grTtave,land_groundT,land_nLev,
     &                        land_deltaT, bi, bj, myThid )
        CALL TIMEAVE_CUMULATE(land_entave, land_enthalp,land_nLev,
     &                        land_deltaT, bi, bj, myThid )
        CALL TIMEAVE_CUMULATE(land_grWtave,land_groundW,land_nLev,
     &                        land_deltaT, bi, bj, myThid )
        CALL TIMEAVE_CUMULATE(land_sTtave, land_skinT ,1,
     &                        land_deltaT, bi, bj, myThid )
        CALL TIMEAVE_CUMULATE(land_hStave, land_hSnow ,1,
     &                        land_deltaT, bi, bj, myThid )
        CALL TIMEAVE_CUMULATE(land_sAtave, land_snowAge,1,
     &                        land_deltaT, bi, bj, myThid )
        CALL TIMEAVE_CUMULATE(land_ROftave,land_runOff,1,
     &                        land_deltaT, bi, bj, myThid )
        CALL TIMEAVE_CUMULATE(land_eROtave,land_enRnOf,1,
     &                        land_deltaT, bi, bj, myThid )

C-    Keep record of how much time has been integrated over
        land_timeAve(bi,bj) = land_timeAve(bi,bj)+land_deltaT

      ENDIF

#endif /* ALLOW_LAND_TAVE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS

      IF ( useDiagnostics ) THEN

        CALL DIAGNOSTICS_FILL(land_runOff, 'RUNOFF  ',0,1,
     &                                                1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(land_enRnOf, 'EnRunOff',0,1,
     &                                                1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(land_HeatFlx,'landHFlx',0,1,
     &                                                1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(land_Pr_m_Ev,'landPmE ',0,1,
     &                                                1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(land_EnWFlux,'ldEnFxPr',0,1,
     &                                                1,bi,bj,myThid)

      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_LAND */

      RETURN
      END
