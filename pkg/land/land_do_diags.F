C $Header: /u/gcmpack/MITgcm/pkg/land/land_do_diags.F,v 1.3 2005/05/15 03:04:56 jmc Exp $
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
      INTEGER k, iterWr
      CHARACTER*(MAX_LEN_MBUF) suff

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( DIFFERENT_MULTIPLE(land_diagFreq,
     &                        myTime+land_deltaT,land_deltaT)
     & .OR. myTime+land_deltaT .EQ. endTime 
     &   ) THEN 
       iterWr = myIter+1
       WRITE(suff,'(I10.10)') iterWr

c      _BARRIER
c      _BEGIN_MASTER( myThid )

       DO k=1,land_nLev
C--   Write ground Temp and soil moisture :
        CALL WRITE_LOCAL_RL('land_groundT','I10',1,
     &       land_groundT(1-OLx,1-OLy,k,bi,bj),bi,bj,k,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_enthalp','I10',1,
     &       land_enthalp(1-OLx,1-OLy,k,bi,bj),bi,bj,k,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_groundW','I10',1,
     &       land_groundW(1-OLx,1-OLy,k,bi,bj),bi,bj,k,iterWr,myThid)
       ENDDO
        CALL WRITE_LOCAL_RL('land_skinT', 'I10',1,
     &       land_skinT(1-OLx,1-OLy,bi,bj),  bi,bj,1,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_hSnow', 'I10',1,
     &       land_hSnow(1-OLx,1-OLy,bi,bj),  bi,bj,1,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_snAge', 'I10',1,
     &       land_snowAge(1-OLx,1-OLy,bi,bj),bi,bj,1,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_RunOff','I10',1,
     &       land_runOff(1-OLx,1-OLy,bi,bj), bi,bj,1,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_enRnOf','I10',1,
     &       land_enRnOf(1-OLx,1-OLy,bi,bj), bi,bj,1,iterWr,myThid)

        CALL WRITE_LOCAL_RL('land_HeatFx','I10',1,
     &       land_HeatFlx(1-OLx,1-OLy,bi,bj), bi,bj,1,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_frWaFx','I10',1,
     &       land_Pr_m_Ev(1-OLx,1-OLy,bi,bj), bi,bj,1,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_EnWaFx','I10',1,
     &       land_EnWFlux(1-OLx,1-OLy,bi,bj), bi,bj,1,iterWr,myThid)

c      _END_MASTER( myThid )
c      _BARRIER
C--
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_LAND_TAVE

      IF (land_taveFreq.GT.0.) THEN

C--   Sum ground Temp, soil moisture and Run-Off:
      CALL TIMEAVE_CUMULATE(land_grTtave,land_groundT,land_nLev,
     &                      land_deltaT, bi, bj, myThid )
      CALL TIMEAVE_CUMULATE(land_entave, land_enthalp,land_nLev,
     &                      land_deltaT, bi, bj, myThid )
      CALL TIMEAVE_CUMULATE(land_grWtave,land_groundW,land_nLev,
     &                      land_deltaT, bi, bj, myThid )
      CALL TIMEAVE_CUMULATE(land_sTtave, land_skinT ,1,
     &                      land_deltaT, bi, bj, myThid )
      CALL TIMEAVE_CUMULATE(land_hStave, land_hSnow ,1,
     &                      land_deltaT, bi, bj, myThid )
      CALL TIMEAVE_CUMULATE(land_sAtave, land_snowAge,1,
     &                      land_deltaT, bi, bj, myThid )
      CALL TIMEAVE_CUMULATE(land_ROftave,land_runOff,1,
     &                      land_deltaT, bi, bj, myThid )
      CALL TIMEAVE_CUMULATE(land_eROtave,land_enRnOf,1,
     &                      land_deltaT, bi, bj, myThid )

C-    Keep record of how much time has been integrated over
        DO K=1,Nr
          land_timeAve(k,bi,bj)=land_timeAve(k,bi,bj)+land_deltaT
        ENDDO

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
