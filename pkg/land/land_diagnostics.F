C $Header: /u/gcmpack/MITgcm/pkg/land/Attic/land_diagnostics.F,v 1.1 2003/06/12 17:54:22 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_DIAGNOSTICS
C     !INTERFACE:
      SUBROUTINE LAND_DIAGNOSTICS( 
     I                land_frc, bi,bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_DIAGNOSTICS
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

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

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

C     == Local Variables == 
      INTEGER i,j,k, iterWr
      CHARACTER*(MAX_LEN_MBUF) suff

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (
     &    DIFFERENT_MULTIPLE(land_diagFreq,myTime+land_deltaT,myTime)
     &    .OR. myTime+land_deltaT .EQ. endTime 
     &   ) THEN 
       iterWr = myIter+1
       WRITE(suff,'(I10.10)') iterWr

c      _BARRIER
c      _BEGIN_MASTER( myThid )

       DO k=1,land_nLev
C--   Write ground Temp and soil moisture :
        CALL WRITE_LOCAL_RL('land_groundT','I10',1,
     &       land_groundT(1-OLx,1-OLy,k,bi,bj),bi,bj,k,iterWr,myThid)
        CALL WRITE_LOCAL_RL('land_groundW','I10',1,
     &       land_groundW(1-OLx,1-OLy,k,bi,bj),bi,bj,k,iterWr,myThid)
       ENDDO
        CALL WRITE_LOCAL_RL('land_RunOff','I10',1,
     &       land_runOff(1-OLx,1-OLy,bi,bj),bi,bj,1,iterWr,myThid)

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
      CALL TIMEAVE_CUMULATE(land_grWtave,land_groundW,land_nLev,
     &                      land_deltaT, bi, bj, myThid )
      CALL TIMEAVE_CUMULATE(land_ROftave,land_runOff,1,
     &                      land_deltaT, bi, bj, myThid )

C-    Keep record of how much time has been integrated over
        DO K=1,Nr
          land_timeAve(k,bi,bj)=land_timeAve(k,bi,bj)+land_deltaT
        ENDDO

      ENDIF

#endif /* ALLOW_LAND_TAVE */

#endif /* ALLOW_LAND */

      RETURN
      END

