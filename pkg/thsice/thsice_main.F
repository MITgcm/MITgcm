C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_main.F,v 1.1 2003/11/23 01:20:13 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
 
C     !ROUTINE: THSICE_MAIN
C     !INTERFACE:
      SUBROUTINE THSICE_MAIN( 
     I                        myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE  THSICE_MAIN             
C     | o Therm_SeaIce main routine. 
C     |   step forward Thermodynamic_SeaIce variables and modify
C     |    ocean surface forcing accordingly.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax

      _RL tauFac

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( stressReduction.GT. 0. _d 0 ) THEN
       iMin = 1-Olx
       iMax = sNx+Olx-1
       jMin = 1-Oly
       jMax = sNy+Oly-1
      ELSE
       iMin = 1
       iMax = sNx
       jMin = 1
       jMax = sNy
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         CALL THSICE_STEP_FWD( 
     I                     bi, bj, iMin, iMax, jMin, jMax, 
     I                     myTime, myIter, myThid )
         IF ( thSIce_taveFreq .GT. 0. _d 0 )
     &   CALL THSICE_AVE( bi,bj, mythid )
c      ENDDO
c     ENDDO

c       IF ( .FALSE. ) THEN
        IF ( stressReduction.GT. 0. _d 0 ) THEN
         DO j = jMin, jMax
          DO i = iMin+1,iMax
            tauFac = stressReduction
     &             *(iceMask(i-1,j,bi,bj)+iceMask(i,j,bi,bj))*0.5 _d 0
            fu(i,j,bi,bj) = (1. _d 0 - tauFac)*fu(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j = jMin+1, jMax
          DO i = iMin, iMax
            tauFac = stressReduction
     &             *(iceMask(i,j-1,bi,bj)+iceMask(i,j,bi,bj))*0.5 _d 0
            fv(i,j,bi,bj) = (1. _d 0 - tauFac)*fv(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF

C--  end bi,bj loop
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif  /*ALLOW_THSICE*/

      RETURN
      END
