C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_main.F,v 1.2 2004/04/07 23:40:34 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
 
CBOP
C     !ROUTINE: THSICE_MAIN
C     !INTERFACE:
      SUBROUTINE THSICE_MAIN( 
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_MAIN             
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
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#ifdef ALLOW_BULK_FORCE
#include "BULKF.h"
#endif
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL evpAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxSW (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

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

         DO j = jMin, jMax
          DO i = iMin, iMax
           hOceMxL(i,j,bi,bj) = hfacC(i,j,1,bi,bj)*drF(1)
           tOceMxL(i,j,bi,bj) = theta(i,j,1,bi,bj)
           sOceMxL(i,j,bi,bj) = salt (i,j,1,bi,bj)
           v2ocMxL(i,j,bi,bj) = 
     &              ( uvel(i,j,1,bi,bj)*uvel(i,j,1,bi,bj)
     &              + uvel(i+1,j,1,bi,bj)*uvel(i+1,j,1,bi,bj)
     &              + vvel(i,j+1,1,bi,bj)*vvel(i,j+1,1,bi,bj)
     &              + vvel(i,j,1,bi,bj)*vvel(i,j,1,bi,bj)
     &              )*0.5 _d 0
           prcAtm(i,j) = 0.
           evpAtm(i,j) = 0.
           flxSW (i,j) = 0.
           snowPrc(i,j,bi,bj) = 0. _d 0
#ifdef ALLOW_BULK_FORCE
           prcAtm(i,j) = ( rain(i,j,bi,bj)+runoff(i,j,bi,bj) )*rhofw
           flxSW (i,j) = solar(i,j,bi,bj)
           IF ( iceMask(i,j,bi,bj).GT.0. _d 0
     &       .AND. Tair(i,j,bi,bj).LE.Tf0kel )  THEN
             snowPrc(i,j,bi,bj) = rain(i,j,bi,bj)*rhofw
           ENDIF
#endif
          ENDDO
         ENDDO

         CALL THSICE_STEP_FWD( 
     I                     bi, bj, iMin, iMax, jMin, jMax, 
     I                     prcAtm, 
     U                     evpAtm, flxSW,
     I                     myTime, myIter, myThid )

         CALL THSICE_AVE( 
     I                   evpAtm, flxSW, 
     I                   bi,bj, myTime, myIter, myThid )

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
