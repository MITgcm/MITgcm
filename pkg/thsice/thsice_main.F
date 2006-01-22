C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_main.F,v 1.6 2006/01/22 15:58:39 jmc Exp $
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
#include "SURFACE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
 
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
C-     needs new Ice Fraction in halo region to apply wind-stress reduction
       iMin = 1-Olx
       iMax = sNx+Olx-1
       jMin = 1-Oly
       jMax = sNy+Oly-1
#ifdef ATMOSPHERIC_LOADING
      ELSEIF ( useRealFreshWaterFlux ) THEN
C-     needs sea-ice loading in part of the halo regions for grad.Phi0surf
C      to be valid at the boundaries ( d/dx 1:sNx+1 ; d/dy 1:sNy+1 )
       iMin = 0
       iMax = sNx+1
       jMin = 0
       jMax = sNy+1
#endif /* ATMOSPHERIC_LOADING */
      ELSE
       iMin = 1
       iMax = sNx
       jMin = 1
       jMax = sNy
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--     Mixed layer thickness: take the 1rst layer
#ifdef NONLIN_FRSURF
        IF ( staggerTimeStep .AND. nonlinFreeSurf.GT.0 ) THEN
         IF ( select_rStar.GT.0 ) THEN
          DO j = jMin, jMax
           DO i = iMin, iMax
             hOceMxL(i,j,bi,bj) = drF(1)*h0FacC(i,j,1,bi,bj)
     &                                  *rStarFacC(i,j,bi,bj) 
           ENDDO
          ENDDO
         ELSE
          DO j = jMin, jMax
           DO i = iMin, iMax
            IF ( ksurfC(i,j,bi,bj).EQ.1 ) THEN
             hOceMxL(i,j,bi,bj) = drF(1)*hFac_surfC(i,j,bi,bj)
            ELSE
             hOceMxL(i,j,bi,bj) = drF(1)*hfacC(i,j,1,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ELSE
#else /* ndef NONLIN_FRSURF */
        IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */
          DO j = jMin, jMax
           DO i = iMin, iMax
             hOceMxL(i,j,bi,bj) = drF(1)*hfacC(i,j,1,bi,bj)
           ENDDO
          ENDDO
        ENDIF

         DO j = jMin, jMax
          DO i = iMin, iMax
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
           siceAlb(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
#ifdef ALLOW_BULK_FORCE
         IF ( useBulkforce ) THEN
           CALL THSICE_GET_PRECIP(
     I                  iceMask,
     O                  prcAtm, snowPrc(1-Olx,1-Oly,bi,bj), flxSW,
     I                  iMin,iMax,jMin,jMax, bi,bj, myThid )
         ENDIF
#endif

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

#ifdef ATMOSPHERIC_LOADING
c     IF (useRealFreshWaterFlux) _EXCH_XY_RS(sIceLoad, myThid)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif  /*ALLOW_THSICE*/

      RETURN
      END
