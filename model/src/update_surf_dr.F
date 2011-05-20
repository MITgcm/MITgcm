C $Header: /u/gcmpack/MITgcm/model/src/update_surf_dr.F,v 1.10 2011/05/20 01:11:59 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_SURF_DR
C     !INTERFACE:
      SUBROUTINE UPDATE_SURF_DR( useLatest, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_SURF_DR
C     | o Update the surface-level thickness fraction (hFacC,W,S)
C     |   according to the surface r-position = Non-Linear FrSurf
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     useLatest :: if true use hFac_surfC, else use hFac_surfNm1C
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: Thread number for this instance of the routine.
      LOGICAL useLatest
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
#ifdef NONLIN_FRSURF
C     Local variables
C     i,j,bi,bj - loop counter
      INTEGER i,j,k,bi,bj
      INTEGER ks
CEOP

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF (useLatest.AND.(nonlinFreeSurf.GT.0)) then

C-- Update the fractional thickness "hFacC" of the surface level kSurfC :
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          ks = kSurfC(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacC(i,j,ks,bi,bj) = hFac_surfC(i,j,bi,bj)
           recip_hFacC(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfC(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update fractional thickness "hFacW" & "hFacS" (at U and V points)

        DO j=1-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx
          ks = kSurfW(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacW(i,j,ks,bi,bj) = hFac_surfW(i,j,bi,bj)
           recip_hFacW(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfW(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
        DO j=2-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          ks = kSurfS(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacS(i,j,ks,bi,bj) = hFac_surfS(i,j,bi,bj)
           recip_hFacS(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfS(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

       ELSEIF (nonlinFreeSurf.GT.0) THEN

C-- Update the fractional thickness "hFacC" of the surface level kSurfC
C:
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          ks = kSurfC(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacC(i,j,ks,bi,bj) = hFac_surfNm1C(i,j,bi,bj)
           recip_hFacC(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfNm1C(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update fractional thickness "hFacW" & "hFacS" (at U and V points)

        DO j=1-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx
          ks = kSurfW(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacW(i,j,ks,bi,bj) = hFac_surfNm1W(i,j,bi,bj)
           recip_hFacW(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfNm1W(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
        DO j=2-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          ks = kSurfS(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacS(i,j,ks,bi,bj) = hFac_surfNm1S(i,j,bi,bj)
           recip_hFacS(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfNm1S(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

       ELSE

        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           hFacC(i,j,k,bi,bj)=h0FacC(i,j,k,bi,bj)
           IF (h0FacC(i,j,k,bi,bj) .NE. 0. ) THEN
            recip_hFacC(i,j,k,bi,bj) = 1. _d 0 / h0FacC(i,j,k,bi,bj)
           ELSE
            recip_hFacC(i,j,k,bi,bj) = 0.
           ENDIF
          ENDDO
         ENDDO
        ENDDO

       ENDIF

C- end bi,bj loop
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
