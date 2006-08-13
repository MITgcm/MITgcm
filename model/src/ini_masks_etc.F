C $Header: /u/gcmpack/MITgcm/model/src/ini_masks_etc.F,v 1.34 2006/08/13 21:43:48 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_MASKS_ETC
C     !INTERFACE:
      SUBROUTINE INI_MASKS_ETC( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_MASKS_ETC
C     | o Initialise masks and topography factors
C     *==========================================================*
C     | These arrays are used throughout the code and describe
C     | the topography of the domain through masks (0s and 1s)
C     | and fractional height factors (0<hFac<1). The latter
C     | distinguish between the lopped-cell and full-step
C     | topographic representations.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  ::  Number of this instance of INI_MASKS_ETC
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     I,J,K   :: Loop counters
C     tmpfld  :: Temporary array used to compute & write Total Depth
      _RS tmpfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj
      INTEGER I, J, K
      _RL hFacCtmp
      _RL hFacMnSz
      _RL tileArea, threadArea
CEOP

C- Calculate lopping factor hFacC : over-estimate the part inside of the domain
C    taking into account the lower_R Boundary (Bathymetrie / Top of Atmos)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
C      o Non-dimensional distance between grid bound. and domain lower_R bound.
           hFacCtmp = (rF(K)-R_low(I,J,bi,bj))*recip_drF(K)
C      o Select between, closed, open or partial (0,1,0-1)
           hFacCtmp=min( max( hFacCtmp, 0. _d 0) , 1. _d 0)
C      o Impose minimum fraction and/or size (dimensional)
           IF (hFacCtmp.LT.hFacMnSz) THEN
            IF (hFacCtmp.LT.hFacMnSz*0.5) THEN
             hFacC(I,J,K,bi,bj)=0.
            ELSE
             hFacC(I,J,K,bi,bj)=hFacMnSz
            ENDIF
           ELSE
             hFacC(I,J,K,bi,bj)=hFacCtmp
           ENDIF
          ENDDO
         ENDDO
        ENDDO

C-  Re-calculate lower-R Boundary position, taking into account hFacC
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          R_low(I,J,bi,bj) = rF(1)
          DO K=Nr,1,-1
           R_low(I,J,bi,bj) = R_low(I,J,bi,bj)
     &                      - drF(k)*hFacC(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO

C-  Calculate lopping factor hFacC : Remove part outside of the domain
C    taking into account the Reference (=at rest) Surface Position Ro_surf
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
C      o Non-dimensional distance between grid boundary and model surface
           hFacCtmp = (rF(k)-Ro_surf(I,J,bi,bj))*recip_drF(K)
C      o Reduce the previous fraction : substract the outside part.
           hFacCtmp = hFacC(I,J,K,bi,bj) - max( hFacCtmp, 0. _d 0)
C      o set to zero if empty Column :
           hFacCtmp = max( hFacCtmp, 0. _d 0)
C      o Impose minimum fraction and/or size (dimensional)
           IF (hFacCtmp.LT.hFacMnSz) THEN
            IF (hFacCtmp.LT.hFacMnSz*0.5) THEN
             hFacC(I,J,K,bi,bj)=0.
            ELSE
             hFacC(I,J,K,bi,bj)=hFacMnSz
            ENDIF
           ELSE
             hFacC(I,J,K,bi,bj)=hFacCtmp
           ENDIF
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_SHELFICE
C--   compute contributions of shelf ice to looping factors
        IF ( useShelfIce ) THEN
        DO K=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
C      o Non-dimensional distance between grid boundary and model surface
           hFacCtmp = (rF(k)-R_shelfIce(I,J,bi,bj))*recip_drF(K)
C      o Reduce the previous fraction : substract the outside part.
           hFacCtmp = hFacC(I,J,K,bi,bj) - max( hFacCtmp, 0. _d 0)
C      o set to zero if empty Column :
           hFacCtmp = max( hFacCtmp, 0. _d 0)
C      o Impose minimum fraction and/or size (dimensional)
           IF (hFacCtmp.LT.hFacMnSz) THEN
            IF (hFacCtmp.LT.hFacMnSz*0.5) THEN
             hFacC(I,J,K,bi,bj)=0.
            ELSE
             hFacC(I,J,K,bi,bj)=hFacMnSz
            ENDIF
           ELSE
             hFacC(I,J,K,bi,bj)=hFacCtmp
           ENDIF
          ENDDO
         ENDDO
        ENDDO
        ENDIF
#endif /* ALLOW_SHELFICE */

C-  Re-calculate Reference surface position, taking into account hFacC
C   initialize Total column fluid thickness and surface k index
C       Note: if no fluid (continent) ==> ksurf = Nr+1
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          tmpfld(I,J,bi,bj) = 0.
          ksurfC(I,J,bi,bj) = Nr+1
          maskH(I,J,bi,bj) = 0.
          Ro_surf(I,J,bi,bj) = R_low(I,J,bi,bj)
          DO K=Nr,1,-1
           Ro_surf(I,J,bi,bj) = Ro_surf(I,J,bi,bj)
     &                        + drF(k)*hFacC(I,J,K,bi,bj)
           IF (hFacC(I,J,K,bi,bj).NE.0.) THEN
            ksurfC(I,J,bi,bj) = k
            maskH(I,J,bi,bj) = 1.
            tmpfld(I,J,bi,bj) = tmpfld(I,J,bi,bj) + 1.
           ENDIF
          ENDDO
          kLowC(I,J,bi,bj) = 0
          DO K= 1, Nr
           IF (hFacC(I,J,K,bi,bj).NE.0) THEN
              kLowC(I,J,bi,bj) = K
           ENDIF
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO

C     CALL PLOT_FIELD_XYRS( tmpfld,
C    &         'Model Depths K Index' , 1, myThid )
      CALL PLOT_FIELD_XYRS(R_low,
     &         'Model R_low (ini_masks_etc)', 1, myThid)
      CALL PLOT_FIELD_XYRS(Ro_surf,
     &         'Model Ro_surf (ini_masks_etc)', 1, myThid)

C     Calculate quantities derived from XY depth map
      threadArea = 0. _d 0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C         Total fluid column thickness (r_unit) :
c         Rcolumn(i,j,bi,bj)= Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
          tmpfld(i,j,bi,bj) = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
C         Inverse of fluid column thickness (1/r_unit)
          IF ( tmpfld(i,j,bi,bj) .LE. 0. ) THEN
           recip_Rcol(i,j,bi,bj) = 0.
          ELSE
           recip_Rcol(i,j,bi,bj) = 1. _d 0 / tmpfld(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
C-      Compute the domain Area:
        tileArea = 0. _d 0
        DO j=1,sNy
         DO i=1,sNx
          tileArea = tileArea + rA(i,j,bi,bj)*maskH(i,j,bi,bj)
         ENDDO
        ENDDO
        threadArea = threadArea + tileArea
       ENDDO
      ENDDO
C     _EXCH_XY_R4(   recip_Rcol, myThid )
C_jmc: apply GLOBAL_SUM to thread-local variable (not in common block)
      _GLOBAL_SUM_R8( threadArea, myThid )
      _BEGIN_MASTER( myThid )
      globalArea = threadArea
      _END_MASTER( myThid )

C     hFacW and hFacS (at U and V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         DO J=1-Oly,sNy+Oly
          hFacW(1-OLx,J,K,bi,bj)= 0.
          DO I=2-Olx,sNx+Olx
           hFacW(I,J,K,bi,bj)=
     &       MIN(hFacC(I,J,K,bi,bj),hFacC(I-1,J,K,bi,bj))
          ENDDO
         ENDDO
         DO I=1-Olx,sNx+Olx
           hFacS(I,1-OLy,K,bi,bj)= 0.
         ENDDO
         DO J=2-Oly,sNy+oly
          DO I=1-Olx,sNx+Olx
           hFacS(I,J,K,bi,bj)=
     &       MIN(hFacC(I,J,K,bi,bj),hFacC(I,J-1,K,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_UV_XYZ_RS(hFacW,hFacS,.FALSE.,myThid)
C     The following block allows thin walls representation of non-periodic
C     boundaries such as happen on the lat-lon grid at the N/S poles.
C     We should really supply a flag for doing this.
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           IF (dyG(I,J,bi,bj).EQ.0.) hFacW(I,J,K,bi,bj)=0.
           IF (dxG(I,J,bi,bj).EQ.0.) hFacS(I,J,K,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-    Write to disk: Total Column Thickness & hFac(C,W,S):
      _BARRIER
c     _BEGIN_MASTER( myThid )
C     This I/O is now done in write_grid.F
C     CALL MDSWRITEFIELD( 'Depth', writeBinaryPrec, .TRUE.,
C    &                    'RS', 1, tmpfld, 1, -1, myThid )
c     CALL WRITE_FLD_XY_RS( 'Depth',' ',tmpfld,0,myThid)
c     CALL WRITE_FLD_XYZ_RS( 'hFacC',' ',hFacC,0,myThid)
c     CALL WRITE_FLD_XYZ_RS( 'hFacW',' ',hFacW,0,myThid)
c     CALL WRITE_FLD_XYZ_RS( 'hFacS',' ',hFacS,0,myThid)
c     _END_MASTER(myThid)

      CALL PLOT_FIELD_XYZRS( hFacC, 'hFacC' , Nr, 1, myThid )
      CALL PLOT_FIELD_XYZRS( hFacW, 'hFacW' , Nr, 1, myThid )
      CALL PLOT_FIELD_XYZRS( hFacS, 'hFacS' , Nr, 1, myThid )

C     Masks and reciprocals of hFac[CWS]
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           IF (hFacC(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_hFacC(I,J,K,bi,bj) = 1. _d 0 / hFacC(I,J,K,bi,bj)
            maskC(I,J,K,bi,bj) = 1.
           ELSE
            recip_hFacC(I,J,K,bi,bj) = 0.
            maskC(I,J,K,bi,bj) = 0.
           ENDIF
           IF (hFacW(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_hFacW(I,J,K,bi,bj) = 1. _d 0 / hFacW(I,J,K,bi,bj)
            maskW(I,J,K,bi,bj) = 1.
           ELSE
            recip_hFacW(I,J,K,bi,bj) = 0.
            maskW(I,J,K,bi,bj) = 0.
           ENDIF
           IF (hFacS(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_hFacS(I,J,K,bi,bj) = 1. _d 0 / hFacS(I,J,K,bi,bj)
            maskS(I,J,K,bi,bj) = 1.
           ELSE
            recip_hFacS(I,J,K,bi,bj) = 0.
            maskS(I,J,K,bi,bj) = 0.
           ENDIF
          ENDDO
         ENDDO
        ENDDO
C-    Calculate surface k index for interface W & S (U & V points)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          ksurfW(I,J,bi,bj) = Nr+1
          ksurfS(I,J,bi,bj) = Nr+1
          DO k=Nr,1,-1
           IF (hFacW(I,J,K,bi,bj).NE.0.) ksurfW(I,J,bi,bj) = k
           IF (hFacS(I,J,K,bi,bj).NE.0.) ksurfS(I,J,bi,bj) = k
          ENDDO
         ENDDO
        ENDDO
C - end bi,bj loops.
       ENDDO
      ENDDO

C     Calculate recipricols grid lengths
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          IF ( dxG(I,J,bi,bj) .NE. 0. )
     &    recip_dxG(I,J,bi,bj)=1. _d 0/dxG(I,J,bi,bj)
          IF ( dyG(I,J,bi,bj) .NE. 0. )
     &    recip_dyG(I,J,bi,bj)=1. _d 0/dyG(I,J,bi,bj)
          IF ( dxC(I,J,bi,bj) .NE. 0. )
     &    recip_dxC(I,J,bi,bj)=1. _d 0/dxC(I,J,bi,bj)
          IF ( dyC(I,J,bi,bj) .NE. 0. )
     &    recip_dyC(I,J,bi,bj)=1. _d 0/dyC(I,J,bi,bj)
          IF ( dxF(I,J,bi,bj) .NE. 0. )
     &    recip_dxF(I,J,bi,bj)=1. _d 0/dxF(I,J,bi,bj)
          IF ( dyF(I,J,bi,bj) .NE. 0. )
     &    recip_dyF(I,J,bi,bj)=1. _d 0/dyF(I,J,bi,bj)
          IF ( dxV(I,J,bi,bj) .NE. 0. )
     &    recip_dxV(I,J,bi,bj)=1. _d 0/dxV(I,J,bi,bj)
          IF ( dyU(I,J,bi,bj) .NE. 0. )
     &    recip_dyU(I,J,bi,bj)=1. _d 0/dyU(I,J,bi,bj)
          IF ( rA(I,J,bi,bj) .NE. 0. )
     &    recip_rA(I,J,bi,bj)=1. _d 0/rA(I,J,bi,bj)
          IF ( rAs(I,J,bi,bj) .NE. 0. )
     &    recip_rAs(I,J,bi,bj)=1. _d 0/rAs(I,J,bi,bj)
          IF ( rAw(I,J,bi,bj) .NE. 0. )
     &    recip_rAw(I,J,bi,bj)=1. _d 0/rAw(I,J,bi,bj)
          IF ( rAz(I,J,bi,bj) .NE. 0. )
     &    recip_rAz(I,J,bi,bj)=1. _d 0/rAz(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c #ifdef ALLOW_NONHYDROSTATIC
C--   Calculate "recip_hFacU" = reciprocal hfac distance/volume for W cells
C NOTE:  not used ; computed locally in CALC_GW
c #endif

      RETURN
      END
