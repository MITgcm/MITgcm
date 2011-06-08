C $Header: /u/gcmpack/MITgcm/model/src/ini_masks_etc.F,v 1.49 2011/06/08 01:18:23 jmc Exp $
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  ::  Number of this instance of INI_MASKS_ETC
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     i,j,k   :: Loop counters
C     tmpfld  :: Temporary array used to compute & write Total Depth
      _RS tmpfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj
      INTEGER i, j, k
      _RL hFacCtmp
      _RL hFacMnSz
CEOP

C---  Calculate reciprocals grid lengths (should be part of INI_GRID)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          IF ( dxG(i,j,bi,bj) .NE. 0. )
     &    recip_dxG(i,j,bi,bj) = 1. _d 0/dxG(i,j,bi,bj)
          IF ( dyG(i,j,bi,bj) .NE. 0. )
     &    recip_dyG(i,j,bi,bj) = 1. _d 0/dyG(i,j,bi,bj)
          IF ( dxC(i,j,bi,bj) .NE. 0. )
     &    recip_dxC(i,j,bi,bj) = 1. _d 0/dxC(i,j,bi,bj)
          IF ( dyC(i,j,bi,bj) .NE. 0. )
     &    recip_dyC(i,j,bi,bj) = 1. _d 0/dyC(i,j,bi,bj)
          IF ( dxF(i,j,bi,bj) .NE. 0. )
     &    recip_dxF(i,j,bi,bj) = 1. _d 0/dxF(i,j,bi,bj)
          IF ( dyF(i,j,bi,bj) .NE. 0. )
     &    recip_dyF(i,j,bi,bj) = 1. _d 0/dyF(i,j,bi,bj)
          IF ( dxV(i,j,bi,bj) .NE. 0. )
     &    recip_dxV(i,j,bi,bj) = 1. _d 0/dxV(i,j,bi,bj)
          IF ( dyU(i,j,bi,bj) .NE. 0. )
     &    recip_dyU(i,j,bi,bj) = 1. _d 0/dyU(i,j,bi,bj)
          IF ( rA (i,j,bi,bj) .NE. 0. )
     &    recip_rA (i,j,bi,bj) = 1. _d 0/rA (i,j,bi,bj)
          IF ( rAs(i,j,bi,bj) .NE. 0. )
     &    recip_rAs(i,j,bi,bj) = 1. _d 0/rAs(i,j,bi,bj)
          IF ( rAw(i,j,bi,bj) .NE. 0. )
     &    recip_rAw(i,j,bi,bj) = 1. _d 0/rAw(i,j,bi,bj)
          IF ( rAz(i,j,bi,bj) .NE. 0. )
     &    recip_rAz(i,j,bi,bj) = 1. _d 0/rAz(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( selectSigmaCoord.EQ.0 ) THEN
C---  r-coordinate with partial-cell or full cell mask

C--   Calculate lopping factor hFacC : over-estimate the part inside of the domain
C     taking into account the lower_R Boundary (Bathymetrie / Top of Atmos)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C      o Non-dimensional distance between grid bound. and domain lower_R bound.
           hFacCtmp = (rF(k)-R_low(i,j,bi,bj))*recip_drF(k)
C      o Select between, closed, open or partial (0,1,0-1)
           hFacCtmp=min( max( hFacCtmp, 0. _d 0) , 1. _d 0)
C      o Impose minimum fraction and/or size (dimensional)
           IF (hFacCtmp.LT.hFacMnSz) THEN
            IF (hFacCtmp.LT.hFacMnSz*0.5) THEN
             hFacC(i,j,k,bi,bj)=0.
            ELSE
             hFacC(i,j,k,bi,bj)=hFacMnSz
            ENDIF
           ELSE
             hFacC(i,j,k,bi,bj)=hFacCtmp
           ENDIF
          ENDDO
         ENDDO
        ENDDO

C-    Re-calculate lower-R Boundary position, taking into account hFacC
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          R_low(i,j,bi,bj) = rF(1)
          DO k=Nr,1,-1
           R_low(i,j,bi,bj) = R_low(i,j,bi,bj)
     &                      - drF(k)*hFacC(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C-    end bi,bj loops.
       ENDDO
      ENDDO

C--   Calculate lopping factor hFacC : Remove part outside of the domain
C     taking into account the Reference (=at rest) Surface Position Ro_surf
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C      o Non-dimensional distance between grid boundary and model surface
           hFacCtmp = (rF(k)-Ro_surf(i,j,bi,bj))*recip_drF(k)
C      o Reduce the previous fraction : substract the outside part.
           hFacCtmp = hFacC(i,j,k,bi,bj) - max( hFacCtmp, 0. _d 0)
C      o set to zero if empty Column :
           hFacCtmp = max( hFacCtmp, 0. _d 0)
C      o Impose minimum fraction and/or size (dimensional)
           IF (hFacCtmp.LT.hFacMnSz) THEN
            IF (hFacCtmp.LT.hFacMnSz*0.5) THEN
             hFacC(i,j,k,bi,bj)=0.
            ELSE
             hFacC(i,j,k,bi,bj)=hFacMnSz
            ENDIF
           ELSE
             hFacC(i,j,k,bi,bj)=hFacCtmp
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_SHELFICE
      IF ( useShelfIce ) THEN
C--   Modify lopping factor hFacC : Remove part outside of the domain
C     taking into account the Reference (=at rest) Surface Position Ro_shelfIce
       CALL SHELFICE_UPDATE_MASKS(
     I     rF, recip_drF,
     U     hFacC,
     I     myThid )
      ENDIF
#endif /* ALLOW_SHELFICE */

C-    Re-calculate Reference surface position, taking into account hFacC
C     initialize Total column fluid thickness and surface k index
C       Note: if no fluid (continent) ==> kSurf = Nr+1
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          tmpfld(i,j,bi,bj) = 0.
          kSurfC(i,j,bi,bj) = Nr+1
c         maskH(i,j,bi,bj)  = 0.
          Ro_surf(i,j,bi,bj) = R_low(i,j,bi,bj)
          DO k=Nr,1,-1
           Ro_surf(i,j,bi,bj) = Ro_surf(i,j,bi,bj)
     &                        + drF(k)*hFacC(i,j,k,bi,bj)
           IF (hFacC(i,j,k,bi,bj).NE.0.) THEN
            kSurfC(i,j,bi,bj) = k
c           maskH(i,j,bi,bj)  = 1.
            tmpfld(i,j,bi,bj) = tmpfld(i,j,bi,bj) + 1.
           ENDIF
          ENDDO
          kLowC(i,j,bi,bj) = 0
          DO k= 1, Nr
           IF (hFacC(i,j,k,bi,bj).NE.0) THEN
              kLowC(i,j,bi,bj) = k
           ENDIF
          ENDDO
          maskInC(i,j,bi,bj)= 0.
          IF ( kSurfC(i,j,bi,bj).LE.Nr ) maskInC(i,j,bi,bj)= 1.
         ENDDO
        ENDDO
C-    end bi,bj loops.
       ENDDO
      ENDDO

      IF ( printDomain ) THEN
c       CALL PLOT_FIELD_XYRS( tmpfld,
c    &           'Model Depths K Index' , -1, myThid )
        CALL PLOT_FIELD_XYRS(R_low,
     &           'Model R_low (ini_masks_etc)', -1, myThid )
        CALL PLOT_FIELD_XYRS(Ro_surf,
     &           'Model Ro_surf (ini_masks_etc)', -1, myThid )
      ENDIF

C--   Calculate quantities derived from XY depth map
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
       ENDDO
      ENDDO

C--   hFacW and hFacS (at U and V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k=1, Nr
         DO j=1-Oly,sNy+Oly
          hFacW(1-OLx,j,k,bi,bj)= 0.
          DO i=2-Olx,sNx+Olx
           hFacW(i,j,k,bi,bj)=
     &       MIN(hFacC(i,j,k,bi,bj),hFacC(i-1,j,k,bi,bj))
          ENDDO
         ENDDO
         DO i=1-Olx,sNx+Olx
           hFacS(i,1-OLy,k,bi,bj)= 0.
         ENDDO
         DO j=2-Oly,sNy+oly
          DO i=1-Olx,sNx+Olx
           hFacS(i,j,k,bi,bj)=
     &       MIN(hFacC(i,j,k,bi,bj),hFacC(i,j-1,k,bi,bj))
          ENDDO
         ENDDO
        ENDDO
C     rLow & reference rSurf at Western & Southern edges (U and V points)
        i = 1-OlX
        DO j=1-Oly,sNy+Oly
           rLowW (i,j,bi,bj) = 0.
           rSurfW(i,j,bi,bj) = 0.
        ENDDO
        j = 1-Oly
        DO i=1-Olx,sNx+Olx
           rLowS (i,j,bi,bj) = 0.
           rSurfS(i,j,bi,bj) = 0.
        ENDDO
        DO j=1-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx
           rSurfW(i,j,bi,bj) =
     &           MIN( Ro_surf(i-1,j,bi,bj), Ro_surf(i,j,bi,bj) )
           rLowW(i,j,bi,bj)  =
     &           MAX(   R_low(i-1,j,bi,bj),   R_low(i,j,bi,bj) )
         ENDDO
        ENDDO
        DO j=2-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           rSurfS(i,j,bi,bj) =
     &           MIN( Ro_surf(i,j-1,bi,bj), Ro_surf(i,j,bi,bj) )
           rLowS(i,j,bi,bj)  =
     &           MAX(   R_low(i,j-1,bi,bj),   R_low(i,j,bi,bj) )
         ENDDO
        ENDDO
C-    end bi,bj loops.
       ENDDO
      ENDDO
      CALL EXCH_UV_XYZ_RS(hFacW,hFacS,.FALSE.,myThid)
      CALL EXCH_UV_XY_RS( rSurfW, rSurfS, .FALSE., myThid )
      CALL EXCH_UV_XY_RS( rLowW,  rLowS,  .FALSE., myThid )

C--   The following block allows thin walls representation of non-periodic
C     boundaries such as happen on the lat-lon grid at the N/S poles.
C     We should really supply a flag for doing this.
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k=1, Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF (dyG(i,j,bi,bj).EQ.0.) hFacW(i,j,k,bi,bj)=0.
           IF (dxG(i,j,bi,bj).EQ.0.) hFacS(i,j,k,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Calculate surface k index for interface W & S (U & V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          kSurfW(i,j,bi,bj) = Nr+1
          kSurfS(i,j,bi,bj) = Nr+1
          DO k=Nr,1,-1
           IF (hFacW(i,j,k,bi,bj).NE.0.) kSurfW(i,j,bi,bj) = k
           IF (hFacS(i,j,k,bi,bj).NE.0.) kSurfS(i,j,bi,bj) = k
          ENDDO
          maskInW(i,j,bi,bj)= 0.
          IF ( kSurfW(i,j,bi,bj).LE.Nr ) maskInW(i,j,bi,bj)= 1.
          maskInS(i,j,bi,bj)= 0.
          IF ( kSurfS(i,j,bi,bj).LE.Nr ) maskInS(i,j,bi,bj)= 1.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      ELSE
#ifndef DISABLE_SIGMA_CODE
C---  Sigma and Hybrid-Sigma set-up:
        CALL INI_SIGMA_HFAC( myThid )
#endif /* DISABLE_SIGMA_CODE */
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Write to disk: Total Column Thickness & hFac(C,W,S):
C     This I/O is now done in write_grid.F
c     CALL WRITE_FLD_XY_RS( 'Depth',' ',tmpfld,0,myThid)
c     CALL WRITE_FLD_XYZ_RS( 'hFacC',' ',hFacC,0,myThid)
c     CALL WRITE_FLD_XYZ_RS( 'hFacW',' ',hFacW,0,myThid)
c     CALL WRITE_FLD_XYZ_RS( 'hFacS',' ',hFacS,0,myThid)

      IF ( printDomain ) THEN
        CALL PLOT_FIELD_XYZRS( hFacC, 'hFacC' , Nr, 0, myThid )
        CALL PLOT_FIELD_XYZRS( hFacW, 'hFacW' , Nr, 0, myThid )
        CALL PLOT_FIELD_XYZRS( hFacS, 'hFacS' , Nr, 0, myThid )
      ENDIF

C--   Masks and reciprocals of hFac[CWS]
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF (hFacC(i,j,k,bi,bj) .NE. 0. ) THEN
            recip_hFacC(i,j,k,bi,bj) = 1. _d 0 / hFacC(i,j,k,bi,bj)
            maskC(i,j,k,bi,bj) = 1.
           ELSE
            recip_hFacC(i,j,k,bi,bj) = 0.
            maskC(i,j,k,bi,bj) = 0.
           ENDIF
           IF (hFacW(i,j,k,bi,bj) .NE. 0. ) THEN
            recip_hFacW(i,j,k,bi,bj) = 1. _d 0 / hFacW(i,j,k,bi,bj)
            maskW(i,j,k,bi,bj) = 1.
           ELSE
            recip_hFacW(i,j,k,bi,bj) = 0.
            maskW(i,j,k,bi,bj) = 0.
           ENDIF
           IF (hFacS(i,j,k,bi,bj) .NE. 0. ) THEN
            recip_hFacS(i,j,k,bi,bj) = 1. _d 0 / hFacS(i,j,k,bi,bj)
            maskS(i,j,k,bi,bj) = 1.
           ELSE
            recip_hFacS(i,j,k,bi,bj) = 0.
            maskS(i,j,k,bi,bj) = 0.
           ENDIF
          ENDDO
         ENDDO
        ENDDO
C-    end bi,bj loops.
       ENDDO
      ENDDO

c #ifdef ALLOW_NONHYDROSTATIC
C--   Calculate "recip_hFacU" = reciprocal hfac distance/volume for W cells
C NOTE:  not used ; computed locally in CALC_GW
c #endif

      RETURN
      END
