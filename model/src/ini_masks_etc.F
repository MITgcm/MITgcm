C $Header: /u/gcmpack/MITgcm/model/src/ini_masks_etc.F,v 1.24 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

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
C     myThid -  Number of this instance of INI_MASKS_ETC
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables in common ==
C     tmpfld  - Temporary array used to compute & write Total Depth
C               has to be in common for multi threading
      COMMON / LOCAL_INI_MASKS_ETC / tmpfld
      _RS tmpfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I, J, K
#ifdef ALLOW_NONHYDROSTATIC
      INTEGER Km1
      _RL hFacUpper,hFacLower
#endif
      _RL hFacCtmp
      _RL hFacMnSz
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

C-  Re-calculate Reference surface position, taking into account hFacC
C   initialize Total column fluid thickness and surface k index
C       Note: if no fluid (continent) ==> ksurf = Nr+1
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          tmpfld(I,J,bi,bj) = 0.
          ksurfC(I,J,bi,bj) = Nr+1
          Ro_surf(I,J,bi,bj) = R_low(I,J,bi,bj)
          DO K=Nr,1,-1
           Ro_surf(I,J,bi,bj) = Ro_surf(I,J,bi,bj)
     &                        + drF(k)*hFacC(I,J,K,bi,bj)
           IF (hFacC(I,J,K,bi,bj).NE.0.) THEN
            ksurfC(I,J,bi,bj) = k
            tmpfld(i,j,bi,bj) = tmpfld(i,j,bi,bj) + 1.
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
           recip_Rcol(i,j,bi,bj) = 1. / tmpfld(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     _EXCH_XY_R4(   recip_Rcol, myThid )

C     hFacW and hFacS (at U and V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         DO J=1,sNy
          DO I=1,sNx
           hFacW(I,J,K,bi,bj)=
     &       MIN(hFacC(I,J,K,bi,bj),hFacC(I-1,J,K,bi,bj))
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
           IF (DYG(I,J,bi,bj).EQ.0.) hFacW(I,J,K,bi,bj)=0.
           IF (DXG(I,J,bi,bj).EQ.0.) hFacS(I,J,K,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-    Write to disk: Total Column Thickness & hFac(C,W,S):
      _BARRIER
      _BEGIN_MASTER( myThid )
C     CALL MDSWRITEFIELD( 'Depth', writeBinaryPrec, .TRUE.,
C    &                    'RS', 1, tmpfld, 1, -1, myThid )
      CALL WRITE_FLD_XY_RS( 'Depth',' ',tmpfld,0,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacC',' ',hFacC,0,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacW',' ',hFacW,0,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacS',' ',hFacS,0,myThid)
      _END_MASTER(myThid)

      CALL PLOT_FIELD_XYZRS( hFacC, 'hFacC' , Nr, 1, myThid )
      CALL PLOT_FIELD_XYZRS( hFacW, 'hFacW' , Nr, 1, myThid )
      CALL PLOT_FIELD_XYZRS( hFacS, 'hFacS' , Nr, 1, myThid )

C     Masks and reciprocals of hFac[CWS]
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           IF (HFacC(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_HFacC(I,J,K,bi,bj) = 1. / HFacC(I,J,K,bi,bj)
            maskC(I,J,K,bi,bj) = 1.
           ELSE
            recip_HFacC(I,J,K,bi,bj) = 0.
            maskC(I,J,K,bi,bj) = 0.
           ENDIF
           IF (HFacW(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_HFacW(I,J,K,bi,bj) = 1. / HFacW(I,J,K,bi,bj)
            maskW(I,J,K,bi,bj) = 1.
           ELSE
            recip_HFacW(I,J,K,bi,bj) = 0.
            maskW(I,J,K,bi,bj) = 0.
           ENDIF
           IF (HFacS(I,J,K,bi,bj) .NE. 0. ) THEN
            recip_HFacS(I,J,K,bi,bj) = 1. / HFacS(I,J,K,bi,bj)
            maskS(I,J,K,bi,bj) = 1.
           ELSE
            recip_HFacS(I,J,K,bi,bj) = 0.
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
C     _EXCH_XYZ_R4(recip_HFacC    , myThid )
C     _EXCH_XYZ_R4(recip_HFacW    , myThid )
C     _EXCH_XYZ_R4(recip_HFacS    , myThid )
C     _EXCH_XYZ_R4(maskW    , myThid )
C     _EXCH_XYZ_R4(maskS    , myThid )

C     Calculate recipricols grid lengths
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          IF ( dxG(I,J,bi,bj) .NE. 0. )
     &    recip_dxG(I,J,bi,bj)=1.d0/dxG(I,J,bi,bj)
          IF ( dyG(I,J,bi,bj) .NE. 0. )
     &    recip_dyG(I,J,bi,bj)=1.d0/dyG(I,J,bi,bj)
          IF ( dxC(I,J,bi,bj) .NE. 0. )
     &    recip_dxC(I,J,bi,bj)=1.d0/dxC(I,J,bi,bj)
          IF ( dyC(I,J,bi,bj) .NE. 0. )
     &    recip_dyC(I,J,bi,bj)=1.d0/dyC(I,J,bi,bj)
          IF ( dxF(I,J,bi,bj) .NE. 0. )
     &    recip_dxF(I,J,bi,bj)=1.d0/dxF(I,J,bi,bj)
          IF ( dyF(I,J,bi,bj) .NE. 0. )
     &    recip_dyF(I,J,bi,bj)=1.d0/dyF(I,J,bi,bj)
          IF ( dxV(I,J,bi,bj) .NE. 0. )
     &    recip_dxV(I,J,bi,bj)=1.d0/dxV(I,J,bi,bj)
          IF ( dyU(I,J,bi,bj) .NE. 0. )
     &    recip_dyU(I,J,bi,bj)=1.d0/dyU(I,J,bi,bj)
          IF ( rA(I,J,bi,bj) .NE. 0. )
     &    recip_rA(I,J,bi,bj)=1.d0/rA(I,J,bi,bj)
          IF ( rAs(I,J,bi,bj) .NE. 0. )
     &    recip_rAs(I,J,bi,bj)=1.d0/rAs(I,J,bi,bj)
          IF ( rAw(I,J,bi,bj) .NE. 0. )
     &    recip_rAw(I,J,bi,bj)=1.d0/rAw(I,J,bi,bj)
          IF ( rAz(I,J,bi,bj) .NE. 0. )
     &    recip_rAz(I,J,bi,bj)=1.d0/rAz(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Do not need these since above denominators are valid over full range
C     _EXCH_XY_R4(recip_dxG, myThid )
C     _EXCH_XY_R4(recip_dyG, myThid )
C     _EXCH_XY_R4(recip_dxC, myThid )
C     _EXCH_XY_R4(recip_dyC, myThid )
C     _EXCH_XY_R4(recip_dxF, myThid )
C     _EXCH_XY_R4(recip_dyF, myThid )
C     _EXCH_XY_R4(recip_dxV, myThid )
C     _EXCH_XY_R4(recip_dyU, myThid )
C     _EXCH_XY_R4(recip_rAw, myThid )
C     _EXCH_XY_R4(recip_rAs, myThid )

#ifdef ALLOW_NONHYDROSTATIC
C--   Calculate the reciprocal hfac distance/volume for W cells
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         Km1=max(K-1,1)
         hFacUpper=drF(Km1)/(drF(Km1)+drF(K))
         IF (Km1.EQ.K) hFacUpper=0.
         hFacLower=drF(K)/(drF(Km1)+drF(K))
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           IF (hFacC(I,J,K,bi,bj).NE.0.) THEN
            IF (hFacC(I,J,K,bi,bj).LE.0.5) THEN
            recip_hFacU(I,J,K,bi,bj)=
     &         hFacUpper+hFacLower*hFacC(I,J,K,bi,bj)
            ELSE
             recip_hFacU(I,J,K,bi,bj)=1.
            ENDIF
           ELSE
            recip_hFacU(I,J,K,bi,bj)=0.
           ENDIF
           IF (recip_hFacU(I,J,K,bi,bj).NE.0.)
     &      recip_hFacU(I,J,K,bi,bj)=1./recip_hFacU(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     _EXCH_XY_R4(recip_hFacU, myThid )
#endif
C
      RETURN
      END
