C $Header: /u/gcmpack/MITgcm/model/src/ini_masks_etc.F,v 1.18 2000/04/11 13:39:09 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_MASKS_ETC( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_MASKS_ETC                                 |
C     | o Initialise masks and topography factors                |
C     |==========================================================|
C     | These arrays are used throughout the code and describe   |
C     | the topography of the domain through masks (0s and 1s)   |
C     | and fractional height factors (0<hFac<1). The latter     |
C     | distinguish between the lopped-cell and full-step        |
C     | topographic representations.                             |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_MASKS_ETC
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I, J, K
      _RL hFacTmp
#ifdef ALLOW_NONHYDROSTATIC
      INTEGER Km1
      _RL hFacUpper,hFacLower
#endif

C     Calculate lopping factor hFacC
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         DO J=1,sNy
          DO I=1,sNx
C          Round depths within a small fraction of layer depth to that
C          layer depth.
           IF ( ABS(H(I,J,bi,bj)-rF(K)) .LT. 
     &          1. _d -6*ABS(rF(K)) .AND.
     &          ABS(H(I,J,bi,bj)-rF(K)) .LT. 
     &          1. _d -6*ABS(H(I,J,bi,bj)) )THEN
            H(I,J,bi,bj) = rF(K)
           ENDIF
           IF     ( H(I,J,bi,bj)*rkFac .GE. rF(K)*rkFac ) THEN
C           Top of cell is below base of domain
            hFacC(I,J,K,bi,bj) = 0.
           ELSEIF ( H(I,J,bi,bj)*rkFac .LE. rF(K+1)*rkFac ) THEN
C           Base of domain is below bottom of this cell
            hFacC(I,J,K,bi,bj) = 1.
           ELSE
C           Base of domain is in this cell
C           Set hFac to the fraction of the cell that is open.
            hFacC(I,J,K,bi,bj) =
     &       (rF(K)*rkFac-H(I,J,bi,bj)*rkFac)*recip_drF(K)
           ENDIF
C Impose minimum fraction and/or size
           hFacTmp=max( hFacMin , min(hFacMinDr*recip_drF(k),1.) )
           IF (hFacC(I,J,K,bi,bj).LT.hFacTmp) THEN
            IF (hFacC(I,J,K,bi,bj).LT.hFacTmp*0.5) THEN
             hFacC(I,J,K,bi,bj)=0.
            ELSE
             hFacC(I,J,K,bi,bj)=hFacTmp
            ENDIF
           ENDIF
           depthInK(i,j,bi,bj) = depthInK(i,j,bi,bj) + 1.
Crg  &                          +hFacC(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XYZ_R4(hFacC , myThid )
      _EXCH_XY_R4( depthInK, myThid )

      CALL PLOT_FIELD_XYRS( depthInK, 
     & 'Model Depths K Index' , 1, myThid )

C Re-calculate depth of ocean, taking into account hFacC
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          H(I,J,bi,bj)=0.
          DO K=1,Nr
           H(I,J,bi,bj)=H(I,J,bi,bj)-
     &       rkFac*drF(k)*hFacC(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(H , myThid )
      CALL WRITE_FLD_XY_RS( 'Depth',' ',H,0,myThid)
C     CALL MDSWRITEFIELD( 'Depth', writeBinaryPrec, .TRUE.,
C    &                    'RS', 1, H, 1, -1, myThid )

C     Calculate quantities derived from XY depth map
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
C         Inverse of depth
          IF ( h(i,j,bi,bj) .EQ. 0. _d 0 ) THEN
           recip_H(i,j,bi,bj) = 0. _d 0
          ELSE
           recip_H(i,j,bi,bj) = 1. _d 0 /  abs( H(i,j,bi,bj) )
          ENDIF
          depthInK(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(   recip_H, myThid )

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
      _EXCH_XYZ_R4(hFacW , myThid )
      _EXCH_XYZ_R4(hFacS , myThid )

C     Masks and reciprocals of hFac[CWS]
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           IF (HFacC(I,J,K,bi,bj) .NE. 0. _d 0 ) THEN
            recip_HFacC(I,J,K,bi,bj) = 1. _d 0 / HFacC(I,J,K,bi,bj)
           ELSE
            recip_HFacC(I,J,K,bi,bj) = 0. _d 0
           ENDIF
           IF (HFacW(I,J,K,bi,bj) .NE. 0. _d 0 ) THEN
            recip_HFacW(I,J,K,bi,bj) = 1. _d 0 / HFacW(I,J,K,bi,bj)
            maskW(I,J,K,bi,bj) = 1. _d 0
           ELSE
            recip_HFacW(I,J,K,bi,bj) = 0. _d 0
            maskW(I,J,K,bi,bj) = 0.0 _d 0
           ENDIF
           IF (HFacS(I,J,K,bi,bj) .NE. 0. _d 0 ) THEN
            recip_HFacS(I,J,K,bi,bj) = 1. _d 0 / HFacS(I,J,K,bi,bj)
            maskS(I,J,K,bi,bj) = 1. _d 0
           ELSE
            recip_HFacS(I,J,K,bi,bj) = 0. _d 0
            maskS(I,J,K,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XYZ_R4(recip_HFacC    , myThid )
      _EXCH_XYZ_R4(recip_HFacW    , myThid )
      _EXCH_XYZ_R4(recip_HFacS    , myThid )
      _EXCH_XYZ_R4(maskW    , myThid )
      _EXCH_XYZ_R4(maskS    , myThid )

C     Calculate recipricols grid lengths
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          recip_dxG(I,J,bi,bj)=1.d0/dxG(I,J,bi,bj)
          recip_dyG(I,J,bi,bj)=1.d0/dyG(I,J,bi,bj)
          recip_dxC(I,J,bi,bj)=1.d0/dxC(I,J,bi,bj)
          recip_dyC(I,J,bi,bj)=1.d0/dyC(I,J,bi,bj)
          recip_dxF(I,J,bi,bj)=1.d0/dxF(I,J,bi,bj)
          recip_dyF(I,J,bi,bj)=1.d0/dyF(I,J,bi,bj)
          recip_dxV(I,J,bi,bj)=1.d0/dxV(I,J,bi,bj)
          recip_dyU(I,J,bi,bj)=1.d0/dyU(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(recip_dxG, myThid )
      _EXCH_XY_R4(recip_dyG, myThid )
      _EXCH_XY_R4(recip_dxC, myThid )
      _EXCH_XY_R4(recip_dyC, myThid )
      _EXCH_XY_R4(recip_dxF, myThid )
      _EXCH_XY_R4(recip_dyF, myThid )
      _EXCH_XY_R4(recip_dxV, myThid )
      _EXCH_XY_R4(recip_dyU, myThid )

#ifdef ALLOW_NONHYDROSTATIC
C--   Calculate the reciprocal hfac distance/volume for W cells
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         Km1=max(K-1,1)
         hFacUpper=drF(Km1)/(drF(Km1)+drF(K))
         IF (Km1.EQ.K) hFacUpper=0.
         hFacLower=drF(K)/(drF(Km1)+drF(K))
         DO J=1,sNy
          DO I=1,sNx
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
      _EXCH_XY_R4(recip_hFacU, myThid )
#endif
C
      RETURN
      END
