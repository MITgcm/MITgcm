C $Header: /u/gcmpack/MITgcm/model/src/ini_masks_etc.F,v 1.20 2001/02/04 14:38:47 cnh Exp $
C $Name:  $

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
      INTEGER kadj_rf, klev_noH
#ifdef ALLOW_NONHYDROSTATIC
      INTEGER Km1
      _RL hFacUpper,hFacLower
#endif
      _RL hFacCtmp,topo_rkfac
      _RL hFacMnSz

      IF (groundAtK1) THEN
       topo_rkfac = -rkFac
       kadj_rf = 1
       klev_noH = Nr+1
      ELSE
       topo_rkfac = rkFac
       kadj_rf = 0
       klev_noH = 1
      ENDIF

C     Calculate lopping factor hFacC
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
c          IF (groundAtK1) THEN
C          o Non-dimensional distance between grid boundary and model depth
C            for case with "ground" at K=1 (i.e. like a good atmos model)
C          e.g. rkfac=+1 => dR/dk<0 (eg. R=p): hFacCtmp=(H(x,y)-rF(k))/drF(K)
C          e.g. rkfac=-1 => dR/dk>0 (eg. R=z): hFacCtmp=(rF(K)-H(x,y))/drF(K)
c          hFacCtmp=rkFac*(H(I,J,bi,bj)-rF(K+1))*recip_drF(K)
c          ELSE
C          o Non-dimensional distance between grid boundary and model depth
C            for case with "ground" at K=Nr (i.e. like original ocean model)
C          e.g. rkfac=+1 => dR/dk<0 (eg. R=z): hFacCtmp=(rF(K)-H(x,y))/drF(K)
C          e.g. rkfac=-1 => dR/dk>0 (eg. R=p): hFacCtmp=(H(x,y)-rF(k))/drF(K)
c          hFacCtmp=rkFac*(rF(K)-H(I,J,bi,bj))*recip_drF(K)
c          ENDIF
           hFacCtmp=topo_rkfac*(rF(K+kadj_rf)-H(I,J,bi,bj))*recip_drF(K)
C          o Select between, closed, open or partial (0,1,0-1)
           hFacCtmp=min( max( hFacCtmp, 0. _d 0) , 1. _d 0)
C          o And there we have it, the fractional open cell volume
           hFacC(I,J,K,bi,bj)=hFacCtmp
C          o Impose minimum fraction and/or size (dimensional)
           hFacMnSz=max( hFacMin , min(hFacMinDr*recip_drF(k),1. _d 0) )
           IF (hFacC(I,J,K,bi,bj).LT.hFacMnSz) THEN
            IF (hFacC(I,J,K,bi,bj).LT.hFacMnSz*0.5) THEN
             hFacC(I,J,K,bi,bj)=0.
            ELSE
             hFacC(I,J,K,bi,bj)=hFacMnSz
            ENDIF
           ENDIF
           IF (hFacC(I,J,K,bi,bj).NE.0.)
     &         depthInK(i,j,bi,bj) = depthInK(i,j,bi,bj) + 1.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     _EXCH_XYZ_R4(hFacC , myThid )
C     _EXCH_XY_R4( depthInK, myThid )

      CALL PLOT_FIELD_XYRS( depthInK, 
     & 'Model Depths K Index' , 1, myThid )

C Re-calculate depth of ocean, taking into account hFacC
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          H(I,J,bi,bj)=rF(klev_noH)
          DO K=1,Nr
           H(I,J,bi,bj)=H(I,J,bi,bj)-
     &       topo_rkFac*drF(k)*hFacC(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     _EXCH_XY_R4(H , myThid )
      CALL PLOT_FIELD_XYRS(H,'Model depths (ini_masks_etc)',1,myThid)
      CALL WRITE_FLD_XY_RS( 'Depth',' ',H,0,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacC',' ',hFacC,0,myThid)
C     CALL MDSWRITEFIELD( 'Depth', writeBinaryPrec, .TRUE.,
C    &                    'RS', 1, H, 1, -1, myThid )

C     Calculate quantities derived from XY depth map
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         Inverse of depth
          IF ( h(i,j,bi,bj) .EQ. 0. ) THEN
           recip_H(i,j,bi,bj) = 0.
          ELSE
           recip_H(i,j,bi,bj) = 1. / abs( H(i,j,bi,bj) )
          ENDIF
          depthInK(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     _EXCH_XY_R4(   recip_H, myThid )

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
C     Re-do hFacW and hFacS (at U and V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx+1,sNx+Olx ! Note range
           hFacW(I,J,K,bi,bj)=
     &       MIN(hFacC(I,J,K,bi,bj),hFacC(I-1,J,K,bi,bj))
          ENDDO
         ENDDO
         DO I=1-Olx,sNx+Olx
          DO J=1-Oly+1,sNy+Oly ! Note range
           hFacS(I,J,K,bi,bj)=
     &       MIN(hFacC(I,J,K,bi,bj),hFacC(I,J-1,K,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

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
           ELSE
            recip_HFacC(I,J,K,bi,bj) = 0.
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
