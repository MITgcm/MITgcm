C $Header: /u/gcmpack/MITgcm/model/src/calc_gw.F,v 1.9 2001/09/26 18:09:14 cnh Exp $
C     !DESCRIPTION: \bv
C $Name:  $
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GW
C     !INTERFACE:
      SUBROUTINE CALC_GW(     
     I        myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_GW                                               
C     | o Calculate vert. velocity tendency terms ( NH, QH only ) 
C     *==========================================================*
C     | In NH and QH, the vertical momentum tendency must be
C     | calculated explicitly and included as a source term 
C     | for a 3d pressure eqn. Calculate that term here.
C     | This routine is not used in HYD calculations.
C     *==========================================================*
C     \ev 

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GW.h"
#include "CG3D.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Instance number for this innvocation of CALC_GW
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi, bj,      :: Loop counters
C     iMin, iMax,
C     jMin, jMax
C     flx_NS       :: Temp. used for fVol meridional terms.
C     flx_EW       :: Temp. used for fVol zonal terms.
C     flx_Up       :: Temp. used for fVol vertical terms.
C     flx_Dn       :: Temp. used for fVol vertical terms.
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL    flx_NS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    flx_EW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    flx_Dn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    flx_Up(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     I,J,K - Loop counters
      INTEGER i,j,k, kP1, kUp
      _RL  wOverride
      _RS  hFacROpen
      _RS  hFacRClosed
      _RL ab15,ab05
      _RL tmp_VbarZ, tmp_UbarZ, tmp_WbarZ

      _RL  Half
      PARAMETER(Half=0.5D0)

#define I0 1
#define In sNx
#define J0 1
#define Jn sNy
CEOP

C     Adams-Bashforth timestepping weights
      ab15=1.5+abeps
      ab05=-0.5-abeps

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gWNM1(i,j,k,bi,bj) = gW(i,j,k,bi,bj) 
           gW(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C Catch barotropic mode
      IF ( Nr .LT. 2 ) RETURN

C For each tile
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C Boundaries condition at top
        DO J=J0,Jn
         DO I=I0,In
          Flx_Dn(I,J,bi,bj)=0.
         ENDDO
        ENDDO

C Sweep down column
        DO K=2,Nr
         Kp1=K+1
         wOverRide=1.
         if (K.EQ.Nr) then
          Kp1=Nr
          wOverRide=0.
         endif
C Flux on Southern face
         DO J=J0,Jn+1
          DO I=I0,In
           tmp_VbarZ=Half*(
     &          _hFacS(I,J,K-1,bi,bj)*vVel( I ,J,K-1,bi,bj)
     &         +_hFacS(I,J, K ,bi,bj)*vVel( I ,J, K ,bi,bj))
           Flx_NS(I,J,bi,bj)=
     &     tmp_VbarZ*Half*(wVel(I,J,K,bi,bj)+wVel(I,J-1,K,bi,bj))
     &    -viscAh*_recip_dyC(I,J,bi,bj)*(
     &                   wVel(I,J,K,bi,bj)-wVel(I,J-1,K,bi,bj) )
          ENDDO
         ENDDO
C Flux on Western face
         DO J=J0,Jn
          DO I=I0,In+1
           tmp_UbarZ=Half*(
     &         _hFacW(I,J,K-1,bi,bj)*uVel( I ,J,K-1,bi,bj)
     &        +_hFacW(I,J, K ,bi,bj)*uVel( I ,J, K ,bi,bj))
           Flx_EW(I,J,bi,bj)=
     &     tmp_UbarZ*Half*(wVel(I,J,K,bi,bj)+wVel(I-1,J,K,bi,bj))
     &    -viscAh*_recip_dxC(I,J,bi,bj)*(
     &                   wVel(I,J,K,bi,bj)-wVel(I-1,J,K,bi,bj) )
          ENDDO
         ENDDO
C Flux on Lower face
         DO J=J0,Jn
          DO I=I0,In
           Flx_Up(I,J,bi,bj)=Flx_Dn(I,J,bi,bj)
           tmp_WbarZ=Half*(wVel(I,J,K,bi,bj)+wVel(I,J,Kp1,bi,bj))
           Flx_Dn(I,J,bi,bj)=
     &     tmp_WbarZ*tmp_WbarZ
     &    -viscAr*recip_drF(K)*( wVel(I,J,K,bi,bj)
     &                -wOverRide*wVel(I,J,Kp1,bi,bj) )
          ENDDO
         ENDDO
C        Divergence of fluxes
         DO J=J0,Jn
          DO I=I0,In
           gW(I,J,K,bi,bj) = 0.
     &      -(
     &        +_recip_dxF(I,J,bi,bj)*(
     &              Flx_EW(I+1,J,bi,bj)-Flx_EW(I,J,bi,bj) )
     &        +_recip_dyF(I,J,bi,bj)*(
     &              Flx_NS(I,J+1,bi,bj)-Flx_NS(I,J,bi,bj) )
     &        +recip_drC(K)         *(
     &              Flx_Up(I,J,bi,bj)  -Flx_Dn(I,J,bi,bj) )
     &       )
caja    * recip_hFacU(I,J,K,bi,bj)
caja   NOTE:  This should be included   
caja   but we need an hFacUW (above U points)
caja           and an hFacUS (above V points) too...
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

     
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=2,Nr
         DO j=J0,Jn
          DO i=I0,In
           wVel(i,j,k,bi,bj) = wVel(i,j,k,bi,bj) 
     &     +deltatMom*( ab15*gW(i,j,k,bi,bj)
     &                 +ab05*gWNM1(i,j,k,bi,bj) )
           IF (hFacC(I,J,K,bi,bj).EQ.0.) wVel(i,j,k,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
C--   This call is aesthetic: it makes the W field
C     consistent with the OBs but this has no algorithmic
C     impact. This is purely for diagnostic purposes.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          CALL OBCS_APPLY_W( bi, bj, K, wVel, myThid )
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS */

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END

