C $Header: /u/gcmpack/MITgcm/model/src/calc_gw.F,v 1.15 2004/05/25 07:48:22 mlosch Exp $
C     !DESCRIPTION: \bv
C $Name:  $

#include "PACKAGES_CONFIG.h"
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
      _RS  hFacWtmp
      _RS  hFacStmp
      _RL ab15,ab05
      _RL slipSideFac
      _RL tmp_VbarZ, tmp_UbarZ, tmp_WbarZ

      _RL  Half
      PARAMETER(Half=0.5D0)
CEOP

ceh3 needs an IF ( useNONHYDROSTATIC ) THEN

      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy

C     Adams-Bashforth timestepping weights
      ab15 =  1.5 _d 0 + abeps
      ab05 = -0.5 _d 0 - abeps

C     Lateral friction (no-slip, free slip, or half slip):
      IF ( no_slip_sides ) THEN
        slipSideFac = -1. _d 0
      ELSE
        slipSideFac =  1. _d 0 
      ENDIF
CML   half slip was used before ; keep it for now, but half slip is
CML   not used anywhere in the code as far as I can see
C        slipSideFac = 0. _d 0

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
        DO J=jMin,jMax
         DO I=iMin,iMax
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
         DO J=jMin,jMax+1
          DO I=iMin,iMax
C     First compute the fraction of open water for the w-control volume
C     at the southern face
           hFacStmp=max(hFacS(I,J,K-1,bi,bj)-Half,0)
     &         +    min(hFacS(I,J,K  ,bi,bj),Half)
           tmp_VbarZ=Half*(
     &          _hFacS(I,J,K-1,bi,bj)*vVel( I ,J,K-1,bi,bj)
     &         +_hFacS(I,J, K ,bi,bj)*vVel( I ,J, K ,bi,bj))
           Flx_NS(I,J,bi,bj)=
     &     tmp_VbarZ*Half*(wVel(I,J,K,bi,bj)+wVel(I,J-1,K,bi,bj))
     &    -viscAh*_recip_dyC(I,J,bi,bj)  
     &       *(hFacStmp*(wVel(I,J,K,bi,bj)-wVel(I,J-1,K,bi,bj))
     &        +(1. _d 0 - hFacStmp)*(1. _d 0 - slipSideFac)
     &         *wVel(I,J,K,bi,bj))
C     The last term is the weighted average of the viscous stress at the open
C     fraction of the w control volume and at the closed fraction of the
C     the control volume. A more compact but less intelligible version
C     of the last three lines is:
CML     &       *( (1 _d 0 - slipSideFac*(1 _d 0 - hFacStmp))
CML     &       *wVel(I,J,K,bi,bi) + hFacStmp*wVel(I,J-1,K,bi,bj) )
          ENDDO
         ENDDO
C Flux on Western face
         DO J=jMin,jMax
          DO I=iMin,iMax+1
C     First compute the fraction of open water for the w-control volume
C     at the western face
           hFacWtmp=max(hFacW(I,J,K-1,bi,bj)-Half,0)
     &         +    min(hFacW(I,J,K  ,bi,bj),Half)
                 tmp_UbarZ=Half*(
     &         _hFacW(I,J,K-1,bi,bj)*uVel( I ,J,K-1,bi,bj)
     &        +_hFacW(I,J, K ,bi,bj)*uVel( I ,J, K ,bi,bj))
           Flx_EW(I,J,bi,bj)=
     &     tmp_UbarZ*Half*(wVel(I,J,K,bi,bj)+wVel(I-1,J,K,bi,bj))
     &    -viscAh*_recip_dxC(I,J,bi,bj)
     &      *(hFacWtmp*(wVel(I,J,K,bi,bj)-wVel(I-1,J,K,bi,bj))
     &       +(1 _d 0 - hFacWtmp)*(1 _d 0 - slipSideFac)
     &        *wVel(I,J,K,bi,bj) )
C     The last term is the weighted average of the viscous stress at the open
C     fraction of the w control volume and at the closed fraction of the
C     the control volume. A more compact but less intelligible version
C     of the last three lines is:
CML     &       *( (1 _d 0 - slipSideFac*(1 _d 0 - hFacWtmp))
CML     &       *wVel(I,J,K,bi,bi) + hFacWtmp*wVel(I-1,J,K,bi,bj) )
          ENDDO
         ENDDO
C Flux on Lower face
         DO J=jMin,jMax
          DO I=iMin,iMax
           Flx_Up(I,J,bi,bj)=Flx_Dn(I,J,bi,bj)
           tmp_WbarZ=Half*(wVel(I,J,K,bi,bj) 
     &         +wOverRide*wVel(I,J,Kp1,bi,bj))
           Flx_Dn(I,J,bi,bj)=
     &     tmp_WbarZ*tmp_WbarZ
     &    -viscAr*recip_drF(K) 
     &       *( wVel(I,J,K,bi,bj)-wOverRide*wVel(I,J,Kp1,bi,bj) )
          ENDDO
         ENDDO
C        Divergence of fluxes
         DO J=jMin,jMax
          DO I=iMin,iMax
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
         DO j=jMin,jMax
          DO i=iMin,iMax
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
