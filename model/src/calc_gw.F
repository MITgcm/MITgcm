C $Header: /u/gcmpack/MITgcm/model/src/calc_gw.F,v 1.26 2006/06/07 01:55:12 heimbach Exp $
C     !DESCRIPTION: \bv
C $Name:  $

c #include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GW
C     !INTERFACE:
      SUBROUTINE CALC_GW(
     I                    myTime, myIter, myThid )
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
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "NH_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
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
      _RL    fZon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fMer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    del2w(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     i,j,k - Loop counters
      INTEGER i,j,k, kP1
      _RL  wOverride
      _RS  hFacWtmp
      _RS  hFacStmp
      _RS  hFacCtmp
      _RS  recip_hFacCtmp
      _RL slipSideFac
      _RL tmp_VbarZ, tmp_UbarZ, tmp_WbarZ

      _RL  Half
      PARAMETER(Half=0.5D0)
CEOP

C Catch barotropic mode
      IF ( Nr .LT. 2 ) RETURN

      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy

C     Lateral friction (no-slip, free slip, or half slip):
      IF ( no_slip_sides ) THEN
        slipSideFac = -1. _d 0
      ELSE
        slipSideFac =  1. _d 0
      ENDIF
CML   half slip was used before ; keep the line for now, but half slip is
CML   not used anywhere in the code as far as I can see.
C        slipSideFac = 0. _d 0

C For each tile
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C Initialise gW to zero
        DO K=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gW(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO

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
C     horizontal bi-harmonic dissipation
         IF (momViscosity .AND. viscA4W.NE.0. ) THEN
C     calculate the horizontal Laplacian of vertical flow
C     Zonal flux d/dx W
          DO j=1-Oly,sNy+Oly
           fZon(1-Olx,j)=0.
           DO i=1-Olx+1,sNx+Olx
            fZon(i,j) = drF(k)*_hFacC(i,j,k,bi,bj)
     &           *_dyG(i,j,bi,bj)
     &           *_recip_dxC(i,j,bi,bj)
     &           *(wVel(i,j,k,bi,bj)-wVel(i-1,j,k,bi,bj))
#ifdef COSINEMETH_III
     &           *sqcosFacU(J,bi,bj)
#endif
           ENDDO
          ENDDO	
C     Meridional flux d/dy W
          DO i=1-Olx,sNx+Olx
           fMer(I,1-Oly)=0.
          ENDDO
          DO j=1-Oly+1,sNy+Oly
           DO i=1-Olx,sNx+Olx
            fMer(i,j) = drF(k)*_hFacC(i,j,k,bi,bj)
     &           *_dxG(i,j,bi,bj)
     &           *_recip_dyC(i,j,bi,bj)
     &           *(wVel(i,j,k,bi,bj)-wVel(i,j-1,k,bi,bj))
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &           *sqCosFacV(j,bi,bj)
#endif
#endif
           ENDDO
          ENDDO

C     del^2 W
C     Difference of zonal fluxes ...
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx-1
            del2w(i,j)=fZon(i+1,j)-fZon(i,j)
           ENDDO
           del2w(sNx+Olx,j)=0.
          ENDDO

C     ... add difference of meridional fluxes and divide by volume
          DO j=1-Oly,sNy+Oly-1
           DO i=1-Olx,sNx+Olx
C     First compute the fraction of open water for the w-control volume
C     at the southern face
            hFacCtmp=max( _hFacC(I,J,K-1,bi,bj)-Half,0. _d 0 )
     &           +   min( _hFacC(I,J,K  ,bi,bj)     ,Half )
            IF (hFacCtmp .GT. 0.) THEN
             recip_hFacCtmp = 1./hFacCtmp
            ELSE
             recip_hFacCtmp = 0. _d 0
            ENDIF
            del2w(i,j)=recip_rA(i,j,bi,bj)
     &           *recip_drC(k)*recip_hFacCtmp
     &           *(
     &           del2w(i,j)
     &           +( fMer(i,j+1)-fMer(i,j) )
     &           )
           ENDDO
          ENDDO
C-- No-slip BCs impose a drag at walls...
CML ************************************************************
CML   No-slip Boundary conditions for bi-harmonic dissipation
CML   need to be implemented here!
CML ************************************************************
         ELSE
C-    Initialize del2w to zero:
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            del2w(i,j) = 0. _d 0
           ENDDO
          ENDDO
         ENDIF

C Flux on Southern face
         DO J=jMin,jMax+1
          DO I=iMin,iMax
C     First compute the fraction of open water for the w-control volume
C     at the southern face
           hFacStmp=max(_hFacS(I,J,K-1,bi,bj)-Half,0. _d 0)
     &         +    min(_hFacS(I,J,K  ,bi,bj),Half)
           tmp_VbarZ=Half*(
     &          _hFacS(I,J,K-1,bi,bj)*vVel( I ,J,K-1,bi,bj)
     &         +_hFacS(I,J, K ,bi,bj)*vVel( I ,J, K ,bi,bj))
           Flx_NS(I,J,bi,bj)=
     &     tmp_VbarZ*Half*(wVel(I,J,K,bi,bj)+wVel(I,J-1,K,bi,bj))
     &    -viscAhW*_recip_dyC(I,J,bi,bj)
     &       *(hFacStmp*(wVel(I,J,K,bi,bj)-wVel(I,J-1,K,bi,bj))
     &        +(1. _d 0 - hFacStmp)*(1. _d 0 - slipSideFac)
     &         *wVel(I,J,K,bi,bj))
     &    +viscA4W*_recip_dyC(I,J,bi,bj)*(del2w(I,J)-del2w(I,J-1))
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &    *sqCosFacV(j,bi,bj)
#else
     &    *CosFacV(j,bi,bj)
#endif
#endif
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
           hFacWtmp=max(_hFacW(I,J,K-1,bi,bj)-Half,0. _d 0)
     &         +    min(_hFacW(I,J,K  ,bi,bj),Half)
           tmp_UbarZ=Half*(
     &         _hFacW(I,J,K-1,bi,bj)*uVel( I ,J,K-1,bi,bj)
     &        +_hFacW(I,J, K ,bi,bj)*uVel( I ,J, K ,bi,bj))
           Flx_EW(I,J,bi,bj)=
     &     tmp_UbarZ*Half*(wVel(I,J,K,bi,bj)+wVel(I-1,J,K,bi,bj))
     &    -viscAhW*_recip_dxC(I,J,bi,bj)
     &      *(hFacWtmp*(wVel(I,J,K,bi,bj)-wVel(I-1,J,K,bi,bj))
     &       +(1 _d 0 - hFacWtmp)*(1 _d 0 - slipSideFac)
     &        *wVel(I,J,K,bi,bj) )
     &    +viscA4W*_recip_dxC(I,J,bi,bj)*(del2w(I,J)-del2w(I-1,J))
#ifdef COSINEMETH_III
     &                *sqCosFacU(j,bi,bj)
#else
     &                *CosFacU(j,bi,bj)
#endif
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Compute effective gW_[n+1/2] terms (including Adams-Bashforth weights)
C     and save gW_[n] into gwNm1 for the next time step.
c#ifdef ALLOW_ADAMSBASHFORTH_3
c        CALL ADAMS_BASHFORTH3(
c    I                          bi, bj, k,
c    U                          gW, gwNm,
c    I                          momStartAB, myIter, myThid )
c#else /* ALLOW_ADAMSBASHFORTH_3 */
         CALL ADAMS_BASHFORTH2(
     I                          bi, bj, k,
     U                          gW, gwNm1,
     I                          myIter, myThid )
c#endif /* ALLOW_ADAMSBASHFORTH_3 */

C-    end of the k loop
        ENDDO

C-    end of bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
