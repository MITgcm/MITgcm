C $Header: /u/gcmpack/MITgcm/model/src/calc_gw.F,v 1.28 2006/07/07 20:09:37 jmc Exp $
C     !DESCRIPTION: \bv
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GW
C     !INTERFACE:
      SUBROUTINE CALC_GW(
     I               bi, bj, KappaRU, KappaRV,
     I               myTime, myIter, myThid )
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
C     bi,bj   :: current tile indices
C     KappaRU :: vertical viscosity at U points
C     KappaRV :: vertical viscosity at V points
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: Thread number for this instance of the routine.
      INTEGER bi,bj
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iMin, iMax,
C     jMin, jMax
C     flx_NS       :: Temp. used for fVol meridional terms.
C     flx_EW       :: Temp. used for fVol zonal terms.
C     flx_Up       :: Temp. used for fVol vertical terms.
C     flx_Dn       :: Temp. used for fVol vertical terms.
      INTEGER iMin,iMax,jMin,jMax
      _RL    flx_NS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_EW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_Dn(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_Up(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    del2w (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     i,j,k - Loop counters
      INTEGER i,j,k, kp1
      _RL  wOverride
      _RS  hFacWtmp
      _RS  hFacStmp
      _RS  hFacCtmp
      _RS  recip_hFacCtmp
      _RL  slipSideFac
      _RL  tmp_VbarZ, tmp_UbarZ, tmp_WbarZ
      _RL  viscLoc
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

C--   Initialise gW to zero
      DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gW(i,j,k,bi,bj) = 0.
         ENDDO
        ENDDO
      ENDDO

C--   Boundaries condition at top
      DO j=jMin,jMax
       DO i=iMin,iMax
         flx_Up(i,j)=0.
       ENDDO
      ENDDO

C---  Sweep down column
      DO k=2,Nr
        kp1=k+1
        wOverRide=1.
        IF (k.EQ.Nr) THEN
          kp1=Nr
          wOverRide=0.
        ENDIF
C--   horizontal bi-harmonic dissipation
        IF (momViscosity .AND. viscA4W.NE.0. ) THEN
C-    calculate the horizontal Laplacian of vertical flow
C     Zonal flux d/dx W
          DO j=1-Oly,sNy+Oly
           fZon(1-Olx,j)=0.
           DO i=1-Olx+1,sNx+Olx
C- Problem here: drF(k)*_hFacC & fZon are not at the same Horiz.&Vert. location
            fZon(i,j) = drF(k)*_hFacC(i,j,k,bi,bj)
     &           *_dyG(i,j,bi,bj)
     &           *_recip_dxC(i,j,bi,bj)
     &           *(wVel(i,j,k,bi,bj)-wVel(i-1,j,k,bi,bj))
#ifdef COSINEMETH_III
     &           *sqcosFacU(j,bi,bj)
#endif
           ENDDO
          ENDDO
C     Meridional flux d/dy W
          DO i=1-Olx,sNx+Olx
           fMer(i,1-Oly)=0.
          ENDDO
          DO j=1-Oly+1,sNy+Oly
           DO i=1-Olx,sNx+Olx
C- Problem here: drF(k)*_hFacC & fMer are not at the same Horiz.&Vert. location
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
            hFacCtmp=max( _hFacC(i,j,k-1,bi,bj)-Half,0. _d 0 )
     &           +   min( _hFacC(i,j,k  ,bi,bj)     ,Half )
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
        DO j=jMin,jMax+1
         DO i=iMin,iMax
C     First compute the fraction of open water for the w-control volume
C     at the southern face
           hFacStmp=max(_hFacS(i,j,k-1,bi,bj)-Half,0. _d 0)
     &         +    min(_hFacS(i,j,k  ,bi,bj),Half)
           tmp_VbarZ=Half*(
     &          _hFacS(i,j,k-1,bi,bj)*vVel( i ,j,k-1,bi,bj)
     &         +_hFacS(i,j, k ,bi,bj)*vVel( i ,j, k ,bi,bj))
           flx_NS(i,j)=
     &     tmp_VbarZ*Half*(wVel(i,j,k,bi,bj)+wVel(i,j-1,k,bi,bj))
     &    -(viscAh_W(i,j,k,bi,bj)+viscAh_W(i,j-1,k,bi,bj))*Half
     &       *_recip_dyC(i,j,bi,bj)
     &       *(hFacStmp*(wVel(i,j,k,bi,bj)-wVel(i,j-1,k,bi,bj))
C- Problem here: No-slip bc CANNOT be written in term of a flux
     &        +(1. _d 0 - hFacStmp)*(1. _d 0 - slipSideFac)
     &         *wVel(i,j,k,bi,bj))
     &    +(viscA4_W(i,j,k,bi,bj)+viscA4_W(i,j-1,k,bi,bj))*Half
     &      *_recip_dyC(i,j,bi,bj)*(del2w(i,j)-del2w(i,j-1))
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
CML     &       *wVel(i,j,k,bi,bi) + hFacStmp*wVel(i,j-1,k,bi,bj) )
         ENDDO
        ENDDO
C Flux on Western face
        DO j=jMin,jMax
         DO i=iMin,iMax+1
C     First compute the fraction of open water for the w-control volume
C     at the western face
           hFacWtmp=max(_hFacW(i,j,k-1,bi,bj)-Half,0. _d 0)
     &         +    min(_hFacW(i,j,k  ,bi,bj),Half)
           tmp_UbarZ=Half*(
     &         _hFacW(i,j,k-1,bi,bj)*uVel( i ,j,k-1,bi,bj)
     &        +_hFacW(i,j, k ,bi,bj)*uVel( i ,j, k ,bi,bj))
           flx_EW(i,j)=
     &     tmp_UbarZ*Half*(wVel(i,j,k,bi,bj)+wVel(i-1,j,k,bi,bj))
     &    -(viscAh_W(i,j,k,bi,bj)+viscAh_W(i-1,j,k,bi,bj))*Half
     &      *_recip_dxC(i,j,bi,bj)
     &      *(hFacWtmp*(wVel(i,j,k,bi,bj)-wVel(i-1,j,k,bi,bj))
C- Problem here: No-slip bc CANNOT be written in term of a flux
     &       +(1 _d 0 - hFacWtmp)*(1 _d 0 - slipSideFac)
     &        *wVel(i,j,k,bi,bj) )
     &    +(viscA4_W(i,j,k,bi,bj)+viscA4_W(i-1,j,k,bi,bj))*Half
     &      *_recip_dxC(i,j,bi,bj)*(del2w(i,j)-del2w(i-1,j))
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
CML     &       *wVel(i,j,k,bi,bi) + hFacWtmp*wVel(i-1,j,k,bi,bj) )
         ENDDO
        ENDDO
C Flux on Lower face
        DO j=jMin,jMax
         DO i=iMin,iMax
C Interpolate vert viscosity to W points
           viscLoc = ( KappaRU(i,j,k)  +KappaRU(i+1,j,k)
     &                +KappaRU(i,j,kp1)+KappaRU(i+1,j,kp1)
     &                +KappaRV(i,j,k)  +KappaRV(i,j+1,k)
     &                +KappaRV(i,j,kp1)+KappaRV(i,j+1,kp1)
     &               )*0.125 _d 0
           tmp_WbarZ = Half*( wVel(i,j, k ,bi,bj)
     &                       +wVel(i,j,kp1,bi,bj)*wOverRide )
           flx_Dn(i,j)=
     &                  tmp_WbarZ*tmp_WbarZ
     &                 -viscLoc*recip_drF(k)
     &                         *( wVel(i,j, k ,bi,bj)
     &                           -wVel(i,j,kp1,bi,bj)*wOverRide )
         ENDDO
        ENDDO
C       Divergence of fluxes
        DO j=jMin,jMax
         DO i=iMin,iMax
           gW(i,j,k,bi,bj) = 0.
     &      -(
     &        +_recip_dxF(i,j,bi,bj)*(
     &              flx_EW(i+1,j)-flx_EW(i,j) )
     &        +_recip_dyF(i,j,bi,bj)*(
     &              flx_NS(i,j+1)-flx_NS(i,j) )
     &        +recip_drC(k)         *(
     &              flx_Up(i,j)  -flx_Dn(i,j) )
     &       )
caja    * recip_hFacU(i,j,k,bi,bj)
caja   NOTE:  This should be included
caja   but we need an hFacUW (above U points)
caja           and an hFacUS (above V points) too...
C--        prepare for next level (k+1)
           flx_Up(i,j)=flx_Dn(i,j)
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Compute effective gW_[n+1/2] terms (including Adams-Bashforth weights)
C     and save gW_[n] into gwNm1 for the next time step.
c#ifdef ALLOW_ADAMSBASHFORTH_3
c       CALL ADAMS_BASHFORTH3(
c    I                         bi, bj, k,
c    U                         gW, gwNm,
c    I                         momStartAB, myIter, myThid )
c#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL ADAMS_BASHFORTH2(
     I                         bi, bj, k,
     U                         gW, gwNm1,
     I                         myIter, myThid )
c#endif /* ALLOW_ADAMSBASHFORTH_3 */

C-    end of the k loop
      ENDDO

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
