C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_forcing_surf.F,v 1.3 2007/12/21 02:54:34 atn Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

CBOP
C !ROUTINE: KPP_FORCING_SURF

C !INTERFACE: ==========================================================
      SUBROUTINE KPP_FORCING_SURF(
     I     rhoSurf, surfForcU, surfForcV,
     I     surfForcT, surfForcS, surfForcTice, 
     I     Qsw, 
#ifdef ALLOW_SALT_PLUME
     I     saltPlumeFlux,
#endif /* ALLOW_SALT_PLUME */
     I     ttalpha, ssbeta,  
     O     ustar, bo, bosol, 
#ifdef ALLOW_SALT_PLUME
     O     boplume,
#endif /* ALLOW_SALT_PLUME */
     O     dVsq,
     I     ikppkey, iMin, iMax, jMin, jMax, bi, bj, myTime, myThid )

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE KPP_FORCING_SURF                              |
C     | o Compute all surface related KPP fields:                |
C     |   - friction velocity ustar                              |
C     |   - turbulent and radiative surface buoyancy forcing,    |
C     |     bo and bosol, and surface haline buoyancy forcing    |
C     |     boplume                                              |
C     |   - velocity shear relative to surface squared (this is  |
C     |     not really a surface affected quantity unless it is  |
C     |     computed with respect to some resolution independent |
C     |     reference level, that is KPP_ESTIMATE_UREF defined ) |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

c     taux / rho = surfForcU                               (N/m^2)
c     tauy / rho = surfForcV                               (N/m^2)
c     ustar = sqrt( sqrt( taux^2 + tauy^2 ) / rho )        (m/s)
c     bo    = - g * ( alpha*surfForcT +
c                     beta *surfForcS ) / rho              (m^2/s^3)
c     bosol = - g * alpha * Qsw * drF(1) / rho             (m^2/s^3)
c     boplume = g * ( beta *saltPlumeFlux/rhoConst )/rho   (m^2/s^3) 
c------------------------------------------------------------------------

c \ev

C !USES: ===============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "KPP_PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C Routine arguments
C     ikppkeyb - key for storing trajectory for adjoint (taf)
c     imin, imax, jmin, jmax  - array computation indices
C     bi, bj - array indices on which to apply calculations
C     myTime - Current time in simulation
C     myThid - Current thread id
c     rhoSurf- density of surface layer                            (kg/m^3)
C     surfForcU     units are  r_unit.m/s^2 (=m^2/s^2 if r=z)
C     surfForcV     units are  r_unit.m/s^2 (=m^2/s^-2 if r=z)
C     surfForcS     units are  r_unit.psu/s (=psu.m/s if r=z)
C            - EmPmR * S_surf plus salinity relaxation*drF(1)
C     surfForcT     units are  r_unit.Kelvin/s (=Kelvin.m/s if r=z)
C            - Qnet (+Qsw) plus temp. relaxation*drF(1)
C                -> calculate        -lambda*(T(model)-T(clim))
C            Qnet assumed to be net heat flux including ShortWave rad.
C     surfForcTice
C            - equivalent Temperature flux in the top level that corresponds
C              to the melting or freezing of sea-ice.
C              Note that the surface level temperature is modified
C              directly by the sea-ice model in order to maintain
C              water temperature under sea-ice at the freezing
C              point.  But we need to keep track of the
C              equivalent amount of heat that this surface-level
C              temperature change implies because it is used by
C              the KPP package (kpp_calc.F and kpp_transport_t.F).
C              Units are r_unit.K/s (=Kelvin.m/s if r=z) (>0 for ocean warming).
C
C     Qsw     - surface shortwave radiation (upwards positive)
C     saltPlumeFlux - salt rejected during freezing (downward = positive)
C     ttalpha - thermal expansion coefficient without 1/rho factor
C               d(rho{k,k})/d(T(k))                           (kg/m^3/C)
C     ssbeta  - salt expansion coefficient without 1/rho factor
C               d(rho{k,k})/d(S(k))                         (kg/m^3/PSU)
C !OUTPUT PARAMETERS: 
C     ustar  (nx,ny)       - surface friction velocity                  (m/s)
C     bo     (nx,ny)       - surface turbulent buoyancy forcing     (m^2/s^3)
C     bosol  (nx,ny)       - surface radiative buoyancy forcing     (m^2/s^3)
C     boplume(nx,ny)       - surface haline buoyancy forcing        (m^2/s^3)
C     dVsq   (nx,ny,Nr)    - velocity shear re surface squared
C                            at grid levels for bldepth             (m^2/s^2)

      INTEGER ikppkey
      INTEGER iMin, iMax, jMin, jMax
      INTEGER bi, bj
      INTEGER myThid
      _RL     myTime

      _RL rhoSurf     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfForcU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL surfForcV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL surfForcT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL surfForcS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL surfForcTice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Qsw         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL TTALPHA     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrp1)
      _RL SSBETA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrp1)

      _RL ustar ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL bo    ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL bosol ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
#ifdef ALLOW_SALT_PLUME
      _RL saltPlumeFlux   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL boplume(1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
#endif /* ALLOW_SALT_PLUME */
      _RL dVsq  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr            )

C !LOCAL VARIABLES: ====================================================
c Local constants
c     minusone, p0, p5, p25, p125, p0625
      _RL        p0    , p5    , p125      
      parameter( p0=0.0, p5=0.5, p125=0.125 )
      integer i, j, k, im1, ip1, jm1, jp1
      _RL tempvar2

      _RL work3 ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )

#ifdef KPP_ESTIMATE_UREF
      _RL tempvar1, dBdz1, dBdz2, ustarX, ustarY
      _RL z0    ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL zRef  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL uRef  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL vRef  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
#endif
CEOP

c------------------------------------------------------------------------
c     friction velocity, turbulent and radiative surface buoyancy forcing
c     -------------------------------------------------------------------
c     taux / rho = surfForcU                               (N/m^2)
c     tauy / rho = surfForcV                               (N/m^2)
c     ustar = sqrt( sqrt( taux^2 + tauy^2 ) / rho )        (m/s)
c     bo    = - g * ( alpha*surfForcT +
c                     beta *surfForcS ) / rho            (m^2/s^3)
c     bosol = - g * alpha * Qsw * drF(1) / rho           (m^2/s^3)
c     boplume = g * ( beta *saltPlumeFlux/rhoConst )/rho (m^2/s^3)
c------------------------------------------------------------------------

c initialize arrays to zero
      DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
            ustar(i,j) = p0
            bo   (I,J) = p0
            bosol(I,J) = p0
#ifdef ALLOW_SALT_PLUME
            boplume(I,J) = p0
#endif /* ALLOW_SALT_PLUME */
         END DO
      END DO

      DO j = jmin, jmax
       jp1 = j + 1
       DO i = imin, imax
        ip1 = i+1
        work3(i,j) =
     &   (surfForcU(i,j,bi,bj) + surfForcU(ip1,j,bi,bj)) *
     &   (surfForcU(i,j,bi,bj) + surfForcU(ip1,j,bi,bj)) +
     &   (surfForcV(i,j,bi,bj) + surfForcV(i,jp1,bi,bj)) *
     &   (surfForcV(i,j,bi,bj) + surfForcV(i,jp1,bi,bj))
       END DO
      END DO
cph(
CADJ store work3 = comlev1_kpp, key = ikppkey
cph)
      DO j = jmin, jmax
       jp1 = j + 1
       DO i = imin, imax
        ip1 = i+1

        if ( work3(i,j) .lt. (phepsi*phepsi*drF(1)*drF(1)) ) then
           ustar(i,j) = SQRT( phepsi * p5 * drF(1) )
        else
           tempVar2 =  SQRT( work3(i,j) ) * p5
           ustar(i,j) = SQRT( tempVar2 )
        endif

       END DO
      END DO

      DO j = jmin, jmax
       jp1 = j + 1
       DO i = imin, imax
        ip1 = i+1
        bo(I,J) = - gravity *
     &       ( TTALPHA(I,J,1) * (surfForcT(i,j,bi,bj)+
     &       surfForcTice(i,j,bi,bj)) +
     &       SSBETA(I,J,1) * surfForcS(i,j,bi,bj) )
     &       / rhoSurf(I,J)
        bosol(I,J) = gravity * TTALPHA(I,J,1) * Qsw(i,j,bi,bj) *
     &       recip_Cp*recip_rhoConst
     &       / rhoSurf(I,J)
       END DO
      END DO

#ifdef ALLOW_SALT_PLUME
      IF ( useSALT_PLUME ) THEN
         DO j = jmin, jmax
            jp1 = j + 1
            DO i = imin, imax
               ip1 = i+1
               boplume(I,J) = - gravity * SSBETA(I,J,1)
     &              * saltPlumeFlux(i,j,bi,bj)
     &              * recip_rhoConst / rhoSurf(I,J)
            END DO
         END DO
      ENDIF
#endif /* ALLOW_SALT_PLUME */

cph(
CADJ store ustar = comlev1_kpp, key = ikppkey
cph)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(bo     ,'KPPbo   ',0,1,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(bosol  ,'KPPbosol',0,1,1,bi,bj,myThid)
#ifdef ALLOW_SALT_PLUME
         CALL DIAGNOSTICS_FILL(boplume,'KPPboplm',0,1,1,bi,bj,myThid)
#endif /* ALLOW_SALT_PLUME */
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

c------------------------------------------------------------------------
c     velocity shear
c     --------------
c     Get velocity shear squared, averaged from "u,v-grid"
c     onto "t-grid" (in (m/s)**2):
c     dVsq(k)=(Uref-U(k))**2+(Vref-V(k))**2      at grid levels
c------------------------------------------------------------------------

c initialize arrays to zero
      DO k = 1, Nr
       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
         dVsq(i,j,k) = p0
        END DO
       END DO
      END DO

c     dVsq computation

#ifdef KPP_ESTIMATE_UREF

c     Get rid of vertical resolution dependence of dVsq term by
c     estimating a surface velocity that is independent of first level
c     thickness in the model.  First determine mixed layer depth hMix.
c     Second zRef = espilon * hMix.  Third determine roughness length
c     scale z0.  Third estimate reference velocity.

      DO j = jmin, jmax
       jp1 = j + 1
       DO i = imin, imax
        ip1 = i + 1

c     Determine mixed layer depth hMix as the shallowest depth at which
c     dB/dz exceeds 5.2e-5 s^-2.
        work1(i,j) = nzmax(i,j,bi,bj)
        DO k = 1, Nr
         IF ( k .LT. nzmax(i,j,bi,bj) .AND.
     &        maskC(I,J,k,bi,bj) .GT. 0. .AND.
     &        dbloc(i,j,k) / drC(k+1) .GT. dB_dz )
     &        work1(i,j) = k
        ENDDO

c     Linearly interpolate to find hMix.
        k = work1(i,j)
        IF ( k .EQ. 0 .OR. nzmax(i,j,bi,bj) .EQ. 1 ) THEN
         zRef(i,j) = p0
        ELSEIF ( k .EQ. 1) THEN
         dBdz2 = dbloc(i,j,1) / drC(2)
         zRef(i,j) = drF(1) * dB_dz / dBdz2
        ELSEIF ( k .LT. nzmax(i,j,bi,bj) ) THEN
         dBdz1 = dbloc(i,j,k-1) / drC(k  )
         dBdz2 = dbloc(i,j,k  ) / drC(k+1)
         zRef(i,j) = rF(k) + drF(k) * (dB_dz - dBdz1) /
     &        MAX ( phepsi, dBdz2 - dBdz1 )
        ELSE
         zRef(i,j) = rF(k+1)
        ENDIF
        
c     Compute roughness length scale z0 subject to 0 < z0
        tempVar1 = p5 * (
     &       (uVel(i,  j,  1,bi,bj)-uVel(i,  j,  2,bi,bj)) *
     &       (uVel(i,  j,  1,bi,bj)-uVel(i,  j,  2,bi,bj)) +
     &       (uVel(ip1,j,  1,bi,bj)-uVel(ip1,j,  2,bi,bj)) *
     &       (uVel(ip1,j,  1,bi,bj)-uVel(ip1,j,  2,bi,bj)) +
     &       (vVel(i,  j,  1,bi,bj)-vVel(i,  j,  2,bi,bj)) *
     &       (vVel(i,  j,  1,bi,bj)-vVel(i,  j,  2,bi,bj)) + 
     &       (vVel(i,  jp1,1,bi,bj)-vVel(i,  jp1,2,bi,bj)) *
     &       (vVel(i,  jp1,1,bi,bj)-vVel(i,  jp1,2,bi,bj)) )
        IF ( tempVar1 .lt. (epsln*epsln) ) THEN
         tempVar2 = epsln
        ELSE
         tempVar2 = SQRT ( tempVar1 )
        ENDIF
        z0(i,j) = rF(2) *
     &       ( rF(3) * LOG ( rF(3) / rF(2) ) /
     &       ( rF(3) - rF(2) ) -
     &       tempVar2 * vonK /
     &       MAX ( ustar(i,j), phepsi ) )
        z0(i,j) = MAX ( z0(i,j), phepsi )

c     zRef is set to 0.1 * hMix subject to z0 <= zRef <= drF(1)
        zRef(i,j) = MAX ( epsilon * zRef(i,j), z0(i,j) )
        zRef(i,j) = MIN ( zRef(i,j), drF(1) )
        
c     Estimate reference velocity uRef and vRef.
        uRef(i,j) = p5 * ( uVel(i,j,1,bi,bj) + uVel(ip1,j,1,bi,bj) )
        vRef(i,j) = p5 * ( vVel(i,j,1,bi,bj) + vVel(i,jp1,1,bi,bj) )
        IF ( zRef(i,j) .LT. drF(1) ) THEN
         ustarX = ( surfForcU(i,  j,bi,bj) + 
     &        surfForcU(ip1,j,bi,bj) ) * p5 *recip_drF(1)
         ustarY = ( surfForcV(i,j,  bi,bj) +
     &        surfForcV(i,jp1,bi,bj) ) * p5 *recip_drF(1)
         tempVar1 = ustarX * ustarX + ustarY * ustarY
         if ( tempVar1 .lt. (epsln*epsln) ) then
          tempVar2 = epsln
         else
          tempVar2 = SQRT ( tempVar1 )
         endif
         tempVar2 = ustar(i,j) *
     &        ( LOG ( zRef(i,j) / rF(2) ) +
     &        z0(i,j) / zRef(i,j) - z0(i,j) / rF(2) ) /
     &        vonK / tempVar2
         uRef(i,j) = uRef(i,j) + ustarX * tempVar2
         vRef(i,j) = vRef(i,j) + ustarY * tempVar2
        ENDIF
        
       ENDDO
      ENDDO

      DO k = 1, Nr
       DO j = jmin, jmax
        jm1 = j - 1
        jp1 = j + 1
        DO i = imin, imax
         im1 = i - 1
         ip1 = i + 1
         dVsq(i,j,k) = p5 * (
     $        (uRef(i,j) - uVel(i,  j,  k,bi,bj)) *
     $        (uRef(i,j) - uVel(i,  j,  k,bi,bj)) +
     $        (uRef(i,j) - uVel(ip1,j,  k,bi,bj)) *
     $        (uRef(i,j) - uVel(ip1,j,  k,bi,bj)) +
     $        (vRef(i,j) - vVel(i,  j,  k,bi,bj)) *
     $        (vRef(i,j) - vVel(i,  j,  k,bi,bj)) + 
     $        (vRef(i,j) - vVel(i,  jp1,k,bi,bj)) *
     $        (vRef(i,j) - vVel(i,  jp1,k,bi,bj)) )
#ifdef KPP_SMOOTH_DVSQ
         dVsq(i,j,k) = p5 * dVsq(i,j,k) + p125 * (
     $        (uRef(i,j) - uVel(i,  jm1,k,bi,bj)) *
     $        (uRef(i,j) - uVel(i,  jm1,k,bi,bj)) +
     $        (uRef(i,j) - uVel(ip1,jm1,k,bi,bj)) *
     $        (uRef(i,j) - uVel(ip1,jm1,k,bi,bj)) +
     $        (uRef(i,j) - uVel(i,  jp1,k,bi,bj)) *
     $        (uRef(i,j) - uVel(i,  jp1,k,bi,bj)) +
     $        (uRef(i,j) - uVel(ip1,jp1,k,bi,bj)) *
     $        (uRef(i,j) - uVel(ip1,jp1,k,bi,bj)) +
     $        (vRef(i,j) - vVel(im1,j,  k,bi,bj)) *
     $        (vRef(i,j) - vVel(im1,j,  k,bi,bj)) + 
     $        (vRef(i,j) - vVel(im1,jp1,k,bi,bj)) *
     $        (vRef(i,j) - vVel(im1,jp1,k,bi,bj)) +
     $        (vRef(i,j) - vVel(ip1,j,  k,bi,bj)) *
     $        (vRef(i,j) - vVel(ip1,j,  k,bi,bj)) + 
     $        (vRef(i,j) - vVel(ip1,jp1,k,bi,bj)) *
     $        (vRef(i,j) - vVel(ip1,jp1,k,bi,bj)) )
#endif /* KPP_SMOOTH_DVSQ */
        ENDDO
       ENDDO
      ENDDO

#else /* KPP_ESTIMATE_UREF */

      DO k = 1, Nr
       DO j = jmin, jmax
        jm1 = j - 1
        jp1 = j + 1
        DO i = imin, imax
         im1 = i - 1
         ip1 = i + 1
         dVsq(i,j,k) = p5 * (
     $        (uVel(i,  j,  1,bi,bj)-uVel(i,  j,  k,bi,bj)) *
     $        (uVel(i,  j,  1,bi,bj)-uVel(i,  j,  k,bi,bj)) +
     $        (uVel(ip1,j,  1,bi,bj)-uVel(ip1,j,  k,bi,bj)) *
     $        (uVel(ip1,j,  1,bi,bj)-uVel(ip1,j,  k,bi,bj)) +
     $        (vVel(i,  j,  1,bi,bj)-vVel(i,  j,  k,bi,bj)) *
     $        (vVel(i,  j,  1,bi,bj)-vVel(i,  j,  k,bi,bj)) + 
     $        (vVel(i,  jp1,1,bi,bj)-vVel(i,  jp1,k,bi,bj)) *
     $        (vVel(i,  jp1,1,bi,bj)-vVel(i,  jp1,k,bi,bj)) )
#ifdef KPP_SMOOTH_DVSQ
         dVsq(i,j,k) = p5 * dVsq(i,j,k) + p125 * (
     $        (uVel(i,  jm1,1,bi,bj)-uVel(i,  jm1,k,bi,bj)) *
     $        (uVel(i,  jm1,1,bi,bj)-uVel(i,  jm1,k,bi,bj)) +
     $        (uVel(ip1,jm1,1,bi,bj)-uVel(ip1,jm1,k,bi,bj)) *
     $        (uVel(ip1,jm1,1,bi,bj)-uVel(ip1,jm1,k,bi,bj)) +
     $        (uVel(i,  jp1,1,bi,bj)-uVel(i,  jp1,k,bi,bj)) *
     $        (uVel(i,  jp1,1,bi,bj)-uVel(i,  jp1,k,bi,bj)) +
     $        (uVel(ip1,jp1,1,bi,bj)-uVel(ip1,jp1,k,bi,bj)) *
     $        (uVel(ip1,jp1,1,bi,bj)-uVel(ip1,jp1,k,bi,bj)) +
     $        (vVel(im1,j,  1,bi,bj)-vVel(im1,j,  k,bi,bj)) *
     $        (vVel(im1,j,  1,bi,bj)-vVel(im1,j,  k,bi,bj)) + 
     $        (vVel(im1,jp1,1,bi,bj)-vVel(im1,jp1,k,bi,bj)) *
     $        (vVel(im1,jp1,1,bi,bj)-vVel(im1,jp1,k,bi,bj)) +
     $        (vVel(ip1,j,  1,bi,bj)-vVel(ip1,j,  k,bi,bj)) *
     $        (vVel(ip1,j,  1,bi,bj)-vVel(ip1,j,  k,bi,bj)) + 
     $        (vVel(ip1,jp1,1,bi,bj)-vVel(ip1,jp1,k,bi,bj)) *
     $        (vVel(ip1,jp1,1,bi,bj)-vVel(ip1,jp1,k,bi,bj)) )
#endif /* KPP_SMOOTH_DVSQ */
        ENDDO
       ENDDO
      ENDDO

#endif /* KPP_ESTIMATE_UREF */

      RETURN
      END

