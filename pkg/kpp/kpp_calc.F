C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_calc.F,v 1.9 2001/09/27 18:08:57 adcroft Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

      subroutine KPP_CALC(
     I     bi, bj, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE KPP_CALC                                      |
C     | o Compute all KPP fields defined in KPP.h                |
C     |==========================================================|
C     | This subroutine serves as an interface between MITGCMUV  |
C     | code and NCOM 1-D routines in kpp_routines.F             |
C     \==========================================================/
      IMPLICIT NONE

c=======================================================================
c
c     written  by  : jan morzel, august  11, 1994
c     modified by  : jan morzel, january 25, 1995 : "dVsq" and 1d code
c                    detlef stammer, august, 1997 : for MIT GCM Classic
c                    d. menemenlis,    july, 1998 : for MIT GCM UV
c
c     compute vertical mixing coefficients based on the k-profile
c     and oceanic planetary boundary layer scheme by large & mcwilliams.
c
c     summary:
c     - compute interior mixing everywhere:
c       interior mixing gets computed at all interfaces due to constant
c       internal wave background activity ("fkpm" and "fkph"), which
c       is enhanced in places of static instability (local richardson
c       number < 0).
c       Additionally, mixing can be enhanced by adding contribution due
c       to shear instability which is a function of the local richardson
c       number 
c     - double diffusivity:
c       interior mixing can be enhanced by double diffusion due to salt
c       fingering and diffusive convection (ifdef "kmixdd").
c     - kpp scheme in the boundary layer:
c 
c       a.boundary layer depth:
c         at every gridpoint the depth of the oceanic boundary layer 
c         ("hbl") gets computed by evaluating bulk richardson numbers.
c       b.boundary layer mixing:
c         within the boundary layer, above hbl, vertical mixing is 
c         determined by turbulent surface fluxes, and interior mixing at
c         the lower boundary, i.e. at hbl.
c     
c     this subroutine provides the interface between the MIT GCM UV and the 
c     subroutine "kppmix", where boundary layer depth, vertical 
c     viscosity, vertical diffusivity, and counter gradient term (ghat)
c     are computed slabwise.
c     note: subroutine "kppmix" uses m-k-s units.
c
c     time level:
c     input tracer and velocity profiles are evaluated at time level 
c     tau, surface fluxes come from tau or tau-1.
c
c     grid option:
c     in this "1-grid" implementation, diffusivity and viscosity
c     profiles are computed on the "t-grid" (by using velocity shear
c     profiles averaged from the "u,v-grid" onto the "t-grid"; note, that
c     the averaging includes zero values on coastal and seafloor grid 
c     points).  viscosity on the "u,v-grid" is computed by averaging the 
c     "t-grid" viscosity values onto the "u,v-grid".
c
c     vertical grid:
c     mixing coefficients get evaluated at the bottom of the lowest 
c     layer, i.e., at depth zw(Nr).  these values are only useful when 
c     the model ocean domain does not include the entire ocean down to
c     the seafloor ("upperocean" setup) and allows flux through the
c     bottom of the domain.  for full-depth runs, these mixing 
c     coefficients are being zeroed out before leaving this subroutine.
c
c-------------------------------------------------------------------------

c global parameters updated by kpp_calc
c     KPPviscAz   - KPP eddy viscosity coefficient                 (m^2/s)
c     KPPdiffKzT  - KPP diffusion coefficient for temperature      (m^2/s)
c     KPPdiffKzS  - KPP diffusion coefficient for salt and tracers (m^2/s)
c     KPPghat     - Nonlocal transport coefficient                 (s/m^2)
c     KPPhbl      - Boundary layer depth on "t-grid"                   (m)
c     KPPfrac     - Fraction of short-wave flux penetrating mixing layer

c--   KPP_CALC computes vertical viscosity and diffusivity for region
c     (-2:sNx+3,-2:sNy+3) as required by CALC_DIFFUSIVITY and requires
c     values of uVel, vVel, SurfaceTendencyU, SurfaceTendencyV in the
c     region (-2:sNx+4,-2:sNy+4).
c     Hence overlap region needs to be set OLx=4, OLy=4.
c     When option FRUGAL_KPP is used, computation in overlap regions
c     is replaced with exchange calls hence reducing overlap requirements
c     to OLx=1, OLy=1.

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#else /* ALLOW_AUTODIFF_TAMC */
      integer ikey
#endif /* ALLOW_AUTODIFF_TAMC */

      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

c Routine arguments
c     bi, bj - array indices on which to apply calculations
c     myTime - Current time in simulation

      INTEGER bi, bj
      INTEGER myThid
      _RL     myTime

#ifdef ALLOW_KPP

c Local constants
c     minusone, p0, p5, p25, p125, p0625
c     imin, imax, jmin, jmax  - array computation indices

      _RL        minusone
      parameter( minusone=-1.0)
      _KPP_RL    p0    , p5    , p25     , p125      , p0625
      parameter( p0=0.0, p5=0.5, p25=0.25, p125=0.125, p0625=0.0625 )
      integer    imin      , imax        , jmin      , jmax
#ifdef FRUGAL_KPP
      parameter( imin=1    , imax=sNx    , jmin=1    , jmax=sNy     )
#else
      parameter( imin=-2   , imax=sNx+3  , jmin=-2   , jmax=sNy+3   )
#endif

c Local arrays and variables
c     work?  (nx,ny)       - horizontal working arrays
c     ustar  (nx,ny)       - surface friction velocity                  (m/s)
c     bo     (nx,ny)       - surface turbulent buoyancy forcing     (m^2/s^3)
c     bosol  (nx,ny)       - surface radiative buoyancy forcing     (m^2/s^3)
c     shsq   (nx,ny,Nr)    - local velocity shear squared
c                            at interfaces for ri_iwmix             (m^2/s^2)
c     dVsq   (nx,ny,Nr)    - velocity shear re surface squared
c                            at grid levels for bldepth             (m^2/s^2)
c     dbloc  (nx,ny,Nr)    - local delta buoyancy at interfaces
c                            for ri_iwmix and bldepth                 (m/s^2)
c     Ritop  (nx,ny,Nr)    - numerator of bulk richardson number
c                            at grid levels for bldepth
c     vddiff (nx,ny,Nrp2,1)- vertical viscosity on "t-grid"           (m^2/s)
c     vddiff (nx,ny,Nrp2,2)- vert. diff. on next row for temperature  (m^2/s)
c     vddiff (nx,ny,Nrp2,3)- vert. diff. on next row for salt&tracers (m^2/s)
c     ghat   (nx,ny,Nr)    - nonlocal transport coefficient           (s/m^2)
c     hbl    (nx,ny)       - mixing layer depth                           (m)
c     kmtj   (nx,ny)       - maximum number of wet levels in each column
c     z0     (nx,ny)       - Roughness length                             (m)
c     zRef   (nx,ny)       - Reference depth: Hmix * epsilon              (m)
c     uRef   (nx,ny)       - Reference zonal velocity                   (m/s)
c     vRef   (nx,ny)       - Reference meridional velocity              (m/s)

      _RL     worka ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      integer work1 ( ibot:itop    , jbot:jtop                    )
      _KPP_RL work2 ( ibot:itop    , jbot:jtop                    )
      _KPP_RL ustar ( ibot:itop    , jbot:jtop                    )
      _KPP_RL bo    ( ibot:itop    , jbot:jtop                    )
      _KPP_RL bosol ( ibot:itop    , jbot:jtop                    )
      _KPP_RL shsq  ( ibot:itop    , jbot:jtop    , Nr            )
      _KPP_RL dVsq  ( ibot:itop    , jbot:jtop    , Nr            )
      _KPP_RL dbloc ( ibot:itop    , jbot:jtop    , Nr            )
      _KPP_RL Ritop ( ibot:itop    , jbot:jtop    , Nr            )
      _KPP_RL vddiff( ibot:itop    , jbot:jtop    , 0:Nrp1, mdiff )
      _KPP_RL ghat  ( ibot:itop    , jbot:jtop    , Nr            )
      _KPP_RL hbl   ( ibot:itop    , jbot:jtop                    )
#ifdef KPP_ESTIMATE_UREF
      _KPP_RL z0    ( ibot:itop    , jbot:jtop                    )
      _KPP_RL zRef  ( ibot:itop    , jbot:jtop                    )
      _KPP_RL uRef  ( ibot:itop    , jbot:jtop                    )
      _KPP_RL vRef  ( ibot:itop    , jbot:jtop                    )
#endif /* KPP_ESTIMATE_UREF */
      
      _KPP_RL tempvar1, tempvar2
      integer i, j, k, kp1, im1, ip1, jm1, jp1

#ifdef KPP_ESTIMATE_UREF
      _KPP_RL dBdz1, dBdz2, ustarX, ustarY
#endif

c     Check to see if new vertical mixing coefficient should be computed now?
      IF ( DIFFERENT_MULTIPLE(kpp_freq,myTime,myTime-deltaTClock) .OR.
     1     myTime .EQ. startTime ) THEN
         
c-----------------------------------------------------------------------
c     prepare input arrays for subroutine "kppmix" to compute
c     viscosity and diffusivity and ghat.
c     All input arrays need to be in m-k-s units.
c
c     note: for the computation of the bulk richardson number in the
c     "bldepth" subroutine, gradients of velocity and buoyancy are
c     required at every depth. in the case of very fine vertical grids
c     (thickness of top layer < 2m), the surface reference depth must
c     be set to zref=epsilon/2*zgrid(k), and the reference value
c     of velocity and buoyancy must be computed as vertical average
c     between the surface and 2*zref.  in the case of coarse vertical
c     grids zref is zgrid(1)/2., and the surface reference value is
c     simply the surface value at zgrid(1).
c-----------------------------------------------------------------------

c------------------------------------------------------------------------
c     density related quantities
c     --------------------------
c
c      work2   - density of surface layer                        (kg/m^3)
c      dbloc   - local buoyancy gradient at Nr interfaces
c                g/rho{k+1,k+1} * [ drho{k,k+1}-drho{k+1,k+1} ]   (m/s^2)
c      dbsfc (stored in Ritop to conserve stack memory)
c              - buoyancy difference with respect to the surface
c                g * [ drho{1,k}/rho{1,k} - drho{k,k}/rho{k,k} ]  (m/s^2)
c      ttalpha (stored in vddiff(:,:,:,1) to conserve stack memory)
c              - thermal expansion coefficient without 1/rho factor
c                d(rho{k,k})/d(T(k))                           (kg/m^3/C)
c      ssbeta (stored in vddiff(:,:,:,2) to conserve stack memory)
c              - salt expansion coefficient without 1/rho factor
c                d(rho{k,k})/d(S(k))                         (kg/m^3/PSU)
c------------------------------------------------------------------------

      CALL TIMER_START('STATEKPP      [KPP_CALC]', myThid)
      CALL STATEKPP(
     I       bi, bj, myThid
     O     , work2, dbloc, Ritop
     O     , vddiff(ibot,jbot,1,1), vddiff(ibot,jbot,1,2)
     &     )
      CALL TIMER_STOP ('STATEKPP      [KPP_CALC]', myThid)

      DO k = 1, Nr
         DO j = jbot, jtop
            DO i = ibot, itop
               ghat(i,j,k) = dbloc(i,j,k)
            ENDDO
         ENDDO
      ENDDO

#ifdef KPP_SMOOTH_DBLOC
c     horizontally smooth dbloc with a 121 filter
c     smooth dbloc stored in ghat to save space
c     dbloc(k) is buoyancy gradientnote between k and k+1
c     levels therefore k+1 mask must be used

      DO k = 1, Nr-1
         CALL KPP_SMOOTH_HORIZ (
     I        k+1, bi, bj,
     U        ghat (ibot,jbot,k) )
      ENDDO

#endif /* KPP_SMOOTH_DBLOC */

#ifdef KPP_SMOOTH_DENS
c     horizontally smooth density related quantities with 121 filters
      CALL KPP_SMOOTH_HORIZ (
     I     1, bi, bj,
     U     work2 )
      DO k = 1, Nr
         CALL KPP_SMOOTH_HORIZ (
     I        k+1, bi, bj,
     U        dbloc (ibot,jbot,k) )
         CALL KPP_SMOOTH_HORIZ (
     I        k, bi, bj,
     U        Ritop (ibot,jbot,k)  )
         CALL KPP_SMOOTH_HORIZ (
     I        k, bi, bj,
     U        vddiff(ibot,jbot,k,1) )
         CALL KPP_SMOOTH_HORIZ (
     I        k, bi, bj,
     U        vddiff(ibot,jbot,k,2) )
      ENDDO
#endif /* KPP_SMOOTH_DENS */

      DO k = 1, Nr
         DO j = jbot, jtop
            DO i = ibot, itop

c     zero out dbloc over land points (so that the convective
c     part of the interior mixing can be diagnosed)
               dbloc(i,j,k) = dbloc(i,j,k) * pMask(i,j,k,bi,bj)
               ghat(i,j,k)  = ghat(i,j,k)  * pMask(i,j,k,bi,bj)
               Ritop(i,j,k) = Ritop(i,j,k) * pMask(i,j,k,bi,bj)
               if(k.eq.nzmax(i,j,bi,bj)) then
                  dbloc(i,j,k) = p0
                  ghat(i,j,k)  = p0
                  Ritop(i,j,k) = p0
               endif

c     numerator of bulk richardson number on grid levels
c     note: land and ocean bottom values need to be set to zero
c     so that the subroutine "bldepth" works correctly
               Ritop(i,j,k) = (zgrid(1)-zgrid(k)) * Ritop(i,j,k)

            END DO
         END DO
      END DO

c------------------------------------------------------------------------
c     friction velocity, turbulent and radiative surface buoyancy forcing
c     -------------------------------------------------------------------
c     taux / rho = SurfaceTendencyU * delZ(1)                     (N/m^2)
c     tauy / rho = SurfaceTendencyV * delZ(1)                     (N/m^2)
c     ustar = sqrt( sqrt( taux^2 + tauy^2 ) / rho )                 (m/s)
c     bo    = - g * ( alpha*SurfaceTendencyT +
c                     beta *SurfaceTendencyS ) * delZ(1) / rho  (m^2/s^3)
c     bosol = - g * alpha * Qsw * delZ(1) / rho                 (m^2/s^3)
c------------------------------------------------------------------------

c initialize arrays to zero
      DO j = jbot, jtop
         DO i = ibot, itop
            ustar(i,j) = p0
            bo   (I,J) = p0
            bosol(I,J) = p0
         END DO
      END DO

      DO j = jmin, jmax
       jp1 = j + 1
       DO i = imin, imax
        ip1 = i+1
        tempVar1 =
     &   (SurfaceTendencyU(i,j,bi,bj) + SurfaceTendencyU(ip1,j,bi,bj)) *
     &   (SurfaceTendencyU(i,j,bi,bj) + SurfaceTendencyU(ip1,j,bi,bj)) +
     &   (SurfaceTendencyV(i,j,bi,bj) + SurfaceTendencyV(i,jp1,bi,bj)) *
     &   (SurfaceTendencyV(i,j,bi,bj) + SurfaceTendencyV(i,jp1,bi,bj))
        if ( tempVar1 .lt. (phepsi*phepsi) ) then
           ustar(i,j) = SQRT( phepsi * p5 * delZ(1) )
        else
           tempVar2 =  SQRT( tempVar1 ) * p5 * delZ(1)
           ustar(i,j) = SQRT( tempVar2 )
        endif
        bo(I,J) = - gravity *
     &       ( vddiff(I,J,1,1) * SurfaceTendencyT(i,j,bi,bj) +
     &         vddiff(I,J,1,2) * SurfaceTendencyS(i,j,bi,bj)
     &       ) *
     &       delZ(1) / work2(I,J)
        bosol(I,J) = gravity * vddiff(I,J,1,1) * Qsw(i,j,bi,bj) *
     &       recip_Cp*recip_rhoNil*recip_dRf(1) *
     &       delZ(1) / work2(I,J)
       END DO
      END DO

c------------------------------------------------------------------------
c     velocity shear
c     --------------
c     Get velocity shear squared, averaged from "u,v-grid"
c     onto "t-grid" (in (m/s)**2):
c     dVsq(k)=(Uref-U(k))**2+(Vref-V(k))**2      at grid levels
c     shsq(k)=(U(k)-U(k+1))**2+(V(k)-V(k+1))**2  at interfaces
c------------------------------------------------------------------------

c initialize arrays to zero
      DO k = 1, Nr
         DO j = jbot, jtop
            DO i = ibot, itop
               shsq(i,j,k) = p0
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
     &              dbloc(i,j,k) / drC(k+1) .GT. dB_dz )
     &              work1(i,j) = k
            END DO

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
     &                     MAX ( phepsi, dBdz2 - dBdz1 )
            ELSE
               zRef(i,j) = rF(k+1)
            ENDIF

c     Compute roughness length scale z0 subject to 0 < z0
               tempVar1 = p5 * (
     &              (uVel(i,  j,  1,bi,bj)-uVel(i,  j,  2,bi,bj)) *
     &              (uVel(i,  j,  1,bi,bj)-uVel(i,  j,  2,bi,bj)) +
     &              (uVel(ip1,j,  1,bi,bj)-uVel(ip1,j,  2,bi,bj)) *
     &              (uVel(ip1,j,  1,bi,bj)-uVel(ip1,j,  2,bi,bj)) +
     &              (vVel(i,  j,  1,bi,bj)-vVel(i,  j,  2,bi,bj)) *
     &              (vVel(i,  j,  1,bi,bj)-vVel(i,  j,  2,bi,bj)) + 
     &              (vVel(i,  jp1,1,bi,bj)-vVel(i,  jp1,2,bi,bj)) *
     &              (vVel(i,  jp1,1,bi,bj)-vVel(i,  jp1,2,bi,bj)) )
               if ( tempVar1 .lt. (epsln*epsln) ) then
                  tempVar2 = epsln
               else
                  tempVar2 = SQRT ( tempVar1 )
               endif
               z0(i,j) = rF(2) *
     &                   ( rF(3) * LOG ( rF(3) / rF(2) ) /
     &                     ( rF(3) - rF(2) ) -
     &                     tempVar2 * vonK /
     &                     MAX ( ustar(i,j), phepsi ) )
               z0(i,j) = MAX ( z0(i,j), phepsi )

c     zRef is set to 0.1 * hMix subject to z0 <= zRef <= drF(1)
               zRef(i,j) = MAX ( epsilon * zRef(i,j), z0(i,j) )
               zRef(i,j) = MIN ( zRef(i,j), drF(1) )

c     Estimate reference velocity uRef and vRef.
               uRef(i,j) = p5 *
     &                     ( uVel(i,j,1,bi,bj) + uVel(ip1,j,1,bi,bj) )
               vRef(i,j) = p5 *
     &                     ( vVel(i,j,1,bi,bj) + vVel(i,jp1,1,bi,bj) )
               IF ( zRef(i,j) .LT. drF(1) ) THEN
                  ustarX = ( SurfaceTendencyU(i,  j,bi,bj) + 
     &                       SurfaceTendencyU(ip1,j,bi,bj) ) * p5
                  ustarY = ( SurfaceTendencyV(i,j,  bi,bj) +
     &                       SurfaceTendencyU(i,jp1,bi,bj) ) * p5
                  tempVar1 = ustarX * ustarX + ustarY * ustarY
                  if ( tempVar1 .lt. (epsln*epsln) ) then
                     tempVar2 = epsln
                  else
                     tempVar2 = SQRT ( tempVar1 )
                  endif
                  tempVar2 = ustar(i,j) *
     &                 ( LOG ( zRef(i,j) / rF(2) ) +
     &                 z0(i,j) / zRef(i,j) - z0(i,j) / rF(2) ) /
     &                 vonK / tempVar2
                  uRef(i,j) = uRef(i,j) + ustarX * tempVar2
                  vRef(i,j) = vRef(i,j) + ustarY * tempVar2
               ENDIF

         END DO
      END DO

      DO k = 1, Nr
         DO j = jmin, jmax
            jm1 = j - 1
            jp1 = j + 1
            DO i = imin, imax
               im1 = i - 1
               ip1 = i + 1
               dVsq(i,j,k) = p5 * (
     $              (uRef(i,j) - uVel(i,  j,  k,bi,bj)) *
     $              (uRef(i,j) - uVel(i,  j,  k,bi,bj)) +
     $              (uRef(i,j) - uVel(ip1,j,  k,bi,bj)) *
     $              (uRef(i,j) - uVel(ip1,j,  k,bi,bj)) +
     $              (vRef(i,j) - vVel(i,  j,  k,bi,bj)) *
     $              (vRef(i,j) - vVel(i,  j,  k,bi,bj)) + 
     $              (vRef(i,j) - vVel(i,  jp1,k,bi,bj)) *
     $              (vRef(i,j) - vVel(i,  jp1,k,bi,bj)) )
#ifdef KPP_SMOOTH_DVSQ
               dVsq(i,j,k) = p5 * dVsq(i,j,k) + p125 * (
     $              (uRef(i,j) - uVel(i,  jm1,k,bi,bj)) *
     $              (uRef(i,j) - uVel(i,  jm1,k,bi,bj)) +
     $              (uRef(i,j) - uVel(ip1,jm1,k,bi,bj)) *
     $              (uRef(i,j) - uVel(ip1,jm1,k,bi,bj)) +
     $              (uRef(i,j) - uVel(i,  jp1,k,bi,bj)) *
     $              (uRef(i,j) - uVel(i,  jp1,k,bi,bj)) +
     $              (uRef(i,j) - uVel(ip1,jp1,k,bi,bj)) *
     $              (uRef(i,j) - uVel(ip1,jp1,k,bi,bj)) +
     $              (vRef(i,j) - vVel(im1,j,  k,bi,bj)) *
     $              (vRef(i,j) - vVel(im1,j,  k,bi,bj)) + 
     $              (vRef(i,j) - vVel(im1,jp1,k,bi,bj)) *
     $              (vRef(i,j) - vVel(im1,jp1,k,bi,bj)) +
     $              (vRef(i,j) - vVel(ip1,j,  k,bi,bj)) *
     $              (vRef(i,j) - vVel(ip1,j,  k,bi,bj)) + 
     $              (vRef(i,j) - vVel(ip1,jp1,k,bi,bj)) *
     $              (vRef(i,j) - vVel(ip1,jp1,k,bi,bj)) )
#endif /* KPP_SMOOTH_DVSQ */
            END DO
         END DO
      END DO

#else /* KPP_ESTIMATE_UREF */

      DO k = 1, Nr
         DO j = jmin, jmax
            jm1 = j - 1
            jp1 = j + 1
            DO i = imin, imax
               im1 = i - 1
               ip1 = i + 1
               dVsq(i,j,k) = p5 * (
     $              (uVel(i,  j,  1,bi,bj)-uVel(i,  j,  k,bi,bj)) *
     $              (uVel(i,  j,  1,bi,bj)-uVel(i,  j,  k,bi,bj)) +
     $              (uVel(ip1,j,  1,bi,bj)-uVel(ip1,j,  k,bi,bj)) *
     $              (uVel(ip1,j,  1,bi,bj)-uVel(ip1,j,  k,bi,bj)) +
     $              (vVel(i,  j,  1,bi,bj)-vVel(i,  j,  k,bi,bj)) *
     $              (vVel(i,  j,  1,bi,bj)-vVel(i,  j,  k,bi,bj)) + 
     $              (vVel(i,  jp1,1,bi,bj)-vVel(i,  jp1,k,bi,bj)) *
     $              (vVel(i,  jp1,1,bi,bj)-vVel(i,  jp1,k,bi,bj)) )
#ifdef KPP_SMOOTH_DVSQ
               dVsq(i,j,k) = p5 * dVsq(i,j,k) + p125 * (
     $              (uVel(i,  jm1,1,bi,bj)-uVel(i,  jm1,k,bi,bj)) *
     $              (uVel(i,  jm1,1,bi,bj)-uVel(i,  jm1,k,bi,bj)) +
     $              (uVel(ip1,jm1,1,bi,bj)-uVel(ip1,jm1,k,bi,bj)) *
     $              (uVel(ip1,jm1,1,bi,bj)-uVel(ip1,jm1,k,bi,bj)) +
     $              (uVel(i,  jp1,1,bi,bj)-uVel(i,  jp1,k,bi,bj)) *
     $              (uVel(i,  jp1,1,bi,bj)-uVel(i,  jp1,k,bi,bj)) +
     $              (uVel(ip1,jp1,1,bi,bj)-uVel(ip1,jp1,k,bi,bj)) *
     $              (uVel(ip1,jp1,1,bi,bj)-uVel(ip1,jp1,k,bi,bj)) +
     $              (vVel(im1,j,  1,bi,bj)-vVel(im1,j,  k,bi,bj)) *
     $              (vVel(im1,j,  1,bi,bj)-vVel(im1,j,  k,bi,bj)) + 
     $              (vVel(im1,jp1,1,bi,bj)-vVel(im1,jp1,k,bi,bj)) *
     $              (vVel(im1,jp1,1,bi,bj)-vVel(im1,jp1,k,bi,bj)) +
     $              (vVel(ip1,j,  1,bi,bj)-vVel(ip1,j,  k,bi,bj)) *
     $              (vVel(ip1,j,  1,bi,bj)-vVel(ip1,j,  k,bi,bj)) + 
     $              (vVel(ip1,jp1,1,bi,bj)-vVel(ip1,jp1,k,bi,bj)) *
     $              (vVel(ip1,jp1,1,bi,bj)-vVel(ip1,jp1,k,bi,bj)) )
#endif /* KPP_SMOOTH_DVSQ */
            END DO
         END DO
      END DO

#endif /* KPP_ESTIMATE_UREF */

c     shsq computation
      DO k = 1, Nrm1
         kp1 = k + 1
         DO j = jmin, jmax
            jm1 = j - 1
            jp1 = j + 1
            DO i = imin, imax
               im1 = i - 1
               ip1 = i + 1
               shsq(i,j,k) = p5 * (
     $              (uVel(i,  j,  k,bi,bj)-uVel(i,  j,  kp1,bi,bj)) *
     $              (uVel(i,  j,  k,bi,bj)-uVel(i,  j,  kp1,bi,bj)) +
     $              (uVel(ip1,j,  k,bi,bj)-uVel(ip1,j,  kp1,bi,bj)) *
     $              (uVel(ip1,j,  k,bi,bj)-uVel(ip1,j,  kp1,bi,bj)) +
     $              (vVel(i,  j,  k,bi,bj)-vVel(i,  j,  kp1,bi,bj)) *
     $              (vVel(i,  j,  k,bi,bj)-vVel(i,  j,  kp1,bi,bj)) + 
     $              (vVel(i,  jp1,k,bi,bj)-vVel(i,  jp1,kp1,bi,bj)) *
     $              (vVel(i,  jp1,k,bi,bj)-vVel(i,  jp1,kp1,bi,bj)) )
#ifdef KPP_SMOOTH_SHSQ
               shsq(i,j,k) = p5 * shsq(i,j,k) + p125 * (
     $              (uVel(i,  jm1,k,bi,bj)-uVel(i,  jm1,kp1,bi,bj)) *
     $              (uVel(i,  jm1,k,bi,bj)-uVel(i,  jm1,kp1,bi,bj)) +
     $              (uVel(ip1,jm1,k,bi,bj)-uVel(ip1,jm1,kp1,bi,bj)) *
     $              (uVel(ip1,jm1,k,bi,bj)-uVel(ip1,jm1,kp1,bi,bj)) +
     $              (uVel(i,  jp1,k,bi,bj)-uVel(i,  jp1,kp1,bi,bj)) *
     $              (uVel(i,  jp1,k,bi,bj)-uVel(i,  jp1,kp1,bi,bj)) +
     $              (uVel(ip1,jp1,k,bi,bj)-uVel(ip1,jp1,kp1,bi,bj)) *
     $              (uVel(ip1,jp1,k,bi,bj)-uVel(ip1,jp1,kp1,bi,bj)) +
     $              (vVel(im1,j,  k,bi,bj)-vVel(im1,j,  kp1,bi,bj)) *
     $              (vVel(im1,j,  k,bi,bj)-vVel(im1,j,  kp1,bi,bj)) + 
     $              (vVel(im1,jp1,k,bi,bj)-vVel(im1,jp1,kp1,bi,bj)) *
     $              (vVel(im1,jp1,k,bi,bj)-vVel(im1,jp1,kp1,bi,bj)) +
     $              (vVel(ip1,j,  k,bi,bj)-vVel(ip1,j,  kp1,bi,bj)) *
     $              (vVel(ip1,j,  k,bi,bj)-vVel(ip1,j,  kp1,bi,bj)) + 
     $              (vVel(ip1,jp1,k,bi,bj)-vVel(ip1,jp1,kp1,bi,bj)) *
     $              (vVel(ip1,jp1,k,bi,bj)-vVel(ip1,jp1,kp1,bi,bj)) )
#endif
            END DO
         END DO
      END DO

c-----------------------------------------------------------------------
c     solve for viscosity, diffusivity, ghat, and hbl on "t-grid"
c-----------------------------------------------------------------------

      DO j = jbot, jtop
         DO i = ibot, itop
            work1(i,j) = nzmax(i,j,bi,bj)
            work2(i,j) = Fcori(i,j,bi,bj)
         END DO
      END DO
      CALL TIMER_START('KPPMIX [KPP_CALC]', myThid)
      CALL KPPMIX (
     I       mytime, mythid
     I     , work1, shsq, dVsq, ustar
     I     , bo, bosol, dbloc, Ritop, work2
     I     , ikey
     O     , vddiff
     U     , ghat
     O     , hbl )

      CALL TIMER_STOP ('KPPMIX [KPP_CALC]', myThid)

#ifdef ALLOW_AUTODIFF_TAMC
cph( storing not necessary
cphCADJ STORE vddiff, ghat  = comlev1_kpp, key = ikey
cph)
#endif /* ALLOW_AUTODIFF_TAMC */

c-----------------------------------------------------------------------
c     zero out land values and transfer to global variables
c-----------------------------------------------------------------------

      DO j = jmin, jmax
       DO i = imin, imax
        DO k = 1, Nr
         KPPviscAz(i,j,k,bi,bj) = vddiff(i,j,k-1,1) * pMask(i,j,k,bi,bj)
         KPPdiffKzS(i,j,k,bi,bj)= vddiff(i,j,k-1,2) * pMask(i,j,k,bi,bj)
         KPPdiffKzT(i,j,k,bi,bj)= vddiff(i,j,k-1,3) * pMask(i,j,k,bi,bj)
         KPPghat(i,j,k,bi,bj)   = ghat(i,j,k)       * pMask(i,j,k,bi,bj)
        END DO
        KPPhbl(i,j,bi,bj) = hbl(i,j) * pMask(i,j,1,bi,bj)
       END DO
      END DO
#ifdef FRUGAL_KPP
      _EXCH_XYZ_R8(KPPviscAz  , myThid )
      _EXCH_XYZ_R8(KPPdiffKzS , myThid )
      _EXCH_XYZ_R8(KPPdiffKzT , myThid )
      _EXCH_XYZ_R8(KPPghat    , myThid )
      _EXCH_XY_R8 (KPPhbl     , myThid )
#endif

#ifdef KPP_SMOOTH_VISC
c     horizontal smoothing of vertical viscosity
      DO k = 1, Nr
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        KPPviscAz(1-OLx,1-OLy,k,bi,bj) )
      END DO
      _EXCH_XYZ_R8(KPPviscAz  , myThid )
#endif /* KPP_SMOOTH_VISC */

#ifdef KPP_SMOOTH_DIFF
c     horizontal smoothing of vertical diffusivity
      DO k = 1, Nr
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        KPPdiffKzS(1-OLx,1-OLy,k,bi,bj) )
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        KPPdiffKzT(1-OLx,1-OLy,k,bi,bj) )
      END DO
      _EXCH_XYZ_R8(KPPdiffKzS , myThid )
      _EXCH_XYZ_R8(KPPdiffKzT , myThid )
#endif /* KPP_SMOOTH_DIFF */

C     Compute fraction of solar short-wave flux penetrating to
C     the bottom of the mixing layer.
      DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            worka(i,j) = KPPhbl(i,j,bi,bj)
         ENDDO
      ENDDO
      CALL SWFRAC(
     I     (sNx+2*OLx)*(sNy+2*OLy), minusone,
     I     mytime, mythid,
     U     worka )
      DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            KPPfrac(i,j,bi,bj) = worka(i,j)
         ENDDO
      ENDDO

      ENDIF

#endif /* ALLOW_KPP */

      RETURN
      END

      subroutine KPP_CALC_DUMMY(
     I     bi, bj, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE KPP_CALC_DUMMY                                |
C     | o Compute all KPP fields defined in KPP.h                |
C     | o Dummy routine for TAMC
C     |==========================================================|
C     | This subroutine serves as an interface between MITGCMUV  |
C     | code and NCOM 1-D routines in kpp_routines.F             |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"
#include "GRID.h"

c Routine arguments
c     bi, bj - array indices on which to apply calculations
c     myTime - Current time in simulation

      INTEGER bi, bj
      INTEGER myThid
      _RL     myTime

#ifdef ALLOW_KPP

c Local constants
      integer i, j, k

      DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            KPPhbl (i,j,bi,bj) = 1.0
            KPPfrac(i,j,bi,bj) = 0.0
            DO k = 1,Nr
               KPPghat   (i,j,k,bi,bj) = 0.0
               KPPviscAz (i,j,k,bi,bj) = viscAz
               KPPdiffKzT(i,j,k,bi,bj) = diffKzT
               KPPdiffKzS(i,j,k,bi,bj) = diffKzS
            ENDDO
         ENDDO
      ENDDO
      
#endif
      RETURN
      END
