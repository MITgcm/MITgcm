#include "CPP_OPTIONS.h"

      subroutine KVMIX(
     I     bi, bj, myTime, myThid )
CADJ I     bi, bj, ikey, myTime, myThid )

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
c     layer, i.e., at depth zw(km).  these values are only useful when 
c     the model ocean domain does not include the entire ocean down to
c     the seafloor ("upperocean" setup) and allows flux through the
c     bottom of the domain.  for full-depth runs, these mixing 
c     coefficients are being zeroed out before leaving this subroutine.
c
c-------------------------------------------------------------------------

      IMPLICIT NONE

c global parameters updated by kvmix
c     KPPviscAz         - KPP eddy viscosity coefficient                  (m^2/s)
c     KPPdiffKzT        - KPP diffusion coefficient for temperature       (m^2/s)
c     KPPdiffKzS        - KPP diffusion coefficient for salt and tracers  (m^2/s)
c     KPPghat           - nonlocal transport coefficient                  (s/m^2)
c     KPPhbl            - boundary layer depth on "t-grid"                    (m)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "KPPMIX.h"
#include "FFIELDS.h"
#include "GRID.h"

      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

      integer    mdiff
      parameter( mdiff = 3 )    ! number of diffusivities for local arrays

c Routine arguments
c     bi, bj - array indices on which to apply calculations
c     myTime - Current time in simulation

      INTEGER bi, bj
      INTEGER ikey
      INTEGER myThid
      _RL     myTime

#ifdef ALLOW_KPP

c Local arrays and variables
c                            horizontal working arrays
c     ustar  (nx,ny)       - surface friction velocity                      (m/s)
c     Bo     (nx,ny)       - surface turbulent buoyancy forcing         (m^2/s^3)
c     Bosol  (nx,ny)       - surface radiative buoyancy forcing         (m^2/s^3)
c     Coriol (nx,ny)       - Coriolis parameter on "t-grid"                 (1/s)
c     shsq   (nx,ny,Nr)    - local velocity shear squared
c                            at interfaces for ri_iwmix                 (m^2/s^2)
c     dVsq   (nx,ny,Nr)    - velocity shear re surface squared
c                            at grid levels for bldepth                 (m^2/s^2)
c     rho1   (nx,ny)       - density of surface layer                    (kg/m^3)
c     ttalpha(nx,ny,Nr+1)  - d(rho) / d(potential temperature)         (kg/m^3/C)
c     ssbeta (nx,ny,Nr+1)  - d(rho) / d(salinity)                    (kg/m^3/PSU)
c     dbloc  (nx,ny,Nr)    - local delta buoyancy at interfaces
c                            for ri_iwmix and bldepth                     (m/s^2)
c     dbsfc  (nx,ny,Nr)    - delta buoyancy with respect to surface
c                            at grid levels for Ritop                     (m/s^2)
c     Ritop  (nx,ny,Nr)    - numerator of bulk richardson number
c                            at grid levels for bldepth
c     vddiff (nx,ny,Nr+2,1)- vertical viscosity on "t-grid"               (m^2/s)
c     vddiff (nx,ny,Nr+2,2)- vert. diff. on next row for temperature      (m^2/s)
c     vddiff (nx,ny,Nr+2,3)- vert. diff. on next row for salt&tracers     (m^2/s)
c     ghat   (nx,ny,Nr)    - nonlocal transport coefficient               (s/m^2)
c     hbl    (nx,ny)       - mixing layer depth                               (m)
c     kmtj   (nx,ny)       - maximum number of wet levels in each column

      _RS ustar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS bo      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS bosol   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS shsq    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS dVsq    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS rho1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS ttalpha (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RS ssbeta  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RS dbloc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS dbsfc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS Ritop   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS vddiff  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,0:Nr+1,mdiff)
      _RS ghat    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS hbl     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      logical    lri               ! mixing process switches
      parameter( lri = .true. )

      _RS        p5    , p25
      parameter (p5=0.5, p25=0.25)

      integer    imt
      parameter (imt = (sNx+2*OLx)*(sNy+2*OLy))
      
      _RS     tmp_dVsq, tmp_shsq, tmp

      _RS     tempVar
      integer i, j, k, mr, kp1, ip1, jp1

c     Check to see if new vertical mixing coefficient should be computed now?
c     Recomputation frequency depends on convective adjustment parameter.
      IF ( DIFFERENT_MULTIPLE(cAdjFreq,myTime,myTime-deltaTClock) .OR.
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

c     velocity shear
c     --------------
c     Get velocity shear squared, averaged from "u,v-grid"
c     onto "t-grid" (in (m/s)**2):
c     dVsq(k)=(Uref-U(k))**2+(Vref-V(k))**2      at grid levels
c     shsq(k)=(U(k)-U(k+1))**2+(V(k)-V(k+1))**2  at interfaces

         DO k = 1, Nr
            DO j = 1-OLy, sNy+OLy
               dVsq(sNx+OLx,j,k) = 0.0
               shsq(sNx+OLx,j,k) = 0.0
            END DO
            DO i = 1-OLx, sNx+OLx
               dVsq(i,sNy+OLy,k) = 0.0
               shsq(i,sNy+OLy,k) = 0.0
            END DO
         END DO

#ifdef OLDER
         DO k = 1, Nr
            DO j = 1-OLy, sNy+OLy-1 
               jp1 = j + 1
               DO i = 1-OLx, sNx+OLx-1
                  ip1 = i + 1
                  dVsq(i,j,k) = p5 * (
     &              (uVel(i  ,j  ,1,bi,bj)-uVel(i  ,j  ,k,bi,bj)) *
     &              (uVel(i  ,j  ,1,bi,bj)-uVel(i  ,j  ,k,bi,bj)) +
     &              (uVel(ip1,j  ,1,bi,bj)-uVel(ip1,j  ,k,bi,bj)) *
     &              (uVel(ip1,j  ,1,bi,bj)-uVel(ip1,j  ,k,bi,bj)) )
     &     + p5 * ( (vVel(i  ,j  ,1,bi,bj)-vVel(i  ,j  ,k,bi,bj)) *
     &              (vVel(i  ,j  ,1,bi,bj)-vVel(i  ,j  ,k,bi,bj)) + 
     &              (vVel(i  ,jp1,1,bi,bj)-vVel(i  ,jp1,k,bi,bj)) *
     &              (vVel(i  ,jp1,1,bi,bj)-vVel(i  ,jp1,k,bi,bj)) )
               END DO
            END DO
         END DO

         DO k = 1, Nr-1
            kp1 = k + 1
            DO j = 1-OLy, sNy+OLy-1   
               jp1 = j + 1
               DO i = 1-OLx, sNx+OLx-1
                  ip1 = i + 1
                  shsq(i,j,k) = p5 * (
     &              (uVel(i  ,j  ,k,bi,bj)-uVel(i  ,j  ,kp1,bi,bj)) *
     &              (uVel(i  ,j  ,k,bi,bj)-uVel(i  ,j  ,kp1,bi,bj)) + 
     &              (uVel(ip1,j  ,k,bi,bj)-uVel(ip1,j  ,kp1,bi,bj)) *
     &              (uVel(ip1,j  ,k,bi,bj)-uVel(ip1,j  ,kp1,bi,bj)) )
     &     + p5 * ( (vVel(i  ,j  ,k,bi,bj)-vVel(i  ,j  ,kp1,bi,bj)) *
     &              (vVel(i  ,j  ,k,bi,bj)-vVel(i  ,j  ,kp1,bi,bj)) + 
     &              (vVel(i  ,jp1,k,bi,bj)-vVel(i  ,jp1,kp1,bi,bj)) *
     &              (vVel(i  ,jp1,k,bi,bj)-vVel(i  ,jp1,kp1,bi,bj)) ) 
               END DO
            END DO
         END DO

#else
c     velocity shear
c     --------------
c     Get velocity shear squared, averaged from "u,v-grid"
c     onto "t-grid" (in (m/s)**2):
c     dVsq(k)=(Uref-U(k))**2+(Vref-V(k))**2      at grid levels
c     Shsq(k)=(U(k)-U(k+1))**2+(V(k)-V(k+1))**2  at interfaces

         DO K = 1, Nr                       ! dVsq
            DO j = 1-OLy, sNy+OLy-1
               jp1 = j + 1
               DO i = 1-OLx, sNx+OLx-1
                  ip1 = i + 1
                  dvsq(i,j,K) = pMask(i,j,K,bi,bj) *
     $              (p5 * maskW(i,j,K,bi,bj) * maskW(ip1,j,K,bi,bj) *
     $              ( (uVel(i  ,j,1,bi,bj)-uVel(i  ,j,K,bi,bj)) *
     $                (uVel(i  ,j,1,bi,bj)-uVel(i  ,j,K,bi,bj)) +
     $                (uVel(ip1,j,1,bi,bj)-uVel(ip1,j,K,bi,bj)) *
     $                (uVel(ip1,j,1,bi,bj)-uVel(ip1,j,K,bi,bj)) ) +
     $              p5 * maskS(i,j,K,bi,bj) * maskS(i,jp1,K,bi,bj) *
     $              ( (vVel(  i,j,1,bi,bj)-vVel(i,j  ,K,bi,bj)) *
     $                (vVel(i,j  ,1,bi,bj)-vVel(i,j  ,K,bi,bj)) + 
     $                (vVel(i,jp1,1,bi,bj)-vVel(i,jp1,K,bi,bj)) *
     $                (vVel(i,jp1,1,bi,bj)-vVel(i,jp1,K,bi,bj)) ))
               END DO
            END DO
         END DO

         DO K = 1, Nr-1                      ! Shsq
            KP1 = K + 1
            DO j = 1-OLy, sNy+OLy-1
               jp1 = j + 1
               DO i = 1-OLx, sNx+OLx-1
                  ip1 = i + 1
                  shsq(i,j,K) = pMask(i,j,KP1,bi,bj) *
     $           (p5 * maskW(i,j,KP1,bi,bj) * maskW(ip1,j,KP1,bi,bj) *
     $           ( (uVel(i  ,j,K,bi,bj)-uVel(i  ,j,KP1,bi,bj)) *
     $             (uVel(i  ,j,K,bi,bj)-uVel(i  ,j,KP1,bi,bj)) + 
     $             (uVel(ip1,j,K,bi,bj)-uVel(ip1,j,KP1,bi,bj)) *
     $             (uVel(ip1,j,K,bi,bj)-uVel(ip1,j,KP1,bi,bj)) )
     $           + p5 * maskS(i,j,KP1,bi,bj) * maskS(i,jp1,KP1,bi,bj) *
     $           ( (vVel(i  ,j,K,bi,bj)-vVel(i,j  ,KP1,bi,bj)) *
     $             (vVel(i  ,j,K,bi,bj)-vVel(i,j  ,KP1,bi,bj)) + 
     $             (vVel(i,jp1,K,bi,bj)-vVel(i,jp1,KP1,bi,bj)) *
     $             (vVel(i,jp1,K,bi,bj)-vVel(i,jp1,KP1,bi,bj)) ))
               END DO
            END DO
         END DO
#endif

         DO j = 1-OLy, sNy+OLy
            DO i = 1-OLx, sNx+OLx
               shsq(i,j,Nr) = 0.
            END DO
         END DO

c     smooth dVsq and shsq zonally with 121 filters
       do mr = 1, num_z_smooth_sh
        do k = 1, Nr
         do j = 1-OLy, sNy+OLy 
          tmp_dVsq = p25*dVsq(1-OLx,j,k)
          tmp_shsq = p25*shsq(1-OLx,j,k)
          do i = 2-OLx, sNx+OLx-1
           tmp = dVsq(i,j,k)
           dVsq(i,j,k) = tmp_dVsq + p5*dVsq(i,j,k) + p25*dVsq(i+1,j,k)
           tmp_dVsq = p25*tmp
           tmp = shsq(i,j,k)
           shsq(i,j,k) = tmp_shsq + p5*shsq(i,j,k) + p25*shsq(i+1,j,k)
           tmp_shsq = p25*tmp
          end do
         end do
        end do
       end do

c     smooth dVsq and shsq meridionally with 121 filters
       do mr = 1, num_m_smooth_sh
        do k = 1, Nr
         do i = 1-OLx, sNx+OLx
          tmp_dVsq = p25*dVsq(i,1-OLy,k)
          tmp_shsq = p25*shsq(i,1-OLy,k)
          do j = 2-OLy, sNy+OLy-1
           tmp = dVsq(i,j,k)
           dVsq(i,j,k) = tmp_dVsq + p5*dVsq(i,j,k) + p25*dVsq(i,j+1,k)
           tmp_dVsq = p25*tmp
           tmp = shsq(i,j,k)
           shsq(i,j,k) = tmp_shsq + p5*shsq(i,j,k) + p25*shsq(i,j+1,k)
           tmp_shsq = p25*tmp
          end do
         end do
        end do
       end do

c-----------------------------------------------------------------------
c     
c     density related quantities
c     --------------------------
c
c      rho1    - density of surface layer                                (kg/m^3)
c      dbloc   - local buoyancy gradient at Nr interfaces
c                g/rho{k+1,k+1} * [ drho{k,k+1}-drho{k+1,k+1} ]           (m/s^2)
c      dbsfc   - buoyancy difference with respect to the surface
c                g * [ drho{1,k}/rho{1,k} - drho{k,k}/rho{k,k} ]          (m/s^2)
c      ttalpha - thermal expansion coefficient without 1/rho factor
c                d(rho{k,k})/d(T(k))                                   (kg/m^3/C)
c      ssbeta  - salt expansion coefficient without 1/rho factor
c                d(rho{k,k})/d(S(k))                                 (kg/m^3/PSU)

         CALL TIMER_START('STATEKPP      [KVMIX]', myThid)
         CALL STATEKPP(
     I          bi, bj, myThid
     O        , rho1, dbloc, dbsfc, ttalpha, ssbeta
     &        )
         CALL TIMER_STOP ('STATEKPP      [KVMIX]', myThid)

c     Transfer from global to local arrays

         DO k = 1, Nr
            DO j = 1-OLy, sNy+OLy
               DO i = 1-OLx, sNx+OLx

c     zero out dbloc over land points (so that the convective
c     part of the interior mixing can be diagnosed)
                  dbloc(i,j,k) = dbloc(i,j,k) * pMask(i,j,k,bi,bj)
                  dbsfc(i,j,k) = dbsfc(i,j,k) * pMask(i,j,k,bi,bj)
                  if(k.eq.nzmax(i,j,bi,bj)) then
                     dbloc(i,j,k) = 0.
                     dbsfc(i,j,k) = 0.
                  endif

c     numerator of bulk richardson number on grid levels
c     note: land and ocean bottom values need to be set to zero
c     so that the subroutine "bldepth" works correctly
                  Ritop(i,j,k) = (zgrid(1)-zgrid(k)) * dbsfc(i,j,k)

               END DO
            END DO
         END DO

c     friction velocity, turbulent and radiative surface buoyancy forcing
c     -------------------------------------------------------------------
c     taux / rho = fu * delZ(1)                                   (N/m^2)
c     tauy / rho = fv * delZ(1)                                   (N/m^2)
c     ustar = sqrt( sqrt( taux^2 + tauy^2 ) / rho )               (m/s)
c     bo    = g * (alpha * Qnet - beta * EmPmR) * delZ(1) / rho   (m^2/s^3)
c     bosol = g * alpha * Qsw / rho                               (m^2/s^3)

         DO j = 1-OLy, sNy+OLy-1
            DO i = 1-OLx, sNx+OLx-1
               ustar(i,j) = SQRT( SQRT(
     &              (fu(i,j,bi,bj) + fu(i+1,j,bi,bj)) * p5 * delZ(1) * 
     &              (fu(i,j,bi,bj) + fu(i+1,j,bi,bj)) * p5 * delZ(1) +
     &              (fv(i,j,bi,bj) + fv(i,j+1,bi,bj)) * p5 * delZ(1) * 
     &              (fv(i,j,bi,bj) + fv(i,j+1,bi,bj)) * p5 * delZ(1) )) 
               bo(i,j) = gravity *
     &                 ( ttalpha(i,j,1) * Qnet(i,j,bi,bj) -
     &                 ssbeta(i,j,1) * EmPmR(i,j,bi,bj) ) *
     &                 delZ(1) / rho1(i,j)
              bosol(i,j) = gravity * ttalpha(i,j,1) * Qsw(i,j,bi,bj)
     &              / rho1(i,j)
            END DO
         END DO

         DO j = 1-OLy, sNy+OLy
            ustar(sNx+OLx,j) = 0.0
            bo   (sNx+OLx,j) = 0.0
            bosol(sNx+OLx,j) = 0.0
         END DO
         DO i = 1-OLx, sNx+OLx
            ustar(i,sNy+OLy) = 0.0
            bo   (i,sNy+OLy) = 0.0
            bosol(i,sNy+OLy) = 0.0
         END DO

c-----------------------------------------------------------------------
c     solve for viscosity, diffusivity, ghat, and hbl on "t-grid"
c-----------------------------------------------------------------------

         CALL TIMER_START('KPPMIX [KVMIX]', myThid)
         CALL KPPMIX (
     I          lri, nzmax(1-OLx,1-OLy,bi,bj), shsq, dVsq, ustar
     I        , bo, bosol, dbloc, Ritop
     I        , fCori(1-OLx,1-OLy,bi,bj)
     I        , ikey
     O        , vddiff, ghat, hbl
     &        )
         CALL TIMER_STOP ('KPPMIX [KVMIX]', myThid)

CADJ STORE vddiff, ghat  = uvtape, key = ikey

c-----------------------------------------------------------------------
c     zero out land values,
c     make sure coefficients are within reasonable bounds,
c     and transfer to global variables
c-----------------------------------------------------------------------

         DO j = 1-OLy, sNy+OLy
            DO i = 1-OLx, sNx+OLx
               DO k = 1, Nr
c KPPviscAz
                  tempVar = min( maxKPPviscAz(k), vddiff(i,j,k-1,1) )
                  tempVar = max( minKPPviscAz, tempVar )
                  KPPviscAz(i,j,k,bi,bj) = tempVar*pMask(i,j,k,bi,bj)
c KPPdiffKzS
                  tempVar = min( maxKPPdiffKzS, vddiff(i,j,k-1,2) )
                  tempVar = max( minKPPdiffKzS, tempVar )
                  KPPdiffKzS(i,j,k,bi,bj) = tempVar*pMask(i,j,k,bi,bj)
c KPPdiffKzT
                  tempVar = min( maxKPPdiffKzT, vddiff(i,j,k-1,3) )
                  tempVar = max( minKPPdiffKzT, tempVar )
                  KPPdiffKzT(i,j,k,bi,bj) = tempVar*pMask(i,j,k,bi,bj)
c KPPghat
                  tempVar = min( maxKPPghat, ghat(i,j,k) )
                  tempVar = max( minKPPghat, tempVar )
                  KPPghat(i,j,k,bi,bj) = tempVar*pMask(i,j,k,bi,bj)
               END DO
c KPPhbl: set to -zgrid(1) over land
               KPPhbl(i,j,bi,bj) = (hbl(i,j) + zgrid(1))
     &              * pMask(i,j,1,bi,bj) -
     &              zgrid(1)
            END DO
         END DO

      ENDIF

#endif

      RETURN
      END

