#include "CPP_OPTIONS.h"

c-- File kppmix.F: subroutines needed to implement
c--                KPP vertical mixing scheme
c-- Contents
c--   kppmix      - main driver and interface routine
c--   bldepth     - determine oceanic planetray boundary layer depth
c--   wscale      - compute turbulent velocity scales
c--   Ri_iwmix    - compute interior viscosity diffusivity coefficients
c--   z121        - apply 121 smoothing
c--   blmix       - boundary layer mixing coefficients
c--   enhance     - enhance diffusivity at the boundary layer interface
c--   kmixinit    - initialize some constants
c--   statekpp    - computes all necessary input arrays for kpp mixing scheme
c--   find_alpha  - calculate [drho(S,T,z) / dT] of a horizontal slice
c--   find_beta   - calculate [drho(S,T,z) / dS] of a horizontal slice
c--   print_diags - compute and print diagnostics
c--   print_maps  - dignostic maps for arays used by kppmix

c*************************************************************************

      SUBROUTINE KPPMIX (
     I     lri, kmtj, shsq, dvsq, ustar, bo, bosol
     I     , dbloc, Ritop, coriol
     I     , ikey
     O     , diffus, ghat, hbl )

c-------------------------------------------------------------------------
c
c     Main driver subroutine for kpp vertical mixing scheme and
c     interface to greater ocean model
c
c     written  by: bill large,    june  6, 1994
c     modified by: jan morzel,    june 30, 1994
c                  bill large,  august 11, 1994
c                  bill large, january 25, 1995 : "dVsq" and 1d code
c                  detlef stammer,  august 1997 : for use with MIT GCM Classic
c                  d. menemenlis,     june 1998 : for use with MIT GCM UV
c
c-----------------------------------------------------------------------

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "KPPMIX.h"
#include "FFIELDS.h"

      integer    kmp1         , imt
      parameter (kmp1 = Nr + 1, imt = (sNx+2*OLx)*(sNy+2*OLy))

c--   number of local array diffusivities
      integer    mdiff
      parameter (mdiff = 3)

c input
c     lri             - mixing process switches
c     kmtj   (imt)    - number of vertical layers on this row
c     shsq   (imt,Nr) - (local velocity shear)^2                        ((m/s)^2)
c     dvsq   (imt,Nr) - (velocity shear re sfc)^2                       ((m/s)^2)
c     ustar  (imt)    - surface friction velocity                           (m/s)
c     bo     (imt)    - surface turbulent buoy. forcing                 (m^2/s^3)
c     bosol  (imt)    - radiative buoyancy forcing                      (m^2/s^3)
c     dbloc  (imt,Nr) - local delta buoyancy across interfaces            (m/s^2)
c     Ritop  (imt,Nr) - numerator of bulk Richardson Number
c                         (zref-z) * delta buoyancy w.r.t. surface      ((m/s)^2)
c     coriol (imt)    - Coriolis parameter                                  (1/s)

      logical lri
      integer kmtj   (imt   )
      _RS    shsq   (imt,Nr)
      _RS    dvsq   (imt,Nr)
      _RS    ustar  (imt   )
      _RS    bo     (imt   )
      _RS    bosol  (imt   )
      _RS    dbloc  (imt,Nr)
      _RS    Ritop  (imt,Nr)
      _RS    coriol (imt   )
      integer ikey

c output
c     diffus (imt,1)  - vertical viscosity coefficient                    (m^2/s)
c     diffus (imt,2)  - vertical scalar diffusivity                       (m^2/s)
c     diffus (imt,3)  - vertical temperature diffusivity                  (m^2/s)
c     ghat   (imt)    - nonlocal transport coefficient                    (s/m^2)
c     hbl    (imt)    - mixing layer depth                                    (m)

      _RS diffus(imt,0:kmp1,mdiff)
      _RS ghat  (imt,Nr)
      _RS hbl   (imt)

#ifdef ALLOW_KPP

c local
c     kbl    (imt         ) - index of first grid level below hbl
c     bfsfc  (imt         ) - surface buoyancy forcing                (m^2/s^3)
c     casea  (imt         ) - 1 in case A; 0 in case B
c     stable (imt         ) - 1 in stable forcing; 0 if unstable
c     dkm1   (imt,   mdiff) - boundary layer diffusivity at kbl-1 level
c     blmc   (imt,Nr,mdiff) - boundary layer mixing coefficients
c     sigma  (imt         ) - normalized depth (d / hbl)
c     Rib    (imt,Nr      ) - bulk Richardson number

      integer kbl (imt         )
      _RS bfsfc   (imt         )
      _RS casea   (imt         )
      _RS stable  (imt         )
      _RS dkm1    (imt,   mdiff)
      _RS blmc    (imt,Nr,mdiff)
      _RS sigma   (imt         )
      _RS Rib     (imt,Nr      )

      integer i, k, md

c-----------------------------------------------------------------------
c compute interior mixing coefficients everywhere, due to constant
c internal wave activity, static instability, and local shear
c instability.
c-----------------------------------------------------------------------

      call Ri_iwmix (
     I       Nr, kmp1, imt, kmtj, shsq, dbloc
     I     , ikey
     O     , diffus )

c-----------------------------------------------------------------------
c set seafloor values to zero and fill extra "kmp1" coefficients
c for blmix
c-----------------------------------------------------------------------

      do md = 1, mdiff
         do i = 1,imt
            do k=kmtj(i),kmp1
               diffus(i,k,md) = 0.0
            end do
         end do
      end do

c-----------------------------------------------------------------------
c compute boundary layer mixing coefficients:
c
c diagnose the new boundary layer depth
c-----------------------------------------------------------------------

      call bldepth (
     I       Nr, kmp1, imt, kmtj
     I     , dvsq, dbloc, Ritop, ustar, bo, bosol, coriol
     I     , ikey
     O     , hbl, bfsfc, stable, casea, kbl, Rib, sigma
     &     )

CADJ STORE hbl,bfsfc,stable,casea,kbl = kpptape, key = ikey

c-----------------------------------------------------------------------
c compute boundary layer diffusivities
c-----------------------------------------------------------------------

      call blmix (
     I       ustar, bfsfc, hbl, stable, casea, diffus, kbl
     O     , dkm1, blmc, ghat, sigma
     &     )

CADJ STORE dkm1,blmc,ghat = kpptape, key = ikey

c-----------------------------------------------------------------------
c enhance diffusivity at interface kbl - 1
c-----------------------------------------------------------------------

      call enhance (
     I       Nr, kmp1, imt, mdiff, dkm1, hbl, kbl, diffus, casea
     U     , ghat
     O     , blmc )

c-----------------------------------------------------------------------
c combine interior and boundary layer coefficients and nonlocal term
c-----------------------------------------------------------------------

      do k = 1, Nr
         do i = 1, imt
            if (k .lt. kbl(i)) then
               do md = 1, mdiff
                  diffus(i,k,md) = blmc(i,k,md)
               end do
            else
               ghat(i,k) = 0.
            endif
         end do
      end do

      return 
      end

c*************************************************************************

      subroutine bldepth (
     I       km, kmp1, imt, kmtj
     I     , dvsq, dbloc, Ritop, ustar, bo, bosol, coriol
     I     , ikey
     O     , hbl, bfsfc, stable, casea, kbl, Rib, sigma
     &     )

c     the oceanic planetary boundary layer depth, hbl, is determined as
c     the shallowest depth where the bulk Richardson number is
c     equal to the critical value, Ricr.
c
c     bulk Richardson numbers are evaluated by computing velocity and
c     buoyancy differences between values at zgrid(kl) < 0 and surface
c     reference values.
c     in this configuration, the reference values are equal to the
c     values in the surface layer.
c     when using a very fine vertical grid, these values should be
c     computed as the vertical average of velocity and buoyancy from
c     the surface down to epsilon*zgrid(kl).
c
c     when the bulk Richardson number at k exceeds Ricr, hbl is
c     linearly interpolated between grid levels zgrid(k) and zgrid(k-1).
c
c     The water column and the surface forcing are diagnosed for
c     stable/ustable forcing conditions, and where hbl is relative
c     to grid points (caseA), so that conditional branches can be
c     avoided in later subroutines.
c 
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPPMIX.h"
#include "FFIELDS.h"

c input
c------
c km kmp1   : number of vertical levels
c imt       : number of horizontal grid points
c kmtj      : number of vertical layers
c dvsq      : (velocity shear re sfc)^2             ((m/s)^2)
c dbloc     : numerator of bulk Richardson Number
c             =(z-zref)*dbsfc, where dbsfc=delta
c             buoyancy with respect to surface      ((m/s)^2)
c ustar     : surface friction velocity                 (m/s)
c bo        : surface turbulent buoyancy forcing    (m^2/s^3)
c bosol     : radiative buoyancy forcing            (m^2/s^3)
c coriol    : Coriolis parameter                        (1/s)
      integer km, kmp1
      integer imt
      integer kmtj(imt)
      _RS dvsq   (imt,km)
      _RS dbloc  (imt,km)
      _RS Ritop  (imt,km)
      _RS ustar  (imt)
      _RS bo     (imt)
      _RS bosol  (imt)
      _RS coriol (imt)
      integer ikey

c  output
c--------
c hbl       : boundary layer depth                        (m)
c bfsfc     : Bo+radiation absorbed to d=hbf*hbl    (m^2/s^3)
c stable    : =1 in stable forcing; =0 unstable
c casea     : =1 in case A, =0 in case B
c kbl       : -1 of first grid level below hbl
c Rib       : Bulk Richardson number
c sigma     : normalized depth (d/hbl)
      _RS hbl    (imt)
      _RS bfsfc  (imt)
      _RS stable (imt)
      _RS casea  (imt)
      integer kbl (imt)
      _RS Rib    (imt,km)
      _RS sigma  (imt)

c  local
c-------
      _RS bvsq, vtsq, hekman, hmonob, hlimit
      _RS stab
      integer i, kl
      _RS minusone
      parameter(minusone=-1.0)

      integer    jwtype
      parameter (jwtype = 3)

      _RS        p5
      parameter (p5 = 0.5)
      _RL        dpshear2

c find bulk Richardson number at every grid level until > Ricr
c
c note: the reference depth is -epsilon/2.*zgrid(k), but the reference
c       u,v,t,s values are simply the surface layer values,
c       and not the averaged values from 0 to 2*ref.depth,
c       which is necessary for very fine grids(top layer < 2m thickness)
c note: max values when Ricr never satisfied are
c       kbl(i)=kmtj(i) and hbl(i)=-zgrid(kmtj(i))

c     initialize hbl and kbl to bottomed out values

      do i = 1, imt
         Rib(i,1) = 0.0
         kbl(i) = max(kmtj(i),1)
         hbl(i) = -zgrid(kbl(i))
      end do

      do kl = 2, km

c     compute bfsfc = sw fraction at hbf * zgrid

c     use caseA as temporary array

         do i = 1, imt
            casea(i) = zgrid(kl)
         end do
         call SWFRAC(
     I        imt, hbf, casea, jwtype,
     O        bfsfc)

         do i = 1, imt

c     use caseA as temporary array

            casea(i) = -zgrid(kl)

c     compute bfsfc= Bo + radiative contribution down to hbf * hbl

            bfsfc(i) = bo(i) + bosol(i)*(1. - bfsfc(i))
            stab     = p5 + sign(p5,bfsfc(i))
            sigma(i) = stab + (1. - stab) * epsilon

         end do

c-----------------------------------------------------------------------
c     compute velocity scales at sigma, for hbl= caseA = -zgrid(kl)
c     use stable instead of ws as temporary
c-----------------------------------------------------------------------

         call wscale1 (
     I        imt, sigma, casea, ustar, bfsfc,
     O        stable )

         do i = 1, imt

c-----------------------------------------------------------------------
c     compute the turbulent shear contribution to Rib
c-----------------------------------------------------------------------

            bvsq = p5 *
     1           ( dbloc(i,kl-1) / (zgrid(kl-1)-zgrid(kl  ))+
     2             dbloc(i,kl  ) / (zgrid(kl  )-zgrid(kl+1)))
            vtsq = -zgrid(kl) * stable(i) * sqrt(abs(bvsq)) * Vtc

c     compute bulk Richardson number at new level, dunder
c     note: Ritop needs to be zero on land and ocean bottom
c     points so that the following if statement gets triggered
c     correctly. otherwise, hbl might get set to (big) negative
c     values, that might exceed the limit for the "exp" function
c     in "SWFRAC"
c
c     assignment to double precision variable to avoid overflow
c
            dpshear2  = dvsq(i,kl) + vtsq + epsln
            Rib(i,kl) = Ritop(i,kl) / dpshear2

         end do
      end do

      do kl = 2, km
         do i = 1, imt
            if (kbl(i).eq.kmtj(i) .and. Rib(i,kl).gt.Ricr) kbl(i) = kl
         end do
      end do

CADJ store kbl = kpptape, key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

      do i = 1, imt
         kl = kbl(i)

c     linearly interpolate to find hbl where Rib = Ricr

         if (kl.gt.1 .and. kl.lt.kmtj(i)) then
            hbl(i) = (-zgrid(kl-1)) + (zgrid(kl-1)-zgrid(kl)) *
     1           (Ricr - Rib(i,kl-1)) / (Rib(i,kl)-Rib(i,kl-1))
         endif
      end do

CADJ store hbl = kpptape, key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

c-----------------------------------------------------------------------
c     find stability and buoyancy forcing for boundary layer
c-----------------------------------------------------------------------

      call SWFRAC(
     I     imt, minusone, hbl, jwtype,
     O     bfsfc)

      do i = 1, imt
         bfsfc(i)  = bo(i) + bosol(i) * (1. - bfsfc(i))
         stable(i) = p5 + sign( p5, bfsfc(i) )
c--      ensures bfsfc is never 0
         bfsfc(i)  = bfsfc(i) + stable(i) * epsln
      end do

c-----------------------------------------------------------------------
c check hbl limits for hekman or hmonob
c-----------------------------------------------------------------------

      do i = 1, imt
         if (bfsfc(i) .gt. 0.0) then
            hekman = cekman * ustar(i) / (abs(coriol(i))+epsln)
            hmonob = cmonob * ustar(i)*ustar(i)*ustar(i)
     &           / vonk / bfsfc(i)
            hlimit = stable(i) * min(hekman,hmonob)
     &             + (stable(i)-1.) * zgrid(km)
            hbl(i) = min(hbl(i),hlimit)
         endif
      end do

CADJ store hbl = kpptape, key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

      do i = 1, imt
         if (bfsfc(i) .gt. 0.0) then
            hbl(i) = max(hbl(i),(-zgrid(1)))
         endif
      end do

      do i = 1, imt
         kbl(i) = kmtj(i)
      end do

CADJ store hbl = kpptape, key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

c-----------------------------------------------------------------------
c      find new kbl
c-----------------------------------------------------------------------

      do kl = 2, km
         do i = 1, imt
            if ( kbl(i).eq.kmtj(i) .and. (-zgrid(kl)).gt.hbl(i) ) then
               kbl(i) = kl
            endif
         end do
      end do

c-----------------------------------------------------------------------
c      find stability and buoyancy forcing for final hbl values
c-----------------------------------------------------------------------

      call SWFRAC(
     I     imt, minusone, hbl, jwtype,
     O     bfsfc)
 
      do i = 1, imt
         bfsfc(i)  = bo(i) + bosol(i) * (1. - bfsfc(i))
         stable(i) = p5 + sign( p5, bfsfc(i) )
         bfsfc(i)  = bfsfc(i) + stable(i) * epsln
      end do

c-----------------------------------------------------------------------
c determine caseA and caseB
c-----------------------------------------------------------------------

      do i = 1, imt
         casea(i) = p5 +
     1        sign(p5, -zgrid(kbl(i)) - p5*hwide(kbl(i)) - hbl(i))
      end do

      return
      end


c*************************************************************************

      subroutine wscale (
     I     imt, sigma, hbl, ustar, bfsfc,
     O     wm, ws )

c     compute turbulent velocity scales.
c     use a 2D-lookup table for wm and ws as functions of ustar and
c     zetahat (=vonk*sigma*hbl*bfsfc).
c
c     note: the lookup table is only used for unstable conditions
c     (zehat.le.0), in the stable domain wm (=ws) gets computed
c     directly.
c 
      IMPLICIT NONE

#include "SIZE.h"
#include "KPPMIX.h"

c input
c------
c imt     : number of horizontal grid points
c sigma   : normalized depth (d/hbl)
c hbl     : boundary layer depth (m)
c ustar   : surface friction velocity         (m/s)
c bfsfc   : total surface buoyancy flux       (m^2/s^3)
      integer imt
      _RS sigma(imt)
      _RS hbl  (imt)           ! boundary layer depth (m)
      _RS ustar(imt)
      _RS bfsfc(imt)
 
c  output
c--------
c wm, ws  : turbulent velocity scales at sigma
      _RS wm(imt), ws(imt)
 
c local
c------
c zehat   : = zeta *  ustar**3
      _RS zehat

      integer iz, izp1, ju, i, jup1
      _RS udiff, zdiff, zfrac, ufrac, fzfrac, wam, wbm, was, wbs, u3

c-----------------------------------------------------------------------
c use lookup table for zehat < zmax only; otherwise use
c stable formulae
c-----------------------------------------------------------------------

      do i = 1, imt
         zehat = vonk*sigma(i)*hbl(i)*bfsfc(i)

         if (zehat .le. zmax) then

            zdiff = zehat - zmin
            iz    = int( zdiff / deltaz )
            iz    = min( iz, nni )
            iz    = max( iz, 0 )
            izp1  = iz + 1
 
            udiff = ustar(i) - umin
            ju    = int( udiff / deltau )
            ju    = min( ju, nnj )
            ju    = max( ju, 0 )
            jup1  = ju + 1
 
            zfrac = zdiff / deltaz - float(iz)
            ufrac = udiff / deltau - float(ju)
 
            fzfrac= 1. - zfrac
            wam   = fzfrac     * wmt(iz,jup1) + zfrac * wmt(izp1,jup1)
            wbm   = fzfrac     * wmt(iz,ju  ) + zfrac * wmt(izp1,ju  )
            wm(i) = (1.-ufrac) * wbm          + ufrac * wam
 
            was   = fzfrac     * wst(iz,jup1) + zfrac * wst(izp1,jup1)
            wbs   = fzfrac     * wst(iz,ju  ) + zfrac * wst(izp1,ju  )
            ws(i) = (1.-ufrac) * wbs          + ufrac * was
 
         else
 
            u3    = ustar(i) * ustar(i) * ustar(i)
            wm(i) = vonk     * ustar(i) * u3 / (u3 + conc1 * zehat)
            ws(i) = wm(i)
 
         endif
 
      end do
 
      return 
      end

c*************************************************************************

      subroutine wscale1 (
     I     imt, sigma, hbl, ustar, bfsfc,
     O     ws )

c     compute turbulent velocity scales.
c     use a 2D-lookup table for wm and ws as functions of ustar and
c     zetahat (=vonk*sigma*hbl*bfsfc).
c
c     note: the lookup table is only used for unstable conditions
c     (zehat.le.0), in the stable domain wm (=ws) gets computed
c     directly.
c 
      IMPLICIT NONE

#include "SIZE.h"
#include "KPPMIX.h"

c input
c------
c imt     : number of horizontal grid points
c sigma   : normalized depth (d/hbl)
c hbl     : boundary layer depth (m)
c ustar   : surface friction velocity         (m/s)
c bfsfc   : total surface buoyancy flux       (m^2/s^3)
      integer imt
      _RS sigma(imt)
      _RS hbl  (imt)           ! boundary layer depth (m)
      _RS ustar(imt)
      _RS bfsfc(imt)
 
c  output
c--------
c ws  : turbulent velocity scales at sigma
      _RS ws(imt)

c local
c------
c zehat   : = zeta *  ustar**3
      _RS zehat

      integer iz, izp1, ju, i, jup1
      _RS udiff, zdiff, zfrac, ufrac, fzfrac, was, wbs

      _RS u3

c-----------------------------------------------------------------------
c use lookup table for zehat < zmax only; otherwise use
c stable formulae
c-----------------------------------------------------------------------
      do i = 1, imt
         zehat = vonk*sigma(i)*hbl(i)*bfsfc(i)

         if (zehat .le. zmax) then

            zdiff = zehat - zmin
            iz    = int( zdiff / deltaz )
            iz    = min( iz, nni )
            iz    = max( iz, 0 )
            izp1  = iz + 1
 
            udiff = ustar(i) - umin
            ju    = int( udiff / deltau )
            ju    = min( ju, nnj )
            ju    = max( ju, 0 )
            jup1  = ju + 1
 
            zfrac = zdiff / deltaz - float(iz)
            ufrac = udiff / deltau - float(ju)

            fzfrac= 1. - zfrac

            was   = fzfrac     * wst(iz,jup1) + zfrac * wst(izp1,jup1)
            wbs   = fzfrac     * wst(iz,ju  ) + zfrac * wst(izp1,ju  )
            ws(i) = (1.-ufrac) * wbs          + ufrac * was
 
         else
            u3    = ustar(i) * ustar(i) * ustar(i)
            ws(i) = vonk     * ustar(i) * u3 / (u3 + conc1 * zehat)
         endif
 
      end do
 
      return 
      end

c*************************************************************************
 
      subroutine Ri_iwmix (
     I       km, kmp1, imt, kmtj, shsq, dbloc
     I     , ikey
     O     , diffus )

c     compute interior viscosity diffusivity coefficients due
c     to shear instability (dependent on a local Richardson number),
c     to background internal wave activity, and
c     to static instability (local Richardson number < 0).

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPPMIX.h"

c  input
      integer km, kmp1          ! number of vertical levels
      integer imt               ! number of horizontal grid points
      integer kmtj (imt)        ! number of vertical layers on this row
      _RS    shsq (imt,km)      ! (local velocity shear)^2              ((m/s)^2)
      _RS    dbloc(imt,km)      ! local delta buoyancy                    (m/s^2)
      integer ikey
 
c output
c     diffus(imt,0:kmp1,1)     ! vertical viscosivity coefficient        (m^2/s)
c     diffus(imt,0:kmp1,2)     ! vertical scalar diffusivity             (m^2/s)
c     diffus(imt,0:kmp1,3)     ! vertical temperature diffusivity        (m^2/s)
      _RS diffus(imt,0:kmp1,3)

c local variables
      _RS Rigg                 ! local Richardson number
      _RS fRi, fcon            ! function of Rig
      _RS ratio
      integer i, ki, mr
      _RS c1, c0

c constants
      c1 = 1.0
      c0 = 0.0

c-----------------------------------------------------------------------
c     compute interior gradient Ri at all interfaces ki=1,km, (not surface)
c     use diffus(*,*,1) as temporary storage of Ri to be smoothed
c     use diffus(*,*,2) as temporary storage for Brunt-Vaisala squared
c     set values at bottom and below to nearest value above bottom
#ifdef RI_IWMIX_ORG
      do ki = 1, km
         do i = 1, imt
            if     (kmtj(i) .EQ. 0      ) then
               diffus(i,ki,1) = 0.
               diffus(i,ki,2) = 0.
            elseif (ki      .GE. kmtj(i)) then
               diffus(i,ki,1) = diffus(i,ki-1,1)
               diffus(i,ki,2) = diffus(i,ki-1,2)
            else
               diffus(i,ki,1) = dbloc(i,ki) * (zgrid(ki)-zgrid(ki+1)) /
     1                          ( shsq(i,ki) + epsln )
               diffus(i,ki,2) = dbloc(i,ki) / (zgrid(ki)-zgrid(ki+1))
            endif
         end do
      end do
#else
      do i = 1, imt
         if     (kmtj(i) .EQ. 0      ) then
            do ki = 1, km
               diffus(i,ki,1) = 0.
               diffus(i,ki,2) = 0.
            end do
         else
            do ki = 1, kmtj(i)-1
               diffus(i,ki,1) = dbloc(i,ki) * (zgrid(ki)-zgrid(ki+1)) /
     1                          ( shsq(i,ki) + epsln )
               diffus(i,ki,2) = dbloc(i,ki) / (zgrid(ki)-zgrid(ki+1))
            end do
            do ki = kmtj(i), km
               diffus(i,ki,1) = diffus(i,kmtj(i)-1,1)
               diffus(i,ki,2) = diffus(i,kmtj(i)-1,2)
            end do
         endif
      end do
#endif

c-----------------------------------------------------------------------
c     vertically smooth Ri
 
      do mr = 1, num_v_smooth_Ri
         call z121 (
     I     kmp1, imt,
     U     diffus(1,0,1))
      end do

      do mr = 1, num_v_smooth_BV
         call z121 (
     I     kmp1, imt,
     U     diffus(1,0,2))
      end do

CADJ store diffus = kpptape, key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy),Nr+2,3 /)

c-----------------------------------------------------------------------
c                           after smoothing loop

      do ki = 1, km
         do i = 1, imt
 
c  evaluate f of Brunt-Vaisala squared for convection, store in fcon

            Rigg  = max ( diffus(i,ki,2) , BVSQcon )
            ratio = min ( (BVSQcon - Rigg) / BVSQcon, c1 )
            fcon  = c1 - ratio * ratio
            fcon  = fcon * fcon * fcon
 
c  evaluate f of smooth Ri for shear instability, store in fRi

            Rigg  = max ( diffus(i,ki,1), c0 )
            ratio = min ( Rigg / Riinfty , c1 )
            fRi   = c1 - ratio * ratio
            fRi   = fRi * fRi * fRi
 
c ----------------------------------------------------------------------
c            evaluate diffusivities and viscosity
c    mixing due to internal waves, and shear and static instability
 
            diffus(i,ki,1) = difmiw + fcon * difmcon + fRi * difm0
            diffus(i,ki,2) = difsiw + fcon * difscon + fRi * difs0
            diffus(i,ki,3) = diffus(i,ki,2)

         end do
      end do
 
c ------------------------------------------------------------------------
c         set surface values to 0.0
 
      do i = 1, imt
         diffus(i,0,1) = c0
         diffus(i,0,2) = c0
         diffus(i,0,3) = c0
      end do
 
      return 
      end 

c*************************************************************************

      subroutine z121 (
     I     kmp1, imt,
     U     v )

c     Apply 121 smoothing in k to 2-d array V(i,k=1,km)
c     top (0) value is used as a dummy
c     bottom (kmp1) value is set to input value from above.

      IMPLICIT NONE

c input
      integer kmp1, imt

c input/output 
c-------------
c v     : 2-D array to be smoothed in kmp1 direction
      _RS v(imt,0:kmp1)

c local
      _RS p25, p5, tmp
      integer i, k, km

      km = kmp1 - 1
      p25 = 0.25
      p5 = 0.5
 
      do i = 1, imt
         v(i,0) = p25*v(i,1)
         v(i,kmp1) = v(i,km)
      end do
 
      do k = 1, km
         do i = 1, imt
            tmp = v(i,k)
            v(i,k) = v(i,0) + p5*v(i,k) + p25*v(i,k+1)
            v(i,0) = p25*tmp
         end do
      end do
 
      return 
      end 

c*************************************************************************

      subroutine blmix (
     I       ustar, bfsfc, hbl, stable, casea, diffus, kbl
     O     , dkm1, blmc, ghat, sigma
     &     )

c     mixing coefficients within boundary layer depend on surface
c     forcing and the magnitude and gradient of interior mixing below
c     the boundary layer ("matching").
c
c     caution: if mixing bottoms out at hbl = -zgrid(km) then
c     fictitious layer at kmp1 is needed with small but finite width
c     hwide(kmp1) (eg. epsln = 1.e-20).
c
      IMPLICIT NONE

#include "SIZE.h"
#include "KPPMIX.h"

      integer    km     , kmp1
      parameter( km = Nr, kmp1 = Nr + 1 )
      integer    imt
      parameter( imt = (sNx+2*OLx)*(sNy+2*OLy))
      integer    mdiff
      parameter( mdiff = 3 )

c input
      _RS ustar (imt)          ! surface friction velocity                 (m/s)
      _RS bfsfc (imt)          ! surface buoyancy forcing              (m^2/s^3)
      _RS hbl   (imt)          ! boundary layer depth                        (m)
      _RS stable(imt)          ! = 1 in stable forcing
      _RS casea (imt)          ! = 1 in case A
      _RS diffus(imt,0:kmp1,mdiff) ! vertical diffusivities              (m^2/s)
      integer kbl(imt)         ! -1 of first grid level below hbl

c output
      _RS dkm1  (imt,mdiff)    ! boundary layer difs at kbl-1 level
      _RS blmc  (imt,km,mdiff) ! boundary layer mixing coefficients      (m^2/s)
      _RS ghat  (imt,km)       ! nonlocal scalar transport
      _RS sigma (imt)          ! normalized depth (d / hbl)

c  local
cc    _RS gat1  (imt,mdiff)    ! now dkm1! shape function at sigma = 1
      _RS dat1  (imt,mdiff)    ! derivative of shape function at sigma = 1
      _RS ws(imt), wm(imt)     ! turbulent velocity scales                (m/s)
      integer i, kn, ki
      _RS     R, dvdzup, dvdzdn, viscp
      _RS     difsp, diftp, visch, difsh, difth
      _RS     f1, sig, a1, a2, a3, delhat
      _RS     GM, Gs, Gt

      _RS        p0
      parameter (p0 = 0.0)
 
c-----------------------------------------------------------------------
c compute velocity scales at hbl
c-----------------------------------------------------------------------

      do i = 1, imt
         sigma(i) = stable(i) * 1.0 + (1. - stable(i)) * epsilon
      end do

      call wscale (
     I        imt, sigma, hbl, ustar, bfsfc,
     O        wm, ws )

      do i = 1, imt
Caja     kn = ifix(casea(i)+epsln) * (kbl(i)-1) +
Caja 1        (1 - ifix(casea(i)+epsln)) * kbl(i)
         kn = int(casea(i)+epsln) * (kbl(i)-1) +
     1        (1 - int(casea(i)+epsln)) * kbl(i)

c-----------------------------------------------------------------------
c find the interior viscosities and derivatives at hbl(i)
c-----------------------------------------------------------------------

         delhat = 0.5*hwide(kn) - zgrid(kn) - hbl(i)
         R      = 1.0 - delhat / hwide(kn)
         dvdzup = (diffus(i,kn-1,1) - diffus(i,kn  ,1)) / hwide(kn)
         dvdzdn = (diffus(i,kn  ,1) - diffus(i,kn+1,1)) / hwide(kn+1)
         viscp  = 0.5 * ( (1.-R) * (dvdzup + abs(dvdzup)) +
     1                        R  * (dvdzdn + abs(dvdzdn))  )

         dvdzup = (diffus(i,kn-1,2) - diffus(i,kn  ,2)) / hwide(kn)
         dvdzdn = (diffus(i,kn  ,2) - diffus(i,kn+1,2)) / hwide(kn+1)
         difsp  = 0.5 * ( (1.-R) * (dvdzup + abs(dvdzup)) +
     1                        R  * (dvdzdn + abs(dvdzdn))  )

         dvdzup = (diffus(i,kn-1,3) - diffus(i,kn  ,3)) / hwide(kn)
         dvdzdn = (diffus(i,kn  ,3) - diffus(i,kn+1,3)) / hwide(kn+1)
         diftp  = 0.5 * ( (1.-R) * (dvdzup + abs(dvdzup)) +
     1                        R  * (dvdzdn + abs(dvdzdn))  )

         visch  = diffus(i,kn,1) + viscp * delhat
         difsh  = diffus(i,kn,2) + difsp * delhat
         difth  = diffus(i,kn,3) + diftp * delhat

c
         f1 = stable(i) * conc1 * bfsfc(i) / (ustar(i)**4+epsln)
 
         dkm1(i,1) = visch / hbl(i) / (wm(i)+epsln)
         dat1(i,1) = -viscp / (wm(i)+epsln) + f1 * visch
         dat1(i,1) = min(dat1(i,1),p0)
 
         dkm1(i,2) = difsh / hbl(i) / (ws(i)+epsln)
         dat1(i,2) = -difsp / (ws(i)+epsln) + f1 * difsh
         dat1(i,2) = min(dat1(i,2),p0)
 
         dkm1(i,3) = difth / hbl(i) / (ws(i)+epsln)
         dat1(i,3) = -diftp / (ws(i)+epsln) + f1 * difth
         dat1(i,3) = min(dat1(i,3),p0)
 
      end do

      do ki = 1, km

c-----------------------------------------------------------------------
c     compute turbulent velocity scales on the interfaces
c-----------------------------------------------------------------------

         do i = 1, imt
            sig      = (-zgrid(ki) + 0.5 * hwide(ki)) / hbl(i)
            sigma(i) = stable(i)*sig + (1.-stable(i))*min(sig,epsilon)
         end do
         call wscale (
     I        imt, sigma, hbl, ustar, bfsfc,
     O        wm, ws )

c-----------------------------------------------------------------------
c     compute the dimensionless shape functions at the interfaces
c-----------------------------------------------------------------------

         do i = 1, imt
            sig = (-zgrid(ki) + 0.5 * hwide(ki)) / hbl(i)
            a1 = sig - 2.
            a2 = 3. - 2. * sig
            a3 = sig - 1.

            GM = a1 + a2 * dkm1(i,1) + a3 * dat1(i,1)
            Gs = a1 + a2 * dkm1(i,2) + a3 * dat1(i,2)
            Gt = a1 + a2 * dkm1(i,3) + a3 * dat1(i,3)

c-----------------------------------------------------------------------
c     compute boundary layer diffusivities at the interfaces
c-----------------------------------------------------------------------

            blmc(i,ki,1) = hbl(i) * wm(i) * sig * (1. + sig * GM)
            blmc(i,ki,2) = hbl(i) * ws(i) * sig * (1. + sig * Gs)
            blmc(i,ki,3) = hbl(i) * ws(i) * sig * (1. + sig * Gt)

c-----------------------------------------------------------------------
c     nonlocal transport term = ghat * <ws>o
c-----------------------------------------------------------------------

            ghat(i,ki) = (1. - stable(i)) * cg
     &           / (ws(i) * hbl(i) + epsln)
 
         end do
      end do

c-----------------------------------------------------------------------
c find diffusivities at kbl-1 grid level
c-----------------------------------------------------------------------

      do i = 1, imt
         sig      = -zgrid(kbl(i)-1) / hbl(i)
         sigma(i) = stable(i) * sig 
     &            + (1. - stable(i)) * min(sig,epsilon)
      end do

      call wscale (
     I        imt, sigma, hbl, ustar, bfsfc,
     O        wm, ws )

      do i = 1, imt
         sig = -zgrid(kbl(i)-1) / hbl(i)
         a1 = sig - 2.
         a2 = 3. - 2. * sig
         a3 = sig - 1.
         GM = a1 + a2 * dkm1(i,1) + a3 * dat1(i,1)
         Gs = a1 + a2 * dkm1(i,2) + a3 * dat1(i,2)
         Gt = a1 + a2 * dkm1(i,3) + a3 * dat1(i,3)
         dkm1(i,1) = hbl(i) * wm(i) * sig * (1. + sig * GM)
         dkm1(i,2) = hbl(i) * ws(i) * sig * (1. + sig * Gs)
         dkm1(i,3) = hbl(i) * ws(i) * sig * (1. + sig * Gt)
      end do

      return 
      end 

c*************************************************************************

      subroutine enhance ( 
     I     km, kmp1, imt, mdiff
     I     , dkm1, hbl, kbl, diffus, casea
     U     , ghat
     O     , blmc
     &     )

c enhance the diffusivity at the kbl-.5 interface

      IMPLICIT NONE

#include "SIZE.h"
#include "KPPMIX.h"

c input
      integer km, kmp1          ! number of vertical levels
      integer imt               ! number of horizontal grid points
      integer mdiff             ! number of viscosities + diffusivities
      _RS     dkm1  (imt,mdiff) ! bl diffusivity at kbl-1 grid level
      _RS     hbl   (imt)       ! boundary layer depth                        (m)
      integer kbl   (imt)       ! grid above hbl
      _RS     diffus(imt,0:kmp1,mdiff) ! vertical diffusivities           (m^2/s)
      _RS     casea (imt)       ! = 1 in caseA, = 0 in case B

c input/output
      _RS     ghat (imt,km)     ! nonlocal transport
c                               ! modified ghat at kbl(i)-1 interface   (s/m**2)

c output
      _RS     blmc  (imt,km,mdiff) ! enhanced bound. layer mixing coeff.

c local
      _RS delta                ! fraction hbl lies beteen zgrid neighbors
      integer ki, i, md
      _RS dkmp5, dstar

      do i = 1, imt
         ki = kbl(i)-1
         if ((ki .ge. 1) .and. (ki .lt. km)) then
            delta = (hbl(i) + zgrid(ki)) / (zgrid(ki) - zgrid(ki+1))
            do md = 1, mdiff
               dkmp5         =      casea(i)  * diffus(i,ki,md) +
     1                         (1.- casea(i)) * blmc  (i,ki,md)
               dstar         = (1.- delta)**2 * dkm1(i,md)
     &                       + delta**2 * dkmp5
               blmc(i,ki,md) = (1.- delta)*diffus(i,ki,md)
     &                       + delta*dstar
            end do
            ghat(i,ki) = (1.- casea(i)) * ghat(i,ki)
         endif
      end do

      return 
      end 

c*************************************************************************

      subroutine KMIXINIT ( myThid )

c     initialize some constants for kppmix.F subroutines, including
c     the 2D-lookup table for wm and ws as functions of ustar and
c     zetahat (=vonk*sigma*hbl*bfsfc) for subroutine wscale

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "KPPMIX.h"
#include "FFIELDS.h"

c routine arguments
c     myThid -  Number of this instance of KMIXINIT
      INTEGER myThid

c local variables
      _RS zehat                ! = zeta * ustar**3
      _RS zeta                 ! = stability parameter d/l
      _RS usta
      INTEGER bi, bj            ! Loop counters
      integer i, j, k

      _RS        p5
      parameter (p5 = 0.5)

C   initialization of most KPP model parameters has been moved to ini_parms.F
C   and can be specified in PARM06 of the data file

c-----------------------------------------------------------------------
c construct the wm and ws lookup tables
c-----------------------------------------------------------------------

      deltaz = (zmax - zmin)/(nni + 1)
      deltau = (umax - umin)/(nnj + 1)
 
      do i = 0, nni + 1
         zehat = deltaz*i + zmin
         do j = 0, nnj + 1
            usta = deltau*j + umin
            zeta = zehat/(usta**3 + epsln)
 
            if (zehat .ge. 0.) then
               wmt(i,j) = vonk*usta/(1. + conc1*zeta)
               wst(i,j) = wmt(i,j)
            else
               if (zeta .gt. zetam) then
                  wmt(i,j) = vonk*usta*(1. - conc2*zeta)**(1./4.)
               else
                  wmt(i,j) = vonk*(conam*usta**3 - concm*zehat)**(1./3.)
               endif
               if (zeta .gt. zetas) then
                  wst(i,j) = vonk*usta*(1. - conc3*zeta)**(1./2.)
               else
                  wst(i,j) = vonk*(conas*usta**3 - concs*zehat)**(1./3.)
               endif
            endif
         end do
      end do
c
      do bj = myByLo(myThid), myByHi(myThid)
         do bi = myBxLo(myThid), myBxHi(myThid)

c     initialize KPPhbl
            do j = 1-OLy, sNy+OLy
               do i = 1-OLx, sNx+OLx
                  KPPhbl(i,j,bi,bj) = 0.
               end do
            end do

c     initialize KPPghat, KPPviscAz, KPPdiffKzT, KPPdiffKzS
c     calculate mask pMask for pressure/tracer cells (0 => land, 1 => water)

            do k = 1, Nr
               do j = 1-OLy, sNy+OLy
                  do i = 1-OLx, sNx+OLx
                     KPPghat       (i,j,k,bi,bj) = 0.
                     KPPviscAz  (i,j,k,bi,bj) = viscAz
                     KPPdiffKzT (i,j,k,bi,bj) = diffKzT
                     KPPdiffKzS (i,j,k,bi,bj) = diffKzS
                     if (_hFacC(i,j,k,bi,bj).eq.0.) then
                        pMask (i,j,k,bi,bj) = 0.
                     else
                        pMask (i,j,k,bi,bj) = 1.
                     endif
                  end do
               end do
            end do

c     compute maximum number of wet levels in each column
            do j = 1-OLy, sNy+OLy
               do i = 1-OLx, sNx+OLx
                  nzmax(i,j,bi,bj) = 0
                  do k = 1, Nr
                     nzmax(i,j,bi,bj) = nzmax(i,j,bi,bj)
     &                                + INT(pMask(i,j,k,bi,bj))
                  end do
               end do
            end do

         end do
      end do
      
c     vertical grid
c     -------------

      zgrid(0)  =  epsln
      hwide(0)  =  epsln
      zgrid(1)  = -delZ(1)*0.5
      hwide(1)  =  delZ(1)

      do k = 2, Nr
         zgrid(k) = zgrid(k-1) - (delZ(k-1)+delZ(k))*0.5
         hwide(k) = delZ(k)
      end do

      k = Nr + 1
      zgrid(k) = zgrid(k-1) * 100.
      hwide(k) = epsln
 
      return 
      end 

c*************************************************************************

      SUBROUTINE STATEKPP (
     I     bi, bj, myThid,
     O     RHO1, DBLOC, DBSFC, TTALPHA, SSBETA)
c
c-----------------------------------------------------------------------
c     "statekpp" computes all necessary input arrays
c     for the kpp mixing scheme
c
c     input:
c      bi, bj = array indices on which to apply calculations
c
c     output:
c      rho1   = potential density of surface layer                       (kg/m^3)
c      dbloc  = local buoyancy gradient at Nr interfaces
c               g/rho{k+1,k+1} * [ drho{k,k+1}-drho{k+1,k+1} ]            (m/s^2)
c      dbsfc  = buoyancy difference with respect to the surface
c               g * [ drho{1,k}/rho{1,k} - drho{k,k}/rho{k,k} ]           (m/s^2)
c      ttalpha= thermal expansion coefficient without 1/rho factor
c               d(rho) / d(potential temperature)                      (kg/m^3/C)
c      ssbeta = salt expansion coefficient without 1/rho factor
c               d(rho) / d(salinity)                                 (kg/m^3/PSU)
c
c     see also subroutines find_rho.F find_alpha.F find_beta.F
c
c     written  by: jan morzel,    feb. 10, 1995 (converted from "sigma" version)
c     modified by: d. menemenlis,     june 1998 : for use with MIT GCM UV
c
c-----------------------------------------------------------------------

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c-------------- Routine arguments -----------------------------------------
      INTEGER bi, bj, myThid
      _RS RHO1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS DBLOC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS DBSFC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS TTALPHA(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RS SSBETA (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)

c--------------------------------------------------------------------------
c
c     local arrays:
c
c     rhok         - density of t(k  ) & s(k  ) at depth k
c     rhokm1       - density of t(k-1) & s(k-1) at depth k
c     rho1k        - density of t(1  ) & s(1  ) at depth k
c     work1, work2 - work arrays for holding horizontal slabs

      _RL RHOK  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL RHOKM1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL RHO1K (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL WORK1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL WORK2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL WORK3 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER I, J, K

c calculate density, alpha, beta in surface layer, and set dbsfc to zero

      call FIND_RHO(
     I     bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,  1, 1, eosType,
     O     WORK1,
     I     myThid )

      call FIND_ALPHA(
     I     bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,  1, 1, eosType,
     O     WORK2 )

      call FIND_BETA(
     I     bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,  1, 1, eosType,
     O     WORK3 )

      DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
            RHO1(I,J)      = WORK1(I,J) + rhonil
            TTALPHA(I,J,1) = WORK2(I,J)
            SSBETA(I,J,1)  = WORK3(I,J)
            DBSFC(I,J,1)   = 0.
         END DO
      END DO

c calculate alpha, beta, and gradients in interior layers

!HPF$  INDEPENDENT, NEW (RHOK,RHOKM1,RHO1K,WORK1,WORK2)
      DO K = 2, Nr

         call FIND_RHO(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,  K,   K, eosType,
     O        RHOK,
     I        myThid )

         call FIND_RHO(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,  K-1, K, eosType,
     O        RHOKM1,
     I        myThid )

         call FIND_RHO(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,  1,   K, eosType,
     O        RHO1K,
     I        myThid )

         call FIND_ALPHA(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,  K,   K, eosType,
     O        WORK1 )

         call FIND_BETA(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy,  K,   K, eosType,
     O        WORK2 )

         DO J = 1-OLy, sNy+OLy
            DO I = 1-OLx, sNx+OLx
               RHOK(I,J)      = RHOK  (I,J) + rhonil
               RHOKM1(I,J)    = RHOKM1(I,J) + rhonil
               RHO1K(I,J)     = RHO1K (I,J) + rhonil
               TTALPHA(I,J,K) = WORK1 (I,J)
               SSBETA(I,J,K)  = WORK2 (I,J)
               DBLOC(I,J,K-1) = gravity * (1. - RHOKM1(I,J) / RHOK(I,J))
               DBSFC(I,J,K)   = gravity * (1. - RHO1K (I,J) / RHOK(I,J))
            END DO
         END DO

      END DO

c     compute arrays for K = Nr+1
      DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
            TTALPHA(I,J,Nr+1) = TTALPHA(I,J,Nr)
            SSBETA(I,J,Nr+1)  = SSBETA(I,J,Nr)
            DBLOC(I,J,Nr)     = 0.
         END DO
      END DO

      RETURN
      END

c*************************************************************************

      subroutine PRINT_DIAGS (
     I     work_array, horiz_size, vert_size
     I     , third_size, third_index, array_id
     &     )

c-------------------------------------------------------------------------
c
c     Print diagnostics for work_array
c     prints number of zero and nonzero values
c     and minimum, maximum, and mean of non zero values
c
c-----------------------------------------------------------------------

      IMPLICIT NONE

#include "EEPARAMS.h"

c input
      integer horiz_size        ! number of horizontal elements
      integer vert_size         ! number of vertical elements
      integer third_size        ! third array dimension
      integer third_index       ! index for third array dimension
      _RS     work_array ( horiz_size, vert_size, third_size )
      character*(50) array_id   ! name of array

c local
c msgBuf    : informational/error message buffer
      character*(MAX_LEN_MBUF) msgBuf

      integer i, k, zero_val, nonzero_val
      _RS     min_val, max_val, mean_val

c identify array and label output
      WRITE(msgBuf,'(A,A)') 'KPP diagnostic: ', array_id
      CALL PRINT_MESSAGE(
     &     msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A)')
     1     'level     zeros non-zeros   minimum      mean   maximum'
      CALL PRINT_MESSAGE(
     &     msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1)

c main loop
      do k = 1, vert_size
         zero_val    =  0
         nonzero_val =  0
         min_val     =  1.e20
         max_val     = -1.e20
         mean_val    =  0.
         do i = 1, horiz_size
            if ( work_array(i,k,third_index) .eq. 0. ) then
               zero_val    = zero_val    +  1
            else
               nonzero_val = nonzero_val +  1
               mean_val = mean_val    +  work_array(i,k,third_index)
               min_val  = min( min_val, work_array(i,k,third_index) )
               max_val  = max( max_val, work_array(i,k,third_index) )
            endif
         enddo
         if ( nonzero_val .EQ. 0 ) then
            min_val = 0.
            max_val = 0.
         else
            mean_val = mean_val / nonzero_val
         endif
         WRITE(msgBuf,'(I5,I10,I10,E10.2,E10.2,E10.2)')
     1        k, zero_val, nonzero_val, min_val, mean_val, max_val
         CALL PRINT_MESSAGE(
     &        msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      enddo

      end

c*************************************************************************

      subroutine PRINT_MAPS (
     I     fld, i_size, j_size, k_size, k_index, fldNam )

c-------------------------------------------------------------------------
c
c     Diagnostic map for work_array
c
c-----------------------------------------------------------------------

      IMPLICIT NONE

#include "EEPARAMS.h"

C--   input
      integer i_size            ! i array dimension
      integer j_size            ! j array dimension
      integer k_size            ! k array dimension
      integer k_index           ! index for k array dimension
      _RS fld ( i_size, j_size, k_size ) ! working array
      CHARACTER*(*) fldNam      ! name of array

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) fldTitle
      INTEGER iStart, iEnd, iStride
      INTEGER jStart, jEnd, jStride
      INTEGER kStart, kEnd, kStride
      INTEGER biStart, biEnd, biStride
      INTEGER bjStart, bjEnd, bjStride

C--   Form name for identifying "plot"
      WRITE(fldTitle,'(A,A)') '// Field ', fldNam

C--    Do "plot" using textual contour map "execution environment" routine
C      Substitute other plotting utilities here!
       iStart   =  1
       iEnd     =  i_size
       iStride  =  1
       jStart   =  j_size
       jEnd     =  1
       jStride  =  -1
       kStart   =  k_index
       kEnd     =  k_index
       kStride  =  1
       biStart  =  1
       biEnd    =  1
       biStride =  1
       bjStart  =  1
       bjEnd    =  1    
       bjStride =  1
       CALL PRINT_MAPRS(
     I        fld, fldTitle, PRINT_MAP_XY,
     I         1, i_size, 1, j_size, 1, k_size, 1, 1,
     I         iStart,   iEnd,  iStride,
     I         jStart,   jEnd,  jStride,
     I         kStart,   kEnd,  kStride,
     I        biStart,  biEnd, biStride,
     I        bjStart,  bjEnd, bjStride )

#endif

      RETURN
      END
