C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_routines.F,v 1.7 2001/02/04 14:38:50 cnh Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

C-- File kpp_routines.F: subroutines needed to implement
C--                      KPP vertical mixing scheme
C--  Contents
C--  o KPPMIX      - Main driver and interface routine.
C--  o BLDEPTH     - Determine oceanic planetary boundary layer depth.
C--  o WSCALE      - Compute turbulent velocity scales.
C--  o RI_IWMIX    - Compute interior viscosity diffusivity coefficients.
C--  o Z121        - Apply 121 vertical smoothing.
C--  o KPP_SMOOTH_HORIZ - Apply horizontal smoothing to KPP array.
C--  o SMOOTH_HORIZ     - Apply horizontal smoothing to global array.
C--  o BLMIX       - Boundary layer mixing coefficients.
C--  o ENHANCE     - Enhance diffusivity at boundary layer interface.
C--  o STATEKPP    - Compute buoyancy-related input arrays.

c***********************************************************************

      SUBROUTINE KPPMIX (
     I       mytime, mythid
     I     , kmtj, shsq, dvsq, ustar
     I     , bo, bosol, dbloc, Ritop, coriol
     I     , ikey
     O     , diffus
     U     , ghat
     O     , hbl )

c-----------------------------------------------------------------------
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
#include "FFIELDS.h"
#include "KPP_PARAMS.h"

c input
c     myTime          - current time in simulation
c     myThid          - thread number for this instance of the routine
c     kmtj   (imt)    - number of vertical layers on this row
c     shsq   (imt,Nr) - (local velocity shear)^2                      ((m/s)^2)
c     dvsq   (imt,Nr) - (velocity shear re sfc)^2                     ((m/s)^2)
c     ustar  (imt)    - surface friction velocity                         (m/s)
c     bo     (imt)    - surface turbulent buoy. forcing               (m^2/s^3)
c     bosol  (imt)    - radiative buoyancy forcing                    (m^2/s^3)
c     dbloc  (imt,Nr) - local delta buoyancy across interfaces          (m/s^2)
c     dblocSm(imt,Nr) - horizontally smoothed dbloc                     (m/s^2)
c                         stored in ghat to save space
c     Ritop  (imt,Nr) - numerator of bulk Richardson Number
c                         (zref-z) * delta buoyancy w.r.t. surface    ((m/s)^2)
c     coriol (imt)    - Coriolis parameter                                (1/s)
c     note: there is a conversion from 2-D to 1-D for input output variables,
c           e.g., hbl(sNx,sNy) -> hbl(imt),
c           where hbl(i,j) -> hbl((j-1)*sNx+i)

      _RL     mytime
      integer mythid
      integer kmtj  (imt   )
      _KPP_RL shsq  (imt,Nr)
      _KPP_RL dvsq  (imt,Nr)
      _KPP_RL ustar (imt   )
      _KPP_RL bo    (imt   )
      _KPP_RL bosol (imt   )
      _KPP_RL dbloc (imt,Nr)
      _KPP_RL Ritop (imt,Nr)
      _KPP_RL coriol(imt   )

      integer ikey

c output
c     diffus (imt,1)  - vertical viscosity coefficient                  (m^2/s)
c     diffus (imt,2)  - vertical scalar diffusivity                     (m^2/s)
c     diffus (imt,3)  - vertical temperature diffusivity                (m^2/s)
c     ghat   (imt)    - nonlocal transport coefficient                  (s/m^2)
c     hbl    (imt)    - mixing layer depth                                  (m)

      _KPP_RL diffus(imt,0:Nrp1,mdiff)
      _KPP_RL ghat  (imt,Nr)
      _KPP_RL hbl   (imt)

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

      integer kbl   (imt         )
      _KPP_RL bfsfc (imt         )
      _KPP_RL casea (imt         )
      _KPP_RL stable(imt         )
      _KPP_RL dkm1  (imt,   mdiff)
      _KPP_RL blmc  (imt,Nr,mdiff)
      _KPP_RL sigma (imt         )
      _KPP_RL Rib   (imt,Nr      )

      integer i, k, md

c-----------------------------------------------------------------------
c compute interior mixing coefficients everywhere, due to constant
c internal wave activity, static instability, and local shear
c instability.
c (ghat is temporary storage for horizontally smoothed dbloc)
c-----------------------------------------------------------------------

CADJ STORE ghat = comlev1_kpp, key = ikey

      call Ri_iwmix (
     I       kmtj, shsq, dbloc, ghat
     I     , ikey
     O     , diffus )

c-----------------------------------------------------------------------
c set seafloor values to zero and fill extra "Nrp1" coefficients
c for blmix
c-----------------------------------------------------------------------

      do md = 1, mdiff
         do i = 1,imt
            do k=kmtj(i),Nrp1
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
     I       mytime, mythid
     I     , kmtj
     I     , dvsq, dbloc, Ritop, ustar, bo, bosol, coriol
     I     , ikey
     O     , hbl, bfsfc, stable, casea, kbl, Rib, sigma
     &     )

CADJ STORE hbl,bfsfc,stable,casea,kbl = comlev1_kpp, key = ikey

c-----------------------------------------------------------------------
c compute boundary layer diffusivities
c-----------------------------------------------------------------------

      call blmix (
     I       ustar, bfsfc, hbl, stable, casea, diffus, kbl
     O     , dkm1, blmc, ghat, sigma, ikey
     &     )

CADJ STORE dkm1,blmc,ghat = comlev1_kpp, key = ikey

c-----------------------------------------------------------------------
c enhance diffusivity at interface kbl - 1
c-----------------------------------------------------------------------

      call enhance (
     I       dkm1, hbl, kbl, diffus, casea
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

#endif /* ALLOW_KPP */

      return 
      end

c*************************************************************************

      subroutine bldepth (
     I       mytime, mythid
     I     , kmtj
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
#include "KPP_PARAMS.h"
#include "FFIELDS.h"

c input
c------
c myTime    : current time in simulation
c myThid    : thread number for this instance of the routine
c kmtj      : number of vertical layers
c dvsq      : (velocity shear re sfc)^2             ((m/s)^2)
c dbloc     : local delta buoyancy across interfaces  (m/s^2)
c Ritop     : numerator of bulk Richardson Number
c             =(z-zref)*dbsfc, where dbsfc=delta
c             buoyancy with respect to surface      ((m/s)^2)
c ustar     : surface friction velocity                 (m/s)
c bo        : surface turbulent buoyancy forcing    (m^2/s^3)
c bosol     : radiative buoyancy forcing            (m^2/s^3)
c coriol    : Coriolis parameter                        (1/s)
      _RL     mytime
      integer mythid
      integer kmtj(imt)
      _KPP_RL dvsq  (imt,Nr)
      _KPP_RL dbloc (imt,Nr)
      _KPP_RL Ritop (imt,Nr)
      _KPP_RL ustar (imt)
      _KPP_RL bo    (imt)
      _KPP_RL bosol (imt)
      _KPP_RL coriol(imt)
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
      _KPP_RL hbl   (imt)
      _KPP_RL bfsfc (imt)
      _KPP_RL stable(imt)
      _KPP_RL casea (imt)
      integer kbl   (imt)
      _KPP_RL Rib   (imt,Nr)
      _KPP_RL sigma (imt)

#ifdef ALLOW_KPP

c  local
c-------
c wm, ws    : turbulent velocity scales         (m/s)
      _KPP_RL wm(imt), ws(imt)
      _RL     worka(imt)

      _KPP_RL bvsq, vtsq, hekman, hmonob, hlimit, tempVar1, tempVar2
      integer i, kl

      _KPP_RL     p5    , eins
      parameter ( p5=0.5, eins=1.0 )
      _RL         minusone
      parameter ( minusone=-1.0 )

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

      do kl = 2, Nr

c     compute bfsfc = sw fraction at hbf * zgrid

         do i = 1, imt
            worka(i) = zgrid(kl)
         end do
         call SWFRAC(
     I        imt, hbf, 
     I        mytime, mythid,
     U        worka )

         do i = 1, imt

c     use caseA as temporary array

            casea(i) = -zgrid(kl)

c     compute bfsfc= Bo + radiative contribution down to hbf * hbl

            bfsfc(i) = bo(i) + bosol(i)*(1. - worka(i))
            stable(i) = p5 + sign(p5,bfsfc(i))
            sigma(i) = stable(i) + (1. - stable(i)) * epsilon

         end do

c-----------------------------------------------------------------------
c     compute velocity scales at sigma, for hbl= caseA = -zgrid(kl)
c-----------------------------------------------------------------------
 
         call wscale (
     I        sigma, casea, ustar, bfsfc,
     O        wm, ws )

         do i = 1, imt

c-----------------------------------------------------------------------
c     compute the turbulent shear contribution to Rib
c-----------------------------------------------------------------------

            bvsq = p5 *
     1           ( dbloc(i,kl-1) / (zgrid(kl-1)-zgrid(kl  ))+
     2             dbloc(i,kl  ) / (zgrid(kl  )-zgrid(kl+1)))

            if (bvsq .eq. 0.) then
              vtsq = 0.0
            else
              vtsq = -zgrid(kl) * ws(i) * sqrt(abs(bvsq)) * Vtc
            endif

c     compute bulk Richardson number at new level
c     note: Ritop needs to be zero on land and ocean bottom
c     points so that the following if statement gets triggered
c     correctly; otherwise, hbl might get set to (big) negative
c     values, that might exceed the limit for the "exp" function
c     in "SWFRAC"

c
c     rg: assignment to double precision variable to avoid overflow
c     ph: test for zero nominator
c

            tempVar1  = dvsq(i,kl) + vtsq           
            tempVar2 = max(tempVar1, phepsi)
            Rib(i,kl) = Ritop(i,kl) / tempVar2

         end do
      end do
      
      do kl = 2, Nr
         do i = 1, imt
            if (kbl(i).eq.kmtj(i) .and. Rib(i,kl).gt.Ricr) kbl(i) = kl
         end do
      end do

CADJ store kbl = comlev1_kpp
CADJ &   , key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

      do i = 1, imt
         kl = kbl(i)
c     linearly interpolate to find hbl where Rib = Ricr
         if (kl.gt.1 .and. kl.lt.kmtj(i)) then
            tempVar1 = (Rib(i,kl)-Rib(i,kl-1))
            hbl(i) = -zgrid(kl-1) + (zgrid(kl-1)-zgrid(kl)) *
     1           (Ricr - Rib(i,kl-1)) / tempVar1
         endif
      end do

CADJ store hbl = comlev1_kpp
CADJ &   , key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

c-----------------------------------------------------------------------
c     find stability and buoyancy forcing for boundary layer
c-----------------------------------------------------------------------

      do i = 1, imt
         worka(i) = hbl(i)
      end do
      call SWFRAC(
     I     imt, minusone, 
     I     mytime, mythid,
     U     worka )

      do i = 1, imt
         bfsfc(i)  = bo(i) + bosol(i) * (1. - worka(i))
      end do
CADJ store bfsfc = comlev1_kpp
CADJ &   , key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

c--   ensure bfsfc is never 0
      do i = 1, imt
         stable(i) = p5 + sign( p5, bfsfc(i) )
         bfsfc(i) = sign(eins,bfsfc(i))*max(phepsi,abs(bfsfc(i)))
      end do

CADJ store bfsfc = comlev1_kpp
CADJ &   , key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

c-----------------------------------------------------------------------
c check hbl limits for hekman or hmonob
c     ph: test for zero nominator
c-----------------------------------------------------------------------

      do i = 1, imt
         if (bfsfc(i) .gt. 0.0) then
            hekman = cekman * ustar(i) / max(abs(Coriol(i)),phepsi)
            hmonob = cmonob * ustar(i)*ustar(i)*ustar(i)
     &           / vonk / bfsfc(i)
            hlimit = stable(i) * min(hekman,hmonob)
     &             + (stable(i)-1.) * zgrid(Nr)
            hbl(i) = min(hbl(i),hlimit)
         end if
      end do
CADJ store hbl = comlev1_kpp
CADJ &   , key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

      do i = 1, imt
         hbl(i) = max(hbl(i),minKPPhbl)
         kbl(i) = kmtj(i)
      end do

CADJ store hbl = comlev1_kpp
CADJ &   , key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

c-----------------------------------------------------------------------
c      find new kbl
c-----------------------------------------------------------------------

      do kl = 2, Nr
         do i = 1, imt
            if ( kbl(i).eq.kmtj(i) .and. (-zgrid(kl)).gt.hbl(i) ) then
               kbl(i) = kl
            endif
         end do
      end do

c-----------------------------------------------------------------------
c      find stability and buoyancy forcing for final hbl values
c-----------------------------------------------------------------------

      do i = 1, imt
         worka(i) = hbl(i)
      end do
      call SWFRAC(
     I     imt, minusone, 
     I     mytime, mythid,
     U     worka )
 
      do i = 1, imt
         bfsfc(i) = bo(i) + bosol(i) * (1. - worka(i))
      end do
CADJ store bfsfc = comlev1_kpp
CADJ &   , key = ikey, shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)

c--   ensures bfsfc is never 0
      do i = 1, imt
         stable(i) = p5 + sign( p5, bfsfc(i) )
         bfsfc(i) = sign(eins,bfsfc(i))*max(phepsi,abs(bfsfc(i)))
      end do

c-----------------------------------------------------------------------
c determine caseA and caseB
c-----------------------------------------------------------------------

      do i = 1, imt
         casea(i) = p5 +
     1        sign(p5, -zgrid(kbl(i)) - p5*hwide(kbl(i)) - hbl(i))
      end do

#endif /* ALLOW_KPP */

      return
      end

c*************************************************************************

      subroutine wscale (
     I     sigma, hbl, ustar, bfsfc,
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
#include "KPP_PARAMS.h"

c input
c------
c sigma   : normalized depth (d/hbl)
c hbl     : boundary layer depth (m)
c ustar   : surface friction velocity         (m/s)
c bfsfc   : total surface buoyancy flux       (m^2/s^3)
      _KPP_RL sigma(imt)
      _KPP_RL hbl  (imt)
      _KPP_RL ustar(imt)
      _KPP_RL bfsfc(imt)
 
c  output
c--------
c wm, ws  : turbulent velocity scales at sigma
      _KPP_RL wm(imt), ws(imt)

#ifdef ALLOW_KPP
 
c local
c------
c zehat   : = zeta *  ustar**3
      _KPP_RL zehat

      integer iz, izp1, ju, i, jup1
      _KPP_RL udiff, zdiff, zfrac, ufrac, fzfrac, wam
      _KPP_RL wbm, was, wbs, u3, tempVar

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

            u3 = ustar(i) * ustar(i) * ustar(i)
            tempVar = u3 + conc1 * zehat
            wm(i) = vonk * ustar(i) * u3 / tempVar
            ws(i) = wm(i)

         endif
 
      end do

#endif /* ALLOW_KPP */
 
      return 
      end

c*************************************************************************
 
      subroutine Ri_iwmix (
     I       kmtj, shsq, dbloc, dblocSm
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
#include "KPP_PARAMS.h"

c  input
c     kmtj   (imt)         number of vertical layers on this row
c     shsq   (imt,Nr)      (local velocity shear)^2               ((m/s)^2)
c     dbloc  (imt,Nr)      local delta buoyancy                     (m/s^2)
c     dblocSm(imt,Nr)      horizontally smoothed dbloc              (m/s^2)
      integer kmtj   (imt)
      _KPP_RL shsq   (imt,Nr)
      _KPP_RL dbloc  (imt,Nr)
      _KPP_RL dblocSm(imt,Nr)
      integer ikey
 
c output
c     diffus(imt,0:Nrp1,1)  vertical viscosivity coefficient        (m^2/s)
c     diffus(imt,0:Nrp1,2)  vertical scalar diffusivity             (m^2/s)
c     diffus(imt,0:Nrp1,3)  vertical temperature diffusivity        (m^2/s)
      _KPP_RL diffus(imt,0:Nrp1,3)

#ifdef ALLOW_KPP

c local variables
c     Rig                   local Richardson number
c     fRi, fcon             function of Rig
      _KPP_RL Rig
      _KPP_RL fRi, fcon
      _KPP_RL ratio
      integer i, ki, mr
      _KPP_RL c1, c0

#ifdef ALLOW_KPP_VERTICALLY_SMOOTH
CADJ INIT kpp_ri_tape_mr = common, 1
#endif

c constants
      c1 = 1.0
      c0 = 0.0

c-----------------------------------------------------------------------
c     compute interior gradient Ri at all interfaces ki=1,Nr, (not surface)
c     use diffus(*,*,1) as temporary storage of Ri to be smoothed
c     use diffus(*,*,2) as temporary storage for Brunt-Vaisala squared
c     set values at bottom and below to nearest value above bottom
#ifdef ALLOW_AUTODIFF_TAMC
C     break data flow dependence on diffus
      diffus(1,1,1) = 0.0
#endif

      do ki = 1, Nr
         do i = 1, imt
            if     (kmtj(i) .EQ. 0      ) then
               diffus(i,ki,1) = 0.
               diffus(i,ki,2) = 0.
            elseif (ki      .GE. kmtj(i)) then
               diffus(i,ki,1) = diffus(i,ki-1,1)
               diffus(i,ki,2) = diffus(i,ki-1,2)
            else
               diffus(i,ki,1) = dblocSm(i,ki) * (zgrid(ki)-zgrid(ki+1))
     &              / max( Shsq(i,ki), phepsi )
               diffus(i,ki,2) = dbloc(i,ki)   / (zgrid(ki)-zgrid(ki+1))
            endif
         end do
      end do

c-----------------------------------------------------------------------
c     vertically smooth Ri
#ifdef ALLOW_KPP_VERTICALLY_SMOOTH
      do mr = 1, num_v_smooth_Ri

CADJ store diffus(:,:,1) = kpp_ri_tape_mr
CADJ &  , key=mr, shape=(/ (sNx+2*OLx)*(sNy+2*OLy),Nr+2 /)

         call z121 (
     U     diffus(1,0,1))
      end do
#endif

c-----------------------------------------------------------------------
c                           after smoothing loop

      do ki = 1, Nr
         do i = 1, imt
 
c  evaluate f of Brunt-Vaisala squared for convection, store in fcon

            Rig   = max ( diffus(i,ki,2) , BVSQcon )
            ratio = min ( (BVSQcon - Rig) / BVSQcon, c1 )
            fcon  = c1 - ratio * ratio
            fcon  = fcon * fcon * fcon
 
c  evaluate f of smooth Ri for shear instability, store in fRi

            Rig  = max ( diffus(i,ki,1), c0 )
            ratio = min ( Rig / Riinfty , c1 )
            fRi   = c1 - ratio * ratio
            fRi   = fRi * fRi * fRi
 
c ----------------------------------------------------------------------
c            evaluate diffusivities and viscosity
c    mixing due to internal waves, and shear and static instability
 
            diffus(i,ki,1) = viscAr + fcon * difmcon + fRi * difm0
            diffus(i,ki,2) = diffKrS + fcon * difscon + fRi * difs0
            diffus(i,ki,3) = diffKrT + fcon * difscon + fRi * difs0

         end do
      end do
 
c ------------------------------------------------------------------------
c         set surface values to 0.0
 
      do i = 1, imt
         diffus(i,0,1) = c0
         diffus(i,0,2) = c0
         diffus(i,0,3) = c0
      end do

#endif /* ALLOW_KPP */
 
      return 
      end 

c*************************************************************************

      subroutine z121 (
     U     v )

c     Apply 121 smoothing in k to 2-d array V(i,k=1,Nr)
c     top (0) value is used as a dummy
c     bottom (Nrp1) value is set to input value from above.

c     Note that it is important to exclude from the smoothing any points
c     that are outside the range of the K(Ri) scheme, ie.  >0.8, or <0.0.
c     Otherwise, there is interference with other physics, especially
c     penetrative convection.

      IMPLICIT NONE
#include "SIZE.h"
#include "KPP_PARAMS.h"

c input/output 
c-------------
c v     : 2-D array to be smoothed in Nrp1 direction
      _KPP_RL v(imt,0:Nrp1)

#ifdef ALLOW_KPP

c local
      _KPP_RL zwork, zflag
      _KPP_RL KRi_range(1:Nrp1)
      integer i, k, km1, kp1

      _KPP_RL     p0      , p25       , p5      , p2
      parameter ( p0 = 0.0, p25 = 0.25, p5 = 0.5, p2 = 2.0 )

      KRi_range(Nrp1) = p0

#ifdef ALLOW_AUTODIFF_TAMC
C--   dummy assignment to end declaration part for TAMC
      i = 0

C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
CADJ INIT z121tape = common, Nr
#endif /* ALLOW_AUTODIFF_TAMC */

      do i = 1, imt

         k = 1
CADJ STORE v(i,k) = z121tape
         v(i,Nrp1) = v(i,Nr)

         do k = 1, Nr
            KRi_range(k) = p5 + SIGN(p5,v(i,k))
            KRi_range(k) = KRi_range(k) *
     &                     ( p5 + SIGN(p5,(Riinfty-v(i,k))) )
         end do

         zwork  = KRi_range(1) * v(i,1)
         v(i,1) = p2 * v(i,1) +
     &            KRi_range(1) * KRi_range(2) * v(i,2)
         zflag  = p2 + KRi_range(1) * KRi_range(2)
         v(i,1) = v(i,1) / zflag

         do k = 2, Nr
CADJ STORE v(i,k), zwork = z121tape
            km1 = k - 1
            kp1 = k + 1
            zflag = v(i,k)
            v(i,k) = p2 * v(i,k) +
     &           KRi_range(k) * KRi_range(kp1) * v(i,kp1) +
     &           KRi_range(k) * zwork
            zwork = KRi_range(k) * zflag
            zflag = p2 + KRi_range(k)*(KRi_range(kp1)+KRi_range(km1))
            v(i,k) = v(i,k) / zflag
         end do

      end do

#endif /* ALLOW_KPP */

      return
      end

c*************************************************************************

      subroutine kpp_smooth_horiz (
     I     k, bi, bj,
     U     fld )

c     Apply horizontal smoothing to KPP array

      IMPLICIT NONE
#include "SIZE.h"
#include "KPP_PARAMS.h"

c     input
c     bi, bj : array indices
c     k      : vertical index used for masking
      integer k, bi, bj

c     input/output
c     fld    : 2-D array to be smoothed
      _KPP_RL fld( ibot:itop, jbot:jtop )

#ifdef ALLOW_KPP

c     local
      integer i, j, im1, ip1, jm1, jp1
      _KPP_RL tempVar
      _KPP_RL fld_tmp( ibot:itop, jbot:jtop )

      integer    imin       , imax       , jmin       , jmax
      parameter( imin=ibot+1, imax=itop-1, jmin=jbot+1, jmax=jtop-1 )

      _KPP_RL    p0    , p5    , p25     , p125      , p0625
      parameter( p0=0.0, p5=0.5, p25=0.25, p125=0.125, p0625=0.0625 )

      DO j = jmin, jmax
         jm1 = j-1
         jp1 = j+1
         DO i = imin, imax
            im1 = i-1
            ip1 = i+1
            tempVar =
     &           p25   *   pMask(i  ,j  ,k,bi,bj)   +
     &           p125  * ( pMask(im1,j  ,k,bi,bj)   +
     &                     pMask(ip1,j  ,k,bi,bj)   +
     &                     pMask(i  ,jm1,k,bi,bj)   +
     &                     pMask(i  ,jp1,k,bi,bj) ) +
     &           p0625 * ( pMask(im1,jm1,k,bi,bj)   +
     &                     pMask(im1,jp1,k,bi,bj)   +
     &                     pMask(ip1,jm1,k,bi,bj)   +
     &                     pMask(ip1,jp1,k,bi,bj) )
            IF ( tempVar .GE. p25 ) THEN
               fld_tmp(i,j) = (
     &              p25  * fld(i  ,j  )*pMask(i  ,j  ,k,bi,bj) +
     &              p125 *(fld(im1,j  )*pMask(im1,j  ,k,bi,bj) +
     &                     fld(ip1,j  )*pMask(ip1,j  ,k,bi,bj) +
     &                     fld(i  ,jm1)*pMask(i  ,jm1,k,bi,bj) +
     &                     fld(i  ,jp1)*pMask(i  ,jp1,k,bi,bj))+
     &              p0625*(fld(im1,jm1)*pMask(im1,jm1,k,bi,bj) +
     &                     fld(im1,jp1)*pMask(im1,jp1,k,bi,bj) +
     &                     fld(ip1,jm1)*pMask(ip1,jm1,k,bi,bj) +
     &                     fld(ip1,jp1)*pMask(ip1,jp1,k,bi,bj)))
     &              / tempVar
            ELSE
               fld_tmp(i,j) = fld(i,j)
            ENDIF
         ENDDO
      ENDDO

c     transfer smoothed field to output array
      DO j = jmin, jmax
         DO i = imin, imax
            fld(i,j) = fld_tmp(i,j)
         ENDDO
      ENDDO

#endif /* ALLOW_KPP */

      return
      end

c*************************************************************************

      subroutine smooth_horiz (
     I     k, bi, bj,
     U     fld )

c     Apply horizontal smoothing to global _RL 2-D array

      IMPLICIT NONE
#include "SIZE.h"
#include "KPP_PARAMS.h"

c     input
c     bi, bj : array indices
c     k      : vertical index used for masking
      integer k, bi, bj

c     input/output
c     fld    : 2-D array to be smoothed
      _RL fld( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )

#ifdef ALLOW_KPP

c     local
      integer i, j, im1, ip1, jm1, jp1
      _RL tempVar
      _RL fld_tmp( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )

      integer   imin      , imax          , jmin      , jmax
      parameter(imin=2-OLx, imax=sNx+OLx-1, jmin=2-OLy, jmax=sNy+OLy-1)

      _RL        p0    , p5    , p25     , p125      , p0625
      parameter( p0=0.0, p5=0.5, p25=0.25, p125=0.125, p0625=0.0625 )

      DO j = jmin, jmax
         jm1 = j-1
         jp1 = j+1
         DO i = imin, imax
            im1 = i-1
            ip1 = i+1
            tempVar =
     &           p25   *   pMask(i  ,j  ,k,bi,bj)   +
     &           p125  * ( pMask(im1,j  ,k,bi,bj)   +
     &                     pMask(ip1,j  ,k,bi,bj)   +
     &                     pMask(i  ,jm1,k,bi,bj)   +
     &                     pMask(i  ,jp1,k,bi,bj) ) +
     &           p0625 * ( pMask(im1,jm1,k,bi,bj)   +
     &                     pMask(im1,jp1,k,bi,bj)   +
     &                     pMask(ip1,jm1,k,bi,bj)   +
     &                     pMask(ip1,jp1,k,bi,bj) )
            IF ( tempVar .GE. p25 ) THEN
               fld_tmp(i,j) = (
     &              p25  * fld(i  ,j  )*pMask(i  ,j  ,k,bi,bj) +
     &              p125 *(fld(im1,j  )*pMask(im1,j  ,k,bi,bj) +
     &                     fld(ip1,j  )*pMask(ip1,j  ,k,bi,bj) +
     &                     fld(i  ,jm1)*pMask(i  ,jm1,k,bi,bj) +
     &                     fld(i  ,jp1)*pMask(i  ,jp1,k,bi,bj))+
     &              p0625*(fld(im1,jm1)*pMask(im1,jm1,k,bi,bj) +
     &                     fld(im1,jp1)*pMask(im1,jp1,k,bi,bj) +
     &                     fld(ip1,jm1)*pMask(ip1,jm1,k,bi,bj) +
     &                     fld(ip1,jp1)*pMask(ip1,jp1,k,bi,bj)))
     &              / tempVar
            ELSE
               fld_tmp(i,j) = fld(i,j)
            ENDIF
         ENDDO
      ENDDO

c     transfer smoothed field to output array
      DO j = jmin, jmax
         DO i = imin, imax
            fld(i,j) = fld_tmp(i,j)
         ENDDO
      ENDDO

#endif /* ALLOW_KPP */

      return
      end

c*************************************************************************

      subroutine blmix (
     I       ustar, bfsfc, hbl, stable, casea, diffus, kbl
     O     , dkm1, blmc, ghat, sigma, ikey
     &     )

c     mixing coefficients within boundary layer depend on surface
c     forcing and the magnitude and gradient of interior mixing below
c     the boundary layer ("matching").
c
c     caution: if mixing bottoms out at hbl = -zgrid(Nr) then
c     fictitious layer at Nrp1 is needed with small but finite width
c     hwide(Nrp1) (eg. epsln = 1.e-20).
c
      IMPLICIT NONE

#include "SIZE.h"
#include "KPP_PARAMS.h"

c input
c     ustar (imt)                 surface friction velocity             (m/s)
c     bfsfc (imt)                 surface buoyancy forcing          (m^2/s^3)
c     hbl   (imt)                 boundary layer depth                    (m)
c     stable(imt)                 = 1 in stable forcing
c     casea (imt)                 = 1 in case A
c     diffus(imt,0:Nrp1,mdiff)    vertical diffusivities              (m^2/s)
c     kbl(imt)                    -1 of first grid level below hbl
      _KPP_RL ustar (imt)
      _KPP_RL bfsfc (imt)
      _KPP_RL hbl   (imt)
      _KPP_RL stable(imt)
      _KPP_RL casea (imt)
      _KPP_RL diffus(imt,0:Nrp1,mdiff)
      integer kbl(imt)

c output
c     dkm1 (imt,mdiff)            boundary layer difs at kbl-1 level
c     blmc (imt,Nr,mdiff)         boundary layer mixing coefficients  (m^2/s)
c     ghat (imt,Nr)               nonlocal scalar transport
c     sigma(imt)                  normalized depth (d / hbl)
      _KPP_RL dkm1 (imt,mdiff)
      _KPP_RL blmc (imt,Nr,mdiff)
      _KPP_RL ghat (imt,Nr)
      _KPP_RL sigma(imt)
      integer ikey

#ifdef ALLOW_KPP

c  local
c     gat1*(imt)                 shape function at sigma = 1
c     dat1*(imt)                 derivative of shape function at sigma = 1
c     ws(imt), wm(imt)           turbulent velocity scales             (m/s)
      _KPP_RL gat1m(imt), gat1s(imt), gat1t(imt) 
      _KPP_RL dat1m(imt), dat1s(imt), dat1t(imt)
      _KPP_RL ws(imt), wm(imt)
      integer i, kn, ki
      _KPP_RL R, dvdzup, dvdzdn, viscp
      _KPP_RL difsp, diftp, visch, difsh, difth
      _KPP_RL f1, sig, a1, a2, a3, delhat
      _KPP_RL Gm, Gs, Gt
      _KPP_RL tempVar

      _KPP_RL    p0    , eins
      parameter (p0=0.0, eins=1.0)

c-----------------------------------------------------------------------
c compute velocity scales at hbl
c-----------------------------------------------------------------------

      do i = 1, imt
         sigma(i) = stable(i) * 1.0 + (1. - stable(i)) * epsilon
      end do

      call wscale (
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws )

      do i = 1, imt
         wm(i) = sign(eins,wm(i))*max(phepsi,abs(wm(i)))
         ws(i) = sign(eins,ws(i))*max(phepsi,abs(ws(i)))
      end do
CADJ STORE wm = comlev1_kpp, key = ikey
CADJ STORE ws = comlev1_kpp, key = ikey

      do i = 1, imt

         kn = int(caseA(i)+phepsi) *(kbl(i) -1) +
     $        (1 - int(caseA(i)+phepsi)) * kbl(i)

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

         f1 = stable(i) * conc1 * bfsfc(i) / 
     &        max(ustar(i)**4,phepsi)
         gat1m(i) = visch / hbl(i) / wm(i)
         dat1m(i) = -viscp / wm(i) + f1 * visch
         dat1m(i) = min(dat1m(i),p0)
 
         gat1s(i) = difsh  / hbl(i) / ws(i)
         dat1s(i) = -difsp / ws(i) + f1 * difsh 
         dat1s(i) = min(dat1s(i),p0)
 
         gat1t(i) = difth /  hbl(i) / ws(i)
         dat1t(i) = -diftp / ws(i) + f1 * difth 
         dat1t(i) = min(dat1t(i),p0)
 
      end do

      do ki = 1, Nr

c-----------------------------------------------------------------------
c     compute turbulent velocity scales on the interfaces
c-----------------------------------------------------------------------

         do i = 1, imt
            sig      = (-zgrid(ki) + 0.5 * hwide(ki)) / hbl(i)
            sigma(i) = stable(i)*sig + (1.-stable(i))*min(sig,epsilon)
         end do
         call wscale (
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws )

c-----------------------------------------------------------------------
c     compute the dimensionless shape functions at the interfaces
c-----------------------------------------------------------------------

         do i = 1, imt
            sig = (-zgrid(ki) + 0.5 * hwide(ki)) / hbl(i)
            a1 = sig - 2.
            a2 = 3. - 2. * sig
            a3 = sig - 1.

            Gm = a1 + a2 * gat1m(i) + a3 * dat1m(i)
            Gs = a1 + a2 * gat1s(i) + a3 * dat1s(i)
            Gt = a1 + a2 * gat1t(i) + a3 * dat1t(i)

c-----------------------------------------------------------------------
c     compute boundary layer diffusivities at the interfaces
c-----------------------------------------------------------------------

            blmc(i,ki,1) = hbl(i) * wm(i) * sig * (1. + sig * Gm)
            blmc(i,ki,2) = hbl(i) * ws(i) * sig * (1. + sig * Gs)
            blmc(i,ki,3) = hbl(i) * ws(i) * sig * (1. + sig * Gt)

c-----------------------------------------------------------------------
c     nonlocal transport term = ghat * <ws>o
c-----------------------------------------------------------------------

            tempVar = ws(i) * hbl(i)
            ghat(i,ki) = (1.-stable(i)) * cg / max(phepsi,tempVar)
 
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
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws )

      do i = 1, imt
         sig = -zgrid(kbl(i)-1) / hbl(i)
         a1 = sig - 2.
         a2 = 3. - 2. * sig
         a3 = sig - 1.
         Gm = a1 + a2 * gat1m(i) + a3 * dat1m(i)
         Gs = a1 + a2 * gat1s(i) + a3 * dat1s(i)
         Gt = a1 + a2 * gat1t(i) + a3 * dat1t(i)
         dkm1(i,1) = hbl(i) * wm(i) * sig * (1. + sig * Gm)
         dkm1(i,2) = hbl(i) * ws(i) * sig * (1. + sig * Gs)
         dkm1(i,3) = hbl(i) * ws(i) * sig * (1. + sig * Gt)
      end do

#endif /* ALLOW_KPP */

      return 
      end 

c*************************************************************************

      subroutine enhance ( 
     I       dkm1, hbl, kbl, diffus, casea
     U     , ghat
     O     , blmc
     &     )

c enhance the diffusivity at the kbl-.5 interface

      IMPLICIT NONE

#include "SIZE.h"
#include "KPP_PARAMS.h"

c input
c     dkm1(imt,mdiff)          bl diffusivity at kbl-1 grid level
c     hbl(imt)                  boundary layer depth                 (m)
c     kbl(imt)                  grid above hbl
c     diffus(imt,0:Nrp1,mdiff) vertical diffusivities           (m^2/s)
c     casea(imt)                = 1 in caseA, = 0 in case B
      _KPP_RL dkm1  (imt,mdiff)
      _KPP_RL hbl   (imt)
      integer kbl   (imt)
      _KPP_RL diffus(imt,0:Nrp1,mdiff)
      _KPP_RL casea (imt)

c input/output
c     nonlocal transport, modified ghat at kbl(i)-1 interface    (s/m**2)
      _KPP_RL ghat (imt,Nr)

c output
c     enhanced bound. layer mixing coeff.
      _KPP_RL blmc  (imt,Nr,mdiff)

#ifdef ALLOW_KPP

c local
c     fraction hbl lies beteen zgrid neighbors
      _KPP_RL delta
      integer ki, i, md
      _KPP_RL dkmp5, dstar

      do i = 1, imt
         ki = kbl(i)-1
         if ((ki .ge. 1) .and. (ki .lt. Nr)) then
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

#endif /* ALLOW_KPP */

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
c      rho1   = potential density of surface layer                     (kg/m^3)
c      dbloc  = local buoyancy gradient at Nr interfaces
c               g/rho{k+1,k+1} * [ drho{k,k+1}-drho{k+1,k+1} ]          (m/s^2)
c      dbsfc  = buoyancy difference with respect to the surface
c               g * [ drho{1,k}/rho{1,k} - drho{k,k}/rho{k,k} ]         (m/s^2)
c      ttalpha= thermal expansion coefficient without 1/rho factor
c               d(rho) / d(potential temperature)                    (kg/m^3/C)
c      ssbeta = salt expansion coefficient without 1/rho factor
c               d(rho) / d(salinity)                               (kg/m^3/PSU)
c
c     see also subroutines find_rho.F find_alpha.F find_beta.F
c
c     written  by: jan morzel,   feb. 10, 1995 (converted from "sigma" version)
c     modified by: d. menemenlis,    june 1998 : for use with MIT GCM UV
c
c-----------------------------------------------------------------------

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP_PARAMS.h"
#include "DYNVARS.h"

c-------------- Routine arguments -----------------------------------------
      INTEGER bi, bj, myThid
      _KPP_RL RHO1   ( ibot:itop, jbot:jtop       )
      _KPP_RL DBLOC  ( ibot:itop, jbot:jtop, Nr   )
      _KPP_RL DBSFC  ( ibot:itop, jbot:jtop, Nr   )
      _KPP_RL TTALPHA( ibot:itop, jbot:jtop, Nrp1 )
      _KPP_RL SSBETA ( ibot:itop, jbot:jtop, Nrp1 )

#ifdef ALLOW_KPP

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
     I     bi, bj, ibot, itop, jbot, jtop, 1, 1, eosType,
     I     theta, salt,
     O     WORK1,
     I     myThid )

      call FIND_ALPHA(
     I     bi, bj, ibot, itop, jbot, jtop, 1, 1, eosType,
     O     WORK2 )

      call FIND_BETA(
     I     bi, bj, ibot, itop, jbot, jtop, 1, 1, eosType,
     O     WORK3 )

      DO J = jbot, jtop
         DO I = ibot, itop
            RHO1(I,J)      = WORK1(I,J) + rhonil
            TTALPHA(I,J,1) = WORK2(I,J)
            SSBETA(I,J,1)  = WORK3(I,J)
            DBSFC(I,J,1)   = 0.
         END DO
      END DO

c calculate alpha, beta, and gradients in interior layers

CHPF$  INDEPENDENT, NEW (RHOK,RHOKM1,RHO1K,WORK1,WORK2)
      DO K = 2, Nr

         call FIND_RHO(
     I        bi, bj, ibot, itop, jbot, jtop, K, K, eosType,
     I        theta, salt,
     O        RHOK,
     I        myThid )

         call FIND_RHO(
     I        bi, bj, ibot, itop, jbot, jtop, K-1, K, eosType,
     I        theta, salt,
     O        RHOKM1,
     I        myThid )

         call FIND_RHO(
     I        bi, bj, ibot, itop, jbot, jtop, 1, K, eosType,
     I        theta, salt,
     O        RHO1K,
     I        myThid )

         call FIND_ALPHA(
     I        bi, bj, ibot, itop, jbot, jtop, K, K, eosType,
     O        WORK1 )

         call FIND_BETA(
     I        bi, bj, ibot, itop, jbot, jtop, K, K, eosType,
     O        WORK2 )

         DO J = jbot, jtop
            DO I = ibot, itop
               TTALPHA(I,J,K) = WORK1 (I,J)
               SSBETA(I,J,K)  = WORK2 (I,J)
               DBLOC(I,J,K-1) = gravity * (RHOK(I,J) - RHOKM1(I,J)) /
     &                                    (RHOK(I,J) + rhonil)
               DBSFC(I,J,K)   = gravity * (RHOK(I,J) - RHO1K (I,J)) /
     &                                    (RHOK(I,J) + rhonil)
            END DO
         END DO

      END DO

c     compute arrays for K = Nrp1
      DO J = jbot, jtop
         DO I = ibot, itop
            TTALPHA(I,J,Nrp1) = TTALPHA(I,J,Nr)
            SSBETA(I,J,Nrp1)  = SSBETA(I,J,Nr)
            DBLOC(I,J,Nr)     = 0.
         END DO
      END DO

#endif /* ALLOW_KPP */

      RETURN
      END
