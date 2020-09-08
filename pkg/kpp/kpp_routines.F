#include "KPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_SALT_PLUME
#include "SALT_PLUME_OPTIONS.h"
#endif
#if (defined ALLOW_AUTODIFF_TAMC) && (defined KPP_AUTODIFF_EXCESSIVE_STORE)
# define KPP_AUTODIFF_MORE_STORE
#endif

C-- File kpp_routines.F: subroutines needed to implement
C--                      KPP vertical mixing scheme
C--  Contents
C--  o KPPMIX      - Main driver and interface routine.
C--  o BLDEPTH     - Determine oceanic planetary boundary layer depth.
C--  o WSCALE      - Compute turbulent velocity scales.
C--  o RI_IWMIX    - Compute interior viscosity diffusivity coefficients.
C--  o Z121        - Apply 121 vertical smoothing.
C--  o SMOOTH_HORIZ- Apply horizontal smoothing to global array.
C--  o BLMIX       - Boundary layer mixing coefficients.
C--  o ENHANCE     - Enhance diffusivity at boundary layer interface.
C--  o STATEKPP    - Compute buoyancy-related input arrays.
C--  o KPP_DOUBLEDIFF - Compute double diffusive contribution to diffusivities

c***********************************************************************

      SUBROUTINE KPPMIX (
     I       kmtj, shsq, dvsq, ustar, msk
     I     , bo, bosol
#ifdef ALLOW_SALT_PLUME
     I     , boplume,SPDepth
#ifdef SALT_PLUME_SPLIT_BASIN
     I     , lon,lat
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
     I     , dbloc, Ritop, coriol
     I     , diffusKzS, diffusKzT
     I     , ikppkey
     O     , diffus
     U     , ghat
     O     , hbl
     I     , bi, bj, myTime, myIter, myThid )

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
#include "KPP_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

c input
c   bi, bj :: Array indices on which to apply calculations
c   myTime :: Current time in simulation
c   myIter :: Current iteration number in simulation
c   myThid :: My Thread Id. number
c     kmtj   (imt)     - number of vertical layers on this row
c     msk    (imt)     - surface mask (=1 if water, =0 otherwise)
c     shsq   (imt,Nr)  - (local velocity shear)^2                     ((m/s)^2)
c     dvsq   (imt,Nr)  - (velocity shear re sfc)^2                    ((m/s)^2)
c     ustar  (imt)     - surface friction velocity                        (m/s)
c     bo     (imt)     - surface turbulent buoy. forcing              (m^2/s^3)
c     bosol  (imt)     - radiative buoyancy forcing                   (m^2/s^3)
c     boplume(imt,Nrp1)- haline buoyancy forcing                      (m^2/s^3)
c     dbloc  (imt,Nr)  - local delta buoyancy across interfaces         (m/s^2)
c     dblocSm(imt,Nr)  - horizontally smoothed dbloc                    (m/s^2)
c                          stored in ghat to save space
c     Ritop  (imt,Nr)  - numerator of bulk Richardson Number
c                          (zref-z) * delta buoyancy w.r.t. surface   ((m/s)^2)
c     coriol (imt)     - Coriolis parameter                               (1/s)
c     diffusKzS(imt,Nr)- background vertical diffusivity for scalars    (m^2/s)
c     diffusKzT(imt,Nr)- background vertical diffusivity for theta      (m^2/s)
c     note: there is a conversion from 2-D to 1-D for input output variables,
c           e.g., hbl(sNx,sNy) -> hbl(imt),
c           where hbl(i,j) -> hbl((j-1)*sNx+i)
      INTEGER bi, bj
      _RL     myTime
      integer myIter
      integer myThid
      integer kmtj (imt   )
      _RL shsq     (imt,Nr)
      _RL dvsq     (imt,Nr)
      _RL ustar    (imt   )
      _RL bo       (imt   )
      _RL bosol    (imt   )
#ifdef ALLOW_SALT_PLUME
      _RL boplume  (imt,Nrp1)
      _RL SPDepth  (imt   )
#ifdef SALT_PLUME_SPLIT_BASIN
      _RL lon  (imt   )
      _RL lat  (imt   )
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
      _RL dbloc    (imt,Nr)
      _RL Ritop    (imt,Nr)
      _RL coriol   (imt   )
      _RS msk      (imt   )
      _RL diffusKzS(imt,Nr)
      _RL diffusKzT(imt,Nr)

      integer ikppkey

c output
c     diffus (imt,1)  - vertical viscosity coefficient                  (m^2/s)
c     diffus (imt,2)  - vertical scalar diffusivity                     (m^2/s)
c     diffus (imt,3)  - vertical temperature diffusivity                (m^2/s)
c     ghat   (imt)    - nonlocal transport coefficient                  (s/m^2)
c     hbl    (imt)    - mixing layer depth                                  (m)

      _RL diffus(imt,0:Nrp1,mdiff)
      _RL ghat  (imt,Nr)
      _RL hbl   (imt)

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

      integer kbl(imt         )
      _RL bfsfc  (imt         )
      _RL casea  (imt         )
      _RL stable (imt         )
      _RL dkm1   (imt,   mdiff)
      _RL blmc   (imt,Nr,mdiff)
      _RL sigma  (imt         )
      _RL Rib    (imt,Nr      )

      integer i, k, md

c-----------------------------------------------------------------------
c compute interior mixing coefficients everywhere, due to constant
c internal wave activity, static instability, and local shear
c instability.
c (ghat is temporary storage for horizontally smoothed dbloc)
c-----------------------------------------------------------------------

cph(
cph these storings avoid recomp. of Ri_iwmix
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ghat  = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE dbloc = comlev1_kpp, key=ikppkey, kind=isbyte
#endif
cph)
      call Ri_iwmix (
     I       kmtj, shsq, dbloc, ghat
     I     , diffusKzS, diffusKzT
     I     , ikppkey
     O     , diffus, myThid )

cph(
cph these storings avoid recomp. of Ri_iwmix
cph DESPITE TAFs 'not necessary' warning!
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dbloc  = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE shsq   = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE ghat   = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE diffus = comlev1_kpp, key=ikppkey, kind=isbyte
#endif
cph)

c-----------------------------------------------------------------------
c set seafloor values to zero and fill extra "Nrp1" coefficients
c for blmix
c-----------------------------------------------------------------------

      do md = 1, mdiff
       do k=1,Nrp1
         do i = 1,imt
             if(k.ge.kmtj(i))  diffus(i,k,md) = 0.0
            end do
         end do
      end do

c-----------------------------------------------------------------------
c compute boundary layer mixing coefficients:
c
c diagnose the new boundary layer depth
c-----------------------------------------------------------------------

      call bldepth (
     I       kmtj
     I     , dvsq, dbloc, Ritop, ustar, bo, bosol
#ifdef ALLOW_SALT_PLUME
     I     , boplume,SPDepth
#ifdef SALT_PLUME_SPLIT_BASIN
     I     , lon,lat
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
     I     , coriol
     I     , ikppkey
     O     , hbl, bfsfc, stable, casea, kbl, Rib, sigma
     I     , bi, bj, myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hbl,bfsfc,stable,casea,kbl = comlev1_kpp,
CADJ &     key=ikppkey, kind=isbyte
#endif

c-----------------------------------------------------------------------
c compute boundary layer diffusivities
c-----------------------------------------------------------------------

      call blmix (
     I       ustar, bfsfc, hbl, stable, casea, diffus, kbl
     O     , dkm1, blmc, ghat, sigma, ikppkey
     I     , myThid )
cph(
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dkm1,blmc,ghat = comlev1_kpp,
CADJ &     key=ikppkey, kind=isbyte
CADJ STORE hbl, kbl, diffus, casea = comlev1_kpp,
CADJ &     key=ikppkey, kind=isbyte
#endif
cph)

c-----------------------------------------------------------------------
c enhance diffusivity at interface kbl - 1
c-----------------------------------------------------------------------

      call enhance (
     I       dkm1, hbl, kbl, diffus, casea
     U     , ghat
     O     , blmc
     I     , myThid )

cph(
cph avoids recomp. of enhance
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE blmc = comlev1_kpp, key=ikppkey, kind=isbyte
#endif
cph)

c-----------------------------------------------------------------------
c combine interior and boundary layer coefficients and nonlocal term
c !!!NOTE!!! In shallow (2-level) regions and for shallow mixed layers
c (< 1 level), diffusivity blmc can become negative.  The max-s below
c are a hack until this problem is properly diagnosed and fixed.
c-----------------------------------------------------------------------
      do k = 1, Nr
         do i = 1, imt
            if (k .lt. kbl(i)) then
#ifdef ALLOW_SHELFICE
C     when there is shelfice on top (msk(i)=0), reset the boundary layer
C     mixing coefficients blmc to pure Ri-number based mixing
               blmc(i,k,1) = max ( blmc(i,k,1)*msk(i),
     &              diffus(i,k,1) )
               blmc(i,k,2) = max ( blmc(i,k,2)*msk(i),
     &              diffus(i,k,2) )
               blmc(i,k,3) = max ( blmc(i,k,3)*msk(i),
     &              diffus(i,k,3) )
#endif /* not ALLOW_SHELFICE */
               diffus(i,k,1) = max ( blmc(i,k,1), viscArNr(1) )
               diffus(i,k,2) = max ( blmc(i,k,2), diffusKzS(i,Nr) )
               diffus(i,k,3) = max ( blmc(i,k,3), diffusKzT(i,Nr) )
            else
               ghat(i,k) = 0. _d 0
            endif
         end do
      end do

#endif /* ALLOW_KPP */

      return
      end

c*************************************************************************

      subroutine bldepth (
     I       kmtj
     I     , dvsq, dbloc, Ritop, ustar, bo, bosol
#ifdef ALLOW_SALT_PLUME
     I     , boplume,SPDepth
#ifdef SALT_PLUME_SPLIT_BASIN
     I     , lon,lat
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
     I     , coriol
     I     , ikppkey
     O     , hbl, bfsfc, stable, casea, kbl, Rib, sigma
     I     , bi, bj, myTime, myIter, myThid )

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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

c input
c------
c   bi, bj :: Array indices on which to apply calculations
c   myTime :: Current time in simulation
c   myIter :: Current iteration number in simulation
c   myThid :: My Thread Id. number
c kmtj      : number of vertical layers
c dvsq      : (velocity shear re sfc)^2             ((m/s)^2)
c dbloc     : local delta buoyancy across interfaces  (m/s^2)
c Ritop     : numerator of bulk Richardson Number
c             =(z-zref)*dbsfc, where dbsfc=delta
c             buoyancy with respect to surface      ((m/s)^2)
c ustar     : surface friction velocity                 (m/s)
c bo        : surface turbulent buoyancy forcing    (m^2/s^3)
c bosol     : radiative buoyancy forcing            (m^2/s^3)
c boplume   : haline buoyancy forcing               (m^2/s^3)
c coriol    : Coriolis parameter                        (1/s)
      INTEGER bi, bj
      _RL     myTime
      integer myIter
      integer myThid
      integer kmtj(imt)
      _RL dvsq    (imt,Nr)
      _RL dbloc   (imt,Nr)
      _RL Ritop   (imt,Nr)
      _RL ustar   (imt)
      _RL bo      (imt)
      _RL bosol   (imt)
      _RL coriol  (imt)
      integer ikppkey
#ifdef ALLOW_SALT_PLUME
      _RL boplume (imt,Nrp1)
      _RL SPDepth (imt)
#ifdef SALT_PLUME_SPLIT_BASIN
      _RL lon (imt)
      _RL lat (imt)
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */

c  output
c--------
c hbl       : boundary layer depth                        (m)
c bfsfc     : Bo+radiation absorbed to d=hbf*hbl    (m^2/s^3)
c stable    : =1 in stable forcing; =0 unstable
c casea     : =1 in case A, =0 in case B
c kbl       : -1 of first grid level below hbl
c Rib       : Bulk Richardson number
c sigma     : normalized depth (d/hbl)
      _RL hbl    (imt)
      _RL bfsfc  (imt)
      _RL stable (imt)
      _RL casea  (imt)
      integer kbl(imt)
      _RL Rib    (imt,Nr)
      _RL sigma  (imt)

#ifdef ALLOW_KPP

c  local
c-------
c wm, ws    : turbulent velocity scales         (m/s)
      _RL wm(imt), ws(imt)
      _RL worka(imt)
      _RL bvsq, vtsq, hekman, hmonob, hlimit, tempVar1, tempVar2
      integer i, kl

      _RL         p5    , eins
      parameter ( p5=0.5, eins=1.0 )
      _RL         minusone
      parameter ( minusone=-1.0 )
#ifdef SALT_PLUME_VOLUME
      integer km, km1
      _RL temp
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      integer kkppkey
#endif

#ifdef ALLOW_DIAGNOSTICS
c     KPPBFSFC - Bo+radiation absorbed to d=hbf*hbl + plume (m^2/s^3)
      _RL KPPBFSFC(imt,Nr)
      _RL KPPRi(imt,Nr)
#endif /* ALLOW_DIAGNOSTICS */

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
         Rib(i,1) = 0. _d 0
         if (kmtj(i).LT.1) kmtj(i) = 1
         kbl(i) = kmtj(i)
         kl     = kbl(i)
         hbl(i) = -zgrid(kl)
      end do

#ifdef ALLOW_DIAGNOSTICS
      do kl = 1, Nr
         do i = 1, imt
            KPPBFSFC(i,kl) = 0. _d 0
            KPPRi(i,kl) = 0. _d 0
         enddo
      enddo
#endif /* ALLOW_DIAGNOSTICS */

      do kl = 2, Nr

#ifdef ALLOW_AUTODIFF_TAMC
         kkppkey = (ikppkey-1)*Nr + kl
#endif

c     compute bfsfc = sw fraction at hbf * zgrid

         do i = 1, imt
            worka(i) = zgrid(kl)
         end do
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp_k, key = kkppkey, kind=isbyte
#endif
         call SWFRAC(
     I       imt, hbf,
     U       worka,
     I       myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp_k, key = kkppkey, kind=isbyte
#endif

         do i = 1, imt

c     use caseA as temporary array

            casea(i) = -zgrid(kl)

c     compute bfsfc= Bo + radiative contribution down to hbf * hbl

            bfsfc(i) = bo(i) + bosol(i)*(1. - worka(i))

         end do
#ifdef ALLOW_SALT_PLUME
c     compute bfsfc = plume fraction at hbf * zgrid
         IF ( useSALT_PLUME ) THEN
#ifndef SALT_PLUME_VOLUME
           do i = 1, imt
              worka(i) = zgrid(kl)
           enddo
Ccatn: in original way: accumulate all fractions of boplume above zgrid(kl)
           call SALT_PLUME_FRAC(
     I         imt, hbf,SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I         lon,lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
     U         worka,
     I         myTime, myIter, myThid)
           do i = 1, imt
              bfsfc(i) = bfsfc(i) + boplume(i,1)*(worka(i))
C            km=max(1,kbl(i)-1)
C            temp = (plumefrac(i,km)+plumefrac(i,kbl(i)))/2.0
C            bfsfc(i) = bfsfc(i) + boplume(i,1)*temp
           enddo
#else /* def SALT_PLUME_VOLUME */
catn: in vol way: need to integrate down to hbl, so first locate
c     k level associated with this hbl, then sum up all SPforc[T,S]
           DO i = 1, imt
            km =max(1,kbl(i)-1)
            km1=max(1,kbl(i))
            temp = (boplume(i,km)+boplume(i,km1))*p5
            bfsfc(i) = bfsfc(i) + temp
           ENDDO
#endif /* ndef SALT_PLUME_VOLUME */
         ENDIF
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_DIAGNOSTICS
         do i = 1, imt
            KPPBFSFC(i,kl) = bfsfc(i)
         enddo
#endif /* ALLOW_DIAGNOSTICS */

         do i = 1, imt
            stable(i) = p5 + sign(p5,bfsfc(i))
            sigma(i) = stable(i) + (1. - stable(i)) * epsilon
         enddo

c-----------------------------------------------------------------------
c     compute velocity scales at sigma, for hbl= caseA = -zgrid(kl)
c-----------------------------------------------------------------------

         call wscale (
     I        sigma, casea, ustar, bfsfc,
     O        wm, ws, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store ws = comlev1_kpp_k, key = kkppkey, kind=isbyte
#endif

         do i = 1, imt

c-----------------------------------------------------------------------
c     compute the turbulent shear contribution to Rib
c-----------------------------------------------------------------------

            bvsq = p5 *
     1           ( dbloc(i,kl-1) / (zgrid(kl-1)-zgrid(kl  ))+
     2             dbloc(i,kl  ) / (zgrid(kl  )-zgrid(kl+1)))

            if (bvsq .eq. 0. _d 0) then
              vtsq = 0. _d 0
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
#ifdef ALLOW_DIAGNOSTICS
            KPPRi(i,kl) = Rib(i,kl)
#endif

         end do
      end do

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(KPPBFSFC,'KPPbfsfc',0,Nr,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(KPPRi   ,'KPPRi   ',0,Nr,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

cph(
cph  without this store, there is a recomputation error for
cph  rib in adbldepth (probably partial recomputation problem)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store Rib = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy),Nr /)
#endif
cph)

      do kl = 2, Nr
         do i = 1, imt
            if (kbl(i).eq.kmtj(i) .and. Rib(i,kl).gt.Ricr) kbl(i) = kl
         end do
      end do

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store kbl = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

      do i = 1, imt
         kl = kbl(i)
c     linearly interpolate to find hbl where Rib = Ricr
         if (kl.gt.1 .and. kl.lt.kmtj(i)) then
            tempVar1 = (Rib(i,kl)-Rib(i,kl-1))
            hbl(i) = -zgrid(kl-1) + (zgrid(kl-1)-zgrid(kl)) *
     1           (Ricr - Rib(i,kl-1)) / tempVar1
         endif
      end do

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store hbl = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

c-----------------------------------------------------------------------
c     find stability and buoyancy forcing for boundary layer
c-----------------------------------------------------------------------

      do i = 1, imt
         worka(i) = hbl(i)
      end do
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif
      call SWFRAC(
     I       imt, minusone,
     U       worka,
     I       myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

      do i = 1, imt
         bfsfc(i)  = bo(i) + bosol(i) * (1. - worka(i))
      end do

#ifdef ALLOW_SALT_PLUME
      IF ( useSALT_PLUME ) THEN
#ifndef SALT_PLUME_VOLUME
        do i = 1, imt
           worka(i) = hbl(i)
        enddo
        call SALT_PLUME_FRAC(
     I         imt,minusone,SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I         lon,lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
     U         worka,
     I         myTime, myIter, myThid )
        do i = 1, imt
           bfsfc(i) = bfsfc(i) + boplume(i,1) * (worka(i))
C            km=max(1,kbl(i)-1)
C            temp = (plumefrac(i,km)+plumefrac(i,kbl(i)))/2.0
C            bfsfc(i) = bfsfc(i) + boplume(i,1)*temp
        enddo
#else /* def SALT_PLUME_VOLUME */
        DO i = 1, imt
            km =max(1,kbl(i)-1)
            km1=max(1,kbl(i))
            temp = (boplume(i,km)+boplume(i,km1))/2.0
            bfsfc(i) = bfsfc(i) + temp
        ENDDO
#endif /* ndef SALT_PLUME_VOLUME */
      ENDIF
#endif /* ALLOW_SALT_PLUME */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store bfsfc = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

c--   ensure bfsfc is never 0
      do i = 1, imt
         stable(i) = p5 + sign( p5, bfsfc(i) )
         bfsfc(i) = sign(eins,bfsfc(i))*max(phepsi,abs(bfsfc(i)))
      end do

cph(
cph  added stable to store list to avoid extensive recomp.
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store bfsfc, stable = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif
cph)

c-----------------------------------------------------------------------
c check hbl limits for hekman or hmonob
c     ph: test for zero nominator
c-----------------------------------------------------------------------

      IF ( LimitHblStable ) THEN
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
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store hbl = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

      do i = 1, imt
         hbl(i) = max(hbl(i),minKPPhbl)
         kbl(i) = kmtj(i)
      end do

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store hbl = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif
      call SWFRAC(
     I     imt, minusone,
     U     worka,
     I     myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

      do i = 1, imt
         bfsfc(i) = bo(i) + bosol(i) * (1. - worka(i))
      end do

#ifdef ALLOW_SALT_PLUME
      IF ( useSALT_PLUME ) THEN
#ifndef SALT_PLUME_VOLUME
        do i = 1, imt
           worka(i) = hbl(i)
        enddo
        call SALT_PLUME_FRAC(
     I         imt,minusone,SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I         lon,lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
     U         worka,
     I         myTime, myIter, myThid )
        do i = 1, imt
           bfsfc(i) = bfsfc(i) + boplume(i,1) * (worka(i))
C            km=max(1,kbl(i)-1)
C            temp = (plumefrac(i,km)+plumefrac(i,kbl(i)))/2.0
C            bfsfc(i) = bfsfc(i) + boplume(i,1)*temp
        enddo
#else /* def SALT_PLUME_VOLUME */
        DO i = 1, imt
            km =max(1,kbl(i)-1)
            km1=max(1,kbl(i)-0)
            temp = (boplume(i,km)+boplume(i,km1))/2.0
            bfsfc(i) = bfsfc(i) + temp
        ENDDO
#endif /* ndef SALT_PLUME_VOLUME */
      ENDIF
#endif /* ALLOW_SALT_PLUME */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store bfsfc = comlev1_kpp
CADJ &   , key=ikppkey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

c--   ensures bfsfc is never 0
      do i = 1, imt
         stable(i) = p5 + sign( p5, bfsfc(i) )
         bfsfc(i) = sign(eins,bfsfc(i))*max(phepsi,abs(bfsfc(i)))
      end do

c-----------------------------------------------------------------------
c determine caseA and caseB
c-----------------------------------------------------------------------

      do i = 1, imt
         kl = kbl(i)
         casea(i) = p5 +
     1        sign(p5, -zgrid(kl) - p5*hwide(kl) - hbl(i))
      end do

#endif /* ALLOW_KPP */

      return
      end

c*************************************************************************

      subroutine wscale (
     I     sigma, hbl, ustar, bfsfc,
     O     wm, ws,
     I     myThid )

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
c myThid  : thread number for this instance of the routine
      integer myThid
      _RL sigma(imt)
      _RL hbl  (imt)
      _RL ustar(imt)
      _RL bfsfc(imt)

c  output
c--------
c wm, ws  : turbulent velocity scales at sigma
      _RL wm(imt), ws(imt)

#ifdef ALLOW_KPP

c local
c------
c zehat   : = zeta *  ustar**3
      _RL zehat

      integer iz, izp1, ju, i, jup1
      _RL udiff, zdiff, zfrac, ufrac, fzfrac, wam
      _RL wbm, was, wbs, u3, tempVar

c-----------------------------------------------------------------------
c use lookup table for zehat < zmax only; otherwise use
c stable formulae
c-----------------------------------------------------------------------

      do i = 1, imt
         zehat = vonk*sigma(i)*hbl(i)*bfsfc(i)

         if (zehat .le. zmax) then

            zdiff = zehat - zmin
C     For extremely negative buoyancy forcing bfsfc, zehat and hence
C     zdiff can become very negative (default value of zmin = 4.e-7) and
C     the extrapolation beyond the limit zmin of the lookup table can
C     give very bad values and may make the model crash. Here is a
C     simple fix (thanks to Dimitry Sidorenko) that effectively replaces
C     linear extrapolation with nearest neighbor extrapolation so that
C     only the lower limit values of the lookup tables wmt/wst are used.
C     Alternatively, one can get rid of the lookup table altogether
C     and compute the coefficients online (done in NEMO, for example).
C           zdiff = max( 0. _d 0, zehat - zmin )
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
     I       kmtj, shsq, dbloc, dblocSm,
     I       diffusKzS, diffusKzT,
     I       ikppkey,
     O       diffus,
     I       myThid )

c     compute interior viscosity diffusivity coefficients due
c     to shear instability (dependent on a local Richardson number),
c     to background internal wave activity, and
c     to static instability (local Richardson number < 0).

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP_PARAMS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_PARAMS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

c  input
c     kmtj   (imt)         number of vertical layers on this row
c     shsq   (imt,Nr)      (local velocity shear)^2               ((m/s)^2)
c     dbloc  (imt,Nr)      local delta buoyancy                     (m/s^2)
c     dblocSm(imt,Nr)      horizontally smoothed dbloc              (m/s^2)
c     diffusKzS(imt,Nr)- background vertical diffusivity for scalars    (m^2/s)
c     diffusKzT(imt,Nr)- background vertical diffusivity for theta      (m^2/s)
c     myThid :: My Thread Id. number
      integer kmtj (imt)
      _RL shsq     (imt,Nr)
      _RL dbloc    (imt,Nr)
      _RL dblocSm  (imt,Nr)
      _RL diffusKzS(imt,Nr)
      _RL diffusKzT(imt,Nr)
      integer ikppkey
      integer myThid

c output
c     diffus(imt,0:Nrp1,1)  vertical viscosivity coefficient        (m^2/s)
c     diffus(imt,0:Nrp1,2)  vertical scalar diffusivity             (m^2/s)
c     diffus(imt,0:Nrp1,3)  vertical temperature diffusivity        (m^2/s)
      _RL diffus(imt,0:Nrp1,3)

#ifdef ALLOW_KPP

c local variables
c     Rig                   local Richardson number
c     fRi, fcon             function of Rig
      _RL Rig
      _RL fRi, fcon
      _RL ratio
      integer i, ki, kp1
      _RL c1, c0

#ifdef ALLOW_KPP_VERTICALLY_SMOOTH
      integer mr
CADJ INIT kpp_ri_tape_mr = common, 1
#endif

c constants
      c1 = 1. _d 0
      c0 = 0. _d 0

c-----------------------------------------------------------------------
c     compute interior gradient Ri at all interfaces ki=1,Nr, (not surface)
c     use diffus(*,*,1) as temporary storage of Ri to be smoothed
c     use diffus(*,*,2) as temporary storage for Brunt-Vaisala squared
c     set values at bottom and below to nearest value above bottom
#ifdef ALLOW_AUTODIFF
C     break data flow dependence on diffus
      diffus(1,1,1) = 0.0
      do ki = 1, Nr
         do i = 1, imt
            diffus(i,ki,1) = 0.
            diffus(i,ki,2) = 0.
            diffus(i,ki,3) = 0.
         enddo
      enddo
#endif

      do ki = 1, Nr
         do i = 1, imt
            if     (kmtj(i) .LE. 1      ) then
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store diffus = comlev1_kpp, key=ikppkey, kind=isbyte
#endif

c-----------------------------------------------------------------------
c     vertically smooth Ri
#ifdef ALLOW_KPP_VERTICALLY_SMOOTH
      do mr = 1, num_v_smooth_Ri

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store diffus(:,:,1) = kpp_ri_tape_mr
CADJ &  , key=mr, shape=(/ (sNx+2*OLx)*(sNy+2*OLy),Nr+2 /)
#endif

         call z121 (
     U     diffus(1,0,1),
     I     myThid )
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

            kp1 = MIN(ki+1,Nr)
#ifdef EXCLUDE_KPP_SHEAR_MIX
            diffus(i,ki,1) = viscArNr(1)
            diffus(i,ki,2) = diffusKzS(i,kp1)
            diffus(i,ki,3) = diffusKzT(i,kp1)
#else /* EXCLUDE_KPP_SHEAR_MIX */
# ifdef ALLOW_AUTODIFF
            if ( inAdMode .AND. .NOT. inAdExact ) then
              diffus(i,ki,1) = viscArNr(1)
              diffus(i,ki,2) = diffusKzS(i,kp1)
              diffus(i,ki,3) = diffusKzT(i,kp1)
            else
# else /* ALLOW_AUTODIFF */
            if ( .TRUE. ) then
# endif /* ALLOW_AUTODIFF */
              diffus(i,ki,1) = viscArNr(1) + fcon*difmcon + fRi*difm0
              diffus(i,ki,2) = diffusKzS(i,kp1)+fcon*difscon+fRi*difs0
              diffus(i,ki,3) = diffusKzT(i,kp1)+fcon*diftcon+fRi*dift0
            endif
#endif /* EXCLUDE_KPP_SHEAR_MIX */
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
     U     v,
     I     myThid )

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
c myThid: thread number for this instance of the routine
      integer myThid
      _RL v(imt,0:Nrp1)

#ifdef ALLOW_KPP

c local
      _RL zwork, zflag
      _RL KRi_range(1:Nrp1)
      integer i, k, km1, kp1

      _RL         p0      , p25       , p5      , p2
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE v(i,k) = z121tape
#endif
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE v(i,k), zwork = z121tape
#endif
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

      subroutine smooth_horiz (
     I     k, bi, bj,
     U     fld,
     I     myThid )

c     Apply horizontal smoothing to global _RL 2-D array

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "KPP_PARAMS.h"

c     input
c     bi, bj : array indices
c     k      : vertical index used for masking
c     myThid : thread number for this instance of the routine
      INTEGER myThid
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
     &           p25   *   maskC(i  ,j  ,k,bi,bj)   +
     &           p125  * ( maskC(im1,j  ,k,bi,bj)   +
     &                     maskC(ip1,j  ,k,bi,bj)   +
     &                     maskC(i  ,jm1,k,bi,bj)   +
     &                     maskC(i  ,jp1,k,bi,bj) ) +
     &           p0625 * ( maskC(im1,jm1,k,bi,bj)   +
     &                     maskC(im1,jp1,k,bi,bj)   +
     &                     maskC(ip1,jm1,k,bi,bj)   +
     &                     maskC(ip1,jp1,k,bi,bj) )
            IF ( tempVar .GE. p25 ) THEN
               fld_tmp(i,j) = (
     &              p25  * fld(i  ,j  )*maskC(i  ,j  ,k,bi,bj) +
     &              p125 *(fld(im1,j  )*maskC(im1,j  ,k,bi,bj) +
     &                     fld(ip1,j  )*maskC(ip1,j  ,k,bi,bj) +
     &                     fld(i  ,jm1)*maskC(i  ,jm1,k,bi,bj) +
     &                     fld(i  ,jp1)*maskC(i  ,jp1,k,bi,bj))+
     &              p0625*(fld(im1,jm1)*maskC(im1,jm1,k,bi,bj) +
     &                     fld(im1,jp1)*maskC(im1,jp1,k,bi,bj) +
     &                     fld(ip1,jm1)*maskC(ip1,jm1,k,bi,bj) +
     &                     fld(ip1,jp1)*maskC(ip1,jp1,k,bi,bj)))
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
     O     , dkm1, blmc, ghat, sigma, ikppkey
     I     , myThid )

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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

c input
c     ustar (imt)                 surface friction velocity             (m/s)
c     bfsfc (imt)                 surface buoyancy forcing          (m^2/s^3)
c     hbl   (imt)                 boundary layer depth                    (m)
c     stable(imt)                 = 1 in stable forcing
c     casea (imt)                 = 1 in case A
c     diffus(imt,0:Nrp1,mdiff)    vertical diffusivities              (m^2/s)
c     kbl   (imt)                 -1 of first grid level below hbl
c     myThid               thread number for this instance of the routine
      integer myThid
      _RL ustar (imt)
      _RL bfsfc (imt)
      _RL hbl   (imt)
      _RL stable(imt)
      _RL casea (imt)
      _RL diffus(imt,0:Nrp1,mdiff)
      integer kbl(imt)

c output
c     dkm1 (imt,mdiff)            boundary layer difs at kbl-1 level
c     blmc (imt,Nr,mdiff)         boundary layer mixing coefficients  (m^2/s)
c     ghat (imt,Nr)               nonlocal scalar transport
c     sigma(imt)                  normalized depth (d / hbl)
      _RL dkm1 (imt,mdiff)
      _RL blmc (imt,Nr,mdiff)
      _RL ghat (imt,Nr)
      _RL sigma(imt)
      integer ikppkey

#ifdef ALLOW_KPP

c  local
c     gat1*(imt)                 shape function at sigma = 1
c     dat1*(imt)                 derivative of shape function at sigma = 1
c     ws(imt), wm(imt)           turbulent velocity scales             (m/s)
      _RL gat1m(imt), gat1s(imt), gat1t(imt)
      _RL dat1m(imt), dat1s(imt), dat1t(imt)
      _RL ws(imt), wm(imt)
      integer i, kn, ki, kl
      _RL R, dvdzup, dvdzdn, viscp
      _RL difsp, diftp, visch, difsh, difth
      _RL f1, sig, a1, a2, a3, delhat
      _RL Gm, Gs, Gt
      _RL tempVar

      _RL    p0    , eins
      parameter (p0=0.0, eins=1.0)
#ifdef ALLOW_AUTODIFF_TAMC
      integer kkppkey
#endif

c-----------------------------------------------------------------------
c compute velocity scales at hbl
c-----------------------------------------------------------------------

      do i = 1, imt
         sigma(i) = stable(i) * 1.0 + (1. - stable(i)) * epsilon
      end do

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sigma = comlev1_kpp, key=ikppkey, kind=isbyte
#endif
      call wscale (
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wm = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE ws = comlev1_kpp, key=ikppkey, kind=isbyte
#endif

      do i = 1, imt
         wm(i) = sign(eins,wm(i))*max(phepsi,abs(wm(i)))
         ws(i) = sign(eins,ws(i))*max(phepsi,abs(ws(i)))
      end do
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wm = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE ws = comlev1_kpp, key=ikppkey, kind=isbyte
#endif

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

         gat1s(i) = difsh  / hbl(i) / ws(i)
         dat1s(i) = -difsp / ws(i) + f1 * difsh

         gat1t(i) = difth /  hbl(i) / ws(i)
         dat1t(i) = -diftp / ws(i) + f1 * difth

      end do
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE gat1m = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE gat1s = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE gat1t = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE dat1m = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE dat1s = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE dat1t = comlev1_kpp, key=ikppkey, kind=isbyte
#endif
      do i = 1, imt
         dat1m(i) = min(dat1m(i),p0)
         dat1s(i) = min(dat1s(i),p0)
         dat1t(i) = min(dat1t(i),p0)
      end do
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE dat1m = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE dat1s = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE dat1t = comlev1_kpp, key=ikppkey, kind=isbyte
#endif

      do ki = 1, Nr

#ifdef ALLOW_AUTODIFF_TAMC
         kkppkey = (ikppkey-1)*Nr + ki
#endif

c-----------------------------------------------------------------------
c     compute turbulent velocity scales on the interfaces
c-----------------------------------------------------------------------

         do i = 1, imt
            sig      = (-zgrid(ki) + 0.5 * hwide(ki)) / hbl(i)
            sigma(i) = stable(i)*sig + (1.-stable(i))*min(sig,epsilon)
         end do
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE wm = comlev1_kpp_k, key = kkppkey
CADJ STORE ws = comlev1_kpp_k, key = kkppkey
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sigma = comlev1_kpp_k, key = kkppkey
#endif
         call wscale (
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wm = comlev1_kpp_k, key = kkppkey
CADJ STORE ws = comlev1_kpp_k, key = kkppkey
#endif

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
         kl = kbl(i)
         sig      = -zgrid(kl-1) / hbl(i)
         sigma(i) = stable(i) * sig
     &            + (1. - stable(i)) * min(sig,epsilon)
      end do

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE wm = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE ws = comlev1_kpp, key=ikppkey, kind=isbyte
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sigma = comlev1_kpp, key=ikppkey, kind=isbyte
#endif
      call wscale (
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wm = comlev1_kpp, key=ikppkey, kind=isbyte
CADJ STORE ws = comlev1_kpp, key=ikppkey, kind=isbyte
#endif

      do i = 1, imt
         kl = kbl(i)
         sig = -zgrid(kl-1) / hbl(i)
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
     &     , myThid )

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
c     myThid                    thread number for this instance of the routine
      integer   myThid
      _RL dkm1  (imt,mdiff)
      _RL hbl   (imt)
      integer kbl   (imt)
      _RL diffus(imt,0:Nrp1,mdiff)
      _RL casea (imt)

c input/output
c     nonlocal transport, modified ghat at kbl(i)-1 interface    (s/m**2)
      _RL ghat (imt,Nr)

c output
c     enhanced bound. layer mixing coeff.
      _RL blmc  (imt,Nr,mdiff)

#ifdef ALLOW_KPP

c local
c     fraction hbl lies beteen zgrid neighbors
      _RL delta
      integer ki, i, md
      _RL dkmp5, dstar

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
     O     RHO1, DBLOC, DBSFC, TTALPHA, SSBETA,
     I     ikppkey, bi, bj, myThid )
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
c               d(rho) / d(salinity)                          ((kg/m^3)/(g/kg))
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
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

c-------------- Routine arguments -----------------------------------------
      INTEGER bi, bj, myThid
      _RL RHO1   ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy       )
      _RL DBLOC  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr   )
      _RL DBSFC  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr   )
      _RL TTALPHA( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1 )
      _RL SSBETA ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1 )

#ifdef ALLOW_KPP

c--------------------------------------------------------------------------
c
c     local arrays:
c
c     rhok         - density of t(k  ) & s(k  ) at depth k
c     rhokm1       - density of t(k-1) & s(k-1) at depth k
c     rho1k        - density of t(1  ) & s(1  ) at depth k
c     work1,2,3    - work arrays for holding horizontal slabs

      _RL RHOK  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL RHOKM1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL RHO1K (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL WORK1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL WORK2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL WORK3 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      INTEGER I, J, K
      INTEGER ikppkey, kkppkey

c calculate density, alpha, beta in surface layer, and set dbsfc to zero

      kkppkey = (ikppkey-1)*Nr + 1

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
CADJ STORE salt (:,:,1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */
      CALL FIND_RHO_2D(
     I     1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I     theta(1-OLx,1-OLy,1,bi,bj), salt(1-OLx,1-OLy,1,bi,bj),
     O     WORK1,
     I     1, bi, bj, myThid )
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
CADJ STORE salt (:,:,1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */

      call FIND_ALPHA(
     I     bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1, 1,
     O     WORK2, myThid )

      call FIND_BETA(
     I     bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1, 1,
     O     WORK3, myThid )

      DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
            RHO1(I,J)      = WORK1(I,J) + rhoConst
            TTALPHA(I,J,1) = WORK2(I,J)
            SSBETA(I,J,1)  = WORK3(I,J)
            DBSFC(I,J,1)   = 0.
         END DO
      END DO

c calculate alpha, beta, and gradients in interior layers

CHPF$  INDEPENDENT, NEW (RHOK,RHOKM1,RHO1K,WORK1,WORK2)
      DO K = 2, Nr

      kkppkey = (ikppkey-1)*Nr + k

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,k,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        RHOK,
     I        k, bi, bj, myThid )

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,k-1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
CADJ STORE salt (:,:,k-1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I        theta(1-OLx,1-OLy,k-1,bi,bj),salt(1-OLx,1-OLy,k-1,bi,bj),
     O        RHOKM1,
     I        k-1, bi, bj, myThid )

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
CADJ STORE salt (:,:,1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I        theta(1-OLx,1-OLy,1,bi,bj), salt(1-OLx,1-OLy,1,bi,bj),
     O        RHO1K,
     I        1, bi, bj, myThid )

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE rhok  (:,:)          = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
CADJ STORE rhokm1(:,:)          = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
CADJ STORE rho1k (:,:)          = comlev1_kpp_k,
CADJ &     key=kkppkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */

         call FIND_ALPHA(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, K, K,
     O        WORK1, myThid )

         call FIND_BETA(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, K, K,
     O        WORK2, myThid )

         DO J = 1-OLy, sNy+OLy
            DO I = 1-OLx, sNx+OLx
               TTALPHA(I,J,K) = WORK1 (I,J)
               SSBETA(I,J,K)  = WORK2 (I,J)
               DBLOC(I,J,K-1) = gravity * (RHOK(I,J) - RHOKM1(I,J)) /
     &                                    (RHOK(I,J) + rhoConst)
               DBSFC(I,J,K)   = gravity * (RHOK(I,J) - RHO1K (I,J)) /
     &                                    (RHOK(I,J) + rhoConst)
            END DO
         END DO

      END DO

c     compute arrays for K = Nrp1
      DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
            TTALPHA(I,J,Nrp1) = TTALPHA(I,J,Nr)
            SSBETA(I,J,Nrp1)  = SSBETA(I,J,Nr)
            DBLOC(I,J,Nr)     = 0.
         END DO
      END DO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(DBSFC ,'KPPdbsfc',0,Nr,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DBLOC ,'KPPdbloc',0,Nr,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_KPP */

      RETURN
      END

c*************************************************************************

      SUBROUTINE KPP_DOUBLEDIFF (
     I     TTALPHA, SSBETA,
     U     kappaRT,
     U     kappaRS,
     I     ikppkey, imin, imax, jmin, jmax, bi, bj, myThid )
c
c-----------------------------------------------------------------------
c     "KPP_DOUBLEDIFF" adds the double diffusive contributions
C     as Rrho-dependent parameterizations to kappaRT and kappaRS
c
c     input:
c     bi, bj  = array indices on which to apply calculations
c     imin, imax, jmin, jmax = array boundaries
c     ikppkey = key for TAMC/TAF automatic differentiation
c     myThid  = thread id
c
c      ttalpha= thermal expansion coefficient without 1/rho factor
c               d(rho) / d(potential temperature)                    (kg/m^3/C)
c      ssbeta = salt expansion coefficient without 1/rho factor
c               d(rho) / d(salinity)                          ((kg/m^3)/(g/kg))
c     output: updated
c     kappaRT/S :: background diffusivities for temperature and salinity
c
c     written  by: martin losch,   sept. 15, 2009
c

c-----------------------------------------------------------------------

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP_PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

c-------------- Routine arguments -----------------------------------------
      INTEGER ikppkey, imin, imax, jmin, jmax, bi, bj, myThid

      _RL TTALPHA( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1 )
      _RL SSBETA ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1 )
      _RL KappaRT( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr   )
      _RL KappaRS( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr   )

#ifdef ALLOW_KPP

C--------------------------------------------------------------------------
C
C     local variables
C     I,J,K :: loop indices
C     kkppkey :: key for TAMC/TAF automatic differentiation
C
      INTEGER I, J, K
      INTEGER kkppkey
C     alphaDT   :: d\rho/d\theta * d\theta
C     betaDS    :: d\rho/dsalt * dsalt
C     Rrho      :: "density ratio" R_{\rho} = \alpha dT/dz / \beta dS/dz
C     nuddt/s   :: double diffusive diffusivities
C     numol     :: molecular diffusivity
C     rFac      :: abbreviation for 1/(R_{\rho0}-1)

      _RL alphaDT ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )
      _RL betaDS  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )
      _RL nuddt   ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )
      _RL nudds   ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )
      _RL Rrho
      _RL numol, rFac, nutmp
      INTEGER Km1

C     set some constants here
      numol = 1.5 _d -06
      rFac  = 1. _d 0 / (Rrho0 - 1. _d 0 )
C
      kkppkey = (ikppkey-1)*Nr + 1

CML#ifdef KPP_AUTODIFF_MORE_STORE
CMLCADJ STORE theta(:,:,1,bi,bj) = comlev1_kpp_k,
CMLCADJ &     key=kkppkey, kind=isbyte
CMLCADJ STORE salt (:,:,1,bi,bj) = comlev1_kpp_k,
CMLCADJ &     key=kkppkey, kind=isbyte
CML#endif /* KPP_AUTODIFF_MORE_STORE */

      DO K = 1, Nr
       Km1 = MAX(K-1,1)
       DO J = 1-OLy, sNy+OLy
        DO I = 1-OLx, sNx+OLx
         alphaDT(I,J) = ( theta(I,J,Km1,bi,bj)-theta(I,J,K,bi,bj) )
     &        * 0.5 _d 0 * ABS( TTALPHA(I,J,Km1) + TTALPHA(I,J,K) )
         betaDS(I,J)  = ( salt(I,J,Km1,bi,bj)-salt(I,J,K,bi,bj) )
     &        * 0.5 _d 0 * ( SSBETA(I,J,Km1) + SSBETA(I,J,K) )
         nuddt(I,J) = 0. _d 0
         nudds(I,J) = 0. _d 0
        ENDDO
       ENDDO
       IF ( K .GT. 1 ) THEN
        DO J = jMin, jMax
         DO I = iMin, iMax
          Rrho  = 0. _d 0
C     Now we have many different cases
C     a. alphaDT > 0 and betaDS > 0 => salt fingering
C        (salinity destabilizes)
          IF (      alphaDT(I,J) .GT. betaDS(I,J)
     &         .AND. betaDS(I,J) .GT. 0. _d 0 ) THEN
           Rrho = MIN( alphaDT(I,J)/betaDS(I,J), Rrho0 )
C     Large et al. 1994, eq. 31a
C          nudds(I,J) = dsfmax * ( 1. _d 0 - (Rrho - 1. _d 0) * rFac )**3
           nutmp      =          ( 1. _d 0 - (Rrho - 1. _d 0) * rFac )
           nudds(I,J) = dsfmax * nutmp * nutmp * nutmp
C     Large et al. 1994, eq. 31c
           nuddt(I,J) = 0.7 _d 0 * nudds(I,J)
          ELSEIF (   alphaDT(I,J) .LT. 0. _d 0
     &          .AND. betaDS(I,J) .LT. 0. _d 0
     &          .AND.alphaDT(I,J) .GT. betaDS(I,J) ) THEN
C     b. alphaDT < 0 and betaDS < 0 => semi-convection, diffusive convection
C        (temperature destabilizes)
C     for Rrho >= 1 the water column is statically unstable and we never
C     reach this point
           Rrho = alphaDT(I,J)/betaDS(I,J)
C     Large et al. 1994, eq. 32
           nuddt(I,J) = numol * 0.909 _d 0
     &          * exp ( 4.6 _d 0 * exp (
     &          - 5.4 _d 0 * ( 1. _d 0/Rrho - 1. _d 0 ) ) )
CMLC     or
CMLC     Large et al. 1994, eq. 33
CML         nuddt(I,J) = numol * 8.7 _d 0 * Rrho**1.1
C     Large et al. 1994, eqs. 34
           nudds(I,J) = nuddt(I,J) * MAX( 0.15 _d 0 * Rrho,
     &          1.85 _d 0 * Rrho - 0.85 _d 0 )
          ELSE
C     Do nothing, because in this case the water colume is unstable
C     => double diffusive processes are negligible and mixing due
C     to shear instability will dominate
          ENDIF
         ENDDO
        ENDDO
C     ENDIF ( K .GT. 1 )
       ENDIF
C
       DO J = 1-OLy, sNy+OLy
        DO I = 1-OLx, sNx+OLx
         kappaRT(I,J,K) = kappaRT(I,J,K) + nuddt(I,J)
         kappaRS(I,J,K) = kappaRS(I,J,K) + nudds(I,J)
        ENDDO
       ENDDO
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(nuddt,'KPPnuddt',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(nudds,'KPPnudds',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
C     end of K-loop
      ENDDO
#endif /* ALLOW_KPP */

      RETURN
      END
