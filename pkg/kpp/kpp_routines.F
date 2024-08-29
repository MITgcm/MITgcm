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
     I       kmtj, shsq, dvsq, ustar, msk,
     I       bo, bosol,
#ifdef ALLOW_SALT_PLUME
     I       boplume, SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I       lon, lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
     I       dbloc, Ritop, coriol,
     I       diffusKzS, diffusKzT,
     I       ikey,
     O       diffus,
     U       ghat,
     O       hbl,
     I       bi, bj, myTime, myIter, myThid )

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
C     ikey :: tape key TAF-AD simulations (depends on tiles)
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
      INTEGER myIter
      INTEGER myThid
      INTEGER kmtj (imt   )
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

      INTEGER ikey

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

      INTEGER kbl(imt         )
      _RL bfsfc  (imt         )
      _RL casea  (imt         )
      _RL stable (imt         )
      _RL dkm1   (imt,   mdiff)
      _RL blmc   (imt,Nr,mdiff)
      _RL sigma  (imt         )
      _RL Rib    (imt,Nr      )

      INTEGER i, k, md

c-----------------------------------------------------------------------
c compute interior mixing coefficients everywhere, due to constant
c internal wave activity, static instability, and local shear
c instability.
c (ghat is temporary storage for horizontally smoothed dbloc)
c-----------------------------------------------------------------------

cph(
cph these storings avoid recomp. of Ri_iwmix
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ghat  = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE dbloc = comlev1_kpp, key=ikey, kind=isbyte
#endif
cph)
      CALL Ri_iwmix (
     I       kmtj, shsq, dbloc, ghat,
     I       diffusKzS, diffusKzT,
     I       ikey,
     O       diffus, myThid )

cph(
cph these storings avoid recomp. of Ri_iwmix
cph DESPITE TAFs 'not necessary' warning!
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dbloc  = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE shsq   = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE ghat   = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE diffus = comlev1_kpp, key=ikey, kind=isbyte
#endif
cph)

c-----------------------------------------------------------------------
c set seafloor values to zero and fill extra "Nrp1" coefficients
c for blmix
c-----------------------------------------------------------------------

      DO md = 1, mdiff
       DO k=1,Nrp1
        DO i = 1,imt
         IF (k.GE.kmtj(i)) diffus(i,k,md) = 0.0
        ENDDO
       ENDDO
      ENDDO

c-----------------------------------------------------------------------
c compute boundary layer mixing coefficients:
c
c diagnose the new boundary layer depth
c-----------------------------------------------------------------------

      CALL bldepth (
     I       kmtj,
     I       dvsq, dbloc, Ritop, ustar, bo, bosol,
#ifdef ALLOW_SALT_PLUME
     I       boplume, SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I       lon, lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
     I       coriol,
     I       ikey,
     O       hbl, bfsfc, stable, casea, kbl, Rib, sigma,
     I       bi, bj, myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hbl,bfsfc,stable,casea,kbl = comlev1_kpp,
CADJ &     key=ikey, kind=isbyte
#endif

c-----------------------------------------------------------------------
c compute boundary layer diffusivities
c-----------------------------------------------------------------------

      CALL blmix (
     I       ustar, bfsfc, hbl, stable, casea, diffus, kbl,
     O       dkm1, blmc, ghat, sigma, ikey,
     I       myThid )
cph(
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dkm1,blmc,ghat = comlev1_kpp,
CADJ &     key=ikey, kind=isbyte
CADJ STORE hbl, kbl, diffus, casea = comlev1_kpp,
CADJ &     key=ikey, kind=isbyte
#endif
cph)

c-----------------------------------------------------------------------
c enhance diffusivity at interface kbl - 1
c-----------------------------------------------------------------------

      CALL enhance (
     I       dkm1, hbl, kbl, diffus, casea,
     U       ghat,
     O       blmc,
     I       myThid )

cph(
cph avoids recomp. of enhance
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE blmc = comlev1_kpp, key=ikey, kind=isbyte
#endif
cph)

c-----------------------------------------------------------------------
c combine interior and boundary layer coefficients and nonlocal term
c !!!NOTE!!! In shallow (2-level) regions and for shallow mixed layers
c (< 1 level), diffusivity blmc can become negative.  The max-s below
c are a hack until this problem is properly diagnosed and fixed.
c-----------------------------------------------------------------------
      DO k = 1, Nr
         DO i = 1, imt
            IF (k .LT. kbl(i)) THEN
#ifdef ALLOW_SHELFICE
C     when there is shelfice on top (msk(i)=0), reset the boundary layer
C     mixing coefficients blmc to pure Ri-number based mixing
               blmc(i,k,1) = MAX ( blmc(i,k,1)*msk(i),
     &              diffus(i,k,1) )
               blmc(i,k,2) = MAX ( blmc(i,k,2)*msk(i),
     &              diffus(i,k,2) )
               blmc(i,k,3) = MAX ( blmc(i,k,3)*msk(i),
     &              diffus(i,k,3) )
#endif /* not ALLOW_SHELFICE */
               diffus(i,k,1) = MAX ( blmc(i,k,1), viscArNr(1) )
               diffus(i,k,2) = MAX ( blmc(i,k,2), diffusKzS(i,Nr) )
               diffus(i,k,3) = MAX ( blmc(i,k,3), diffusKzT(i,Nr) )
            ELSE
               ghat(i,k) = 0. _d 0
            ENDIF
         ENDDO
      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END

c*************************************************************************

      subroutine bldepth (
     I       kmtj,
     I       dvsq, dbloc, Ritop, ustar, bo, bosol,
#ifdef ALLOW_SALT_PLUME
     I       boplume, SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I       lon, lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
     I       coriol,
     I       ikey,
     O       hbl, bfsfc, stable, casea, kbl, Rib, sigma,
     I       bi, bj, myTime, myIter, myThid )

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
      INTEGER myIter
      INTEGER myThid
      INTEGER kmtj(imt)
      _RL dvsq    (imt,Nr)
      _RL dbloc   (imt,Nr)
      _RL Ritop   (imt,Nr)
      _RL ustar   (imt)
      _RL bo      (imt)
      _RL bosol   (imt)
      _RL coriol  (imt)
      INTEGER ikey
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
      INTEGER kbl(imt)
      _RL Rib    (imt,Nr)
      _RL sigma  (imt)

#ifdef ALLOW_KPP

c  local
c-------
c wm, ws    : turbulent velocity scales         (m/s)
      _RL wm(imt), ws(imt)
      _RL worka(imt)
      _RL bvsq, vtsq, hekman, hmonob, hlimit, tempVar1, tempVar2
      INTEGER i, kl

      _RL         p5    , eins
      PARAMETER ( p5=0.5, eins=1.0 )
      _RL         minusone
      PARAMETER ( minusone=-1.0 )
#ifdef SALT_PLUME_VOLUME
      INTEGER km, km1
      _RL temp
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER kkey
#endif

#ifdef ALLOW_DIAGNOSTICS
c     KPPBFSFC - Bo+radiation absorbed to d=hbf*hbl + plume (m^2/s^3)
      _RL KPPBFSFC(imt,Nr)
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

      DO i = 1, imt
         Rib(i,1) = 0. _d 0
         kbl(i) = kmtj(i)
         IF (kmtj(i).LT.1) kbl(i) = 1
         kl     = kbl(i)
         hbl(i) = -zgrid(kl)
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      DO kl = 1, Nr
         DO i = 1, imt
            KPPBFSFC(i,kl) = 0. _d 0
         ENDDO
      ENDDO
#endif /* ALLOW_DIAGNOSTICS */

      DO kl = 2, Nr

#ifdef ALLOW_AUTODIFF_TAMC
         kkey = (ikey-1)*Nr + kl
#endif

c     compute bfsfc = sw fraction at hbf * zgrid

         DO i = 1, imt
            worka(i) = zgrid(kl)
         ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp_k, key = kkey, kind=isbyte
#endif
         CALL SWFRAC(
     I       imt, hbf,
     U       worka,
     I       myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp_k, key = kkey, kind=isbyte
#endif

         DO i = 1, imt

c     use caseA as temporary array

            casea(i) = -zgrid(kl)

c     compute bfsfc= Bo + radiative contribution down to hbf * hbl

            bfsfc(i) = bo(i) + bosol(i)*(1. - worka(i))

         ENDDO
#ifdef ALLOW_SALT_PLUME
c     compute bfsfc = plume fraction at hbf * zgrid
         IF ( useSALT_PLUME ) THEN
#ifndef SALT_PLUME_VOLUME
           DO i = 1, imt
              worka(i) = zgrid(kl)
           ENDDO
Ccatn: in original way: accumulate all fractions of boplume above zgrid(kl)
           CALL SALT_PLUME_FRAC(
     I         imt, hbf,SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I         lon,lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
     U         worka,
     I         myTime, myIter, myThid)
           DO i = 1, imt
              bfsfc(i) = bfsfc(i) + boplume(i,1)*(worka(i))
C            km=MAX(1,kbl(i)-1)
C            temp = (plumefrac(i,km)+plumefrac(i,kbl(i)))/2.0
C            bfsfc(i) = bfsfc(i) + boplume(i,1)*temp
           ENDDO
#else /* def SALT_PLUME_VOLUME */
catn: in vol way: need to integrate down to hbl, so first locate
c     k level associated with this hbl, then sum up all SPforc[T,S]
           DO i = 1, imt
            km =MAX(1,kbl(i)-1)
            km1=MAX(1,kbl(i))
            temp = (boplume(i,km)+boplume(i,km1))*p5
            bfsfc(i) = bfsfc(i) + temp
           ENDDO
#endif /* ndef SALT_PLUME_VOLUME */
         ENDIF
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_DIAGNOSTICS
         DO i = 1, imt
            KPPBFSFC(i,kl) = bfsfc(i)
         ENDDO
#endif /* ALLOW_DIAGNOSTICS */

         DO i = 1, imt
            stable(i) = p5 + sign(p5,bfsfc(i))
            sigma(i) = stable(i) + (1. - stable(i)) * epsilon
         ENDDO

c-----------------------------------------------------------------------
c     compute velocity scales at sigma, for hbl= caseA = -zgrid(kl)
c-----------------------------------------------------------------------

         CALL wscale (
     I        sigma, casea, ustar, bfsfc,
     O        wm, ws, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store ws = comlev1_kpp_k, key = kkey, kind=isbyte
#endif

         DO i = 1, imt

c-----------------------------------------------------------------------
c     compute the turbulent shear contribution to Rib
c-----------------------------------------------------------------------

            bvsq = p5 *
     1           ( dbloc(i,kl-1) / (zgrid(kl-1)-zgrid(kl  ))+
     2             dbloc(i,kl  ) / (zgrid(kl  )-zgrid(kl+1)))
CMLC     Van Roekel et al 2018 suggest this, but our solution is
CMLC     probably OK, too:
CML            bvsq = MAX
CML     1           ( dbloc(i,kl-1) / (zgrid(kl-1)-zgrid(kl  )),
CML     2             dbloc(i,kl  ) / (zgrid(kl  )-zgrid(kl+1)))

            IF (bvsq .EQ. 0. _d 0) THEN
              vtsq = 0. _d 0
            ELSE
              vtsq = -zgrid(kl) * ws(i) * SQRT(ABS(bvsq)) * Vtc
            ENDIF

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
#ifdef KPP_SMOOTH_REGULARISATION
            tempVar2 = tempVar1 + phepsi
#else
            tempVar2 = MAX(tempVar1, phepsi)
#endif /* KPP_SMOOTH_REGULARISATION */
            Rib(i,kl) = Ritop(i,kl) / tempVar2

         ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(KPPBFSFC,'KPPbfsfc',0,Nr,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(Rib     ,'KPPRi   ',0,Nr,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

cph(
cph  without this store, there is a recomputation error for
cph  rib in adbldepth (probably partial recomputation problem)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store Rib = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy),Nr /)
#endif
cph)

      DO kl = 2, Nr
         DO i = 1, imt
            IF (kbl(i).EQ.kmtj(i) .AND. Rib(i,kl).GT.Ricr) kbl(i) = kl
         ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store kbl = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

      DO i = 1, imt
         kl = kbl(i)
c     linearly interpolate to find hbl where Rib = Ricr
         IF (kl.GT.1 .AND. kl.LT.kmtj(i)) THEN
            tempVar1 = (Rib(i,kl)-Rib(i,kl-1))
            hbl(i) = -zgrid(kl-1) + (zgrid(kl-1)-zgrid(kl)) *
     1           (Ricr - Rib(i,kl-1)) / tempVar1
C     this is the MOM5 formulation, (nearly) identical results
CML            hbl(i) = ((Rib(i,kl-1)-Ricr)*zgrid(kl) -
CML     1           (Rib(i,kl)-Ricr)*zgrid(kl-1))/tempVar1
         ENDIF
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store hbl = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

c-----------------------------------------------------------------------
c     find stability and buoyancy forcing for boundary layer
c-----------------------------------------------------------------------

      DO i = 1, imt
         worka(i) = hbl(i)
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif
      CALL SWFRAC(
     I       imt, minusone,
     U       worka,
     I       myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

      DO i = 1, imt
         bfsfc(i)  = bo(i) + bosol(i) * (1. - worka(i))
      ENDDO

#ifdef ALLOW_SALT_PLUME
# ifdef ALLOW_AUTODIFF_TAMC
C     suppress a TAF warning
CADJ incomplete worka
# endif
      IF ( useSALT_PLUME ) THEN
#ifndef SALT_PLUME_VOLUME
        DO i = 1, imt
           worka(i) = hbl(i)
        ENDDO
        CALL SALT_PLUME_FRAC(
     I         imt,minusone,SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I         lon,lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
     U         worka,
     I         myTime, myIter, myThid )
        DO i = 1, imt
           bfsfc(i) = bfsfc(i) + boplume(i,1) * (worka(i))
C            km=MAX(1,kbl(i)-1)
C            temp = (plumefrac(i,km)+plumefrac(i,kbl(i)))/2.0
C            bfsfc(i) = bfsfc(i) + boplume(i,1)*temp
        ENDDO
#else /* def SALT_PLUME_VOLUME */
        DO i = 1, imt
            km =MAX(1,kbl(i)-1)
            km1=MAX(1,kbl(i))
            temp = (boplume(i,km)+boplume(i,km1))/2.0
            bfsfc(i) = bfsfc(i) + temp
        ENDDO
#endif /* ndef SALT_PLUME_VOLUME */
      ENDIF
#endif /* ALLOW_SALT_PLUME */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store bfsfc = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

c--   ensure bfsfc is never 0
      DO i = 1, imt
         stable(i) = p5 + sign( p5, bfsfc(i) )
         bfsfc(i) = sign(eins,bfsfc(i))*MAX(phepsi,ABS(bfsfc(i)))
      ENDDO

cph(
cph  added stable to store list to avoid extensive recomp.
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store bfsfc, stable = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif
cph)

c-----------------------------------------------------------------------
c check hbl limits for hekman or hmonob
c     ph: test for zero nominator
c-----------------------------------------------------------------------

      IF ( LimitHblStable ) THEN
       DO i = 1, imt
        IF (bfsfc(i) .GT. 0.0) THEN
         hekman = cekman * ustar(i) / MAX(ABS(Coriol(i)),phepsi)
         hmonob = cmonob * ustar(i)*ustar(i)*ustar(i)
     &        / vonk / bfsfc(i)
         hlimit = stable(i) * MIN(hekman,hmonob)
     &        + (stable(i)-1.) * zgrid(Nr)
         hbl(i) = MIN(hbl(i),hlimit)
        ENDIF
       ENDDO
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store hbl = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

      DO i = 1, imt
         hbl(i) = MAX(hbl(i),minKPPhbl)
         kbl(i) = kmtj(i)
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store hbl = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

c-----------------------------------------------------------------------
c      find new kbl
c-----------------------------------------------------------------------

      DO kl = 2, Nr
         DO i = 1, imt
            IF ( kbl(i).EQ.kmtj(i) .AND. (-zgrid(kl)).GT.hbl(i) ) THEN
               kbl(i) = kl
            ENDIF
         ENDDO
      ENDDO

c-----------------------------------------------------------------------
c      find stability and buoyancy forcing for final hbl values
c-----------------------------------------------------------------------

      DO i = 1, imt
         worka(i) = hbl(i)
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif
      CALL SWFRAC(
     I     imt, minusone,
     U     worka,
     I     myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store worka = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

      DO i = 1, imt
         bfsfc(i) = bo(i) + bosol(i) * (1. - worka(i))
      ENDDO

#ifdef ALLOW_SALT_PLUME
      IF ( useSALT_PLUME ) THEN
#ifndef SALT_PLUME_VOLUME
        DO i = 1, imt
           worka(i) = hbl(i)
        ENDDO
        CALL SALT_PLUME_FRAC(
     I         imt,minusone,SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I         lon,lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
     U         worka,
     I         myTime, myIter, myThid )
        DO i = 1, imt
           bfsfc(i) = bfsfc(i) + boplume(i,1) * (worka(i))
C            km=MAX(1,kbl(i)-1)
C            temp = (plumefrac(i,km)+plumefrac(i,kbl(i)))/2.0
C            bfsfc(i) = bfsfc(i) + boplume(i,1)*temp
        ENDDO
#else /* def SALT_PLUME_VOLUME */
        DO i = 1, imt
            km =MAX(1,kbl(i)-1)
            km1=MAX(1,kbl(i)-0)
            temp = (boplume(i,km)+boplume(i,km1))/2.0
            bfsfc(i) = bfsfc(i) + temp
        ENDDO
#endif /* ndef SALT_PLUME_VOLUME */
      ENDIF
#endif /* ALLOW_SALT_PLUME */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store bfsfc = comlev1_kpp, key=ikey, kind=isbyte,
CADJ &     shape = (/ (sNx+2*OLx)*(sNy+2*OLy) /)
#endif

c--   ensures bfsfc is never 0
      DO i = 1, imt
         stable(i) = p5 + sign( p5, bfsfc(i) )
         bfsfc(i) = sign(eins,bfsfc(i))*MAX(phepsi,ABS(bfsfc(i)))
      ENDDO

c-----------------------------------------------------------------------
c determine caseA and caseB
c-----------------------------------------------------------------------

      DO i = 1, imt
         kl = kbl(i)
         casea(i) = p5 +
     1        sign(p5, -zgrid(kl) - p5*hwide(kl) - hbl(i))
      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END

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
c     (zehat.LE.0), in the stable domain wm (=ws) gets computed
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
      INTEGER myThid
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

      INTEGER iz, izp1, ju, i, jup1
      _RL udiff, zdiff, zfrac, ufrac, fzfrac, wam
      _RL wbm, was, wbs, u3, tempVar

c-----------------------------------------------------------------------
c use lookup table for zehat < zmax only; otherwise use
c stable formulae
c-----------------------------------------------------------------------

      DO i = 1, imt
         zehat = vonk*sigma(i)*hbl(i)*bfsfc(i)

         IF (zehat .LE. zmax) THEN

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
C           zdiff = MAX( 0. _d 0, zehat - zmin )
            iz    = INT( zdiff / deltaz )
            iz    = MIN( iz, nni )
            iz    = MAX( iz, 0 )
            izp1  = iz + 1

            udiff = ustar(i) - umin
            ju    = INT( udiff / deltau )
            ju    = MIN( ju, nnj )
            ju    = MAX( ju, 0 )
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

         ELSE

            u3 = ustar(i) * ustar(i) * ustar(i)
            tempVar = u3 + conc1 * zehat
            wm(i) = vonk * ustar(i) * u3 / tempVar
            ws(i) = wm(i)

         ENDIF

      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END

c*************************************************************************

      subroutine Ri_iwmix (
     I       kmtj, shsq, dbloc, dblocSm,
     I       diffusKzS, diffusKzT,
     I       ikey,
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
      INTEGER kmtj (imt)
      _RL shsq     (imt,Nr)
      _RL dbloc    (imt,Nr)
      _RL dblocSm  (imt,Nr)
      _RL diffusKzS(imt,Nr)
      _RL diffusKzT(imt,Nr)
      INTEGER ikey
      INTEGER myThid

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
      INTEGER i, ki, kp1
      _RL c1, c0

#ifdef ALLOW_KPP_VERTICALLY_SMOOTH
      INTEGER mr
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
      DO ki = 1, Nr
         DO i = 1, imt
            diffus(i,ki,1) = 0.
            diffus(i,ki,2) = 0.
            diffus(i,ki,3) = 0.
         ENDDO
      ENDDO
#endif

      DO ki = 1, Nr
         DO i = 1, imt
            IF     (kmtj(i) .LE. 1      ) THEN
               diffus(i,ki,1) = 0.
               diffus(i,ki,2) = 0.
            ELSEIF (ki      .GE. kmtj(i)) THEN
               diffus(i,ki,1) = diffus(i,ki-1,1)
               diffus(i,ki,2) = diffus(i,ki-1,2)
            ELSE
               diffus(i,ki,1) = dblocSm(i,ki) * (zgrid(ki)-zgrid(ki+1))
#ifdef KPP_SMOOTH_REGULARISATION
     &            / ( Shsq(i,ki) + phepsi**2 )
#else
     &            / MAX( Shsq(i,ki), phepsi )
#endif
               diffus(i,ki,2) = dbloc(i,ki)   / (zgrid(ki)-zgrid(ki+1))
            ENDIF
         ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ store diffus = comlev1_kpp, key=ikey, kind=isbyte
#endif

c-----------------------------------------------------------------------
c     vertically smooth Ri
#ifdef ALLOW_KPP_VERTICALLY_SMOOTH
      DO mr = 1, num_v_smooth_Ri

#ifdef ALLOW_AUTODIFF_TAMC
CADJ store diffus(:,:,1) = kpp_ri_tape_mr, key=mr
CADJ &  , shape=(/ (sNx+2*OLx)*(sNy+2*OLy),Nr+2 /)
CMLCADJ store diffus(:,:,2) = kpp_ri_tape_mr, key=mr
CMLCADJ &  , shape=(/ (sNx+2*OLx)*(sNy+2*OLy),Nr+2 /)
#endif

         CALL z121 (
     U     diffus(1,0,1),
     I     myThid )
C     it may also make sense to smooth buoyancy vertically
CML         CALL z121 (
CML     U     diffus(1,0,2),
CML     I     myThid )
      ENDDO
#endif

c-----------------------------------------------------------------------
c                           after smoothing loop

      DO ki = 1, Nr
         DO i = 1, imt

c  evaluate f of Brunt-Vaisala squared for convection, store in fcon

            Rig   = MAX ( diffus(i,ki,2) , BVSQcon )
            ratio = MIN ( (BVSQcon - Rig) / BVSQcon, c1 )
            fcon  = c1 - ratio * ratio
            fcon  = fcon * fcon * fcon

c  evaluate f of smooth Ri for shear instability, store in fRi

            Rig  = MAX ( diffus(i,ki,1), c0 )
            ratio = MIN ( Rig / Riinfty , c1 )
            fRi   = c1 - ratio * ratio
            fRi   = fRi * fRi * fRi
#ifdef KPP_SCALE_SHEARMIXING
C     reduce shear mixing when there is no shear (Polzin, 1996, JPO, 1409-1425)
C     importend from MOM5 code
            fRi   = fRi * shsq(i,ki)*shsq(i,ki)
     &           /(shsq(i,ki)*shsq(i,ki) + 1. _d -16)
#endif
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
            IF ( inAdMode .AND. .NOT. inAdExact ) THEN
              diffus(i,ki,1) = viscArNr(1)
              diffus(i,ki,2) = diffusKzS(i,kp1)
              diffus(i,ki,3) = diffusKzT(i,kp1)
            ELSE
# else /* ALLOW_AUTODIFF */
            IF ( .TRUE. ) THEN
# endif /* ALLOW_AUTODIFF */
              diffus(i,ki,1) = viscArNr(1) + fcon*difmcon + fRi*difm0
              diffus(i,ki,2) = diffusKzS(i,kp1)+fcon*difscon+fRi*difs0
              diffus(i,ki,3) = diffusKzT(i,kp1)+fcon*diftcon+fRi*dift0
            ENDIF
#endif /* EXCLUDE_KPP_SHEAR_MIX */
         ENDDO
      ENDDO

c ------------------------------------------------------------------------
c         set surface values to 0.0

      DO i = 1, imt
         diffus(i,0,1) = c0
         diffus(i,0,2) = c0
         diffus(i,0,3) = c0
      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END

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
      INTEGER myThid
      _RL v(imt,0:Nrp1)

#ifdef ALLOW_KPP

c local
      _RL zwork, zflag
      _RL KRi_range(1:Nrp1)
      INTEGER i, k, km1, kp1

      _RL         p0      , p25       , p5      , p2
      PARAMETER ( p0 = 0.0, p25 = 0.25, p5 = 0.5, p2 = 2.0 )

      KRi_range(Nrp1) = p0

#ifdef ALLOW_AUTODIFF_TAMC
C--   dummy assignment to end declaration part for TAF
      i = 0
C--   HPF directive to help TAF
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */

      DO i = 1, imt

         k = 1
         v(i,Nrp1) = v(i,Nr)

         DO k = 1, Nr
            KRi_range(k) = p5 + SIGN(p5,v(i,k))
            KRi_range(k) = KRi_range(k) *
     &                     ( p5 + SIGN(p5,(Riinfty-v(i,k))) )
         ENDDO

         zwork  = KRi_range(1) * v(i,1)
         v(i,1) = p2 * v(i,1) +
     &            KRi_range(1) * KRi_range(2) * v(i,2)
         zflag  = p2 + KRi_range(1) * KRi_range(2)
         v(i,1) = v(i,1) / zflag

         DO k = 2, Nr
            km1 = k - 1
            kp1 = k + 1
            zflag = v(i,k)
            v(i,k) = p2 * v(i,k) +
     &           KRi_range(k) * KRi_range(kp1) * v(i,kp1) +
     &           KRi_range(k) * zwork
            zwork = KRi_range(k) * zflag
            zflag = p2 + KRi_range(k)*(KRi_range(kp1)+KRi_range(km1))
            v(i,k) = v(i,k) / zflag
         ENDDO

      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END

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
      INTEGER k, bi, bj

c     input/output
c     fld    : 2-D array to be smoothed
      _RL fld( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )

#ifdef ALLOW_KPP

c     local
      INTEGER i, j, im1, ip1, jm1, jp1
      _RL tempVar
      _RL fld_tmp( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )

      INTEGER   iMin      , iMax          , jMin      , jMax
      PARAMETER(iMin=2-OLx, iMax=sNx+OLx-1, jMin=2-OLy, jMax=sNy+OLy-1)

      _RL        p0    , p5    , p25     , p125      , p0625
      PARAMETER( p0=0.0, p5=0.5, p25=0.25, p125=0.125, p0625=0.0625 )

      DO j = jMin, jMax
         jm1 = j-1
         jp1 = j+1
         DO i = iMin, iMax
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
      DO j = jMin, jMax
         DO i = iMin, iMax
            fld(i,j) = fld_tmp(i,j)
         ENDDO
      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END

c*************************************************************************

      subroutine blmix (
     I       ustar, bfsfc, hbl, stable, casea, diffus, kbl,
     O       dkm1, blmc, ghat, sigma, ikey,
     I       myThid )

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
      INTEGER myThid
      _RL ustar (imt)
      _RL bfsfc (imt)
      _RL hbl   (imt)
      _RL stable(imt)
      _RL casea (imt)
      _RL diffus(imt,0:Nrp1,mdiff)
      INTEGER kbl(imt)

c output
c     dkm1 (imt,mdiff)            boundary layer difs at kbl-1 level
c     blmc (imt,Nr,mdiff)         boundary layer mixing coefficients  (m^2/s)
c     ghat (imt,Nr)               nonlocal scalar transport
c     sigma(imt)                  normalized depth (d / hbl)
      _RL dkm1 (imt,mdiff)
      _RL blmc (imt,Nr,mdiff)
      _RL ghat (imt,Nr)
      _RL sigma(imt)
      INTEGER ikey

#ifdef ALLOW_KPP

c  local
c     gat1*(imt)                 shape function at sigma = 1
c     dat1*(imt)                 derivative of shape function at sigma = 1
c     ws(imt), wm(imt)           turbulent velocity scales             (m/s)
      _RL gat1m(imt), gat1s(imt), gat1t(imt)
      _RL dat1m(imt), dat1s(imt), dat1t(imt)
      _RL ws(imt), wm(imt)
      INTEGER i, kn, ki, kl
#ifndef KPP_DO_NOT_MATCH_DIFFUSIVITIES
# ifndef KPP_DO_NOT_MATCH_DERIVATIVES
      _RL R, dvdzup, dvdzdn
# endif
      _RL delhat
#endif
      _RL viscp, difsp, diftp, visch, difsh, difth
      _RL f1, sig, a1, a2, a3
      _RL Gm, Gs, Gt
      _RL tempVar

      _RL    p0    , eins
      PARAMETER (p0=0.0, eins=1.0)
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER kkey
#endif

c-----------------------------------------------------------------------
c compute velocity scales at hbl
c-----------------------------------------------------------------------

      DO i = 1, imt
         sigma(i) = stable(i) * 1.0 + (1. - stable(i)) * epsilon
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sigma = comlev1_kpp, key=ikey, kind=isbyte
#endif
      CALL wscale (
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wm = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE ws = comlev1_kpp, key=ikey, kind=isbyte
#endif

      DO i = 1, imt
         wm(i) = sign(eins,wm(i))*MAX(phepsi,ABS(wm(i)))
         ws(i) = sign(eins,ws(i))*MAX(phepsi,ABS(ws(i)))
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wm = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE ws = comlev1_kpp, key=ikey, kind=isbyte
#endif

      DO i = 1, imt

         kn = INT(caseA(i)+phepsi) *(kbl(i) -1) +
     $        (1 - INT(caseA(i)+phepsi)) * kbl(i)

c-----------------------------------------------------------------------
c find the interior viscosities and derivatives at hbl(i)
c-----------------------------------------------------------------------

C     initialise diffusivities (*h) and derivatives (*p)
#ifdef KPP_DO_NOT_MATCH_DIFFUSIVITIES
         visch  = 0.
         difsh  = 0.
         difth  = 0.
         viscp  = 0.
         difsp  = 0.
         diftp  = 0.
#else /* DO_MATCH_DIFFUSIVITIES */
# ifdef KPP_DO_NOT_MATCH_DERIVATIVES
         viscp  = 0.
         difsp  = 0.
         diftp  = 0.
         delhat = 0.
# else /*  DO_MATCH_DERIVATIVES */
         delhat = 0.5*hwide(kn) - zgrid(kn) - hbl(i)
         R      = 1.0 - delhat / hwide(kn)
         dvdzup = (diffus(i,kn-1,1) - diffus(i,kn  ,1)) / hwide(kn)
         dvdzdn = (diffus(i,kn  ,1) - diffus(i,kn+1,1)) / hwide(kn+1)
CML   This is the same as:
CML      viscp  = (1.-R) * MAX(dvdzup,0.) + R  * MAX(dvdzdn,0.)
CML   why do we need that here?
         viscp  = 0.5 * ( (1.-R) * (dvdzup + ABS(dvdzup)) +
     1                        R  * (dvdzdn + ABS(dvdzdn))  )

         dvdzup = (diffus(i,kn-1,2) - diffus(i,kn  ,2)) / hwide(kn)
         dvdzdn = (diffus(i,kn  ,2) - diffus(i,kn+1,2)) / hwide(kn+1)
         difsp  = 0.5 * ( (1.-R) * (dvdzup + ABS(dvdzup)) +
     1                        R  * (dvdzdn + ABS(dvdzdn))  )

         dvdzup = (diffus(i,kn-1,3) - diffus(i,kn  ,3)) / hwide(kn)
         dvdzdn = (diffus(i,kn  ,3) - diffus(i,kn+1,3)) / hwide(kn+1)
         diftp  = 0.5 * ( (1.-R) * (dvdzup + ABS(dvdzup)) +
     1                        R  * (dvdzdn + ABS(dvdzdn))  )
# endif /* KPP_DO_NOT_MATCH_DERIVATIVES */
         visch  = diffus(i,kn,1) + viscp * delhat
         difsh  = diffus(i,kn,2) + difsp * delhat
         difth  = diffus(i,kn,3) + diftp * delhat
#endif /* KPP_DO_NOT_MATCH_DIFFUSIVITIES */

         f1 = stable(i) * conc1 * bfsfc(i) /
#ifdef KPP_SMOOTH_REGULARISATION
     &        (ustar(i)**4 + phepsi)
#else
     &        MAX(ustar(i)**4,phepsi)
#endif
         gat1m(i) = visch / hbl(i) / wm(i)
         dat1m(i) = -viscp / wm(i) + f1 * visch

         gat1s(i) = difsh  / hbl(i) / ws(i)
         dat1s(i) = -difsp / ws(i) + f1 * difsh

         gat1t(i) = difth /  hbl(i) / ws(i)
         dat1t(i) = -diftp / ws(i) + f1 * difth

      ENDDO
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE gat1m = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE gat1s = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE gat1t = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE dat1m = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE dat1s = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE dat1t = comlev1_kpp, key=ikey, kind=isbyte
#endif
      DO i = 1, imt
         dat1m(i) = MIN(dat1m(i),p0)
         dat1s(i) = MIN(dat1s(i),p0)
         dat1t(i) = MIN(dat1t(i),p0)
      ENDDO
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE dat1m = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE dat1s = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE dat1t = comlev1_kpp, key=ikey, kind=isbyte
#endif

      DO ki = 1, Nr

#ifdef ALLOW_AUTODIFF_TAMC
         kkey = (ikey-1)*Nr + ki
#endif

c-----------------------------------------------------------------------
c     compute turbulent velocity scales on the interfaces
c-----------------------------------------------------------------------

         DO i = 1, imt
            sig      = (-zgrid(ki) + 0.5 * hwide(ki)) / hbl(i)
            sigma(i) = stable(i)*sig + (1.-stable(i))*MIN(sig,epsilon)
         ENDDO
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE wm = comlev1_kpp_k, key = kkey
CADJ STORE ws = comlev1_kpp_k, key = kkey
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sigma = comlev1_kpp_k, key = kkey
#endif
         CALL wscale (
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wm = comlev1_kpp_k, key = kkey
CADJ STORE ws = comlev1_kpp_k, key = kkey
#endif

c-----------------------------------------------------------------------
c     compute the dimensionless shape functions at the interfaces
c-----------------------------------------------------------------------

         DO i = 1, imt
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
#ifdef KPP_SMOOTH_REGULARISATION
            ghat(i,ki) = (1.-stable(i)) * cg / (phepsi+tempVar)
#else
            ghat(i,ki) = (1.-stable(i)) * cg / MAX(phepsi,tempVar)
#endif
         ENDDO
      ENDDO

c-----------------------------------------------------------------------
c find diffusivities at kbl-1 grid level
c-----------------------------------------------------------------------

      DO i = 1, imt
         kl = kbl(i)
         sig      = -zgrid(kl-1) / hbl(i)
         sigma(i) = stable(i) * sig
     &            + (1. - stable(i)) * MIN(sig,epsilon)
      ENDDO

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE wm = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE ws = comlev1_kpp, key=ikey, kind=isbyte
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sigma = comlev1_kpp, key=ikey, kind=isbyte
#endif
      CALL wscale (
     I        sigma, hbl, ustar, bfsfc,
     O        wm, ws, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wm = comlev1_kpp, key=ikey, kind=isbyte
CADJ STORE ws = comlev1_kpp, key=ikey, kind=isbyte
#endif

      DO i = 1, imt
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
      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END

c*************************************************************************

      subroutine enhance (
     I       dkm1, hbl, kbl, diffus, casea,
     U       ghat,
     O       blmc,
     &       myThid )

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
      INTEGER   myThid
      _RL dkm1  (imt,mdiff)
      _RL hbl   (imt)
      INTEGER kbl   (imt)
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
      INTEGER ki, i, md
      _RL dkmp5, dstar

      DO i = 1, imt
         ki = kbl(i)-1
         IF ((ki .ge. 1) .AND. (ki .LT. Nr)) THEN
            delta = (hbl(i) + zgrid(ki)) / (zgrid(ki) - zgrid(ki+1))
            DO md = 1, mdiff
               dkmp5         =      casea(i)  * diffus(i,ki,md) +
     1                         (1.- casea(i)) * blmc  (i,ki,md)
C     I think that this is meant here, but I cannot be sure because there
C     there is no reference for this. In MOM6/CVMix, the square is outside
C     of the parentheses
CML               dstar         = (1.- delta**2) * dkm1(i,md)
               dstar         = (1.- delta)**2 * dkm1(i,md)
     &                       + delta**2 * dkmp5
               blmc(i,ki,md) = (1.- delta)*diffus(i,ki,md)
     &                       + delta*dstar
            ENDDO
            ghat(i,ki) = (1.- casea(i)) * ghat(i,ki)
         ENDIF
      ENDDO

#endif /* ALLOW_KPP */

      RETURN
      END

c*************************************************************************

      SUBROUTINE STATEKPP (
     O     RHO1, DBLOC, DBSFC, TTALPHA, SSBETA,
     I     ikey, bi, bj, myThid )
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

      INTEGER i, j, k
      INTEGER ikey, kkey

c calculate density, alpha, beta in surface layer, and set dbsfc to zero

      kkey = (ikey-1)*Nr + 1

      k = 1
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,k,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */
      CALL FIND_RHO_2D(
     I     1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I     theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O     WORK1,
     I     k, bi, bj, myThid )
#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,k,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */

      CALL FIND_ALPHA(
     I     bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1, 1,
     O     WORK2, myThid )

      CALL FIND_BETA(
     I     bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1, 1,
     O     WORK3, myThid )

      DO j = 1-OLy, sNy+OLy
       DO i = 1-OLx, sNx+OLx
        RHO1(i,j)      = WORK1(i,j) + rhoConst
        TTALPHA(i,j,1) = WORK2(i,j)
        SSBETA(i,j,1)  = WORK3(i,j)
        DBSFC(i,j,1)   = 0.
       ENDDO
      ENDDO

c calculate alpha, beta, and gradients in interior layers

CHPF$  INDEPENDENT, NEW (RHOK,RHOKM1,RHO1K,WORK1,WORK2)
      DO k = 2, Nr

         kkey = (ikey-1)*Nr + k

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,k,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        RHOK,
     I        k, bi, bj, myThid )

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,k-1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
CADJ STORE salt (:,:,k-1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I        theta(1-OLx,1-OLy,k-1,bi,bj),salt(1-OLx,1-OLy,k-1,bi,bj),
     O        RHOKM1,
     I        k-1, bi, bj, myThid )

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE theta(:,:,1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
CADJ STORE salt (:,:,1,bi,bj) = comlev1_kpp_k,
CADJ &     key=kkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I        theta(1-OLx,1-OLy,1,bi,bj), salt(1-OLx,1-OLy,1,bi,bj),
     O        RHO1K,
     I        1, bi, bj, myThid )

#ifdef KPP_AUTODIFF_MORE_STORE
CADJ STORE rhok  (:,:) = comlev1_kpp_k, key=kkey, kind=isbyte
CADJ STORE rhokm1(:,:) = comlev1_kpp_k, key=kkey, kind=isbyte
CADJ STORE rho1k (:,:) = comlev1_kpp_k, key=kkey, kind=isbyte
#endif /* KPP_AUTODIFF_MORE_STORE */

         CALL FIND_ALPHA(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k, k,
     O        WORK1, myThid )

         CALL FIND_BETA(
     I        bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k, k,
     O        WORK2, myThid )

         DO j = 1-OLy, sNy+OLy
            DO i = 1-OLx, sNx+OLx
               TTALPHA(i,j,k) = WORK1 (i,j)
               SSBETA(i,j,k)  = WORK2 (i,j)
               DBLOC(i,j,k-1) = gravity * (RHOK(i,j) - RHOKM1(i,j)) /
     &                                    (RHOK(i,j) + rhoConst)
               DBSFC(i,j,k)   = gravity * (RHOK(i,j) - RHO1K (i,j)) /
     &                                    (RHOK(i,j) + rhoConst)
            ENDDO
         ENDDO

      ENDDO

c     compute arrays for k = Nrp1
      DO j = 1-OLy, sNy+OLy
       DO i = 1-OLx, sNx+OLx
        TTALPHA(i,j,Nrp1) = TTALPHA(i,j,Nr)
        SSBETA(i,j,Nrp1)  = SSBETA(i,j,Nr)
        DBLOC(i,j,Nr)     = 0.
       ENDDO
      ENDDO

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
     I     ikey, iMin, iMax, jMin, jMax, bi, bj, myThid )
c
c-----------------------------------------------------------------------
c     "KPP_DOUBLEDIFF" adds the double diffusive contributions
C     as Rrho-dependent parameterizations to kappaRT and kappaRS
c
c     input:
c     bi, bj  = array indices on which to apply calculations
c     iMin, iMax, jMin, jMax = array boundaries
c     ikey = key for TAF automatic differentiation
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
      INTEGER ikey, iMin, iMax, jMin, jMax, bi, bj, myThid

      _RL TTALPHA( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1 )
      _RL SSBETA ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1 )
      _RL KappaRT( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr   )
      _RL KappaRS( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr   )

#ifdef ALLOW_KPP

C--------------------------------------------------------------------------
C
C     local variables
C     i,j,k :: loop indices
C     kkey  :: key for TAF automatic differentiation
C
      INTEGER i, j, k
      INTEGER kkey
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
      kkey = (ikey-1)*Nr + 1

CML#ifdef KPP_AUTODIFF_MORE_STORE
CMLCADJ STORE theta(:,:,1,bi,bj) = comlev1_kpp_k,
CMLCADJ &     key=kkey, kind=isbyte
CMLCADJ STORE salt (:,:,1,bi,bj) = comlev1_kpp_k,
CMLCADJ &     key=kkey, kind=isbyte
CML#endif /* KPP_AUTODIFF_MORE_STORE */

      DO k = 1, Nr
       Km1 = MAX(k-1,1)
       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
         alphaDT(i,j) = ( theta(i,j,km1,bi,bj)-theta(i,j,k,bi,bj) )
     &        * 0.5 _d 0 * ABS( TTALPHA(i,j,km1) + TTALPHA(i,j,k) )
         betaDS(i,j)  = ( salt(i,j,km1,bi,bj)-salt(i,j,k,bi,bj) )
     &        * 0.5 _d 0 * ( SSBETA(i,j,km1) + SSBETA(i,j,k) )
         nuddt(i,j) = 0. _d 0
         nudds(i,j) = 0. _d 0
        ENDDO
       ENDDO
       IF ( k .GT. 1 ) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          Rrho  = 0. _d 0
C     Now we have many different cases
C     a. alphaDT > 0 and betaDS > 0 => salt fingering
C        (salinity destabilizes)
          IF (      alphaDT(i,j) .GT. betaDS(i,j)
     &         .AND. betaDS(i,j) .GT. 0. _d 0 ) THEN
           Rrho = MIN( alphaDT(i,j)/betaDS(i,j), Rrho0 )
C     Large et al. 1994, eq. 31a
C          nudds(i,j) = dsfmax * ( 1. _d 0 - (Rrho - 1. _d 0) * rFac )**3
           nutmp      =          ( 1. _d 0 - (Rrho - 1. _d 0) * rFac )
           nudds(i,j) = dsfmax * nutmp * nutmp * nutmp
C     Large et al. 1994, eq. 31c
           nuddt(i,j) = 0.7 _d 0 * nudds(i,j)
          ELSEIF (   alphaDT(i,j) .LT. 0. _d 0
     &          .AND. betaDS(i,j) .LT. 0. _d 0
     &          .AND.alphaDT(i,j) .GT. betaDS(i,j) ) THEN
C     b. alphaDT < 0 and betaDS < 0 => semi-convection, diffusive convection
C        (temperature destabilizes)
C     for Rrho >= 1 the water column is statically unstable and we never
C     reach this point
           Rrho = alphaDT(i,j)/betaDS(i,j)
C     Large et al. 1994, eq. 32
           nuddt(i,j) = numol * 0.909 _d 0
     &          * exp ( 4.6 _d 0 * exp (
     &          - 5.4 _d 0 * ( 1. _d 0/Rrho - 1. _d 0 ) ) )
CMLC     or
CMLC     Large et al. 1994, eq. 33
CML         nuddt(i,j) = numol * 8.7 _d 0 * Rrho**1.1
C     Large et al. 1994, eqs. 34
           nudds(i,j) = nuddt(i,j) * MAX( 0.15 _d 0 * Rrho,
     &          1.85 _d 0 * Rrho - 0.85 _d 0 )
          ELSE
C     Do nothing, because in this case the water colume is unstable
C     => double diffusive processes are negligible and mixing due
C     to shear instability will dominate
          ENDIF
         ENDDO
        ENDDO
C     ENDIF ( k .GT. 1 )
       ENDIF
C
       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
         kappaRT(i,j,k) = kappaRT(i,j,k) + nuddt(i,j)
         kappaRS(i,j,k) = kappaRS(i,j,k) + nudds(i,j)
        ENDDO
       ENDDO
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(nuddt,'KPPnuddt',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(nudds,'KPPnudds',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
C     end of k-loop
      ENDDO
#endif /* ALLOW_KPP */

      RETURN
      END
