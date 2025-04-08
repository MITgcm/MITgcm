#include "KPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME_OPTIONS.h"
#endif

C-- File kpp_calc.F:
C--  Contents
C--  o KPP_CALC         - Main KPP interface routine.
C--  o KPP_CALC_DUMMY   - Dummy routine for TAF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: KPP_CALC

C !INTERFACE: ==========================================================
      SUBROUTINE KPP_CALC(
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KPP_CALC                                      |
C     | o Compute all KPP fields defined in KPP.h                |
C     *==========================================================*
C     | This subroutine serves as an interface between MITGCMUV  |
C     | code and NCOM 1-D routines in kpp_routines.F             |
C     *==========================================================*

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
c     this subroutine provides the interface between the MITGCM and
c     the routine "kppmix", where boundary layer depth, vertical
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
c     KPPplumefrac- Fraction of saltplume (flux) penetrating mixing layer

c--   KPP_CALC computes vertical viscosity and diffusivity for region
c     (-2:sNx+3,-2:sNy+3) as required by CALC_DIFFUSIVITY and requires
c     values of uVel, vVel, surfaceForcingU, surfaceForcingV in the
c     region (-2:sNx+4,-2:sNy+4).
c     Hence overlap region needs to be set OLx=4, OLy=4.
c \ev

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME.h"
#endif /* ALLOW_SALT_PLUME */
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C !INPUT PARAMETERS: ===================================================
c Routine arguments
c     bi, bj :: Current tile indices
c     myTime :: Current time in simulation
c     myIter :: Current iteration number in simulation
c     myThid :: My Thread Id. number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_KPP
C !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C !LOCAL VARIABLES: ====================================================
c Local constants
c     minusone, p0, p5, p25, p125, p0625
c     iMin, iMax, jMin, jMax  - array computation indices
      _RL        minusone
      PARAMETER( minusone=-1.0)
      _RL        p0    , p5    , p25     , p125      , p0625
      PARAMETER( p0=0.0, p5=0.5, p25=0.25, p125=0.125, p0625=0.0625 )
      INTEGER   iMin      ,iMax          ,jMin      ,jMax
      PARAMETER(iMin=2-OLx,iMax=sNx+OLx-1,jMin=2-OLy,jMax=sNy+OLy-1)

c Local arrays and variables
c     work?  (nx,ny)       - horizontal working arrays
c     temp?  (nx,ny,Nr)    - 3d working arrays
c     ustar  (nx,ny)       - surface friction velocity                  (m/s)
c     bo     (nx,ny)       - surface turbulent buoyancy forcing     (m^2/s^3)
c     bosol  (nx,ny)       - surface radiative buoyancy forcing     (m^2/s^3)
c     boplume(nx,ny,Nrp1)  - surface haline buoyancy forcing        (m^2/s^3)
c     shsq   (nx,ny,Nr)    - local velocity shear squared
c                            at interfaces for ri_iwmix             (m^2/s^2)
c     dVsq   (nx,ny,Nr)    - velocity shear re surface squared
c                            at grid levels for bldepth             (m^2/s^2)
c     dbloc  (nx,ny,Nr)    - local delta buoyancy at interfaces
c                            for ri_iwmix and bldepth                 (m/s^2)
c     Ritop  (nx,ny,Nr)    - numerator of bulk richardson number
c                            at grid levels for bldepth
c     vddiff (nx,ny,Nrp2,1)- vertical viscosity on "t-grid"           (m^2/s)
c     vddiff (nx,ny,Nrp2,2)- vert. diff. on next row for salt&tracers (m^2/s)
c     vddiff (nx,ny,Nrp2,3)- vert. diff. on next row for temperature  (m^2/s)
c     ghat   (nx,ny,Nr)    - nonlocal transport coefficient           (s/m^2)
c     hbl    (nx,ny)       - mixing layer depth                           (m)
c     kmtj   (nx,ny)       - maximum number of wet levels in each column
      INTEGER work1 ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy            )
      _RL worka ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL work2 ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL ustar ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL bo    ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
      _RL bosol ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
#ifdef ALLOW_SALT_PLUME
      _RL temp1   ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr          )
      _RL temp2   ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr          )
      _RL boplume ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1        )
#ifdef SALT_PLUME_SPLIT_BASIN
      _RL lon ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                  )
      _RL lat ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                  )
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
      _RL shsq  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr            )
      _RL dVsq  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr            )
      _RL dbloc ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr            )
      _RL Ritop ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr            )
      _RL vddiff( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, 0:Nrp1, mdiff )
      _RL ghat  ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr            )
      _RL hbl   ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy                )
cph(
      _RL TTALPHA( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1 )
      _RL SSBETA ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nrp1 )
cph)
      integer i, j, kSurf, k, kp1, km1, im1, ip1, jm1, jp1
C     ikey :: tape key (depends on tiles)
      integer ikey
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      ikey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#else /* ALLOW_AUTODIFF_TAMC */
      ikey = 0
#endif /* ALLOW_AUTODIFF_TAMC */

c     Check to see if new vertical mixing coefficient should be computed now?
      IF ( DIFFERENT_MULTIPLE(kpp_freq,myTime,deltaTClock)
     1     .OR. myTime .EQ. startTime ) THEN

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
c                d(rho{k,k})/d(S(k))                    ((kg/m^3)/(g/kg))
c------------------------------------------------------------------------

      CALL STATEKPP(
     O     work2, dbloc, Ritop,
     O     TTALPHA, SSBETA,
     I     ikey, bi, bj, myThid )

      DO k = 1, Nr
       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
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
       CALL SMOOTH_HORIZ (
     I      k+1, bi, bj,
     U      ghat (1-OLx,1-OLy,k),
     I      myThid )
      ENDDO

#endif /* KPP_SMOOTH_DBLOC */

#ifdef KPP_SMOOTH_DENS
c     horizontally smooth density related quantities with 121 filters
      CALL SMOOTH_HORIZ (
     I     1, bi, bj,
     U     work2,
     I     myThid )
      DO k = 1, Nr
         CALL SMOOTH_HORIZ (
     I        k+1, bi, bj,
     U        dbloc (1-OLx,1-OLy,k),
     I        myThid )
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        Ritop (1-OLx,1-OLy,k),
     I        myThid )
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        TTALPHA(1-OLx,1-OLy,k),
     I        myThid )
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        SSBETA(1-OLx,1-OLy,k),
     I        myThid )
      ENDDO
#endif /* KPP_SMOOTH_DENS */

c     surface index
      kSurf = 1
      DO k = 1, Nr
       km1 = max(1,k-1)
       kp1 = min(Nr,k+1)
       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx

c     zero out dbloc over land points (so that the convective
c     part of the interior mixing can be diagnosed).
c     ML: Note that dbloc and ghat (=smoothed dbloc) are defined at
c     the bottom of the cell rather than the (MITgcm-standard) top
c     of the cell; therefore the proper masking is maskC(k)*maskC(k+1)
         dbloc(i,j,k) = dbloc(i,j,k) * maskC(i,j,k,bi,bj)
     &        * maskC(i,j,kp1,bi,bj)
         ghat(i,j,k)  = ghat(i,j,k)  * maskC(i,j,k,bi,bj)
     &        * maskC(i,j,kp1,bi,bj)
#ifdef ALLOW_SHELFICE
         IF ( useShelfIce ) kSurf = MAX(1,kTopC(i,j,bi,bj))
#endif /* ALLOW_SHELFICE */
         Ritop(i,j,k) = Ritop(i,j,k) * maskC(i,j,k,bi,bj)
     &        * maskC(i,j,kSurf,bi,bj)
         IF(k.EQ.nzmax(i,j,bi,bj)) THEN
          dbloc(i,j,k) = p0
          ghat(i,j,k)  = p0
          Ritop(i,j,k) = p0
         ENDIF

c     numerator of bulk richardson number on grid levels
c     note: land and ocean bottom values need to be set to zero
c     so that the subroutine "bldepth" works correctly
         Ritop(i,j,k) = (zgrid(1)-zgrid(k)) * Ritop(i,j,k)

        ENDDO
       ENDDO
      ENDDO

cph(
cph  this avoids a single or double recomp./call of statekpp
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE work2              = comlev1_kpp, key = ikey
#ifdef KPP_AUTODIFF_EXCESSIVE_STORE
CADJ STORE dbloc, Ritop, ghat = comlev1_kpp, key = ikey
CADJ STORE vddiff             = comlev1_kpp, key = ikey
CADJ STORE TTALPHA, SSBETA    = comlev1_kpp, key = ikey
#endif
#endif /* ALLOW_AUTODIFF_TAMC */
cph)

CML#ifdef ALLOW_SHELFICE
CMLC     For the pbl parameterisation to work underneath the ice shelves
CMLC     it needs to know the surface (ice-ocean) fluxes. However, masking
CMLC     and indexing problems make this part of the code not work
CMLC     underneath the ice shelves and the following lines are only here
CMLC     to remind me that this still needs to be sorted out.
CML      shelfIceFac = 0. _d 0
CML      IF ( useShelfIce ) selfIceFac = 1. _d 0
CML      DO j = jMin, jMax
CML       DO i = iMin, iMax
CML        surfForcT = surfaceForcingT(i,j,bi,bj)
CML     &       + shelficeForcingT(i,j,bi,bj) * shelfIceFac
CML        surfForcS = surfaceForcingS(i,j,bi,bj)
CML     &       + shelficeForcingS(i,j,bi,bj) * shelfIceFac
CML       ENDDO
CML      ENDDO
CML#endif /* ALLOW_SHELFICE */

c------------------------------------------------------------------------
c     friction velocity, turbulent and radiative surface buoyancy forcing
c     -------------------------------------------------------------------
c     taux / rho = surfaceForcingU                               (N/m^2)
c     tauy / rho = surfaceForcingV                               (N/m^2)
c     ustar = sqrt( sqrt( taux^2 + tauy^2 ) / rho )                (m/s)
c     bo    = - g * ( alpha*surfaceForcingT +
c                     beta *surfaceForcingS ) / rho            (m^2/s^3)
c     bosol = - g * alpha * Qsw * drF(1) / rho                 (m^2/s^3)
c     boplume = g * (beta * saltPlumeFlux/rhoConst ) /rho      (m^2/s^3)
c             = g * (beta * SPforcingS   /rhoConst ) /rho
c              +g * (alpha* SPforcingT   / ??
c------------------------------------------------------------------------
c     velocity shear
c     --------------
c     Get velocity shear squared, averaged from "u,v-grid"
c     onto "t-grid" (in (m/s)**2):
c     dVsq(k)=(Uref-U(k))**2+(Vref-V(k))**2      at grid levels
c     shsq(k)=(U(k)-U(k+1))**2+(V(k)-V(k+1))**2  at interfaces
c
c     note: Vref can depend on the surface fluxes that is why we compute
c     dVsq in the subroutine that does the surface related stuff
c     (admittedly this is a bit messy)
c------------------------------------------------------------------------

#ifdef ALLOW_SALT_PLUME
      DO j=jMin,jMax
       DO i=iMin,iMax
#ifndef SALT_PLUME_VOLUME
        temp1(i,j,1) = saltPlumeFlux(i,j,bi,bj)
        temp2(i,j,1) = 0. _d 0
        DO k=2,Nr
         temp1(i,j,k) = 0. _d 0
         temp2(i,j,k) = 0. _d 0
        ENDDO
#else /* def SALT_PLUME_VOLUME */
        DO k=1,Nr
         temp1(i,j,k) = SPforcingS(i,j,k,bi,bj)
         temp2(i,j,k) = SPforcingT(i,j,k,bi,bj)
        ENDDO
#endif /* SALT_PLUME_VOLUME */
       ENDDO
      ENDDO
#endif /* ALLOW_SALT_PLUME */

      CALL KPP_FORCING_SURF(
     I     work2, surfaceForcingU, surfaceForcingV,
     I     surfaceForcingT, surfaceForcingS, adjustColdSST_diag,
     I     Qsw,
#ifdef KPP_ESTIMATE_UREF
     I     dbloc,
#endif /* KPP_ESTIMATE_UREF */
#ifdef ALLOW_SALT_PLUME
     I     temp1, temp2,
#endif /* ALLOW_SALT_PLUME */
     I     ttalpha, ssbeta,
     O     ustar, bo, bosol,
#ifdef ALLOW_SALT_PLUME
     O     boplume,
#endif /* ALLOW_SALT_PLUME */
     O     dVsq,
     I     ikey, iMin, iMax, jMin, jMax, bi, bj, myTime, myThid )

CMLcph(
CML#ifdef ALLOW_AUTODIFF_TAMC
CMLCADJ STORE ustar = comlev1_kpp, key = ikey
CML#endif
CMLcph)

c initialize arrays to zero
      DO k = 1, Nr
       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
         shsq(i,j,k) = p0
        ENDDO
       ENDDO
      ENDDO

c     shsq computation
      DO k = 1, Nrm1
       kp1 = k + 1
       DO j = jMin, jMax
        jm1 = j - 1
        jp1 = j + 1
        DO i = iMin, iMax
         im1 = i - 1
         ip1 = i + 1
         shsq(i,j,k) = p5 * (
     &        (uVel(i,  j,  k,bi,bj)-uVel(i,  j,  kp1,bi,bj)) *
     &        (uVel(i,  j,  k,bi,bj)-uVel(i,  j,  kp1,bi,bj)) +
     &        (uVel(ip1,j,  k,bi,bj)-uVel(ip1,j,  kp1,bi,bj)) *
     &        (uVel(ip1,j,  k,bi,bj)-uVel(ip1,j,  kp1,bi,bj)) +
     &        (vVel(i,  j,  k,bi,bj)-vVel(i,  j,  kp1,bi,bj)) *
     &        (vVel(i,  j,  k,bi,bj)-vVel(i,  j,  kp1,bi,bj)) +
     &        (vVel(i,  jp1,k,bi,bj)-vVel(i,  jp1,kp1,bi,bj)) *
     &        (vVel(i,  jp1,k,bi,bj)-vVel(i,  jp1,kp1,bi,bj)) )
#ifdef KPP_SMOOTH_SHSQ
         shsq(i,j,k) = p5 * shsq(i,j,k) + p125 * (
     &        (uVel(i,  jm1,k,bi,bj)-uVel(i,  jm1,kp1,bi,bj)) *
     &        (uVel(i,  jm1,k,bi,bj)-uVel(i,  jm1,kp1,bi,bj)) +
     &        (uVel(ip1,jm1,k,bi,bj)-uVel(ip1,jm1,kp1,bi,bj)) *
     &        (uVel(ip1,jm1,k,bi,bj)-uVel(ip1,jm1,kp1,bi,bj)) +
     &        (uVel(i,  jp1,k,bi,bj)-uVel(i,  jp1,kp1,bi,bj)) *
     &        (uVel(i,  jp1,k,bi,bj)-uVel(i,  jp1,kp1,bi,bj)) +
     &        (uVel(ip1,jp1,k,bi,bj)-uVel(ip1,jp1,kp1,bi,bj)) *
     &        (uVel(ip1,jp1,k,bi,bj)-uVel(ip1,jp1,kp1,bi,bj)) +
     &        (vVel(im1,j,  k,bi,bj)-vVel(im1,j,  kp1,bi,bj)) *
     &        (vVel(im1,j,  k,bi,bj)-vVel(im1,j,  kp1,bi,bj)) +
     &        (vVel(im1,jp1,k,bi,bj)-vVel(im1,jp1,kp1,bi,bj)) *
     &        (vVel(im1,jp1,k,bi,bj)-vVel(im1,jp1,kp1,bi,bj)) +
     &        (vVel(ip1,j,  k,bi,bj)-vVel(ip1,j,  kp1,bi,bj)) *
     &        (vVel(ip1,j,  k,bi,bj)-vVel(ip1,j,  kp1,bi,bj)) +
     &        (vVel(ip1,jp1,k,bi,bj)-vVel(ip1,jp1,kp1,bi,bj)) *
     &        (vVel(ip1,jp1,k,bi,bj)-vVel(ip1,jp1,kp1,bi,bj)) )
#endif
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(shsq,  'KPPshsq ',0,Nr,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
cph(
#ifdef ALLOW_AUTODIFF_TAMC
#ifdef KPP_AUTODIFF_EXCESSIVE_STORE
CADJ STORE dvsq, shsq = comlev1_kpp, key = ikey
#endif
#endif /* ALLOW_AUTODIFF_TAMC */
cph)

c-----------------------------------------------------------------------
c     solve for viscosity, diffusivity, ghat, and hbl on "t-grid"
c-----------------------------------------------------------------------

#if ( !defined EXCLUDE_PCELL_MIX_CODE && defined  ALLOW_AUTODIFF_TAMC )
CADJ STORE KPPdiffKzS(:,:,:,bi,bj) = comlev1_kpp, key = ikey
CADJ STORE KPPdiffKzT(:,:,:,bi,bj) = comlev1_kpp, key = ikey
#endif
c     precompute background vertical diffusivities, which are needed for
c     matching diffusivities at bottom of KPP PBL
      CALL CALC_3D_DIFFUSIVITY(
     I        bi,bj,1-OLx,sNx+OLx,1-OLy,sNy+OLy,
     I        GAD_SALINITY, .FALSE., .FALSE.,
     O        KPPdiffKzS(1-OLx,1-OLy,1,bi,bj),
     I        myThid)
      CALL CALC_3D_DIFFUSIVITY(
     I        bi,bj,1-OLx,sNx+OLx,1-OLy,sNy+OLy,
     I        GAD_TEMPERATURE, .FALSE., .FALSE.,
     O        KPPdiffKzT(1-OLx,1-OLy,1,bi,bj),
     I        myThid)
#ifndef EXCLUDE_KPP_DOUBLEDIFF
      IF ( KPPuseDoubleDiff ) THEN
C     Add the contribution of double diffusive effects (salt fingering
C     and diffusive convection) here. It would be more logical to add
C     them right after Ri_iwmix within kppmix, but ttalpha, ssbeta, theta
C     and salt are not passed to kppmix and are thus not available there.
       CALL KPP_DOUBLEDIFF(
     I      TTALPHA, SSBETA,
     U      KPPdiffKzT(1-OLx,1-OLy,1,bi,bj),
     U      KPPdiffKzS(1-OLx,1-OLy,1,bi,bj),
     I      ikey,1-OLx,sNx+OLx,1-OLy,sNy+OLy,bi,bj,myThid)
      ENDIF
#endif /* ndef EXCLUDE_KPP_DOUBLEDIFF */

      DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
            work1(i,j) = nzmax(i,j,bi,bj)
            work2(i,j) = Fcori(i,j,bi,bj)
         ENDDO
      ENDDO
      CALL KPPMIX (
     I       work1, shsq, dVsq, ustar,
     I       maskC(1-OLx,1-OLy,1,bi,bj),
     I       bo, bosol,
#ifdef ALLOW_SALT_PLUME
     I       boplume, SaltPlumeDepth(1-OLx,1-OLy,bi,bj),
#ifdef SALT_PLUME_SPLIT_BASIN
     I       XC(1-OLx,1-OLy,bi,bj), YC(1-OLx,1-OLy,bi,bj),
#endif /* SALT_PLUME_SPLIT_BASIN */
#endif /* ALLOW_SALT_PLUME */
     I       dbloc, Ritop, work2,
     I       KPPdiffKzS(1-OLx,1-OLy,1,bi,bj),
     I       KPPdiffKzT(1-OLx,1-OLy,1,bi,bj),
     I       ikey,
     O       vddiff,
     U       ghat,
     O       hbl,
     I       bi, bj, myTime, myIter, myThid )

c-----------------------------------------------------------------------
c     zero out land values and transfer to global variables
c-----------------------------------------------------------------------

      DO k = 1, Nr
       km1 = max(1,k-1)
       DO j = jMin, jMax
        DO i = iMin, iMax
         KPPviscAz(i,j,k,bi,bj) = vddiff(i,j,k-1,1) * maskC(i,j,k,bi,bj)
     &        * maskC(i,j,km1,bi,bj)
         KPPdiffKzS(i,j,k,bi,bj)= vddiff(i,j,k-1,2) * maskC(i,j,k,bi,bj)
     &        * maskC(i,j,km1,bi,bj)
         KPPdiffKzT(i,j,k,bi,bj)= vddiff(i,j,k-1,3) * maskC(i,j,k,bi,bj)
     &        * maskC(i,j,km1,bi,bj)
         KPPghat(i,j,k,bi,bj)   = ghat(i,j,k)       * maskC(i,j,k,bi,bj)
     &        * maskC(i,j,km1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      kSurf = 1
      DO j = jMin, jMax
       DO i = iMin, iMax
#ifdef ALLOW_SHELFICE
        IF ( useShelfIce ) kSurf = MAX(1,kTopC(i,j,bi,bj))
#endif /* ALLOW_SHELFICE */
        KPPhbl(i,j,bi,bj) = hbl(i,j) * maskC(i,j,kSurf,bi,bj)
       ENDDO
      ENDDO

#ifdef KPP_SMOOTH_VISC
c     horizontal smoothing of vertical viscosity
      DO k = 1, Nr
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        KPPviscAz(1-OLx,1-OLy,k,bi,bj),
     I        myThid )
      ENDDO
C jmc: No EXCH inside bi,bj loop !!!
c     _EXCH_XYZ_RL(KPPviscAz  , myThid )
#endif /* KPP_SMOOTH_VISC */

#ifdef KPP_SMOOTH_DIFF
c     horizontal smoothing of vertical diffusivity
      DO k = 1, Nr
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        KPPdiffKzS(1-OLx,1-OLy,k,bi,bj),
     I        myThid )
         CALL SMOOTH_HORIZ (
     I        k, bi, bj,
     U        KPPdiffKzT(1-OLx,1-OLy,k,bi,bj),
     I        myThid )
      ENDDO
#endif /* KPP_SMOOTH_DIFF */

cph(
cph  crucial: this avoids full recomp./call of kppmix
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KPPhbl = comlev1_kpp, key = ikey
#endif /* ALLOW_AUTODIFF_TAMC */
cph)

C     Compute fraction of solar short-wave flux penetrating to
C     the bottom of the mixing layer.
      DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            worka(i,j) = KPPhbl(i,j,bi,bj)
         ENDDO
      ENDDO
      CALL SWFRAC(
     I     (sNx+2*OLx)*(sNy+2*OLy), minusone,
     U     worka,
     I     myTime, myIter, myThid )
      DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            KPPfrac(i,j,bi,bj) = worka(i,j)
         ENDDO
      ENDDO

#ifdef ALLOW_SALT_PLUME
C     Compute fraction of saltplume (flux) penetrating to
C     the bottom of the mixing layer.
      IF ( useSALT_PLUME ) THEN
#ifndef SALT_PLUME_VOLUME
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         work2(i,j) = SaltPlumeDepth(i,j,bi,bj)
         worka(i,j) = KPPhbl(i,j,bi,bj)
#ifdef SALT_PLUME_SPLIT_BASIN
         lon(i,j) = XC(i,j,bi,bj)
         lat(i,j) = YC(i,j,bi,bj)
#endif /* SALT_PLUME_SPLIT_BASIN */
        ENDDO
       ENDDO
       CALL SALT_PLUME_FRAC(
     I      (sNx+2*OLx)*(sNy+2*OLy), minusone, work2,
#ifdef SALT_PLUME_SPLIT_BASIN
     I      lon,lat,
#endif /* SALT_PLUME_SPLIT_BASIN */
     U      worka,
     I      myTime, myIter, myThid )
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         KPPplumefrac(i,j,bi,bj) = 1. _d 0 - worka(i,j)
        ENDDO
       ENDDO
#else /* SALT_PLUME_VOLUME */
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         KPPplumefrac(i,j,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
#endif /* SALT_PLUME_VOLUME */
      ENDIF
#endif /* ALLOW_SALT_PLUME */

      ENDIF

#endif /* ALLOW_KPP */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: KPP_CALC_DUMMY

C !INTERFACE: ==========================================================
      SUBROUTINE KPP_CALC_DUMMY(
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KPP_CALC_DUMMY                                |
C     | o Compute all KPP fields defined in KPP.h                |
C     | o Dummy routine for TAF                                  |
C     *==========================================================*

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C !INPUT PARAMETERS: ===================================================
c     bi, bj :: Current tile indices
c     myTime :: Current time in simulation
c     myIter :: Current iteration number in simulation
c     myThid :: My Thread Id. number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_KPP
C !LOCAL VARIABLES: ====================================================
      INTEGER i, j, k
#if ( !defined EXCLUDE_PCELL_MIX_CODE && defined  ALLOW_AUTODIFF_TAMC )
      INTEGER ikey
#endif
CEOP

      DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            KPPhbl (i,j,bi,bj) = 1.0
            KPPfrac(i,j,bi,bj) = 0.0
#ifdef ALLOW_SALT_PLUME
            KPPplumefrac(i,j,bi,bj) = 0.0
#endif /* ALLOW_SALT_PLUME */
            DO k = 1,Nr
               KPPghat   (i,j,k,bi,bj) = 0.0
               KPPviscAz (i,j,k,bi,bj) = viscArNr(1)
            ENDDO
         ENDDO
      ENDDO

#if ( !defined EXCLUDE_PCELL_MIX_CODE && defined  ALLOW_AUTODIFF_TAMC )
      ikey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
CADJ STORE KPPdiffKzS(:,:,:,bi,bj) = comlev1_kpp, key = ikey
CADJ STORE KPPdiffKzT(:,:,:,bi,bj) = comlev1_kpp, key = ikey
#endif
      CALL CALC_3D_DIFFUSIVITY(
     I     bi,bj,1-OLx,sNx+OLx,1-OLy,sNy+OLy,
     I     GAD_SALINITY, .FALSE., .FALSE.,
     O     KPPdiffKzS(1-OLx,1-OLy,1,bi,bj),
     I     myThid)
      CALL CALC_3D_DIFFUSIVITY(
     I     bi,bj,1-OLx,sNx+OLx,1-OLy,sNy+OLy,
     I     GAD_TEMPERATURE, .FALSE., .FALSE.,
     O     KPPdiffKzT(1-OLx,1-OLy,1,bi,bj),
     I     myThid)

#endif
      RETURN
      END
