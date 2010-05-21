C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_bulkformulae.F,v 1.22 2010/05/21 07:50:51 mlosch Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_bulkformulae(mytime, myiter, mythid)

c     ==================================================================
c     SUBROUTINE exf_bulkformulae
c     ==================================================================
c
c     o Use bulk formulae to estimate turbulent and/or radiative
c       fluxes at the surface.
c
c     NOTES:
c     ======
c
c     See EXF_OPTIONS.h for a description of the various possible
c     ocean-model forcing configurations.
c
c     The bulk formulae of pkg/exf are not valid for sea-ice covered
c     oceans but they can be used in combination with a sea-ice model,
c     for example, pkg/seaice, to specify open water flux contributions.
c
c     ==================================================================
c
c       The calculation of the bulk surface fluxes has been adapted from
c       the NCOM model which uses the formulae given in Large and Pond
c       (1981 & 1982 )
c
c
c       Header taken from NCOM version: ncom1.4.1
c       -----------------------------------------
c
c       Following procedures and coefficients in Large and Pond
c       (1981 ; 1982)
c
c       Output: Bulk estimates of the turbulent surface fluxes.
c       -------
c
c       hs  - sensible heat flux  (W/m^2), into ocean
c       hl  - latent   heat flux  (W/m^2), into ocean
c
c       Input:
c       ------
c
c       us  - mean wind speed (m/s)     at height hu (m)
c       th  - mean air temperature (K)  at height ht (m)
c       qh  - mean air humidity (kg/kg) at height hq (m)
c       sst - sea surface temperature (K)
c       tk0 - Kelvin temperature at 0 Celsius (K)
c
c       Assume 1) a neutral 10m drag coefficient =
c
c                 cdn = .0027/u10 + .000142 + .0000764 u10
c
c              2) a neutral 10m stanton number =
c
c                 ctn = .0327 sqrt(cdn), unstable
c                 ctn = .0180 sqrt(cdn), stable
c
c              3) a neutral 10m dalton number =
c
c                 cen = .0346 sqrt(cdn)
c
c              4) the saturation humidity of air at
c
c                 t(k) = exf_BulkqSat(t)  (kg/m^3)
c
c       Note:  1) here, tstar = <wt>/u*, and qstar = <wq>/u*.
c              2) wind speeds should all be above a minimum speed,
c                 say 0.5 m/s.
c              3) with optional iteration loop, niter=3, should suffice.
c              4) this version is for analyses inputs with hu = 10m and
c                 ht = hq.
c              5) sst enters in Celsius.
c
c     ==================================================================
c
c       started: Christian Eckert eckert@mit.edu 27-Aug-1999
c
c       changed: Christian Eckert eckert@mit.edu 14-Jan-2000
c              - restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  04-May-2000
c              - changed the handling of precip and sflux with respect
c                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
c              - included some CPP flags ALLOW_BULKFORMULAE to make
c                sure ALLOW_ATM_TEMP, ALLOW_ATM_WIND are used only in
c                conjunction with defined ALLOW_BULKFORMULAE
c              - statement functions discarded
c
c              Ralf.Giering@FastOpt.de 25-Mai-2000
c              - total rewrite using new subroutines
c
c              Detlef Stammer: include river run-off. Nov. 21, 2001
c
c              heimbach@mit.edu, 10-Jan-2002
c              - changes to enable field swapping
c
c       mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_bulkformulae
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
c #include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

#ifdef ALLOW_BULKFORMULAE

c     == local variables ==

      integer bi,bj
      integer i,j,k

      _RL czol
      _RL zwln               ! = log(zwd/zref)
      _RL ztln               ! = log(zth/zref)
      _RL windStress         ! surface wind-stress (@ grid cell center)

      integer iter
      _RL     tmpbulk
      _RL     hqol
      _RL     htol
      _RL     huol
      _RL     psimh
      _RL     psixh
      _RL     re                ! = Ce / sqrt(Cd)     [-]
      _RL     rh                ! = Ch / sqrt(Cd)     [-]
      _RL     ren, rhn          ! neutral, zref (=10m) values of re, rh
      _RL     tsf
      _RL     ssq
      _RL     stable
      _RL     t0
      _RL     wsm                ! limited wind speed [m/s] (> umin)
      _RL     uzn
      _RL     shn
      _RL     xsq
      _RL     x
      _RL     recip_rhoConstFresh
C     these need to 2D-arrays for vectorizing code
      _RL tstar (1:sNx,1:sNy)
      _RL qstar (1:sNx,1:sNy)
      _RL ustar (1:sNx,1:sNy)
      _RL tau   (1:sNx,1:sNy)
      _RL rd    (1:sNx,1:sNy)
      _RL rdn   (1:sNx,1:sNy)
      _RL delq  (1:sNx,1:sNy)
      _RL deltap(1:sNx,1:sNy)
#ifdef ALLOW_AUTODIFF_TAMC
      integer ikey_1
      integer ikey_2
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      zwln = LOG(hu/zref)
      ztln = LOG(ht/zref)
      czol = hu*karman*gravity_mks
      recip_rhoConstFresh = 1. _d 0/rhoConstFresh
c--   Use atmospheric state to compute surface fluxes.

c     Loop over tiles.
#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
#endif
      do bj = mybylo(mythid),mybyhi(mythid)
#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAMC
CHPF$  INDEPENDENT
#endif
       do bi = mybxlo(mythid),mybxhi(mythid)
        k = 1
        do j = 1,sny
         do i = 1,snx

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1

          ikey_1 = i
     &         + sNx*(j-1)
     &         + sNx*sNy*act1
     &         + sNx*sNy*max1*act2
     &         + sNx*sNy*max1*max2*act3
     &         + sNx*sNy*max1*max2*max3*act4
#endif

c--   Compute the turbulent surface fluxes.

#ifdef ALLOW_ATM_TEMP

c             Initial guess: z/l=0.0; hu=ht=hq=z
c             Iterations:    converge on z/l and hence the fluxes.
c             t0     : virtual temperature (K)
c             ssq    : sea surface humidity (kg/kg)
c             deltap : potential temperature diff (K)

#ifdef ALLOW_AUTODIFF_TAMC
          deltap(i,j) = 0. _d 0
          delq(i,j)   = 0. _d 0
#endif
          if ( atemp(i,j,bi,bj) .ne. 0. _d 0 ) then
           tsf         = theta(i,j,k,bi,bj) + cen2kel
           tmpbulk     = cvapor_fac * exp( - cvapor_exp/tsf )
           ssq         = saltsat*tmpbulk/atmrho
           deltap(i,j) = atemp(i,j,bi,bj) + gamma_blk*ht - tsf
           delq(i,j)   = aqh(i,j,bi,bj) - ssq
           stable      = exf_half + sign(exf_half, deltap(i,j))
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif
#ifdef ALLOW_ATM_WIND
           wsm         = sh(i,j,bi,bj)
           tmpbulk     = exf_scal_BulkCdn *
     &          ( cdrag_1/wsm + cdrag_2 + cdrag_3*wsm )
           rdn(i,j)    = sqrt(tmpbulk)
           ustar(i,j)  = rdn(i,j)*wsm
#else /* ifndef ALLOW_ATM_WIND */
C     in this case ustress and vstress are defined a u and v points
C     respectively, and we need to average to mass points to avoid
C     tau = 0 near coasts or other boundaries
c            tau(i,j)   = sqrt(0.5*
c    &                   (ustress(i,  j,bi,bj)*ustress(i  ,j,bi,bj)
c    &                   +ustress(i+1,j,bi,bj)*ustress(i+1,j,bi,bj)
c    &                   +vstress(i,j,  bi,bj)*vstress(i,j  ,bi,bj)
c    &                   +vstress(i,j+1,bi,bj)*vstress(i,j+1,bi,bj))
c    &                   )
           windStress  = wStress(i,j,bi,bj)
           ustar(i,j)  = sqrt(windStress/atmrho)
c          tau(i,j)    = wStress/ustar
           tau(i,j)    = sqrt(windStress*atmrho)
#endif /* ifndef ALLOW_ATM_WIND */

C--  initial guess for exchange other coefficients:
           rhn = (exf_one-stable)*cstanton_1 + stable*cstanton_2
           ren = cdalton
C--  calculate turbulent scales
           tstar(i,j)  = rhn*deltap(i,j)
           qstar(i,j)  = ren*delq(i,j)

          else
C     atemp(i,j,bi,bj) .EQ. 0. 
           tstar (i,j) = 0. _d 0
           qstar (i,j) = 0. _d 0
           ustar (i,j) = 0. _d 0
           tau   (i,j) = 0. _d 0
           rdn   (i,j) = 0. _d 0
          end if
         end do
        end do

        do iter = 1,niter_bulk
         do j = 1,sny
          do i = 1,snx
           if ( atemp(i,j,bi,bj) .ne. 0. _d 0 ) then
            
#ifdef ALLOW_AUTODIFF_TAMC
            ikey_2 = i
     &           + sNx*(j-1)
     &           + sNx*sNy*(iter-1)
     &           + sNx*sNy*niter_bulk*act1
     &           + sNx*sNy*niter_bulk*max1*act2
     &           + sNx*sNy*niter_bulk*max1*max2*act3
     &           + sNx*sNy*niter_bulk*max1*max2*max3*act4
CADJ STORE rdn(i,j)              = comlev1_exf_2, key = ikey_2
CADJ STORE ustar(i,j)            = comlev1_exf_2, key = ikey_2
CADJ STORE qstar(i,j)            = comlev1_exf_2, key = ikey_2
CADJ STORE tstar(i,j)            = comlev1_exf_2, key = ikey_2
CADJ STORE sh(i,j,bi,bj)         = comlev1_exf_2, key = ikey_2
CADJ STORE wspeed(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
#endif

            t0     = atemp(i,j,bi,bj)*
     &           (exf_one + humid_fac*aqh(i,j,bi,bj))
            huol   = ( tstar(i,j)/t0 +
     &                 qstar(i,j)/(exf_one/humid_fac+aqh(i,j,bi,bj))
     &                )*czol/(ustar(i,j)*ustar(i,j))
            huol   = max(huol,zolmin)
            htol   = huol*ht/hu
            hqol   = huol*hq/hu
            stable = exf_half + sign(exf_half, huol)
            
c     Evaluate all stability functions assuming hq = ht.
#ifdef ALLOW_ATM_WIND
            xsq    = max(sqrt(abs(exf_one - 16.*huol)),exf_one)
            x      = sqrt(xsq)
            psimh  = -psim_fac*huol*stable
     &              +(exf_one-stable)*
     &               ( LOG( (exf_one + exf_two*x + xsq)*
     &                      (exf_one+xsq)*.125 _d 0 )
     &                 -exf_two*ATAN(x) + exf_half*pi )
#endif /* ALLOW_ATM_WIND */
            xsq    = max(sqrt(abs(exf_one - 16.*htol)),exf_one)
            psixh = -psim_fac*htol*stable + (exf_one-stable)*
     &               ( exf_two*LOG(exf_half*(exf_one+xsq)) )
            
#ifdef ALLOW_ATM_WIND
c     Shift wind speed using old coefficient
c           rd(i,j)= rdn(i,j)/(exf_one - rdn(i,j)/karman*psimh )
c           shn    = sh(i,j,bi,bj)*rd(i,j)/rdn(i,j)
c     ML: the original formulation above is replaced below, but does
c     not give the same results, strange
            shn    = sh(i,j,bi,bj)/(exf_one - rdn(i,j)/karman*psimh )
            uzn    = max(shn, umin)

c     Update the transfer coefficients at 10 meters
c     and neutral stability.

            tmpbulk  = exf_scal_BulkCdn *
     &           ( cdrag_1/uzn + cdrag_2 + cdrag_3*uzn )
            rdn(i,j) = sqrt(tmpbulk)

c     Shift all coefficients to the measurement height
c     and stability.
            rd(i,j)  = rdn(i,j)/(exf_one - rdn(i,j)/karman*psimh)
#endif /* ALLOW_ATM_WIND */
            rhn = (exf_one-stable)*cstanton_1 + stable*cstanton_2
            ren = cdalton
            rh     = rhn/( exf_one + rhn/karman*(ztln - psixh) )
            re     = ren/( exf_one + ren/karman*(ztln - psixh) )

c                 Update ustar, tstar, qstar using updated, shifted
c                 coefficients.
            qstar(i,j) = re*delq(i,j)
            tstar(i,j) = rh*deltap(i,j)
#ifdef ALLOW_ATM_WIND
            ustar(i,j) = rd(i,j)*sh(i,j,bi,bj)
            tau(i,j)   = atmrho*rd(i,j)*wspeed(i,j,bi,bj)
#endif /* ALLOW_ATM_WIND */

           endif
          enddo
         enddo
c        end of bulk_iter loop
        enddo

        do j = 1,sny
         do i = 1,snx
          if ( atemp(i,j,bi,bj) .ne. 0. _d 0 ) then

#ifdef ALLOW_AUTODIFF_TAMC
          ikey_1 = i
     &         + sNx*(j-1)
     &         + sNx*sNy*act1
     &         + sNx*sNy*max1*act2
     &         + sNx*sNy*max1*max2*act3
     &         + sNx*sNy*max1*max2*max3*act4
CADJ STORE qstar(i,j)    = comlev1_exf_1, key = ikey_1
CADJ STORE tstar(i,j)    = comlev1_exf_1, key = ikey_1
CADJ STORE tau(i,j)      = comlev1_exf_1, key = ikey_1
CADJ STORE rd(i,j)       = comlev1_exf_1, key = ikey_1
#endif
           hs(i,j,bi,bj) = atmcp*tau(i,j)*tstar(i,j)
           hl(i,j,bi,bj) = flamb*tau(i,j)*qstar(i,j)
#ifndef EXF_READ_EVAP
cdm        evap(i,j,bi,bj) = tau(i,j)*qstar(i,j)
cdm !!! need to change sign and to convert from kg/m^2/s to m/s !!!
           evap(i,j,bi,bj) = -recip_rhoConstFresh*tau(i,j)*qstar(i,j)
#endif
#ifdef ALLOW_ATM_WIND
           tmpbulk =  tau(i,j)*rd(i,j)
           ustress(i,j,bi,bj) = tmpbulk*uwind(i,j,bi,bj)
           vstress(i,j,bi,bj) = tmpbulk*vwind(i,j,bi,bj)
#endif /* ALLOW_ATM_WIND */

c IF ( ATEMP(i,j,bi,bj) .NE. 0. )
          else
#ifdef ALLOW_ATM_WIND
           ustress(i,j,bi,bj) = 0. _d 0
           vstress(i,j,bi,bj) = 0. _d 0
#endif /* ALLOW_ATM_WIND */
           hflux  (i,j,bi,bj) = 0. _d 0
           hs(i,j,bi,bj)      = 0. _d 0
           hl(i,j,bi,bj)      = 0. _d 0

c IF ( ATEMP(i,j,bi,bj) .NE. 0. )
          endif

#else  /* ifndef ALLOW_ATM_TEMP */
#ifdef ALLOW_ATM_WIND
          wsm                = sh(i,j,bi,bj)
          tmpbulk            = exf_scal_BulkCdn *
     &         ( cdrag_1/wsm + cdrag_2 + cdrag_3*wsm )
          ustress(i,j,bi,bj) = atmrho*tmpbulk*wspeed(i,j,bi,bj)*
     &         uwind(i,j,bi,bj)
          vstress(i,j,bi,bj) = atmrho*tmpbulk*wspeed(i,j,bi,bj)*
     &         vwind(i,j,bi,bj)
#endif
#endif /* ifndef ALLOW_ATM_TEMP */
         enddo
        enddo
       enddo
      enddo

#endif /* ALLOW_BULKFORMULAE */

      return
      end
