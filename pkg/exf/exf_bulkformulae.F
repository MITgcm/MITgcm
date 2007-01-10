C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_bulkformulae.F,v 1.13 2007/01/10 21:44:38 jmc Exp $
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

#include "exf_param.h"
#include "exf_fields.h"
#include "exf_constants.h"

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

      _RL     aln

#ifdef ALLOW_ATM_TEMP
      integer iter
      _RL     delq
      _RL     deltap
      _RL     hqol
      _RL     htol
      _RL     huol
      _RL     psimh
      _RL     psixh
      _RL     qstar
      _RL     rd
      _RL     re
      _RL     rdn
      _RL     rh
      _RL     ssttmp
      _RL     ssq
      _RL     stable
      _RL     tstar
      _RL     t0
      _RL     ustar
      _RL     uzn
      _RL     shn
      _RL     xsq
      _RL     x
      _RL     tau
#ifdef ALLOW_AUTODIFF_TAMC
      integer ikey_1
      integer ikey_2
#endif
#endif /* ALLOW_ATM_TEMP */

      _RL     tmpbulk

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

      _RL       exf_BulkqSat
      external  exf_BulkqSat
      _RL       exf_BulkCdn
      external  exf_BulkCdn
      _RL       exf_BulkRhn
      external  exf_BulkRhn

c     == end of interface ==

cph   This statement cannot be a PARAMETER statement in the header,
cph   but must come here; it is not fortran77 standard
      aln = log(ht/zref)

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

          if ( atemp(i,j,bi,bj) .ne. 0. _d 0 ) then
             t0     = atemp(i,j,bi,bj)*
     &            (exf_one + humid_fac*aqh(i,j,bi,bj))
             ssttmp = theta(i,j,k,bi,bj)
             tmpbulk= exf_BulkqSat(ssttmp + cen2kel)
             ssq    = saltsat*tmpbulk/atmrho
             deltap = atemp(i,j,bi,bj)   + gamma_blk*ht -
     &            ssttmp - cen2kel
             delq   = aqh(i,j,bi,bj) - ssq
             stable = exf_half + sign(exf_half, deltap)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif
#ifdef ALLOW_ATM_WIND
             tmpbulk= exf_BulkCdn(sh(i,j,bi,bj))
             rdn    = sqrt(tmpbulk)
             ustar  = rdn*sh(i,j,bi,bj)
#else /* ifndef ALLOW_ATM_WIND */
             tau   = sqrt(ustress(i,j,bi,bj)*ustress(i,j,bi,bj)
     &                   +vstress(i,j,bi,bj)*vstress(i,j,bi,bj))
             ustar = sqrt(tau/atmrho)
#endif /* ifndef ALLOW_ATM_WIND */
             tmpbulk= exf_BulkRhn(stable)
             tstar  = tmpbulk*deltap
             qstar  = cdalton*delq

             do iter = 1,niter_bulk

#ifdef ALLOW_AUTODIFF_TAMC
                   ikey_2 = iter
     &                  + niter_bulk*(i-1)
     &                  + niter_bulk*sNx*(j-1)
     &                  + niter_bulk*sNx*sNy*act1
     &                  + niter_bulk*sNx*sNy*max1*act2
     &                  + niter_bulk*sNx*sNy*max1*max2*act3
     &                  + niter_bulk*sNx*sNy*max1*max2*max3*act4

CADJ STORE rdn    = comlev1_exf_2, key = ikey_2
CADJ STORE ustar  = comlev1_exf_2, key = ikey_2
CADJ STORE qstar  = comlev1_exf_2, key = ikey_2
CADJ STORE tstar  = comlev1_exf_2, key = ikey_2
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
CADJ STORE us(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
#endif

                  huol   = czol*(tstar/t0 +
     &                     qstar/(exf_one/humid_fac+aqh(i,j,bi,bj)))/
     &                           ustar**2
                  huol   = max(huol,zolmin)
                  stable = exf_half + sign(exf_half, huol)
                  htol   = huol*ht/hu
                  hqol   = huol*hq/hu

c                 Evaluate all stability functions assuming hq = ht.
#ifdef ALLOW_ATM_WIND
                  xsq    = max(sqrt(abs(exf_one - 16.*huol)),exf_one)
                   x     = sqrt(xsq)
                  psimh  = -psim_fac*huol*stable +
     &                     (exf_one - stable)*
     &                     (log((exf_one + x*(exf_two + x))*
     &                     (exf_one + xsq)/8.) - exf_two*atan(x) +
     &                     pi*exf_half)
#endif /* ALLOW_ATM_WIND */
                  xsq    = max(sqrt(abs(exf_one - 16.*htol)),exf_one)
                  psixh  = -psim_fac*htol*stable + (exf_one - stable)*
     &                     exf_two*log((exf_one + xsq)/exf_two)

#ifdef ALLOW_ATM_WIND
c                 Shift wind speed using old coefficient
                  rd     = rdn/(exf_one - rdn/karman*psimh )
                  shn    = sh(i,j,bi,bj)*rd/rdn
                  uzn    = max(shn, umin)

c                 Update the transfer coefficients at 10 meters
c                 and neutral stability.

                  tmpbulk= exf_BulkCdn(uzn)
                  rdn    = sqrt(tmpbulk)

c                 Shift all coefficients to the measurement height
c                 and stability.
                  rd     = rdn/(exf_one - rdn/karman*psimh)
#endif /* ALLOW_ATM_WIND */
                  tmpbulk= exf_BulkRhn(stable)
                  rh     = tmpbulk/( exf_one +
     &                               tmpbulk/karman*(aln - psixh) )
                  re     = cdalton/( exf_one +
     &                               cdalton/karman*(aln - psixh) )

c                 Update ustar, tstar, qstar using updated, shifted
c                 coefficients.
                  qstar  = re*delq
                  tstar  = rh*deltap
#ifdef ALLOW_ATM_WIND
                  ustar  = rd*sh(i,j,bi,bj)
                  tau    = atmrho*ustar**2
                  tau    = tau*us(i,j,bi,bj)/sh(i,j,bi,bj)
#endif /* ALLOW_ATM_WIND */

                enddo

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustar  = comlev1_exf_1, key = ikey_1
CADJ STORE qstar  = comlev1_exf_1, key = ikey_1
CADJ STORE tstar  = comlev1_exf_1, key = ikey_1
CADJ STORE tau    = comlev1_exf_1, key = ikey_1
CADJ STORE cw(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
CADJ STORE sw(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif
                hs(i,j,bi,bj)      = atmcp*tau*tstar/ustar
                hl(i,j,bi,bj)      = flamb*tau*qstar/ustar
#ifndef EXF_READ_EVAP
cdm             evap(i,j,bi,bj)    = tau*qstar/ustar
cdm !!! need to change sign and to convert from kg/m^2/s to m/s !!!
                evap(i,j,bi,bj)    = -recip_rhonil*tau*qstar/ustar
#endif
#ifdef ALLOW_ATM_WIND
                ustress(i,j,bi,bj) = tau*cw(i,j,bi,bj)
                vstress(i,j,bi,bj) = tau*sw(i,j,bi,bj)
#endif /* ALLOW_ATM_WIND */
              else
#ifdef ALLOW_ATM_WIND
                ustress(i,j,bi,bj) = 0. _d 0
                vstress(i,j,bi,bj) = 0. _d 0
#endif /* ALLOW_ATM_WIND */
                hflux  (i,j,bi,bj) = 0. _d 0
                hs(i,j,bi,bj)      = 0. _d 0
                hl(i,j,bi,bj)      = 0. _d 0
              endif

#else  /* ifndef ALLOW_ATM_TEMP */
#ifdef ALLOW_ATM_WIND
              tmpbulk= exf_BulkCdn(sh(i,j,bi,bj))
              ustress(i,j,bi,bj) = atmrho*tmpbulk*us(i,j,bi,bj)*
     &                             uwind(i,j,bi,bj)
              vstress(i,j,bi,bj) = atmrho*tmpbulk*us(i,j,bi,bj)*
     &                             vwind(i,j,bi,bj)
#endif
#endif /* ifndef ALLOW_ATM_TEMP */
            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_BULKFORMULAE */

      return
      end
