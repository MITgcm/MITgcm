c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffields.F,v 1.2 2001/07/30 20:41:20 heimbach Exp $

#include "EXF_CPPOPTIONS.h"

      subroutine exf_GetFFields(
     I                           mycurrenttime,
     I                           mycurrentiter,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exf_GetFFields
c     ==================================================================
c
c     o Get the surface fluxes either from file or as derived from bulk
c       formulae that use the atmospheric state.
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
c       ------------------------------------
c
c       By setting CPP options in the header file *EXF_CPPOPTIONS.h* it
c       is possible to combine data sets in four different ways:
c
c       The following options are available:
c
c         ALLOW_ATM_TEMP (UAT)
c         ALLOW_ATM_WIND (UAW)
c
c
c             UAT    |    UAW      |         action
c         ----------------------------------------------------
c         undefined  |  undefined  |  Use surface fluxes.
c         undefined  |    defined  |  Assume cdn(u) given to
c                    |             |  infer the wind stress.
c           defined  |  undefined  |  Compute wind field from
c                    |             |  given stress assuming a
c                    |             |  linear relation.
c           defined  |    defined  |  Use the bulk formulae.
c         ----------------------------------------------------
c
c       Implementations of the bulk formulae exist for the follwing
c       versions of the MITgcm:
c
c       MITgcm  : Patrick Heimbach
c       MITgcmUV: Christian Eckert
c
c       started: Christian Eckert eckert@mit.edu 27-Aug-1999
c
c       changed: Christian Eckert eckert@mit.edu 14-Jan-2000
c
c              - restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c
c              - Changed Routine names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  04-May-2000
c
c              - changed the handling of precip and sflux with respect
c                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
c
c              - included some CPP flags ALLOW_BULKFORMULAE to make
c                sure ALLOW_ATM_TEMP, ALLOW_ATM_WIND are used only in
c                conjunction with defined ALLOW_BULKFORMULAE
c
c              - statement functions discarded
c
c              Ralf.Giering@FastOpt.de 25-Mai-2000
c
c              - total rewrite using new subroutines
c
c     ==================================================================
c     SUBROUTINE exf_GetFFields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "exf_fields.h"
#include "exf_constants.h"

c     == routine arguments ==

      integer mythid
      integer mycurrentiter
      _RL     mycurrenttime

c     == local variables ==

      integer bi,bj
      integer i,j,k

#ifdef ALLOW_BULKFORMULAE

#ifdef ALLOW_ATM_TEMP
      integer iter
      _RL     aln
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
      _RL     ssq
      _RL     stable
      _RL     tau
      _RL     tstar
      _RL     t0
      _RL     ustar
      _RL     uzn
      _RL     xsq
      _RL     x
      _RL     evap(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif /* ALLOW_ATM_TEMP */

      _RL     us
      _RL     cw
      _RL     sw
      _RL     sh
      _RL     hs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     hl(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     hfl

#endif /* ALLOW_BULKFORMULAE */

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

#ifdef ALLOW_BULKFORMULAE
      _RL       exf_BulkqSat
      external  exf_BulkqSat
      _RL       exf_BulkCdn
      external  exf_BulkCdn
      _RL       exf_BulkRhn
      external  exf_BulkRhn
#endif /* ALLOW_BULKFORMULAE */

c     == end of interface ==

c     determine forcing field records

#ifdef ALLOW_BULKFORMULAE

c     Determine where we are in time and set counters, flags and
c     the linear interpolation factors accordingly.
#ifdef ALLOW_ATM_TEMP
c     Atmospheric temperature.
      call exf_set_atemp( atemp
     &                  , mycurrenttime, mycurrentiter, mythid )

c     Atmospheric humidity.
      call exf_set_aqh( aqh
     &                , mycurrenttime, mycurrentiter, mythid )

c     Net long wave radiative flux.
      call exf_set_lwflux( lwflux
     &                   , mycurrenttime, mycurrentiter, mythid )

c     Net short wave radiative flux.
      call exf_set_swflux( swflux
     &                   , mycurrenttime, mycurrentiter, mythid )

c     Precipitation.
      call exf_set_precip( precip
     &                   , mycurrenttime, mycurrentiter, mythid )

      aln = log(ht/zref)

#else

c     Atmospheric heat flux.
      call exf_set_hflux( hflux
     &                  , mycurrenttime, mycurrentiter, mythid )

c     Salt flux.
      call exf_set_sflux( sflux
     &                  , mycurrenttime, mycurrentiter, mythid )

#ifdef ALLOW_KPP
c     Net short wave radiative flux.
      call exf_set_swflux( swflux
     &                   , mycurrenttime, mycurrentiter, mythid )

#endif /* ALLOW_KPP */

#endif /* ALLOW_ATM_TEMP */

#ifdef ALLOW_ATM_WIND
c     Zonal wind.
      call exf_set_uwind( uwind
     &                  , mycurrenttime, mycurrentiter, mythid )

c     Meridional wind.
      call exf_set_vwind( vwind
     &                  , mycurrenttime, mycurrentiter, mythid )

#else
c     Zonal wind stress.
      call exf_set_ustress( ustress
     &                    , mycurrenttime, mycurrentiter, mythid )

c     Meridional wind stress.
      call exf_set_vstress( vstress
     &                    , mycurrenttime, mycurrentiter, mythid )

#endif /* ALLOW_ATM_WIND */

#else /* ALLOW_BULKFORMULAE undefined */
c     Atmospheric heat flux.
      call exf_set_hflux( hflux
     &                  , mycurrenttime, mycurrentiter, mythid )

c     Salt flux.
      call exf_set_sflux( sflux
     &                  , mycurrenttime, mycurrentiter, mythid )

c     Zonal wind stress.
      call exf_set_ustress( ustress
     &                    , mycurrenttime, mycurrentiter, mythid )

c     Meridional wind stress.
      call exf_set_vstress( vstress
     &                    , mycurrenttime, mycurrentiter, mythid )

#ifdef ALLOW_KPP
c     Net short wave radiative flux.
      call exf_set_swflux( swflux
     &                   , mycurrenttime, mycurrentiter, mythid )
#endif /* ALLOW_KPP */

#endif /* ALLOW_BULKFORMULAE */

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          k = 1

          do j = 1-oly,sny+oly
            do i = 1-olx,snx+olx

#ifdef ALLOW_BULKFORMULAE

c             Compute the turbulent surface fluxes.
c                  (Bulk formulae estimates)

#ifdef ALLOW_ATM_WIND
c             Wind speed and direction.
              us = sqrt(uwind(i,j,bi,bj)*uwind(i,j,bi,bj) +
     &                  vwind(i,j,bi,bj)*vwind(i,j,bi,bj))
              if ( us .ne. 0. _d 0 ) then
                cw = uwind(i,j,bi,bj)/us
                sw = vwind(i,j,bi,bj)/us
              else
                cw = 0. _d 0
                sw = 0. _d 0
              endif
              sh = max(us,umin)
#else
#ifdef ALLOW_ATM_TEMP

c             The variables us, sh and rdn have to be computed from given
c             wind stresses inverting relationship for neutral drag coeff.
c             cdn.
c             The inversion is based on linear and quadratic form of
c             cdn(umps); ustar can be directly computed from stress;

              ustar = sqrt(ustress(i,j,bi,bj)*ustress(i,j,bi,bj) + 
     &                     vstress(i,j,bi,bj)*vstress(i,j,bi,bj))/
     &                    atmrho
              cw = ustress(i,j,bi,bj)/ustar
              sw = ustress(i,j,bi,bj)/ustar

              if ( ustar .eq. 0. _d 0 ) then
                us = 0. _d 0
              else if ( ustar .lt. ustofu11 ) then
                tmp1 = -cquadrag_2/cquadrag_1/2
                tmp2 = sqrt(tmp1*tmp1 + ustar*ustar/cquadrag_1)
                us   = sqrt(tmp1 + tmp2)
              else
                tmp3 = clindrag_2/clindrag_1/3
                tmp4 = ustar*ustar/clindrag_1/2 - tmp3**3
                tmp5 = sqrt(ustar*ustar/clindrag_1*
     &                      (ustar*ustar/clindrag_1/4 - tmp3**3))
                us   = (tmp4 + tmp5)**(1/3) +
     &                 tmp3**2 * (tmp4 + tmp5)**(-1/3) - tmp3
              endif

              if ( us .ne. 0 ) then
                 rdn = ustar/us
              else
                 rdn = 0. _d 0
              end if

              sh    = max(us,umin)
#endif /* ALLOW_ATM_TEMP */
#endif /* ALLOW_ATM_WIND */

#ifdef ALLOW_ATM_TEMP
c             Initial guess: z/l=0.0; hu=ht=hq=z
c             Iterations:    converge on z/l and hence the fluxes.
c             t0     : virtual temperature (K)
c             ssq    : sea surface humidity (kg/kg)
c             deltap : potential temperature diff (K)

              if ( atemp(i,j,bi,bj) .ne. 0. _d 0 ) then
                t0     = atemp(i,j,bi,bj)*
     &                   (exf_one + humid_fac*aqh(i,j,bi,bj))
                ssq    = saltsat*
     &                   exf_BulkqSat(theta(i,j,1,bi,bj) + cen2kel)/
     &                   atmrho
                deltap = atemp(i,j,bi,bj)   + gamma_blk*ht -
     &                   theta(i,j,1,bi,bj) - cen2kel
                delq   = aqh(i,j,bi,bj) - ssq
                stable = exf_half + sign(exf_half, deltap)
                rdn    = sqrt(exf_BulkCdn(sh))
                ustar  = rdn*sh
                tstar  = exf_BulkRhn(stable)*deltap 
                qstar  = cdalton*delq 

                do iter = 1,niter_bulk

                  huol   = czol*(tstar/t0 +
     &                     qstar/(exf_one/humid_fac+aqh(i,j,bi,bj)))/
     &                           ustar**2
                  huol   = max(huol,zolmin)
                  stable = exf_half + sign(exf_half, huol)
                  htol   = huol*ht/hu
                  hqol   = huol*hq/hu

c                 Evaluate all stability functions assuming hq = ht.
                  xsq    = max(sqrt(abs(exf_one - 16.*huol)),exf_one)
                   x     = sqrt(xsq)
                  psimh  = -psim_fac*huol*stable +
     &                     (exf_one - stable)*
     &                     log((exf_one + x*(exf_two + x))*
     &                     (exf_one + xsq)/8.) - exf_two*atan(x) +
     &                     pi*exf_half
                  xsq    = max(sqrt(abs(exf_one - 16.*htol)),exf_one)
                  psixh  = -psim_fac*htol*stable + (exf_one - stable)*
     &                     exf_two*log((exf_one + xsq)/exf_two)

c                 Shift wind speed using old coefficient
c                 rd   = rdn/(exf_one + rdn/karman*(log(hu/zref) - psimh) )
                  rd   = rdn/(exf_one - rdn/karman*psimh )
                  uzn  = max(sh*rd/rdn, umin)

c                 Update the transfer coefficients at 10 meters
c                 and neutral stability.
                  rdn = sqrt(exf_BulkCdn(uzn))

c                 Shift all coefficients to the measurement height
c                 and stability.
c                 rd = rdn/(exf_one + rdn/karman*(log(hu/zref) - psimh))
                  rd = rdn/(exf_one - rdn/karman*psimh)
                  rh = exf_BulkRhn(stable)/(exf_one +
     &                                      exf_BulkRhn(stable)/
     &                                     karman*(aln - psixh))
                  re = cdalton/(exf_one + cdalton/karman*(aln - psixh))

c                 Update ustar, tstar, qstar using updated, shifted
c                 coefficients.
                  ustar = rd*sh  
                  qstar = re*delq 
                  tstar = rh*deltap

                enddo

                tau                = atmrho*ustar**2   
                tau                = tau*us/sh
                hs(i,j,bi,bj)      = atmcp*tau*tstar/ustar
                hl(i,j,bi,bj)      = flamb*tau*qstar/ustar

                evap(i,j,bi,bj)    = tau*qstar/ustar

                ustress(i,j,bi,bj) = tau*cw
                vstress(i,j,bi,bj) = tau*sw
ce              hflux(i,j,bi,bj)   = atmcp*tau*tstar/ustar +
ce     &                             flamb*tau*qstar/ustar
              else
                ustress(i,j,bi,bj) = 0. _d 0
                vstress(i,j,bi,bj) = 0. _d 0
                hflux  (i,j,bi,bj) = 0. _d 0
                hs(i,j,bi,bj)      = 0. _d 0
                hl(i,j,bi,bj)      = 0. _d 0
              endif

#else
#ifdef ALLOW_ATM_WIND
              ustress(i,j,bi,bj) = atmrho*exf_BulkCdn(sh)*us*
     &                             uwind(i,j,bi,bj)
              vstress(i,j,bi,bj) = atmrho*exf_BulkCdn(sh)*us*
     &                             vwind(i,j,bi,bj)
#endif /* ALLOW_ATM_WIND */
#endif /* ALLOW_ATM_TEMP */
            enddo
          enddo
        enddo
      enddo

c     Add all contributions.
      k = 1
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do j = 1,sny
            do i = 1,snx
c             Net surface heat flux.
#ifdef ALLOW_ATM_TEMP
              hfl = 0. _d 0
              hfl = hfl - hs(i,j,bi,bj)
              hfl = hfl - hl(i,j,bi,bj)
              hfl = hfl + lwflux(i,j,bi,bj)
#ifndef ALLOW_KPP
              hfl = hfl + swflux(i,j,bi,bj)
#endif /* ALLOW_KPP undef */
c             Heat flux:
              hflux(i,j,bi,bj) = hfl*maskc(i,j,k,bi,bj)
c             Salt flux from Precipitation and Evaporation.
              sflux(i,j,bi,bj) = precip(i,j,bi,bj) - evap(i,j,bi,bj)
#endif /* ALLOW_ATM_TEMP */

#else
              hflux(i,j,bi,bj) = hflux(i,j,bi,bj)*maskc(i,j,k,bi,bj)
              sflux(i,j,bi,bj) = sflux(i,j,bi,bj)*maskc(i,j,k,bi,bj)
#endif /* ALLOW_BULKFORMULAE */
            enddo
          enddo
        enddo
      enddo

c     Update the tile edges.
      _EXCH_XY_R8(hflux,   mythid)
      _EXCH_XY_R8(sflux,   mythid)
      _EXCH_XY_R8(ustress, mythid)
      _EXCH_XY_R8(vstress, mythid)

#ifdef ALLOW_KPP
      _EXCH_XY_R8(swflux, mythid)
#endif /* ALLOW_KPP */

      end
