c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_mapfields.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_MapFields(
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE exf_MapFields
c     ==================================================================
c
c     o Map the external forcing fields on the ocean model arrays. This
c       routine is included to separate the ocean state estimation tool
c       as much as possible from the ocean model. Unit conversion factors
c       are to be set by the user.
c
c       The units have to be such that the individual forcing record has
c       units equal to [quantity/s]. See the header file *FFIELDS.h* of
c       the MITgcmuv.
c
c       Required units such that no scaling has to be applied:
c
c       heat flux:          input file W/m^2
c       salt flux:          input file m/s
c       zonal wind stress:  input file N/m^2
c       merid. wind stress: input file N/m^2
c
c       To allow for such unit conversions this routine contains scaling
c       factors scal_quantity.
c
c     started: Christian Eckert eckert@mit.edu  09-Aug-1999
c
c     changed: Christian Eckert eckert@mit.edu  11-Jan-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c
c              - Changed Routine names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  06-May-2000
c
c              - added and changed CPP flag structure for
c                ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP
c
c              Patrick Heimbach, heimbach@mit.edu  23-May-2000
c
c              - sign change of ustress/vstress incorporated into
c                scaling factors scal_ust, scal_vst
c
c     ==================================================================
c     SUBROUTINE exf_MapFields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FFIELDS.h"
#include "exf_constants.h"
#include "exf_fields.h"
#include "exf_clim_fields.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j
      integer jtlo
      integer jthi
      integer itlo
      integer ithi
      integer jmin
      integer jmax
      integer imin
      integer imax
      _RL     scal_hfl
      _RL     scal_ust
      _RL     scal_vst
      _RL     scal_swf
      _RL     scal_sst
      _RL     scal_sss
#if (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_TEMP))
      _RL     scal_prc
#else
      _RL     scal_sfl
#endif

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      scal_hfl =  1. _d 0
      scal_ust =  -1. _d 0
      scal_vst =  -1. _d 0
      scal_swf =  1. _d 0
      scal_sst =  1. _d 0
      scal_sss =  1. _d 0
#if (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_TEMP))
      scal_prc =  1. _d 0
#else
      scal_sfl =  1. _d 0     
#endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax

c             Heat flux.
              qnet(i,j,bi,bj)  = scal_hfl*hflux(i,j,bi,bj)

c             Salt flux.
#if (defined (ALLOW_BULKFORMULAE)  && defined (ALLOW_ATM_TEMP))
              empmr(i,j,bi,bj) = scal_prc*precip(i,j,bi,bj)
#else
              empmr(i,j,bi,bj) = scal_sfl*sflux(i,j,bi,bj)
#endif

c             Zonal wind stress.
              fu(i,j,bi,bj)    = scal_ust*ustress(i,j,bi,bj)

c             Meridional wind stress.
              fv(i,j,bi,bj)    = scal_vst*vstress(i,j,bi,bj)

#ifdef ALLOW_KPP || (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_TEMP)))
c             Short wave radiative flux.
              qsw(i,j,bi,bj)   = scal_swf*swflux(i,j,bi,bj)
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
              sst(i,j,bi,bj)   = scal_sst*climsst(i,j,bi,bj)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
              sss(i,j,bi,bj)   = scal_sss*climsss(i,j,bi,bj)
#endif

            enddo
          enddo
        enddo
      enddo

c     Update the tile edges.

      _EXCH_XY_R4(  qnet, mythid )
      _EXCH_XY_R4( empmr, mythid )
      _EXCH_XY_R4(    fu, mythid )
      _EXCH_XY_R4(    fv, mythid )
#ifdef ALLOW_KPP
      _EXCH_XY_R4(   qsw, mythid )
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
      _EXCH_XY_R4(   sst, mythid )
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
      _EXCH_XY_R4(   sss, mythid )
#endif

      end
