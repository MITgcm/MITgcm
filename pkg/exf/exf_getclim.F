c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getclim.F,v 1.1 2001/05/14 22:08:40 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_getclim(
     I                        mytime,
     I                        myiter,
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE exf_getclim
c     ==================================================================
c
c     o Get the climatogy fields for the current time step. The switches
c       for the inclusion of the individual forcing components have to
c       be set in EXF_CPPOPTIONS.h .
c
c       A note on surface fluxes:
c
c       The MITgcmUV's vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_getclim
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "exf_fields.h"
#include "exf_clim_fields.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

c     == end of interface ==

#ifdef ALLOW_CLIMTEMP_RELAXATION
c     Get values of climatological temperature fields.
      call exf_set_climtemp ( climtemp
     I                      , mytime, myiter, mythid )
c--   Update the tile edges.
      _EXCH_XY_R8(climtemp, mythid)
#else
c     Do not include relaxation to climatological temperature fields.
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
c     Get values of climatological salinity fields.
      call exf_set_climsalt ( climsalt
     I                      , mytime, myiter, mythid )
c--   Update the tile edges.
      _EXCH_XY_R8(climsalt, mythid)
#else
c     Do not include relaxation to climatological salinity fields.
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
c     Get values of climatological sst fields.
      call exf_set_climsst ( climsst
     I                     , mytime, myiter, mythid )
c--   Update the tile edges.
      _EXCH_XY_R8(climsst, mythid)
#else
c     Do not include relaxation to climatological sst fields.
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
c     Get values of climatological sst fields.
      call exf_set_climsss ( climsss
     I                     , mytime, myiter, mythid )
c--   Update the tile edges.
      _EXCH_XY_R8(climsss, mythid)
#else
c     Do not include relaxation to climatological sst fields.
#endif

      end
