c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getforcing.F,v 1.1 2001/05/14 22:08:40 heimbach Exp $

#include "CPP_OPTIONS.h"


      subroutine exf_GetForcing(
     I                           mytime,
     I                           myiter,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exf_GetForcing
c     ==================================================================
c
c     o Get the forcing fields for the current time step. The switches
c       for the inclusion of the individual forcing components have to
c       be set in CPP_OPTIONS.h .
c
c       A note on surface fluxes:
c
c       The MITgcmUV's vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
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
c     ==================================================================
c     SUBROUTINE exf_GetForcing
c     ==================================================================

      implicit none

c     == global variables ==

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

c     == end of interface ==


c     Get values of climatological fields.
      call exf_getclim(
     I                     mytime, myiter, mythid
     &                   )

c     Get values of the surface fluxes.
      call exf_getsurfacefluxes(
     I                           mytime, myiter, mythid
     &                         )

#ifdef ALLOW_OBCS
      call exf_getobcs(
     I                           mytime, myiter, mythid
     &                         )
#endif

c     Map the forcing fields onto the corresponding model fields.
      call exf_mapfields(
     I                    mythid
     &                  )

      end

