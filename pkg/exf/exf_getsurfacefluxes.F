c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getsurfacefluxes.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

#include "EXF_CPPOPTIONS.h"

      subroutine exf_GetSurfaceFluxes(
     I                                 mytime,
     I                                 myiter,
     I                                 mythid
     &                               )

c     ==================================================================
c     SUBROUTINE exf_GetSurfaceFluxes
c     ==================================================================
c
c     o Mid-level routine for enabling the use of flux fields as control
c       variables.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c
c              - Changed Routine names (package prefix: exf_)
c
c     ==================================================================
c     SUBROUTINE exf_GetSurfaceFluxes
c     ==================================================================

      implicit none

c     == routine arguments ==

      _RL mytime
      integer myiter
      integer mythid

c     == global variables ==

c     == end of interface ==

c     Get the surface forcing fields.
      call exf_GetFFields(  mytime, myiter, mythid )

#ifdef ALLOW_HFLUX_CONTROL
      call ctrl_getheatflux ( mytime, myiter, mythid )
#endif

#ifdef ALLOW_SFLUX_CONTROL
      call ctrl_getsaltflux ( mytime, myiter, mythid )
#endif

#ifdef ALLOW_USTRESS_CONTROL
      call ctrl_getzonstress ( mytime, myiter, mythid )
#endif

#ifdef ALLOW_VSTRESS_CONTROL
      call ctrl_getmerstress ( mytime, myiter, mythid )
#endif

#ifdef ALLOW_SWFLUX_CONTROL
      call ctrl_getswflux ( mytime, myiter, mythid )
#endif

#ifdef ALLOW_LWFLUX_CONTROL
      call ctrl_getlwflux ( mytime, myiter, mythid )
#endif

#ifdef ALLOW_CONTROL_NEW
c     Get the surface forcing control fields.
      call exfa_GetFFields( mytime, myiter, mythid )
#endif

      end

