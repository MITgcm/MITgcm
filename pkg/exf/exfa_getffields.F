c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exfa_getffields.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exfa_GetFFields(
     I                           mycurrenttime,
     I                           mycurrentiter,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exfa_GetFFields
c     ==================================================================
c
c     o Get the surface fluxes either from file or as derived from bulk
c       formulae that use the atmospheric state.
c
c     started: Ralf.Giering@FastOpt.de 24-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exfa_GetFFields
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "exfa_param.h"
#include "exfa_fields.h"

c     == routine arguments ==

      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

c     == local variables ==

c     == end of interface ==

c     determine forcing field records

#ifdef ALLOW_HFLUX_CONTROL
c     Atmospheric heat flux.
      call exf_set_hfluxa( hfluxa
     &                   , mycurrenttime, mycurrentiter, mythid )
      _EXCH_XY_R8(hfluxa,   mythid)
#endif

#ifdef ALLOW_SFLUX_CONTROL
c     Salt flux.
      call exf_set_sfluxa( sfluxa
     &                   , mycurrenttime, mycurrentiter, mythid )
      _EXCH_XY_R8(sfluxa,   mythid)
#endif

#ifdef ALLOW_USTRESS_CONTROL
c     Zonal wind stress.
      call exf_set_ustressa( ustressa
     &                     , mycurrenttime, mycurrentiter, mythid )
      _EXCH_XY_R8(ustressa, mythid)
#endif

#ifdef ALLOW_VSTRESS_CONTROL
c     Meridional wind stress.
      call exf_set_vstressa( vstressa
     &                     , mycurrenttime, mycurrentiter, mythid )
      _EXCH_XY_R8(vstressa, mythid)
#endif

      end
