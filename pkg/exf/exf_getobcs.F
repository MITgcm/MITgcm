#include "EXF_CPPOPTIONS.h"

      subroutine exf_getobcs (   mycurrenttime
     I                         , mycurrentiter
     I                         , mythid
     &                       )

c     ==================================================================
c     SUBROUTINE exf_getobcs
c     ==================================================================
c
c     o set open boundary conditions
c
c     started: heimbach@mit.edu 01-May-2001

c     ==================================================================
c     SUBROUTINE exf_getobcs
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#ifdef ALLOW_OBCS
#include "OBCS.h"
#endif

#include "exf_param.h"

c     == routine arguments ==

      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_OBCS

c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac

c     == end of interface ==

c     get record numbers and interpolation factor for hflux
      call exf_getffieldrec(
     I                       obcsstartdate, obcsperiod
     O                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      call exf_set_obcs_xz(  OBNufile, OBNu, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNvfile, OBNv, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNtfile, OBNt, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNsfile, OBNs, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSufile, OBSu, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSvfile, OBSv, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBStfile, OBSt, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSsfile, OBSs, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )

      call exf_set_obcs_yz(  OBEufile, OBEu, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEvfile, OBEv, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEtfile, OBEt, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEsfile, OBEs, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWufile, OBWu, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWvfile, OBWv, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWtfile, OBWt, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWsfile, OBWs, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )

#endif

      end

