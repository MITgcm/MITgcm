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

#ifdef ALLOW_OBCS_NORTH
      call exf_getffieldrec(
     I                       obcsNstartdate, obcsNperiod
     O                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      call exf_set_obcs_xz(  OBNu, OBNufile, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNv, OBNvfile, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNt, OBNtfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNs, OBNsfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_OBCS_SOUTH
      call exf_getffieldrec(
     I                       obcsSstartdate, obcsSperiod
     O                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      call exf_set_obcs_xz(  OBSu, OBSufile, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSv, OBSvfile, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSt, OBStfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSs, OBSsfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )

#endif

#ifdef ALLOW_OBCS_EAST
      call exf_getffieldrec(
     I                       obcsEstartdate, obcsEperiod
     O                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      call exf_set_obcs_yz(  OBEu, OBEufile, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEv, OBEvfile, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEt, OBEtfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEs, OBEsfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )

#endif

#ifdef ALLOW_OBCS_WEST
      call exf_getffieldrec(
     I                       obcsWstartdate, obcsWperiod
     O                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      call exf_set_obcs_yz(  OBWu, OBWufile, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWv, OBWvfile, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWt, OBWtfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWs, OBWsfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )

#endif

#ifdef ALLOW_OBCSN_CONTROL
      call ctrl_getobcsn ( mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_OBCSS_CONTROL
      call ctrl_getobcss ( mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_OBCSW_CONTROL
      call ctrl_getobcsw ( mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_OBCSE_CONTROL
      call ctrl_getobcse ( mycurrenttime, mycurrentiter, mythid )
#endif


#endif /* ALLOW_OBCS */

      end

