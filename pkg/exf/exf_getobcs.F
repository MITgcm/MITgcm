#include "EXF_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine exf_getobcs ( 
     I                      mycurrenttime
     I                    , mycurrentiter
     I                    , mythid
     &                     )

c     ==================================================================
c     SUBROUTINE exf_getobcs
c     ==================================================================
c
c     o set open boundary conditions
c
c     started: heimbach@mit.edu 01-May-2001
c              gebbie@mit.edu 18-Mar-2003 open boundary control added

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

      call exf_set_obcs_xz(  OBNu, OBNu0, OBNu1, OBNufile, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNv, OBNv0, OBNv1, OBNvfile, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNt, OBNt0, OBNt1, OBNtfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBNs, OBNs0, OBNs1, OBNsfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_OBCS_SOUTH
      call exf_getffieldrec(
     I                       obcsSstartdate, obcsSperiod
     O                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      call exf_set_obcs_xz(  OBSu, OBSu0, OBSu1, OBSufile, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSv, OBSv0, OBSv1, OBSvfile, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSt, OBSt0, OBSt1, OBStfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBSs, OBSs0, OBSs1, OBSsfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )

#endif

#ifdef ALLOW_OBCS_EAST
      call exf_getffieldrec(
     I                       obcsEstartdate, obcsEperiod
     O                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      call exf_set_obcs_yz(  OBEu, OBEu0, OBEu1, OBEufile, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEv, OBEv0, OBEv1, OBEvfile, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEt, OBEt0, OBEt1, OBEtfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBEs, OBEs0, OBEs1, OBEsfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )

#endif

#ifdef ALLOW_OBCS_WEST
      call exf_getffieldrec(
     I                       obcsWstartdate, obcsWperiod
     O                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      call exf_set_obcs_yz(  OBWu, OBWu0, OBWu1, OBWufile, 'u'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWv, OBWv0, OBWv1, OBWvfile, 'v'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWt, OBWt0, OBWt1, OBWtfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBWs, OBWs0, OBWs1, OBWsfile, 's'
     I                     , fac, first, changed, count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid )

#endif

#ifdef ALLOW_OBCS_CONTROL
cgg   WARNING: Assuming North Open Boundary exists and has same
cgg    calendar information as other boundaries.
      call ctrl_obcsbal ( mycurrenttime,mycurrentiter,mythid )
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

