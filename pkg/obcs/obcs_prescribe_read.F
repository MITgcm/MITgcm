C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_prescribe_read.F,v 1.7 2005/10/10 05:53:48 mlosch Exp $
C $Name:  $

# include "OBCS_OPTIONS.h"

      subroutine obcs_prescribe_read ( 
     I                      mycurrenttime
     I                    , mycurrentiter
     I                    , mythid
     &                     )
c     |==================================================================|
c     | SUBROUTINE obcs_prescribe_read                                   |
c     |==================================================================|
c     | read open boundary conditions from file                          |
c     | N.B.: * uses exf and cal routines for file/record handling       |
c     |       * uses ctrl routines for control variable handling         |
c     |==================================================================|

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "OBCS.h"
#ifdef ALLOW_EXF
# include "exf_param.h"
#endif
#ifdef ALLOW_PTRACERS.h
# include "PTRACERS_SIZE.h"
# include "PTRACERS.h"
# include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

c     == routine arguments ==

      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#if (defined (ALLOW_OBCS) && defined (ALLOW_OBCS_PRESCRIBE))

c     == local variables ==

      logical first, changed
      integer count0, count1
      integer year0, year1
      _RL     fac
#ifdef ALLOW_PTRACERS
      integer iTracer, i,j,k
#endif /* ALLOW_PTRACERS */

c     == end of interface ==

#ifdef ALLOW_EXF
#ifdef ALLOW_OBCS_NORTH
      call exf_getffieldrec(
     I                       obcsNstartdate, obcsNperiod
     I                     , obcsNstartdate1, obcsNstartdate2
     I                     , .false.
     O                     , fac, first, changed
     O                     , count0, count1, year0, year1
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
#ifdef ALLOW_PTRACERS
      if ( usePTRACERS ) then
       do itracer = 1, PTRACERS_numInUse
        call exf_set_obcs_xz(  OBNptr (1-Olx,1,1,1,iTracer)
     I                       , OBNptr0(1-Olx,1,1,1,iTracer)
     I                       , OBNptr1(1-Olx,1,1,1,iTracer) 
     I                       , OBNptrFile(iTracer), 's'
     I                       , fac, first, changed, count0, count1
     I                       , mycurrenttime, mycurrentiter, mythid )
       enddo
      endif
#endif /* ALLOW_PTRACERS */
#endif

#ifdef ALLOW_OBCS_SOUTH
      call exf_getffieldrec(
     I                       obcsSstartdate, obcsSperiod
     I                     , obcsSstartdate1, obcsSstartdate2
     I                     , .false.
     O                     , fac, first, changed
     O                     , count0, count1, year0, year1
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

#ifdef ALLOW_PTRACERS
      if ( usePTRACERS ) then
       do itracer = 1, PTRACERS_numInUse
        call exf_set_obcs_xz(  OBSptr (1-Olx,1,1,1,iTracer)
     I                       , OBSptr0(1-Olx,1,1,1,iTracer)
     I                       , OBSptr1(1-Olx,1,1,1,iTracer) 
     I                       , OBSptrFile(iTracer), 's'
     I                       , fac, first, changed, count0, count1
     I                       , mycurrenttime, mycurrentiter, mythid )
       enddo
      endif
#endif /* ALLOW_PTRACERS */
#endif

#ifdef ALLOW_OBCS_EAST
      call exf_getffieldrec(
     I                       obcsEstartdate, obcsEperiod
     I                     , obcsEstartdate1, obcsEstartdate2
     I                     , .false.
     O                     , fac, first, changed
     O                     , count0, count1, year0, year1
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
#ifdef ALLOW_PTRACERS
      if ( usePTRACERS ) then
       do itracer = 1, PTRACERS_numInUse
        call exf_set_obcs_yz(  OBEptr (1-Oly,1,1,1,iTracer)
     I                       , OBEptr0(1-Oly,1,1,1,iTracer)
     I                       , OBEptr1(1-Oly,1,1,1,iTracer) 
     I                       , OBEptrFile(iTracer), 's'
     I                       , fac, first, changed, count0, count1
     I                       , mycurrenttime, mycurrentiter, mythid )
       enddo
      endif
#endif /* ALLOW_PTRACERS */
#endif

#ifdef ALLOW_OBCS_WEST
      call exf_getffieldrec(
     I                       obcsWstartdate, obcsWperiod
     I                     , obcsWstartdate1, obcsWstartdate2
     I                     , .false.
     O                     , fac, first, changed
     O                     , count0, count1, year0, year1
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
#ifdef ALLOW_PTRACERS
      if ( usePTRACERS ) then
       do itracer = 1, PTRACERS_numInUse
        call exf_set_obcs_yz(  OBWptr (1-Oly,1,1,1,iTracer)
     I                       , OBWptr0(1-Oly,1,1,1,iTracer)
     I                       , OBWptr1(1-Oly,1,1,1,iTracer) 
     I                       , OBWptrFile(iTracer), 's'
     I                       , fac, first, changed, count0, count1
     I                       , mycurrenttime, mycurrentiter, mythid )
       enddo
      endif
#endif /* ALLOW_PTRACERS */
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

#else /* not ALLOW_EXF */
      CALL OBCS_EXTERNAL_FIELDS_LOAD( 
     &     myCurrentTime, myCurrentIter, myThid )
#endif /*  ALLOw_EXF */

#endif /* ALLOW_OBCS */

      RETURN
      END
