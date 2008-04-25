C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_prescribe_read.F,v 1.22 2008/04/25 21:57:49 dimitri Exp $
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

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

c     == routine arguments ==

      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#if (defined (ALLOW_OBCS) && defined (ALLOW_OBCS_PRESCRIBE))

c     == local variables ==

c     == end of interface ==

# ifdef ALLOW_EXF
      IF ( useEXF ) THEN
#  ifdef ALLOW_OBCS_NORTH
      call obcs_prescribe_exf_xz (
     I     obcsNstartdate, obcsNperiod,
     I     useOBCSYearlyFields,
     U     OBNu,   OBNu0,   OBNu1,   OBNufile, 
     U     OBNv,   OBNv0,   OBNv1,   OBNvfile, 
     U     OBNt,   OBNt0,   OBNt1,   OBNtfile, 
     U     OBNs,   OBNs0,   OBNs1,   OBNsfile, 
#   ifdef ALLOW_SEAICE
     I     siobNstartdate, siobNperiod,
     U     OBNa,   OBNa0,   OBNa1,   OBNafile,
     U     OBNh,   OBNh0,   OBNh1,   OBNhfile,
     U     OBNsl,  OBNsl0,  OBNsl1,  OBNslfile,
     U     OBNsn,  OBNsn0,  OBNsn1,  OBNsnfile,
     U     OBNuice,OBNuice0,OBNuice1,OBNuicefile,
     U     OBNvice,OBNvice0,OBNvice1,OBNvicefile,
#   endif
#   ifdef ALLOW_PTRACERS
     U     OBNptr ,OBNptr0, OBNptr1, OBNptrFile,
#   endif
     I     mycurrenttime, mycurrentiter, mythid
     &     )
#  endif /* ALLOW_OBCS_NORTH */

#  ifdef ALLOW_OBCS_SOUTH
      call obcs_prescribe_exf_xz (
     I     obcsSstartdate, obcsSperiod,
     I     useOBCSYearlyFields,
     U     OBSu,   OBSu0,   OBSu1,   OBSufile, 
     U     OBSv,   OBSv0,   OBSv1,   OBSvfile, 
     U     OBSt,   OBSt0,   OBSt1,   OBStfile, 
     U     OBSs,   OBSs0,   OBSs1,   OBSsfile, 
#   ifdef ALLOW_SEAICE
     I     siobSstartdate, siobSperiod,
     U     OBSa,   OBSa0,   OBSa1,   OBSafile,
     U     OBSh,   OBSh0,   OBSh1,   OBShfile,
     U     OBSsl,  OBSsl0,  OBSsl1,  OBSslfile,
     U     OBSsn,  OBSsn0,  OBSsn1,  OBSsnfile,
     U     OBSuice,OBSuice0,OBSuice1,OBSuicefile,
     U     OBSvice,OBSvice0,OBSvice1,OBSvicefile,
#   endif
#   ifdef ALLOW_PTRACERS
     U     OBSptr ,OBSptr0, OBSptr1, OBSptrFile,
#   endif
     I     mycurrenttime, mycurrentiter, mythid
     &     )
#  endif /* ALLOW_OBCS_SOUTH */

#  ifdef ALLOW_OBCS_EAST
      call obcs_prescribe_exf_yz (
     I     obcsEstartdate, obcsEperiod,
     I     useOBCSYearlyFields,
     U     OBEu,   OBEu0,   OBEu1,   OBEufile, 
     U     OBEv,   OBEv0,   OBEv1,   OBEvfile, 
     U     OBEt,   OBEt0,   OBEt1,   OBEtfile, 
     U     OBEs,   OBEs0,   OBEs1,   OBEsfile, 
#   ifdef ALLOW_SEAICE
     I     siobEstartdate, siobEperiod,
     U     OBEa,   OBEa0,   OBEa1,   OBEafile,
     U     OBEh,   OBEh0,   OBEh1,   OBEhfile,
     U     OBEsl,  OBEsl0,  OBEsl1,  OBEslfile,
     U     OBEsn,  OBEsn0,  OBEsn1,  OBEsnfile,
     U     OBEuice,OBEuice0,OBEuice1,OBEuicefile,
     U     OBEvice,OBEvice0,OBEvice1,OBEvicefile,
#   endif
#   ifdef ALLOW_PTRACERS
     U     OBEptr ,OBEptr0, OBEptr1, OBEptrFile,
#   endif
     I     mycurrenttime, mycurrentiter, mythid
     &     )
#  endif /* ALLOW_OBCS_EAST */

#  ifdef ALLOW_OBCS_WEST
      call obcs_prescribe_exf_yz (
     I     obcsWstartdate, obcsWperiod,
     I     useOBCSYearlyFields,
     U     OBWu,   OBWu0,   OBWu1,   OBWufile, 
     U     OBWv,   OBWv0,   OBWv1,   OBWvfile, 
     U     OBWt,   OBWt0,   OBWt1,   OBWtfile, 
     U     OBWs,   OBWs0,   OBWs1,   OBWsfile, 
#   ifdef ALLOW_SEAICE
     I     siobEstartdate, siobEperiod,
     U     OBWa,   OBWa0,   OBWa1,   OBWafile,
     U     OBWh,   OBWh0,   OBWh1,   OBWhfile,
     U     OBWsl,  OBWsl0,  OBWsl1,  OBWslfile,
     U     OBWsn,  OBWsn0,  OBWsn1,  OBWsnfile,
     U     OBWuice,OBWuice0,OBWuice1,OBWuicefile,
     U     OBWvice,OBWvice0,OBWvice1,OBWvicefile,
#   endif
#   ifdef ALLOW_PTRACERS
     U     OBWptr ,OBWptr0, OBWptr1, OBWptrFile,
#   endif
     I     mycurrenttime, mycurrentiter, mythid
     &     )
#  endif /* ALLOW_OBCS_WEST */
C     ENDIF useEXF
      ENDIF
# endif /* ALLOW_EXF */

# ifdef ALLOW_OBCS_CONTROL
cgg   WARNING: Assuming North Open Boundary exists and has same
cgg    calendar information as other boundaries.
      call ctrl_obcsbal ( mycurrenttime,mycurrentiter,mythid )
# endif

# ifdef ALLOW_OBCSN_CONTROL
      call ctrl_getobcsn ( mycurrenttime, mycurrentiter, mythid )
# endif

# ifdef ALLOW_OBCSS_CONTROL
      call ctrl_getobcss ( mycurrenttime, mycurrentiter, mythid )
# endif

# ifdef ALLOW_OBCSW_CONTROL
      call ctrl_getobcsw ( mycurrenttime, mycurrentiter, mythid )
# endif

# ifdef ALLOW_OBCSE_CONTROL
      call ctrl_getobcse ( mycurrenttime, mycurrentiter, mythid )
# endif

      IF ( .NOT. useEXF ) THEN
#ifndef ALLOW_AUTODIFF_TAMC
       CALL OBCS_EXTERNAL_FIELDS_LOAD(
     &     myCurrentTime, myCurrentIter, myThid )
#else
       STOP 'PH HAS DISABLED THIS RUNTIME OPTION FOR ALLOW_EXF'
#endif
      ENDIF

#endif /* ALLOW_OBCS and ALLOW_OBCS_PRESCRIBE */

      RETURN
      END


C=========================================================================
C=========================================================================

      subroutine obcs_prescribe_exf_xz (
     I     obcsstartdate, obcsperiod,
     I     useYearlyFields,
     U     OBu,   OBu0,   OBu1,   OBufile, 
     U     OBv,   OBv0,   OBv1,   OBvfile, 
     U     OBt,   OBt0,   OBt1,   OBtfile, 
     U     OBs,   OBs0,   OBs1,   OBsfile, 
#ifdef ALLOW_SEAICE
     I     siobstartdate, siobperiod,
     U     OBa,   OBa0,   OBa1,   OBafile,
     U     OBh,   OBh0,   OBh1,   OBhfile,
     U     OBsl,  OBsl0,  OBsl1,  OBslfile,
     U     OBsn,  OBsn0,  OBsn1,  OBsnfile,
     U     OBuice,OBuice0,OBuice1,OBuicefile,
     U     OBvice,OBvice0,OBvice1,OBvicefile,
#endif
#ifdef ALLOW_PTRACERS
     U     OBptr ,OBptr0, OBptr1, OBptrFile,
#endif
     I     mycurrenttime, mycurrentiter, mythid
     &     )
c     |==================================================================|
c     | SUBROUTINE obcs_prescribe_exf_xz                                 |
c     |==================================================================|
c     | read open boundary conditions from file                          |
c     | N.B.: * uses exf and cal routines for file/record handling       |
c     |       * uses ctrl routines for control variable handling         |
c     |==================================================================|

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif /* ALLOW_PTRACERS */

c     == routine arguments ==

      _RL     obcsstartdate
      _RL     obcsperiod
      LOGICAL useYearlyFields
      _RL OBu     (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBv     (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBt     (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBs     (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBu0    (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBv0    (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBt0    (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBs0    (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBu1    (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBv1    (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBt1    (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBs1    (1-Olx:sNx+Olx,Nr,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) OBuFile,OBvFile,OBtFile,OBsFile
#ifdef ALLOW_SEAICE
      _RL     siobstartdate
      _RL     siobperiod
      _RL OBa     (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBh     (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBa0    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBh0    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBa1    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBh1    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBsl    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBsn    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBsl0   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBsn0   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBsl1   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBsn1   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBuice  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBvice  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBuice0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBvice0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBuice1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBvice1 (1-Olx:sNx+Olx,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM)
     &     OBaFile,OBhFile,OBslFile,OBsnFile,OBuiceFile,OBviceFile
#endif /* ALLOW_SEAICE */
#ifdef ALLOW_PTRACERS
      _RL OBptr (1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
      _RL OBptr0(1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
      _RL OBptr1(1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) OBptrFile(PTRACERS_num)
#endif /* ALLOW_PTRACERS */
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#if defined ALLOW_OBCS && defined ALLOW_OBCS_PRESCRIBE \
    && defined ALLOW_EXF

c     == local variables ==
      logical first, changed
      integer count0, count1
      integer year0, year1
      _RL     fac
# ifdef ALLOW_PTRACERS
      integer iTracer
# endif /* ALLOW_PTRACERS */

c     == end of interface ==
      if ( obcsperiod .eq. -12 ) then
c     obcsperiod=-12 means input file contains 12 monthly means
c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
       call cal_GetMonthsRec(
     O                        fac, first, changed,
     O                        count0, count1,
     I                        mycurrenttime, mycurrentiter, mythid
     &           )

      elseif ( obcsperiod .lt. 0 ) then
       print *, 'obcsperiod is out of range'
       STOP 'ABNORMAL END: S/R OBCS_PRESCIBE_EXF_XZ'
      else
c     get record numbers and interpolation factor
       call exf_GetFFieldRec(
     I                       obcsstartdate, obcsperiod,
     I                       useYearlyFields,
     O                       fac, first, changed,
     O                       count0, count1, year0, year1,
     I                       mycurrenttime, mycurrentiter, mythid
     &                      )
# ifdef ALLOW_SEAICE
       IF (useSEAICE) THEN
        call exf_GetFFieldRec(
     I                       siobstartdate, siobperiod,
     I                       useYearlyFields,
     O                       fac, first, changed,
     O                       count0, count1, year0, year1,
     I                       mycurrenttime, mycurrentiter, mythid
     &                      )
       ENDIF
# endif /* ALLOW_SEAICE */
      endif

      call exf_set_obcs_xz(  OBu, OBu0, OBu1, OBufile, 'u'
     I                     , fac, first, changed, useYearlyFields
     I                     , obcsperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBv, OBv0, OBv1, OBvfile, 'v'
     I                     , fac, first, changed, useYearlyFields
     I                     , obcsperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBt, OBt0, OBt1, OBtfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , obcsperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_xz(  OBs, OBs0, OBs1, OBsfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , obcsperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       call exf_set_obcs_x (  OBa, OBa0, OBa1, OBafile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_x (  OBh, OBh0, OBh1, OBhfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_x (  OBsl, OBsl0, OBsl1, OBslfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_x (  OBsn, OBsn0, OBsn1, OBsnfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_x ( OBuice,OBuice0,OBuice1,OBuicefile,'u'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_x ( OBvice,OBvice0,OBvice1,OBvicefile,'v'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
      ENDIF
# endif /* ALLOW_SEAICE */
# ifdef ALLOW_PTRACERS
      if ( usePTRACERS ) then
       do iTracer = 1, PTRACERS_numInUse
        call exf_set_obcs_xz(  OBptr (1-Olx,1,1,1,iTracer)
     I                       , OBptr0(1-Olx,1,1,1,iTracer)
     I                       , OBptr1(1-Olx,1,1,1,iTracer)
     I                       , OBptrFile(iTracer), 's'
     I                       , fac, first, changed, useYearlyFields
     I                       , obcsperiod, count0, count1, year0, year1
     I                       , mycurrenttime, mycurrentiter, mythid )
       enddo
      endif
# endif /* ALLOW_PTRACERS */

#endif /* ALLOW_OBCS and ALLOW_OBCS_PRESCRIBE and ALLOW_EXF */
      RETURN
      END
C=========================================================================
C=========================================================================

      subroutine obcs_prescribe_exf_yz (
     I     obcsstartdate, obcsperiod,
     I     useYearlyFields,
     U     OBu,   OBu0,   OBu1,   OBufile, 
     U     OBv,   OBv0,   OBv1,   OBvfile, 
     U     OBt,   OBt0,   OBt1,   OBtfile, 
     U     OBs,   OBs0,   OBs1,   OBsfile, 
#ifdef ALLOW_SEAICE
     I     siobstartdate, siobperiod,
     U     OBa,   OBa0,   OBa1,   OBafile,
     U     OBh,   OBh0,   OBh1,   OBhfile,
     U     OBsl,  OBsl0,  OBsl1,  OBslfile,
     U     OBsn,  OBsn0,  OBsn1,  OBsnfile,
     U     OBuice,OBuice0,OBuice1,OBuicefile,
     U     OBvice,OBvice0,OBvice1,OBvicefile,
#endif
#ifdef ALLOW_PTRACERS
     U     OBptr ,OBptr0, OBptr1, OBptrFile,
#endif
     I     mycurrenttime, mycurrentiter, mythid
     &     )
c     |==================================================================|
c     | SUBROUTINE obcs_prescribe_exf_yz                                 |
c     |==================================================================|
c     | read open boundary conditions from file                          |
c     | N.B.: * uses exf and cal routines for file/record handling       |
c     |       * uses ctrl routines for control variable handling         |
c     |==================================================================|

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif /* ALLOW_PTRACERS */

c     == routine arguments ==

      _RL     obcsstartdate
      _RL     obcsperiod
      LOGICAL useYearlyFields
      _RL OBu     (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBv     (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBt     (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBs     (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBu0    (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBv0    (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBt0    (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBs0    (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBu1    (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBv1    (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBt1    (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBs1    (1-Oly:sNy+Oly,Nr,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) OBuFile,OBvFile,OBtFile,OBsFile
#ifdef ALLOW_SEAICE
      _RL     siobstartdate
      _RL     siobperiod
      _RL OBa     (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBh     (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBa0    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBh0    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBa1    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBh1    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBsl    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBsn    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBsl0   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBsn0   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBsl1   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBsn1   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBuice  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBvice  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBuice0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBvice0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBuice1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBvice1 (1-Oly:sNy+Oly,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM)
     &     OBaFile,OBhFile,OBslFile,OBsnFile,OBuiceFile,OBviceFile
#endif /* ALLOW_SEAICE */
#ifdef ALLOW_PTRACERS
      _RL OBptr (1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
      _RL OBptr0(1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
      _RL OBptr1(1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) OBptrFile(PTRACERS_num)
#endif /* ALLOW_PTRACERS */
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#if defined ALLOW_OBCS && defined ALLOW_OBCS_PRESCRIBE \
    && defined ALLOW_EXF

c     == local variables ==
      logical first, changed
      integer count0, count1
      integer year0, year1
      _RL     fac
# ifdef ALLOW_PTRACERS
      integer iTracer
# endif /* ALLOW_PTRACERS */

c     == end of interface ==
      if ( obcsperiod .eq. -12 ) then
c     obcsperiod=-12 means input file contains 12 monthly means
c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
       call cal_GetMonthsRec(
     O                        fac, first, changed,
     O                        count0, count1,
     I                        mycurrenttime, mycurrentiter, mythid
     &           )

      elseif ( obcsperiod .lt. 0 ) then
       print *, 'obcsperiod is out of range'
       STOP 'ABNORMAL END: S/R OBCS_PRESCIBE_EXF_YZ'
      else
c     get record numbers and interpolation factor
       call exf_GetFFieldRec(
     I                       obcsstartdate, obcsperiod,
     I                       useYearlyFields,
     O                       fac, first, changed,
     O                       count0, count1, year0, year1,
     I                       mycurrenttime, mycurrentiter, mythid
     &                      )
# ifdef ALLOW_SEAICE
       IF (useSEAICE) THEN
        call exf_GetFFieldRec(
     I                       siobstartdate, siobperiod,
     I                       useYearlyFields,
     O                       fac, first, changed,
     O                       count0, count1, year0, year1,
     I                       mycurrenttime, mycurrentiter, mythid
     &                      )
       ENDIF
# endif /* ALLOW_SEAICE */
      endif

      call exf_set_obcs_yz(  OBu, OBu0, OBu1, OBufile, 'u'
     I                     , fac, first, changed, useYearlyFields
     I                     , obcsperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBv, OBv0, OBv1, OBvfile, 'v'
     I                     , fac, first, changed, useYearlyFields
     I                     , obcsperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBt, OBt0, OBt1, OBtfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , obcsperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
      call exf_set_obcs_yz(  OBs, OBs0, OBs1, OBsfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , obcsperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       call exf_set_obcs_y (  OBa, OBa0, OBa1, OBafile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_y (  OBh, OBh0, OBh1, OBhfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_y (  OBsl, OBsl0, OBsl1, OBslfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_y (  OBsn, OBsn0, OBsn1, OBsnfile, 's'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_y ( OBuice,OBuice0,OBuice1,OBuicefile,'u'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
       call exf_set_obcs_y ( OBvice,OBvice0,OBvice1,OBvicefile,'v'
     I                     , fac, first, changed, useYearlyFields
     I                     , siobperiod, count0, count1, year0, year1
     I                     , mycurrenttime, mycurrentiter, mythid )
      ENDIF
# endif /* ALLOW_SEAICE */
# ifdef ALLOW_PTRACERS
      if ( usePTRACERS ) then
       do iTracer = 1, PTRACERS_numInUse
        call exf_set_obcs_yz(  OBptr (1-Olx,1,1,1,iTracer)
     I                       , OBptr0(1-Olx,1,1,1,iTracer)
     I                       , OBptr1(1-Olx,1,1,1,iTracer)
     I                       , OBptrFile(iTracer), 's'
     I                       , fac, first, changed, useYearlyFields
     I                       , obcsperiod, count0, count1, year0, year1
     I                       , mycurrenttime, mycurrentiter, mythid )
       enddo
      endif
# endif /* ALLOW_PTRACERS */

#endif /* ALLOW_OBCS and ALLOW_OBCS_PRESCRIBE and ALLOW_EXF */
      RETURN
      END
