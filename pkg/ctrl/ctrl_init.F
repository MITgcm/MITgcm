C
C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_init.F,v 1.10 2003/10/23 04:41:40 edhill Exp $
C $Name:  $

#include "AD_CONFIG.h"
#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_init( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_init
c     ==================================================================
c
c     o Set parts of the vector of control variables and initialize the
c       rest to zero.
c
c       The vector of control variables is initialized here. The
c       temperature and salinity contributions are read from file.
c       Subsequently, the latter are dimensionalized and the tile
c       edges are updated.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 23-Feb-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Patrick Heimbach heimbach@mit.edu 30-May-2000
c              - diffsec was falsely declared.
c
c              Patrick Heimbach heimbach@mit.edu 06-Jun-2000
c              - Transferred some filename declarations
c                from ctrl_pack/ctrl_unpack to here
c              - Transferred mask-per-tile to here
c              - computation of control vector length here
c
c              Patrick Heimbach heimbach@mit.edu 16-Jun-2000
c              - Added call to ctrl_pack
c              - Alternatively: transfer writing of scale files to
c                ctrl_unpack
c
c              Dimitris Menemenlis menemenlis@mit.edu 7-Mar-2003
c              - To be consistent with usage in ctrl_getrec.F,
c                startrec and endrec need to be referenced to
c                model time = 0, not to startTime.
c                Also "- modelstep" -> "+ modelstep/2":
c old:      startrec = int((modelstart - diffsecs)/
c old:   &                 xx_???period) + 1
c old:      endrec = int((modelend   - diffsecs - modelstep)/
c old:   &                 xx_???period) + 2
c new:      startrec = int((modelstart + startTime - diffsecs)/
c new:   &                 xx_???period) + 1
c new:      endrec = int((modelend + startTime - diffsecs + modelstep/2)/
c new:   &                 xx_???period) + 2
c
c     ==================================================================
c     SUBROUTINE ctrl_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ctrl.h"

#ifdef ALLOW_CALENDAR
# include "cal.h"
#endif
#ifdef ALLOW_OBCS_CONTROL
# include "OBCS.h"
#endif
#ifdef ALLOW_ECCO_OPTIMIZATION
# include "optim.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ntmp
      integer ivarindex

      integer iobcs
      integer il
      integer errio
      integer startrec
      integer endrec
      integer difftime(4)
      _RL     diffsecs
      _RL     dummy

      character*(80)   ymaskobcs
      character*(max_len_prec) record
      character*(max_len_mbuf) msgbuf

      integer nwetc3d

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Set loop ranges.
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx


      _BEGIN_MASTER( myThid )

#ifdef ALLOW_CALENDAR

c--     Get the complete dates of the control variables.
#if     (defined  (ALLOW_HFLUX_CONTROL))
c--     The heat flux contribution.
        call cal_FullDate( xx_hfluxstartdate1, xx_hfluxstartdate2,
     &                     xx_hfluxstartdate , mythid )
#elif   (defined  (ALLOW_ATEMP_CONTROL))
c--     Atmos. temperature contribution.
        call cal_FullDate( xx_atempstartdate1, xx_atempstartdate2,
     &                     xx_atempstartdate , mythid )
#endif

#if     (defined  (ALLOW_SFLUX_CONTROL))
c--     The salt flux contribution.
        call cal_FullDate( xx_sfluxstartdate1, xx_sfluxstartdate2,
     &                     xx_sfluxstartdate , mythid )
#elif   (defined  (ALLOW_AQH_CONTROL))
c--     Atmospheric humidity contribution.
        call cal_FullDate( xx_aqhstartdate1, xx_aqhstartdate2,
     &                     xx_aqhstartdate , mythid )
#endif

#if     (defined  (ALLOW_USTRESS_CONTROL))
c--     The zonal wind stress contribution.
        call cal_FullDate( xx_tauustartdate1,  xx_tauustartdate2,
     &                     xx_tauustartdate,   mythid )
#elif   (defined  (ALLOW_UWIND_CONTROL))
c--     Zonal wind speed contribution.
        call cal_FullDate( xx_uwindstartdate1, xx_uwindstartdate2,
     &                     xx_uwindstartdate , mythid )
#endif

#if     (defined  (ALLOW_VSTRESS_CONTROL))
c--     The merid. wind stress contribution.
        call cal_FullDate( xx_tauvstartdate1,  xx_tauvstartdate2,
     &                     xx_tauvstartdate,   mythid )
#elif   (defined  (ALLOW_VWIND_CONTROL))
c--     Merid. wind speed contribution.
        call cal_FullDate( xx_vwindstartdate1, xx_vwindstartdate2,
     &                     xx_vwindstartdate , mythid )
#endif

#ifdef ALLOW_OBCS_CONTROL
        call cal_FullDate( xx_obcsnstartdate1,  xx_obcsnstartdate2,
     &                     xx_obcsnstartdate,   mythid )
        call cal_FullDate( xx_obcssstartdate1,  xx_obcssstartdate2,
     &                     xx_obcssstartdate,   mythid )
        call cal_FullDate( xx_obcswstartdate1,  xx_obcswstartdate2,
     &                     xx_obcswstartdate,   mythid )
        call cal_FullDate( xx_obcsestartdate1,  xx_obcsestartdate2,
     &                     xx_obcsestartdate,   mythid )
#endif

#endif /* ALLOW_CALENDAR */

c--     Set default values.
        do ivarindex = 1,maxcvars
          ncvarindex(ivarindex) = -1
          ncvarrecs(ivarindex)  =  0
          ncvarxmax(ivarindex)  =  0
          ncvarymax(ivarindex)  =  0
          ncvarnrmax(ivarindex) =  0
          ncvargrd(ivarindex)   = '?'
        enddo

        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)')
     &    ' ctrl_init: Initializing temperature and salinity'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)')
     &    '                   part of the control vector.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    '                   The initial surface fluxes are set',
     &                      ' to zero.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )

      _BARRIER

c--   =====================
c--   Initial state fields.
c--   =====================

cph(
cph    index  7-10 reserved for atmos. state,
cph    index 11-14 reserved for open boundaries,
cph    index 15-16 reserved for mixing coeff.
cph    index 17    reserved for passive tracer TR1
cph    index 18,19 reserved for sst, sss
cph    index 20             for hFacC
cph    index 21-22          for efluxy, efluxp
cph    index 23-24          for bottom drag
cph)

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_THETA0_CONTROL
c--   Initial state temperature contribution.

      _BEGIN_MASTER( mythid )
        ivarindex             = 1
        ncvarindex(ivarindex) = 101
        ncvarrecs(ivarindex)  =   1
        ncvarxmax(ivarindex)  = snx
        ncvarymax(ivarindex)  = sny
        ncvarnrmax(ivarindex) =  nr
        ncvargrd(ivarindex)   = 'c'
      _END_MASTER( mythid )

#endif /* ALLOW_THETA0_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_SALT0_CONTROL
c--   Initial state salinity contribution.

      _BEGIN_MASTER( mythid )
        ivarindex             = 2
        ncvarindex(ivarindex) = 102
        ncvarrecs(ivarindex)  =   1
        ncvarxmax(ivarindex)  = snx
        ncvarymax(ivarindex)  = sny
        ncvarnrmax(ivarindex) =  nr
        ncvargrd(ivarindex)   = 'c'
      _END_MASTER( mythid )

#endif /* ALLOW_SALT0_CONTROL */

c--   ===========================
c--   Surface flux contributions.
c--   ===========================

c-------------------------------------------------------------------------------
c--
#if (defined (ALLOW_HFLUX_CONTROL))
c--   Heat flux.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_hfluxstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_hfluxperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_hfluxperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 3
        ncvarindex(ivarindex)    = 103
        ncvarrecs(ivarindex)     = endrec - startrec + 1
        ncvarrecstart(ivarindex) = startrec
        ncvarrecsend(ivarindex)  = endrec
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd(ivarindex)      = 'c'
      _END_MASTER( mythid )

#elif (defined (ALLOW_ATEMP_CONTROL))
c--   Atmos. temperature

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_atempstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_atempperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_atempperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 7
        ncvarindex(ivarindex)    = 107
        ncvarrecs(ivarindex)     = endrec - startrec + 1
        ncvarrecstart(ivarindex) = startrec
        ncvarrecsend(ivarindex)  = endrec
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd(ivarindex)      = 'c'
      _END_MASTER( mythid )

#elif (defined (ALLOW_HFLUX0_CONTROL))
c--   initial forcing only
      _BEGIN_MASTER( mythid )
        ncvarindex(3) = 103
        ncvarrecs(3)  =   1
        ncvarxmax(3)  = snx
        ncvarymax(3)  = sny
        ncvarnrmax(3) =   1
        ncvargrd(3)   = 'c'
      _END_MASTER( mythid )

#endif /* ALLOW_HFLUX_CONTROL */

c-------------------------------------------------------------------------------
c--
#if (defined (ALLOW_SFLUX_CONTROL))
c--   Salt flux.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_sfluxstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_sfluxperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_sfluxperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 4
        ncvarindex(ivarindex)    = 104
        ncvarrecs(ivarindex)     = endrec - startrec + 1
        ncvarrecstart(ivarindex) = startrec
        ncvarrecsend(ivarindex)  = endrec
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd(ivarindex)      = 'c'
      _END_MASTER( mythid )

#elif (defined (ALLOW_AQH_CONTROL))
c--   Atmos. humidity

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_aqhstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_aqhperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_aqhperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 8
        ncvarindex(ivarindex)    = 108
        ncvarrecs(ivarindex)     = endrec - startrec + 1
        ncvarrecstart(ivarindex) = startrec
        ncvarrecsend(ivarindex)  = endrec
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd(ivarindex)      = 'c'
      _END_MASTER( mythid )

#elif (defined (ALLOW_SFLUX0_CONTROL))
c--   initial forcing only
      _BEGIN_MASTER( mythid )
        ncvarindex(4) = 104
        ncvarrecs(4)  =   1
        ncvarxmax(4)  = snx
        ncvarymax(4)  = sny
        ncvarnrmax(4) =   1
        ncvargrd(4)   = 'c'
      _END_MASTER( mythid )

#endif /* ALLOW_SFLUX_CONTROL */

c-------------------------------------------------------------------------------
c--
#if (defined (ALLOW_USTRESS_CONTROL))
c--   Zonal wind stress.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_tauustartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_tauuperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_tauuperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 5
        ncvarindex(ivarindex)    = 105
        ncvarrecs(ivarindex)     = endrec - startrec + 1
        ncvarrecstart(ivarindex) = startrec
        ncvarrecsend(ivarindex)  = endrec
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd(ivarindex)      = 'w'
      _END_MASTER( mythid )

#elif (defined (ALLOW_UWIND_CONTROL))
c--   Zonal wind speed.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_uwindstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_uwindperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_uwindperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 9
        ncvarindex(ivarindex)    = 109
        ncvarrecs(ivarindex)     = endrec - startrec + 1
        ncvarrecstart(ivarindex) = startrec
        ncvarrecsend(ivarindex)  = endrec
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd(ivarindex)      = 'w'
      _END_MASTER( mythid )

#elif (defined (ALLOW_TAUU0_CONTROL))
c--   initial forcing only
      _BEGIN_MASTER( mythid )
        ncvarindex(5) = 105
        ncvarrecs(5)  =   1
        ncvarxmax(5)  = snx
        ncvarymax(5)  = sny
        ncvarnrmax(5) =   1
        ncvargrd(5)   = 'w'
      _END_MASTER( mythid )

#endif /* ALLOW_USTRESS_CONTROL */

c-------------------------------------------------------------------------------
c--
#if (defined (ALLOW_VSTRESS_CONTROL))
c--   Meridional wind stress.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_tauvstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_tauvperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_tauvperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 6
        ncvarindex(ivarindex)    = 106
        ncvarrecs(ivarindex)     = endrec - startrec + 1
        ncvarrecstart(ivarindex) = startrec
        ncvarrecsend(ivarindex)  = endrec
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd(ivarindex)      = 's'
      _END_MASTER( mythid )

#elif (defined (ALLOW_VWIND_CONTROL))
c--   Meridional wind speed.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_vwindstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_vwindperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_vwindperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 10
        ncvarindex(ivarindex)    = 110
        ncvarrecs(ivarindex)     = endrec - startrec + 1
        ncvarrecstart(ivarindex) = startrec
        ncvarrecsend(ivarindex)  = endrec
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd(ivarindex)      = 's'
      _END_MASTER( mythid )

#elif (defined (ALLOW_TAUV0_CONTROL))
c--   initial forcing only
      _BEGIN_MASTER( mythid )
        ncvarindex(6) = 106
        ncvarrecs(6)  =   1
        ncvarxmax(6)  = snx
        ncvarymax(6)  = sny
        ncvarnrmax(6) =   1
        ncvargrd(6)   = 's'
      _END_MASTER( mythid )

#endif /* ALLOW_VSTRESS_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSN_CONTROL
c--   Northern obc.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_obcsnstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
cgg   O.B. future values are needed at the last timestep, so lets
cgg   take this into account.
        startrec = int((modelstart - diffsecs)/xx_obcsnperiod) + 1
        endrec   = int((modelend   - diffsecs)/xx_obcsnperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 11
        ncvarindex(ivarindex)    = 111
cgg( Implement heimbach fix for nobcs.
        ncvarrecs(ivarindex)     = (endrec - startrec + 1)*nobcs
        ncvarrecstart(ivarindex) = (startrec - 1)*nobcs + 1
        ncvarrecsend(ivarindex)  = endrec*nobcs
cgg)
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     =   1
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd(ivarindex)      = 'm'
      _END_MASTER( mythid )

#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL
c--   Southern obc.

c-------------------------------------------------------------------------------
c--
      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_obcssstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
cgg   O.B. future values are needed at the last timestep, so lets
cgg   take this into account.
        startrec = int((modelstart - diffsecs)/xx_obcssperiod) + 1
        endrec   = int((modelend   - diffsecs)/xx_obcssperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 12
        ncvarindex(ivarindex)    = 112
cgg( Implement heimbach fix for nobcs.
        ncvarrecs(ivarindex)     = (endrec - startrec + 1)*nobcs
        ncvarrecstart(ivarindex) = (startrec - 1)*nobcs + 1
        ncvarrecsend(ivarindex)  = endrec*nobcs
cph)
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     =   1
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd(ivarindex)      = 'm'
      _END_MASTER( mythid )

#endif /* ALLOW_OBCSS_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSW_CONTROL
c--   Western obc.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_obcswstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart - diffsecs)/xx_obcswperiod) + 1
        endrec   = int((modelend   - diffsecs)/xx_obcswperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 13
        ncvarindex(ivarindex)    = 113
cgg( Implement heimbach fix for nobcs.
        ncvarrecs(ivarindex)     = (endrec - startrec + 1)*nobcs
        ncvarrecstart(ivarindex) = (startrec - 1)*nobcs + 1
        ncvarrecsend(ivarindex)  = endrec*nobcs
cgg)
        ncvarxmax(ivarindex)     =   1
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd(ivarindex)      = 'm'
      _END_MASTER( mythid )

#endif /* ALLOW_OBCSW_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSE_CONTROL
c--   Eastern obc.

      _BEGIN_MASTER( mythid )
#ifdef ALLOW_CALENDAR
        call cal_TimePassed( xx_obcsestartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart - diffsecs)/xx_obcseperiod) + 1
        endrec   = int((modelend   - diffsecs)/xx_obcseperiod) + 2
#else
        startrec = 1
        endrec   = 1
#endif
        ivarindex                = 14
        ncvarindex(ivarindex)    = 114
cgg( Implement heimbach fix for nobcs.
        ncvarrecs(ivarindex)     = (endrec - startrec + 1)*nobcs
        ncvarrecstart(ivarindex) = (startrec - 1)*nobcs + 1
        ncvarrecsend(ivarindex)  = endrec*nobcs
cgg)
        ncvarxmax(ivarindex)     =   1
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd(ivarindex)      = 'm'
      _END_MASTER( mythid )

#endif /* ALLOW_OBCSE_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_DIFFKR_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 15
        ncvarindex(ivarindex)    = 115
        ncvarrecs (ivarindex)    =   1
        ncvarxmax (ivarindex)    = snx
        ncvarymax (ivarindex)    = sny
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd  (ivarindex)    = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_DIFFKR_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_KAPGM_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 16
        ncvarindex(ivarindex)    = 116
        ncvarrecs (ivarindex)    =   1
        ncvarxmax (ivarindex)    = snx
        ncvarymax (ivarindex)    = sny
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd  (ivarindex)    = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_KAPGM_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_TR10_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 17
        ncvarindex(ivarindex)    = 117
        ncvarrecs (ivarindex)    =   1
        ncvarxmax (ivarindex)    = snx
        ncvarymax (ivarindex)    = sny
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd  (ivarindex)    = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_TR10_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_SST0_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 18
        ncvarindex(ivarindex)    = 118
        ncvarrecs (ivarindex)    =   1
        ncvarxmax (ivarindex)    = snx
        ncvarymax (ivarindex)    = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd  (ivarindex)    = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_SST0_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_SSS0_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 19
        ncvarindex(ivarindex)    = 119
        ncvarrecs (ivarindex)    =   1
        ncvarxmax (ivarindex)    = snx
        ncvarymax (ivarindex)    = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd  (ivarindex)    = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_SSS0_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_HFACC_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 20
        ncvarindex(ivarindex)    = 120
        ncvarrecs (ivarindex)    =   1
        ncvarxmax (ivarindex)    = snx
        ncvarymax (ivarindex)    = sny
        ncvargrd  (ivarindex)    = 'c'
#ifdef ALLOW_HFACC3D_CONTROL
        ncvarnrmax(ivarindex)    =  nr
#else
        ncvarnrmax(ivarindex)    =   1
#endif /*ALLOW_HFACC3D_CONTROL*/
      _END_MASTER( mythid )
#endif /* ALLOW_HFACC_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_EFLUXY0_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 21
        ncvarindex(ivarindex)    = 121
        ncvarrecs(ivarindex)     =   1
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd(ivarindex)      = 's'
      _END_MASTER( mythid )
#endif /* ALLOW_EFLUXY0_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_EFLUXP0_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 22
        ncvarindex(ivarindex)    = 122
        ncvarrecs(ivarindex)     =   1
        ncvarxmax(ivarindex)     = snx
        ncvarymax(ivarindex)     = sny
        ncvarnrmax(ivarindex)    =  nr
        ncvargrd(ivarindex)      = 'v'
      _END_MASTER( mythid )
#endif /* ALLOW_EFLUXP0_CONTROL */

c-------------------------------------------------------------------------------
c--
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      _BEGIN_MASTER( mythid )
        ivarindex                = 23
        ncvarindex(ivarindex)    = 123
        ncvarrecs (ivarindex)    =   1
        ncvarxmax (ivarindex)    = snx
        ncvarymax (ivarindex)    = sny
        ncvarnrmax(ivarindex)    =   1
        ncvargrd  (ivarindex)    = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_BOTTOMDRAG_CONTROL */

c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------

c--   Determine the number of wet points in each tile:
c--   maskc, masks, and maskw.

c--   Initialise the counters.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            nwetctile(bi,bj,k) = 0
            nwetstile(bi,bj,k) = 0
            nwetwtile(bi,bj,k) = 0
            nwetvtile(bi,bj,k) = 0
          enddo
        enddo
      enddo

#ifdef ALLOW_OBCS_CONTROL
c--   Initialise obcs counters.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do iobcs = 1,nobcs
#ifdef ALLOW_OBCSN_CONTROL
              nwetobcsn(bi,bj,k,iobcs) = 0
#endif
#ifdef ALLOW_OBCSS_CONTROL
              nwetobcss(bi,bj,k,iobcs) = 0
#endif
#ifdef ALLOW_OBCSW_CONTROL
              nwetobcsw(bi,bj,k,iobcs) = 0
#endif
#ifdef ALLOW_OBCSE_CONTROL
              nwetobcse(bi,bj,k,iobcs) = 0
#endif
            enddo
          enddo
        enddo
      enddo
#endif

c--   Count wet points on each tile.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
c--             Center mask.
                if (hFacC(i,j,k,bi,bj) .ne. 0.) then
                  nwetctile(bi,bj,k) = nwetctile(bi,bj,k) + 1
                endif
c--             South mask.
                if (maskS(i,j,k,bi,bj) .eq. 1.) then
                  nwetstile(bi,bj,k) = nwetstile(bi,bj,k) + 1
                endif
c--             West mask.
                if (maskW(i,j,k,bi,bj) .eq. 1.) then
                  nwetwtile(bi,bj,k) = nwetwtile(bi,bj,k) + 1
                endif
#if (defined (ALLOW_EFLUXP0_CONTROL))
c--             Vertical mask.
                if (hFacV(i,j,k,bi,bj) .ne. 0.) then
                  nwetvtile(bi,bj,k) = nwetvtile(bi,bj,k) + 1
                endif
#endif
              enddo
            enddo
          enddo
        enddo
      enddo

#ifdef ALLOW_OBCSN_CONTROL
c--   Count wet points at Northern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcsn'
      call ctrl_mask_set_xz( 0, OB_Jn, nwetobcsn, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSS_CONTROL
c--   Count wet points at Southern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcss'
      call ctrl_mask_set_xz( 1, OB_Js, nwetobcss, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSW_CONTROL
c--   Count wet points at Western boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcsw'
      call ctrl_mask_set_yz( 1, OB_Iw, nwetobcsw, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSE_CONTROL
c--   Count wet points at Eastern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcse'
      call ctrl_mask_set_yz( 0, OB_Ie, nwetobcse, ymaskobcs, mythid )
#endif

      _BEGIN_MASTER( mythid )
c--   Determine the total number of control variables.
      nvartype   = 0
      nvarlength = 0
      do i = 1,maxcvars
c
         if ( ncvarindex(i) .ne. -1 ) then
            nvartype = nvartype + 1
            do bj = jtlo,jthi
               do bi = itlo,ithi
                  do k = 1,ncvarnrmax(i)
                     if ( ncvargrd(i) .eq. 'c' ) then
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetctile(bi,bj,k)
                     else if ( ncvargrd(i) .eq. 's' ) then
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetstile(bi,bj,k)
                     else if ( ncvargrd(i) .eq. 'w' ) then
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetwtile(bi,bj,k)
                     else if ( ncvargrd(i) .eq. 'v' ) then
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetvtile(bi,bj,k)
                     else if ( ncvargrd(i) .eq. 'm' ) then
#ifdef ALLOW_OBCS_CONTROL
                        do iobcs = 1, nobcs
cgg   This overcounts the number of o.b. control points by a factor of "nobcs".
cgg   As an ad-hoc solution I've divided by nobcs everywhere.
                           if ( i .eq. 11 ) then
#ifdef ALLOW_OBCSN_CONTROL
                              nvarlength = nvarlength + 
     &                             (ncvarrecs(i)/nobcs)
     &                             *nwetobcsn(bi,bj,k,iobcs)
#endif
                           else if ( i .eq. 12 ) then
#ifdef ALLOW_OBCSS_CONTROL
                              nvarlength = nvarlength + 
     &                             (ncvarrecs(i)/nobcs)
     &                             *nwetobcss(bi,bj,k,iobcs)
#endif
                           else if ( i .eq. 13 ) then
#ifdef ALLOW_OBCSW_CONTROL
                              nvarlength = nvarlength + 
     &                             (ncvarrecs(i)/nobcs)
     &                             *nwetobcsw(bi,bj,k,iobcs)
#endif
                           else if ( i .eq. 14 ) then
#ifdef ALLOW_OBCSE_CONTROL
                              nvarlength = nvarlength + 
     &                             (ncvarrecs(i)/nobcs)
     &                             *nwetobcse(bi,bj,k,iobcs)
#endif
                           end if
                        enddo
#endif
                     else
                        print*,'ctrl_init: invalid grid location'
                        print*,'     control variable = ',ncvarindex(i)
                        print*,'     grid location    = ',ncvargrd(i)
                        stop   ' ... stopped in ctrl_init'
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

cph(
      print *, 'ph-wet 1: nvarlength = ', nvarlength
      print *, 'ph-wet 2: surface wet C = ', nwetctile(1,1,1)
      print *, 'ph-wet 3: surface wet W = ', nwetwtile(1,1,1)
      print *, 'ph-wet 4: surface wet S = ', nwetstile(1,1,1)
      print *, 'ph-wet 4a:surface wet V = ', nwetvtile(1,1,1)
      nwetc3d = 0
      do k = 1, Nr
         nwetc3d = nwetc3d + nwetctile(1,1,k)
      end do
      print *, 'ph-wet 5: 3D wet points = ', nwetc3d
      do i = 1, maxcvars
         print *, 'ph-wet 6: no recs for i = ', i, ncvarrecs(i)
      end do
      print *, 'ph-wet 7: ',
     &     2*nwetc3d + 
     &     ncvarrecs(3)*nwetctile(1,1,1) +
     &     ncvarrecs(4)*nwetctile(1,1,1) +
     &     ncvarrecs(5)*nwetwtile(1,1,1) +
     &     ncvarrecs(6)*nwetstile(1,1,1)
      print *, 'ph-wet 8: ',
     &     2*nwetc3d + 
     &     ncvarrecs(7)*nwetctile(1,1,1) +
     &     ncvarrecs(8)*nwetctile(1,1,1) +
     &     ncvarrecs(9)*nwetwtile(1,1,1) +
     &     ncvarrecs(10)*nwetstile(1,1,1)
#ifdef ALLOW_OBCSN_CONTROL
      print *, 'ph-wet 9: surface wet obcsn = '
     &     , nwetobcsn(1,1,1,1), nwetobcsn(1,1,1,2)
     &     , nwetobcsn(1,1,1,3), nwetobcsn(1,1,1,4)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      print *, 'ph-wet 10: surface wet obcss = '
     &     , nwetobcss(1,1,1,1), nwetobcss(1,1,1,2)
     &     , nwetobcss(1,1,1,3), nwetobcss(1,1,1,4)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      print *, 'ph-wet 11: surface wet obcsw = '
     &     , nwetobcsw(1,1,1,1), nwetobcsw(1,1,1,2)
     &     , nwetobcsw(1,1,1,3), nwetobcsw(1,1,1,4)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      print *, 'ph-wet 12: surface wet obcse = '
     &     , nwetobcse(1,1,1,1), nwetobcse(1,1,1,2)
     &     , nwetobcse(1,1,1,3), nwetobcse(1,1,1,4)
#endif
cph)
      
      CALL GLOBAL_SUM_INT( nvarlength,  myThid )

      print *, 'ph-wet 13: global nvarlength vor k=', k, nvarlength

c
c     Summation of wet point counters 
c
      do k = 1, nr

         ntmp=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp=ntmp+nWetcTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp,  myThid )
         nWetcGlobal(k)=ntmp
         print *, 'ph-wet 14a: global nWet... k=', k, ntmp

         ntmp=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp=ntmp+nWetsTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp,  myThid )
         nWetsGlobal(k)=ntmp
         print *, 'ph-wet 14b: global nWet... k=', k, ntmp

         ntmp=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp=ntmp+nWetwTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp,  myThid )
         nWetwGlobal(k)=ntmp
         print *, 'ph-wet 14c: global nWet... k=', k, ntmp

         ntmp=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp=ntmp+nWetvTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp,  myThid )
         nWetvGlobal(k)=ntmp
         print *, 'ph-wet 14d: global nWet... k=', k, ntmp

#ifdef ALLOW_OBCSN_CONTROL
         do iobcs = 1, nobcs
            ntmp=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmp=ntmp+nwetobcsn(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmp,  myThid )
            nwetobcsnglo(k,iobcs)=ntmp
            print *, 'ph-wet 15a: global nWet... k=', k, iobcs, ntmp
         enddo
#endif
#ifdef ALLOW_OBCSS_CONTROL
         do iobcs = 1, nobcs
            ntmp=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmp=ntmp+nwetobcss(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmp,  myThid )
            nwetobcssglo(k,iobcs)=ntmp
            print *, 'ph-wet 15b: global nWet... k=', k, iobcs, ntmp
         enddo
#endif
#ifdef ALLOW_OBCSW_CONTROL
         do iobcs = 1, nobcs
            ntmp=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmp=ntmp+nwetobcsw(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmp,  myThid )
            nwetobcswglo(k,iobcs)=ntmp
            print *, 'ph-wet 15c: global nWet... k=', k, iobcs, ntmp
         enddo
#endif
#ifdef ALLOW_OBCSE_CONTROL
         do iobcs = 1, nobcs
            ntmp=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmp=ntmp+nwetobcse(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmp,  myThid )
            nwetobcseglo(k,iobcs)=ntmp
            print *, 'ph-wet 15d: global nWet... k=', k, iobcs, ntmp
         enddo
#endif

      enddo

      print*, 'ctrl_init: no. of control variables: ', nvartype
      print*, 'ctrl_init: control vector length:    ', nvarlength
      _END_MASTER( mythid )

c     write masks and weights to files to be read by a master process
c
      call active_write_xyz( 'hFacC', hFacC,   1, 0, mythid, dummy)
      call active_write_xyz( 'maskW', maskW,   1, 0, mythid, dummy)
      call active_write_xyz( 'maskS', maskS,   1, 0, mythid, dummy)
#if (defined (ALLOW_EFLUXP0_CONTROL))
      call active_write_xyz( 'hFacV', hFacV,   1, 0, mythid, dummy)
#endif

c--   Summarize the control vector's setup.
      _BEGIN_MASTER( mythid )
cph        call ctrl_Summary( mythid )
      _END_MASTER( mythid )

      _BARRIER

      return
      end

