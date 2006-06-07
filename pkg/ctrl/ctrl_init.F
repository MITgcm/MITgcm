C
C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_init.F,v 1.23 2006/06/07 01:55:13 heimbach Exp $
C $Name:  $

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
c              heimbach@mit.edu totally restructured 28-Oct-2003
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
#include "optim.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_OBCS_CONTROL
# include "OBCS.h"
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
      integer ivar
      integer iobcs
      integer il
      integer errio
      integer startrec
      integer endrec
      integer diffrec
      integer difftime(4)
      _RL     diffsecs

      character*(max_len_prec) record
      character*(max_len_mbuf) msgbuf
      character*2 whichxyz

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

c--     Set default values.
      do ivar = 1,maxcvars
         ncvarindex(ivar) = -1
         ncvarrecs(ivar)  =  0
         ncvarxmax(ivar)  =  0
         ncvarymax(ivar)  =  0
         ncvarnrmax(ivar) =  0
         ncvargrd(ivar)   = '?'
      enddo

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
cph    index 23             for bottom drag
cph    index 24
cph    index 25-26          for edtaux, edtauy
cph    index 27-29          for uvel0, vvel0, etan0
cph    index 30-31          for relax. SST, SSS
cph    index 32    reserved for precip (atmos. state)
cph    index 33    reserved for swflux (atmos. state)
cph    index 34    reserved for swdown (atmos. state)
cph)

c----------------------------------------------------------------------
c--
#ifdef ALLOW_THETA0_CONTROL
c--   Initial state temperature contribution.
      call ctrl_init_ctrlvar ( 
     &     xx_theta_file, 1, 101, 1, 1, 1, 
     &     snx, sny, nr, 'c', '3d', mythid )
#endif /* ALLOW_THETA0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SALT0_CONTROL
c--   Initial state salinity contribution.
      call ctrl_init_ctrlvar ( 
     &     xx_salt_file, 2, 102, 1, 1, 1,
     &     snx, sny, nr, 'c', '3d', mythid )
#endif /* ALLOW_SALT0_CONTROL */

c--   ===========================
c--   Surface flux contributions.
c--   ===========================

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_HFLUX_CONTROL))
c--   Heat flux.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_hfluxstartdate1, xx_hfluxstartdate2,
     &                     xx_hfluxstartdate , mythid )
        call cal_TimePassed( xx_hfluxstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_hfluxperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_hfluxperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_hfluxperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_hflux_file, 3, 103, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#elif (defined (ALLOW_ATEMP_CONTROL))
c--   Atmos. temperature

# ifdef ALLOW_CAL
        call cal_FullDate( xx_atempstartdate1, xx_atempstartdate2,
     &                     xx_atempstartdate , mythid )
        call cal_TimePassed( xx_atempstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_atempperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_atempperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_atempperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_atemp_file, 7, 107, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#elif (defined (ALLOW_HFLUX0_CONTROL))
c--   initial forcing only
        call ctrl_init_ctrlvar ( 
     &       xx_hflux_file, 3, 103, 1, 1, 1, 
     &       snx, sny, 1, 'c', 'xy', mythid )

#endif /* ALLOW_HFLUX_CONTROL */

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_SFLUX_CONTROL))
c--   Salt flux.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_sfluxstartdate1, xx_sfluxstartdate2,
     &                     xx_sfluxstartdate , mythid )
        call cal_TimePassed( xx_sfluxstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_sfluxperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_sfluxperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_sfluxperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_sflux_file, 4, 104, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )
 
#elif (defined (ALLOW_AQH_CONTROL))
c--   Atmos. humidity

# ifdef ALLOW_CAL
        call cal_FullDate( xx_aqhstartdate1, xx_aqhstartdate2,
     &                     xx_aqhstartdate , mythid )
        call cal_TimePassed( xx_aqhstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_aqhperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_aqhperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_aqhperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_aqh_file, 8, 108, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#elif (defined (ALLOW_SFLUX0_CONTROL))
c--   initial forcing only
        call ctrl_init_ctrlvar ( 
     &       xx_sflux_file, 4, 104, 1, 1, 1, 
     &       snx, sny, 1, 'c', 'xy', mythid )

#endif /* ALLOW_SFLUX_CONTROL */

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_USTRESS_CONTROL))
c--   Zonal wind stress.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_tauustartdate1,  xx_tauustartdate2,
     &                     xx_tauustartdate,   mythid )
        call cal_TimePassed( xx_tauustartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_tauuperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_tauuperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_tauuperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_tauu_file, 5, 105, diffrec, startrec, endrec,
     &       snx, sny, 1, 'w', 'xy', mythid )

#elif (defined (ALLOW_UWIND_CONTROL))
c--   Zonal wind speed.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_uwindstartdate1, xx_uwindstartdate2,
     &                     xx_uwindstartdate , mythid )
        call cal_TimePassed( xx_uwindstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_uwindperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_uwindperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_uwindperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_uwind_file, 9, 109, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#elif (defined (ALLOW_TAUU0_CONTROL))
c--   initial forcing only
        call ctrl_init_ctrlvar ( 
     &       xx_tauu_file, 5, 105, 1, 1, 1,
     &       snx, sny, 1, 'w', 'xy', mythid )

#endif /* ALLOW_USTRESS_CONTROL */

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_VSTRESS_CONTROL))
c--   Meridional wind stress.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_tauvstartdate1,  xx_tauvstartdate2,
     &                     xx_tauvstartdate,   mythid )
        call cal_TimePassed( xx_tauvstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_tauvperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_tauvperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_tauvperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_tauv_file, 6, 106, diffrec, startrec, endrec,
     &       snx, sny, 1, 's', 'xy', mythid )

#elif (defined (ALLOW_VWIND_CONTROL))
c--   Meridional wind speed.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_vwindstartdate1, xx_vwindstartdate2,
     &                     xx_vwindstartdate , mythid )
        call cal_TimePassed( xx_vwindstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_vwindperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_vwindperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_vwindperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_vwind_file, 10, 110, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#elif (defined (ALLOW_TAUV0_CONTROL))
c--   initial forcing only
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_tauv_file, 6, 106, 1, 1, 1,
     &       snx, sny, 1, 's', 'xy', mythid )

#endif /* ALLOW_VSTRESS_CONTROL */

c--   ===========================
c--   Open boundary contributions.
c--   ===========================

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSN_CONTROL
c--   Northern obc.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_obcsnstartdate1,  xx_obcsnstartdate2,
     &                     xx_obcsnstartdate,   mythid )
        call cal_TimePassed( xx_obcsnstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart - diffsecs)/xx_obcsnperiod) + 1
        startrec = (startrec - 1)*nobcs + 1
        endrec   = int((modelend   - diffsecs)/xx_obcsnperiod) + 2
        endrec   = (endrec - startrec + 1)*nobcs
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec
        call ctrl_init_ctrlvar ( 
     &       xx_obcsn_file, 11, 111, diffrec, startrec, endrec,
     &       snx, sny, nr, 'm', 'xz', mythid )

#endif /* ALLOW_OBCSN_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSS_CONTROL
c--   Southern obc.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_obcssstartdate1,  xx_obcssstartdate2,
     &                     xx_obcssstartdate,   mythid )
        call cal_TimePassed( xx_obcssstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart - diffsecs)/xx_obcssperiod) + 1
        startrec = (startrec - 1)*nobcs + 1
        endrec   = int((modelend   - diffsecs)/xx_obcssperiod) + 2
        endrec   = (endrec - startrec + 1)*nobcs
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec
        call ctrl_init_ctrlvar ( 
     &       xx_obcss_file, 12, 112, diffrec, startrec, endrec,
     &       snx, sny, nr, 'm', 'xz', mythid )

#endif /* ALLOW_OBCSS_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSW_CONTROL
c--   Western obc.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_obcswstartdate1,  xx_obcswstartdate2,
     &                     xx_obcswstartdate,   mythid )
        call cal_TimePassed( xx_obcswstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart - diffsecs)/xx_obcswperiod) + 1
        startrec = (startrec - 1)*nobcs + 1
        endrec   = int((modelend   - diffsecs)/xx_obcswperiod) + 2
        endrec   = (endrec - startrec + 1)*nobcs
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec
        call ctrl_init_ctrlvar ( 
     &       xx_obcsw_file, 13, 113, diffrec, startrec, endrec,
     &       snx, sny, nr, 'm', 'yz', mythid )

#endif  /* ALLOW_OBCSW_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSE_CONTROL
c--   Eastern obc.

# ifdef ALLOW_CAL
        call cal_FullDate( xx_obcsestartdate1,  xx_obcsestartdate2,
     &                     xx_obcsestartdate,   mythid )
        call cal_TimePassed( xx_obcsestartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        startrec = int((modelstart - diffsecs)/xx_obcseperiod) + 1
        startrec = (startrec - 1)*nobcs + 1
        endrec   = int((modelend   - diffsecs)/xx_obcseperiod) + 2
        endrec   = (endrec - startrec + 1)*nobcs
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec
        call ctrl_init_ctrlvar ( 
     &       xx_obcse_file, 14, 114, diffrec, startrec, endrec,
     &       snx, sny, nr, 'm', 'yz', mythid )

#endif /* ALLOW_OBCSE_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_DIFFKR_CONTROL
        call ctrl_init_ctrlvar ( 
     &       xx_diffkr_file, 15, 115, 1, 1, 1,
     &       snx, sny, nr, 'c', '3d', mythid )
#endif /* ALLOW_DIFFKR_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_KAPGM_CONTROL
        call ctrl_init_ctrlvar ( 
     &       xx_kapgm_file, 16, 116, 1, 1, 1,
     &       snx, sny, nr, 'c', '3d', mythid )
#endif /* ALLOW_KAPGM_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_TR10_CONTROL
        call ctrl_init_ctrlvar ( 
     &       xx_tr1_file, 17, 117, 1, 1, 1,
     &       snx, sny, nr, 'c', '3d', mythid )
#endif /* ALLOW_TR10_CONTROL */

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_SST_CONTROL))

# ifdef ALLOW_CAL
        call cal_FullDate( xx_sststartdate1, xx_sststartdate2,
     &                     xx_sststartdate , mythid )
        call cal_TimePassed( xx_sststartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_sstperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_sstperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_sstperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_sst_file, 18, 118, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#elif (defined (ALLOW_SST0_CONTROL))

        call ctrl_init_ctrlvar ( 
     &       xx_sst_file, 18, 118, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', mythid )

#endif /* ALLOW_SST_CONTROL */

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_SSS_CONTROL))

# ifdef ALLOW_CAL
        call cal_FullDate( xx_sssstartdate1, xx_sssstartdate2,
     &                     xx_sssstartdate , mythid )
        call cal_TimePassed( xx_sssstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_sssperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_sssperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_sssperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_sss_file, 19, 119, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#elif (defined (ALLOW_SSS0_CONTROL))

        call ctrl_init_ctrlvar ( 
     &       xx_sss_file, 19, 119, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', mythid )

#endif /* ALLOW_SSS0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_DEPTH_CONTROL
        call ctrl_init_ctrlvar ( 
     &       xx_depth_file, 20, 120, 1, 1, 1,
     &       snx, sny,  1, 'c', 'xy', mythid )
#endif /* ALLOW_DEPTH_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EFLUXY0_CONTROL
        call ctrl_init_ctrlvar ( 
     &       xx_efluxy_file, 21, 121, 1, 1, 1,
     &       snx, sny, nr, 's', '3d', mythid )
#endif /* ALLOW_EFLUXY0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EFLUXP0_CONTROL
        call ctrl_init_ctrlvar ( 
     &       xx_efluxp_file, 22, 122, 1, 1, 1,
     &       snx, sny, nr, 'v', '3d', mythid )
#endif /* ALLOW_EFLUXP0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_BOTTOMDRAG_CONTROL
        call ctrl_init_ctrlvar ( 
     &       xx_bottomdrag_file, 23, 123, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', mythid )
#endif /* ALLOW_BOTTOMDRAG_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EDTAUX_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_edtaux_file, 25, 125, 1, 1, 1,
     &       snx, sny, nr, 'w', '3d', mythid )
#endif /* ALLOW_EDTAUX_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EDTAUY_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_edtauy_file, 26, 126, 1, 1, 1,
     &       snx, sny, nr, 's', '3d', mythid )
#endif /* ALLOW_EDTAUY_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_UVEL0_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_uvel_file, 27, 127, 1, 1, 1,
     &       snx, sny, nr, 'w', '3d', mythid )
#endif /* ALLOW_UVEL0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_VVEL0_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_vvel_file, 28, 128, 1, 1, 1,
     &       snx, sny, nr, 's', '3d', mythid )
#endif /* ALLOW_VVEL0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_ETAN0_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_etan_file, 29, 129, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', mythid )
#endif /* ALLOW_VVEL0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_RELAXSST_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_relaxsst_file, 30, 130, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', mythid )
#endif /* ALLOW_RELAXSST_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_RELAXSSS_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_relaxsss_file, 31, 131, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', mythid )
#endif /* ALLOW_RELAXSSS_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_PRECIP_CONTROL
c--   Atmos. precipitation

# ifdef ALLOW_CAL
        call cal_FullDate( xx_precipstartdate1, xx_precipstartdate2,
     &                     xx_precipstartdate , mythid )
        call cal_TimePassed( xx_precipstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_precipperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_precipperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_precipperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_precip_file, 32, 132, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#endif /* ALLOW_PRECIP_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SWFLUX_CONTROL
c--   Atmos. swflux

# ifdef ALLOW_CAL
        call cal_FullDate( xx_swfluxstartdate1, xx_swfluxstartdate2,
     &                     xx_swfluxstartdate , mythid )
        call cal_TimePassed( xx_swfluxstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_swfluxperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_swfluxperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_swfluxperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_swflux_file, 33, 133, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#endif /* ALLOW_SWFLUX_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SWDOWN_CONTROL
c--   Atmos. swdown

# ifdef ALLOW_CAL
        call cal_FullDate( xx_swdownstartdate1, xx_swdownstartdate2,
     &                     xx_swdownstartdate , mythid )
        call cal_TimePassed( xx_swdownstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, diffsecs, mythid )
        if ( xx_swdownperiod .EQ. 0 ) then
        startrec=1
        endrec=12
        else
        startrec = int((modelstart + startTime - diffsecs)/
     &                 xx_swdownperiod) + 1
        endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                 xx_swdownperiod) + 2
        endif
# else
        startrec = 1
        endrec   = 1
# endif
        diffrec  = endrec - startrec + 1
        call ctrl_init_ctrlvar ( 
     &       xx_swdown_file, 34, 134, diffrec, startrec, endrec,
     &       snx, sny, 1, 'c', 'xy', mythid )

#endif /* ALLOW_SWDOWN_CONTROL */

c----------------------------------------------------------------------
c----------------------------------------------------------------------

        call ctrl_init_wet( mythid )

c----------------------------------------------------------------------
c----------------------------------------------------------------------

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              wareaunit (i,j,bi,bj) = 1.0
#ifndef ALLOW_ECCO
              whflux    (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wsflux    (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wtauu     (i,j,bi,bj) = maskW(i,j,1,bi,bj)
              wtauv     (i,j,bi,bj) = maskS(i,j,1,bi,bj)
              watemp    (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              waqh      (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wprecip   (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wswflux   (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wswdown   (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wuwind    (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wvwind    (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wsst      (i,j,bi,bj) = maskC(i,j,1,bi,bj)
              wsss      (i,j,bi,bj) = maskC(i,j,1,bi,bj)
#endif
            enddo
          enddo
        enddo
      enddo
 
      return
      end

