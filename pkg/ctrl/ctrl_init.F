#include "CTRL_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine ctrl_init( myThid )

c     ==================================================================
c     SUBROUTINE ctrl_init
c     ==================================================================
c
c     o The vector of control variables is defined here.
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
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "CTRL_GENARR.h"
# include "CTRL_OBCS.h"
# include "optim.h"
#endif
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_DIC_CONTROL
# include "DIC_CTRL.h"
#endif

c     == routine arguments ==

      integer myThid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer ivar
      integer startrec
      integer endrec
      integer diffrec
      integer iarr
      character*(1) ncvargrdTmp(maxcvars)

      _RL dummy
      _RL loctmp3d (1-olx:snx+olx,1-oly:sny+oly,Nr,nsx,nsy)

#ifdef ALLOW_OBCS_CONTROL_MODES
      INTEGER  length_of_rec,dUnit
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      CHARACTER*(MAX_LEN_FNAM) fnamegen
      INTEGER ilgen, k2, diffrecFull, endrecFull
#endif

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(myThid)
      jthi = mybyhi(myThid)
      itlo = mybxlo(myThid)
      ithi = mybxhi(myThid)
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

c     Set unit weight to 1
c

      do bj=1,nSy
         do bi=1,nSx
            do k=1,Nr
             wunit(k,bi,bj) = 1. _d 0
             do j=1-oly,sNy+oly
              do i=1-olx,sNx+olx
               loctmp3d(i,j,k,bi,bj) = 1. _d 0
              enddo
             enddo
            enddo
         enddo
      enddo

#ifdef ALLOW_AUTODIFF
      call active_write_xyz( 'wunit', loctmp3d, 1, 0, mythid, dummy)
#else
      CALL WRITE_REC_XYZ_RL( 'wunit', loctmp3d, 1, 1, myThid )
#endif

      _BARRIER

#ifdef ECCO_CTRL_DEPRECATED

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
cph    index 30-31          for generic 2d, 3d field
cph    index 32    reserved for precip (atmos. state)
cph    index 33    reserved for swflux (atmos. state)
cph    index 34    reserved for swdown (atmos. state)
cph          35                 lwflux
cph          36                 lwdown
cph          37                 evap
cph          38                 snowprecip
cph          39                 apressure
cph          40                 runoff
cph          41                 seaice SIAREA
cph          42                 seaice SIHEFF
cph          43                 seaice SIHSNOW
cph          44                 gmredi kapredi
cph          45                 shelfice shifwflx
cph          47-52              mean atmos. state
cph)

c----------------------------------------------------------------------
c--
#ifdef ALLOW_THETA0_CONTROL
c--   Initial state temperature contribution.
      call ctrl_init_ctrlvar (
     &     xx_theta_file, 1, 101, 1, 1, 1,
     &     snx, sny, nr, 'c', '3d', myThid )
#endif /* ALLOW_THETA0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SALT0_CONTROL
c--   Initial state salinity contribution.
      call ctrl_init_ctrlvar (
     &     xx_salt_file, 2, 102, 1, 1, 1,
     &     snx, sny, nr, 'c', '3d', myThid )
#endif /* ALLOW_SALT0_CONTROL */

c--   ===========================
c--   Surface flux contributions.
c--   ===========================

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_HFLUX_CONTROL))
c--   Heat flux.
      call ctrl_init_rec ( xx_hflux_file,
     I     xx_hfluxstartdate1, xx_hfluxstartdate2, xx_hfluxperiod, 1,
     O     xx_hfluxstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_hflux_file, 3, 103, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#elif (defined (ALLOW_ATEMP_CONTROL))
c--   Atmos. temperature
      call ctrl_init_rec ( xx_atemp_file,
     I     xx_atempstartdate1, xx_atempstartdate2, xx_atempperiod, 1,
     O     xx_atempstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_atemp_file, 7, 107, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#elif (defined (ALLOW_HFLUX0_CONTROL))
c--   initial forcing only
      call ctrl_init_ctrlvar (
     &     xx_hflux_file, 3, 103, 1, 1, 1,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_HFLUX_CONTROL */

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_SFLUX_CONTROL))
c--   Salt flux.
      call ctrl_init_rec ( xx_sflux_file,
     I     xx_sfluxstartdate1, xx_sfluxstartdate2, xx_sfluxperiod, 1,
     O     xx_sfluxstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_sflux_file, 4, 104, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#elif (defined (ALLOW_AQH_CONTROL))
c--   Atmos. humidity
      call ctrl_init_rec ( xx_aqh_file,
     I     xx_aqhstartdate1, xx_aqhstartdate2, xx_aqhperiod, 1,
     O     xx_aqhstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_aqh_file, 8, 108, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#elif (defined (ALLOW_SFLUX0_CONTROL))
c--   initial forcing only
      call ctrl_init_ctrlvar (
     &     xx_sflux_file, 4, 104, 1, 1, 1,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_SFLUX_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EXF
      IF ( .NOT.useAtmWind ) THEN
#endif
#if (defined (ALLOW_USTRESS_CONTROL))
c--   Zonal wind stress.
      call ctrl_init_rec ( xx_tauu_file,
     I     xx_tauustartdate1, xx_tauustartdate2, xx_tauuperiod, 1,
     O     xx_tauustartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_tauu_file, 5, 105, diffrec, startrec, endrec,
#ifndef ALLOW_ROTATE_UV_CONTROLS
     &     snx, sny, 1, 'w', 'xy', myThid )
#else
     &     snx, sny, 1, 'c', 'xy', myThid )
#endif

#elif (defined (ALLOW_TAUU0_CONTROL))
c--   initial forcing only
      call ctrl_init_ctrlvar (
     &     xx_tauu_file, 5, 105, 1, 1, 1,
     &     snx, sny, 1, 'w', 'xy', myThid )

#endif /* ALLOW_USTRESS_CONTROL */
#ifdef ALLOW_EXF
      ENDIF
#endif

#if (defined (ALLOW_UWIND_CONTROL))
#ifdef ALLOW_EXF
      IF ( useAtmWind ) THEN
#endif
c--   Zonal wind speed.
      call ctrl_init_rec ( xx_uwind_file,
     I     xx_uwindstartdate1, xx_uwindstartdate2, xx_uwindperiod, 1,
     O     xx_uwindstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_uwind_file, 9, 109, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif /* ALLOW_UWIND_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EXF
      IF ( .NOT.useAtmWind ) THEN
#endif
#if (defined (ALLOW_VSTRESS_CONTROL))
c--   Meridional wind stress.
      call ctrl_init_rec ( xx_tauv_file,
     I     xx_tauvstartdate1, xx_tauvstartdate2, xx_tauvperiod, 1,
     O     xx_tauvstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_tauv_file, 6, 106, diffrec, startrec, endrec,
#ifndef ALLOW_ROTATE_UV_CONTROLS
     &     snx, sny, 1, 's', 'xy', myThid )
#else
     &     snx, sny, 1, 'c', 'xy', myThid )
#endif

#elif (defined (ALLOW_TAUV0_CONTROL))
c--   initial forcing only
      call ctrl_init_ctrlvar (
     &     xx_tauv_file, 6, 106, 1, 1, 1,
     &     snx, sny, 1, 's', 'xy', myThid )

#endif /* ALLOW_VSTRESS_CONTROL */
#ifdef ALLOW_EXF
      ENDIF
#endif

#if (defined (ALLOW_VWIND_CONTROL))
#ifdef ALLOW_EXF
      IF ( useAtmWind ) THEN
#endif
c--   Meridional wind speed.
      call ctrl_init_rec ( xx_vwind_file,
     I     xx_vwindstartdate1, xx_vwindstartdate2, xx_vwindperiod, 1,
     O     xx_vwindstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_vwind_file, 10, 110, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif /* ALLOW_VWIND_CONTROL */

#endif /* ECCO_CTRL_DEPRECATED */

c--   ===========================
c--   Open boundary contributions.
c--   ===========================

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSN_CONTROL
c--   Northern obc.
      call ctrl_init_rec ( xx_obcsn_file,
     I     xx_obcsnstartdate1, xx_obcsnstartdate2, xx_obcsnperiod, 4,
     O     xx_obcsnstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_obcsn_file, 11, 111, diffrec, startrec, endrec,
     &     snx, 1, nr, 'm', 'xz', myThid )
#endif /* ALLOW_OBCSN_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSS_CONTROL
c--   Southern obc.
      call ctrl_init_rec ( xx_obcss_file,
     I     xx_obcssstartdate1, xx_obcssstartdate2, xx_obcssperiod, 4,
     O     xx_obcssstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_obcss_file, 12, 112, diffrec, startrec, endrec,
     &     snx, 1, nr, 'm', 'xz', myThid )
#endif /* ALLOW_OBCSS_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSW_CONTROL
c--   Western obc.
      call ctrl_init_rec ( xx_obcsw_file,
     I     xx_obcswstartdate1, xx_obcswstartdate2, xx_obcswperiod, 4,
     O     xx_obcswstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_obcsw_file, 13, 113, diffrec, startrec, endrec,
     &     1, sny, nr, 'm', 'yz', myThid )
#endif  /* ALLOW_OBCSW_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCSE_CONTROL
c--   Eastern obc.
      call ctrl_init_rec ( xx_obcse_file,
     I     xx_obcsestartdate1, xx_obcsestartdate2, xx_obcseperiod, 4,
     O     xx_obcsestartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_obcse_file, 14, 114, diffrec, startrec, endrec,
     &     1, sny, nr, 'm', 'yz', myThid )
#endif /* ALLOW_OBCSE_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_OBCS_CONTROL_MODES
cih  Get matrices for reconstruction from barotropic-barclinic modes
CMM  To use modes now hardcoded with ECCO_CPPOPTION.  Would be good to have
c     run-time option and also define filename=baro_invmodes.bin
        CALL MDSFINDUNIT( dUnit, myThid )
        length_of_rec = MDS_RECLEN( 64, NR*NR, myThid )
        open(dUnit, file='baro_invmodes.bin', status='old',
     &         access='direct', recl=length_of_rec )
        do j = 1,Nr
           read(dUnit,rec=j) ((modesv(k,i,j), k=1,Nr), i=1,Nr)
        end do
        CLOSE( dUnit )
CMM  double precision modesv is size [NR,NR,NR]
c     dim one is z-space
c     dim two is mode space
c     dim three is the total depth for which this set of modes applies
c     so for example modesv(:,2,nr) will be the second mode
c     in z-space for the full model depth
c    The modes are to be orthogonal when weighted by dz.
c     i.e. if f_i(z) = mode i, sum_j(f_i(z_j)*f_j(z_j)*dz_j = delta_ij
c    first mode should also be constant in depth...barotropic
c    For a matlab code example how to construct the orthonormal modes,
c     which are ideally the solution of planetary vertical mode equation
c     using model mean dRho/dz, see
c     MITgcm/verification/obcs_ctrl/input/gendata.m
c    This code is compatible with partial cells
#endif

#ifdef ECCO_CTRL_DEPRECATED

c----------------------------------------------------------------------
c--
#ifdef ALLOW_DIFFKR_CONTROL
      call ctrl_init_ctrlvar (
     &     xx_diffkr_file, 15, 115, 1, 1, 1,
     &     snx, sny, nr, 'c', '3d', myThid )
#endif /* ALLOW_DIFFKR_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_KAPGM_CONTROL
      call ctrl_init_ctrlvar (
     &     xx_kapgm_file, 16, 116, 1, 1, 1,
     &     snx, sny, nr, 'c', '3d', myThid )
#endif /* ALLOW_KAPGM_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_TR10_CONTROL
      call ctrl_init_ctrlvar (
     &     xx_tr1_file, 17, 117, 1, 1, 1,
     &     snx, sny, nr, 'c', '3d', myThid )
#endif /* ALLOW_TR10_CONTROL */

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_SST_CONTROL))
      call ctrl_init_rec ( xx_sst_file,
     I     xx_sststartdate1, xx_sststartdate2, xx_sstperiod, 1,
     O     xx_sststartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_sst_file, 18, 118, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#elif (defined (ALLOW_SST0_CONTROL))
      call ctrl_init_ctrlvar (
     &     xx_sst_file, 18, 118, 1, 1, 1,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_SST_CONTROL */

c----------------------------------------------------------------------
c--
#if (defined (ALLOW_SSS_CONTROL))
      call ctrl_init_rec ( xx_sss_file,
     I     xx_sssstartdate1, xx_sssstartdate2, xx_sssperiod, 1,
     O     xx_sssstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_sss_file, 19, 119, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#elif (defined (ALLOW_SSS0_CONTROL))
      call ctrl_init_ctrlvar (
     &     xx_sss_file, 19, 119, 1, 1, 1,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_SSS0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_DEPTH_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_depth_file, 20, 120, 1, 1, 1,
     &       snx, sny,  1, 'c', 'xy', myThid )
#endif /* ALLOW_DEPTH_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EFLUXY0_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_efluxy_file, 21, 121, 1, 1, 1,
     &       snx, sny, nr, 's', '3d', myThid )
#endif /* ALLOW_EFLUXY0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EFLUXP0_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_efluxp_file, 22, 122, 1, 1, 1,
     &       snx, sny, nr, 'v', '3d', myThid )
#endif /* ALLOW_EFLUXP0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_BOTTOMDRAG_CONTROL_NONGENERIC
        call ctrl_init_ctrlvar (
     &       xx_bottomdrag_file, 23, 123, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
#endif /* ALLOW_BOTTOMDRAG_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_HFLUXM_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_hfluxm_file, 24, 124, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
#endif /* ALLOW_HFLUXM_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EDDYPSI_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_edtaux_file, 25, 125, 1, 1, 1,
     &       snx, sny, nr, 'w', '3d', myThid )

        call ctrl_init_ctrlvar (
     &       xx_edtauy_file, 26, 126, 1, 1, 1,
     &       snx, sny, nr, 's', '3d', myThid )
#endif /* ALLOW_EDDYPSI_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_UVEL0_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_uvel_file, 27, 127, 1, 1, 1,
     &       snx, sny, nr, 'w', '3d', myThid )
#endif /* ALLOW_UVEL0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_VVEL0_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_vvel_file, 28, 128, 1, 1, 1,
     &       snx, sny, nr, 's', '3d', myThid )
#endif /* ALLOW_VVEL0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_ETAN0_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_etan_file, 29, 129, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
#endif /* ALLOW_VVEL0_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_GEN2D_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_gen2d_file, 30, 130, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
#endif /* ALLOW_GEN2D_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_GEN3D_CONTROL
        call ctrl_init_ctrlvar (
     &       xx_gen3d_file, 31, 131, 1, 1, 1,
     &       snx, sny, nr, 'c', '3d', myThid )
#endif /* ALLOW_GEN3D_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_PRECIP_CONTROL
c--   Atmos. precipitation
      call ctrl_init_rec ( xx_precip_file,
     I     xx_precipstartdate1, xx_precipstartdate2, xx_precipperiod,1,
     O     xx_precipstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_precip_file, 32, 132, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_PRECIP_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SWFLUX_CONTROL
c--   Atmos. swflux
      call ctrl_init_rec ( xx_swflux_file,
     I     xx_swfluxstartdate1, xx_swfluxstartdate2, xx_swfluxperiod, 1,
     O     xx_swfluxstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_swflux_file, 33, 133, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_SWFLUX_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SWDOWN_CONTROL
c--   Atmos. swdown
      call ctrl_init_rec ( xx_swdown_file,
     I     xx_swdownstartdate1, xx_swdownstartdate2, xx_swdownperiod, 1,
     O     xx_swdownstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_swdown_file, 34, 134, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_SWDOWN_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_LWFLUX_CONTROL
c--   Atmos. lwflux
      call ctrl_init_rec ( xx_lwflux_file,
     I     xx_lwfluxstartdate1, xx_lwfluxstartdate2, xx_lwfluxperiod, 1,
     O     xx_lwfluxstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_lwflux_file, 35, 135, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_LWFLUX_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_LWDOWN_CONTROL
c--   Atmos. lwdown
      call ctrl_init_rec ( xx_lwdown_file,
     I     xx_lwdownstartdate1, xx_lwdownstartdate2, xx_lwdownperiod, 1,
     O     xx_lwdownstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_lwdown_file, 36, 136, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_LWDOWN_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_EVAP_CONTROL
c--   Atmos. evap
      call ctrl_init_rec ( xx_evap_file,
     I     xx_evapstartdate1, xx_evapstartdate2, xx_evapperiod, 1,
     O     xx_evapstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_evap_file, 37, 137, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_EVAP_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SNOWPRECIP_CONTROL
c--   Atmos. snowprecip
      call ctrl_init_rec ( xx_snowprecip_file,
     I     xx_snowprecipstartdate1, xx_snowprecipstartdate2,
     I     xx_snowprecipperiod, 1,
     O     xx_snowprecipstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_snowprecip_file, 38, 138, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_SNOWPRECIP_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_APRESSURE_CONTROL
c--   Atmos. apressure
      call ctrl_init_rec ( xx_apressure_file,
     I     xx_apressurestartdate1, xx_apressurestartdate2,
     I     xx_apressureperiod, 1,
     O     xx_apressurestartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_apressure_file, 39, 139, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )

#endif /* ALLOW_APRESSURE_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_RUNOFF_CONTROL
c--   Atmos. runoff
      call ctrl_init_rec ( xx_runoff_file,
     I     xx_runoffstartdate1, xx_runoffstartdate2, xx_runoffperiod, 1,
     O     xx_runoffstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_runoff_file, 40, 140, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )
#endif /* ALLOW_RUNOFF_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SIAREA_CONTROL
C--   so far there are no xx_siareastartdate1, etc., so we need to fudge it.
CML      call ctrl_init_rec ( xx_siarea_file,
CML     I     xx_siareastartdate1, xx_siareastartdate2, xx_siareaperiod, 1,
CML     O     xx_siareastartdate, diffrec, startrec, endrec,
CML     I     myThid )
      startrec = 1
      endrec   = 1
      diffrec  = endrec - startrec + 1
      call ctrl_init_ctrlvar (
     &     xx_siarea_file, 41, 141, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )
#endif /* ALLOW_siarea_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SIHEFF_CONTROL
C--   so far there are no xx_siheffstartdate1, etc., so we need to fudge it.
CML      call ctrl_init_rec ( xx_siheff_file,
CML     I     xx_siheffstartdate1, xx_siheffstartdate2, xx_siheffperiod, 1,
CML     O     xx_siheffstartdate, diffrec, startrec, endrec,
CML     I     myThid )
      startrec = 1
      endrec   = 1
      diffrec  = endrec - startrec + 1
      call ctrl_init_ctrlvar (
     &     xx_siheff_file, 42, 142, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )
#endif /* ALLOW_siheff_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_SIHSNOW_CONTROL
C--   so far there are no xx_sihsnowstartdate1, etc., so we need to fudge it.
CML      call ctrl_init_rec ( xx_sihsnow_file,
CML     I     xx_sihsnowstartdate1, xx_sihsnowstartdate2, xx_sihsnowperiod, 1,
CML     O     xx_sihsnowstartdate, diffrec, startrec, endrec,
CML     I     myThid )
      startrec = 1
      endrec   = 1
      diffrec  = endrec - startrec + 1
      call ctrl_init_ctrlvar (
     &     xx_sihsnow_file, 43, 143, diffrec, startrec, endrec,
     &     snx, sny, 1, 'c', 'xy', myThid )
#endif /* ALLOW_sihsnow_CONTROL */


c----------------------------------------------------------------------
c--
#ifdef ALLOW_KAPREDI_CONTROL
      call ctrl_init_ctrlvar (
     &     xx_kapredi_file, 44, 144, 1, 1, 1,
     &     snx, sny, nr, 'c', '3d', myThid )
#endif /* ALLOW_KAPREDI_CONTROL */

c----------------------------------------------------------------------
c----------------------------------------------------------------------

#ifdef ALLOW_SHIFWFLX_CONTROL
c--   freshwater flux underneath ice-shelves
      call ctrl_init_rec ( xx_shifwflx_file,
     I     xx_shifwflxstartdate1, xx_shifwflxstartdate2,
     I     xx_shifwflxperiod, 1,
     O     xx_shifwflxstartdate, diffrec, startrec, endrec,
     I     myThid )
      call ctrl_init_ctrlvar (
     &     xx_shifwflx_file, 45, 145, diffrec, startrec, endrec,
     &     snx, sny, 1, 'i', 'xy', myThid )
#endif /* ALLOW_SHIFWFLX_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_ATM_MEAN_CONTROL
# ifdef ALLOW_ATEMP_CONTROL
       call ctrl_init_ctrlvar (
     &       xx_atemp_mean_file, 47, 147, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
# endif
# ifdef ALLOW_AQH_CONTROL
       call ctrl_init_ctrlvar (
     &       xx_aqh_mean_file,   48, 148, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
# endif
# ifdef ALLOW_UWIND_CONTROL
       call ctrl_init_ctrlvar (
     &       xx_uwind_mean_file, 49, 149, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
# endif
# ifdef ALLOW_VWIND_CONTROL
       call ctrl_init_ctrlvar (
     &       xx_vwind_mean_file, 50, 150, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
# endif
# ifdef ALLOW_PRECIP_CONTROL
       call ctrl_init_ctrlvar (
     &       xx_precip_mean_file,51, 151, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
# endif
# ifdef ALLOW_SWDOWN_CONTROL
       call ctrl_init_ctrlvar (
     &       xx_swdown_mean_file,52, 152, 1, 1, 1,
     &       snx, sny, 1, 'c', 'xy', myThid )
# endif
#endif /* ALLOW_ATM_MEAN_CONTROL */

#endif /* ECCO_CTRL_DEPRECATED */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_GENARR2D_CONTROL
       do iarr = 1, maxCtrlArr2D
        ncvargrdtmp='c'
# ifdef ALLOW_SHI2D_CONTROL
C       Under iceshelf, use maskSHI for these
        if ((xx_genarr2d_file(iarr)(1:11).eq.'xx_shicoeff').or.
     &      (xx_genarr2d_file(iarr)(1:11).eq.'xx_shicdrag')) then
            ncvargrdtmp='i'
        endif
# endif /* ALLOW_SHI2D_CONTROL */

# ifndef ALLOW_OPENAD
        if (xx_genarr2d_weight(iarr).NE.' ')
     &  call ctrl_init_ctrlvar (
# else
        call ctrl_init_ctrlvar (
# endif
     &       xx_genarr2d_file(iarr)(1:MAX_LEN_FNAM),
     &       100+iarr, 200+iarr, 1, 1, 1,
     &       snx, sny, 1, ncvargrdtmp, 'xy', myThid )

       enddo
#endif /* ALLOW_GENARR2D_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_GENARR3D_CONTROL
       do iarr = 1, maxCtrlArr3D
#ifndef ALLOW_OPENAD
        if (xx_genarr3d_weight(iarr).NE.' ')
     &  call ctrl_init_ctrlvar (
#else
        call ctrl_init_ctrlvar (
#endif
     &       xx_genarr3d_file(iarr)(1:MAX_LEN_FNAM),
     &       200+iarr, 300+iarr, 1, 1, 1,
     &       snx, sny, nr, xx_genarr3d_grid(iarr), '3d', myThid )
       enddo
#endif /* ALLOW_GENARR3D_CONTROL */

c----------------------------------------------------------------------
c--
#ifdef ALLOW_GENTIM2D_CONTROL
       do iarr = 1, maxCtrlTim2D

#ifdef ALLOW_CAL
        if (xx_gentim2d_startdate1(iarr).EQ.0) then
          xx_gentim2d_startdate1(iarr)=startdate_1
          xx_gentim2d_startdate2(iarr)=startdate_2
        endif
#endif

        call ctrl_init_rec ( xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
     I       xx_gentim2d_startdate1(iarr),
     I       xx_gentim2d_startdate2(iarr),
     I       xx_gentim2d_period(iarr),
     I       1,
     O       xx_gentim2d_startdate(1,iarr),
     O       diffrec, startrec, endrec,
     I       myThid )
C

#ifndef ALLOW_OPENAD
        if (xx_gentim2d_weight(iarr).NE.' ') then
#endif
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'replicate')
     &   xx_gentim2d_preproc(k2,iarr)='docycle'
         if (xx_gentim2d_preproc(k2,iarr).EQ.'doglomean')
     &   xx_gentim2d_glosum(iarr)     = .TRUE.
         if (xx_gentim2d_preproc(k2,iarr).EQ.'documul')
     &   xx_gentim2d_cumsum(iarr)     = .TRUE.
        enddo
C
        diffrecFull=diffrec
        endrecFull=endrec
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'docycle') then
           if (xx_gentim2d_preproc_i(k2,iarr).NE.0) then
            diffrec=min(diffrec,xx_gentim2d_preproc_i(k2,iarr))
            endrec=min(endrec,xx_gentim2d_preproc_i(k2,iarr))
           endif
         endif
        enddo
C
        ilgen=ilnblnk( xx_gentim2d_file(iarr) )
        write(fnamegen(1:MAX_LEN_FNAM),'(2a)')
     &       xx_gentim2d_file(iarr)(1:ilgen),'.effective'
        call ctrl_init_ctrlvar (
     &       fnamegen(1:MAX_LEN_FNAM),
     &       300+iarr, 400+iarr,
     &       diffrecFull, startrec, endrecFull,
     &       snx, sny, 1, xx_gentim2d_grid(iarr), 'xy', myThid )
C
        ilgen=ilnblnk( xx_gentim2d_file(iarr) )
        write(fnamegen(1:MAX_LEN_FNAM),'(2a)')
     &       xx_gentim2d_file(iarr)(1:ilgen),'.tmp'
        call ctrl_init_ctrlvar (
     &       fnamegen(1:MAX_LEN_FNAM),
     &       300+iarr, 400+iarr,
     &       diffrecFull, startrec, endrecFull,
     &       snx, sny, 1, xx_gentim2d_grid(iarr), 'xy', myThid )
C
        call ctrl_init_ctrlvar (
     &       xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
     &       300+iarr, 400+iarr,
     &       diffrec, startrec, endrec,
     &       snx, sny, 1, xx_gentim2d_grid(iarr), 'xy', myThid )
C
#ifndef ALLOW_OPENAD
       endif
#endif
C
       enddo
#endif /* ALLOW_GENTIM2D_CONTROL */

c----------------------------------------------------------------------
c----------------------------------------------------------------------

      call ctrl_init_wet( myThid )

c----------------------------------------------------------------------
c----------------------------------------------------------------------

#ifdef ALLOW_DIC_CONTROL
      do i = 1, dic_n_control
       xx_dic(i) = 0. _d 0
      enddo
#endif

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
          wlwflux   (i,j,bi,bj) = maskC(i,j,1,bi,bj)
          wlwdown   (i,j,bi,bj) = maskC(i,j,1,bi,bj)
          wevap     (i,j,bi,bj) = maskC(i,j,1,bi,bj)
          wsnowprecip(i,j,bi,bj) = maskC(i,j,1,bi,bj)
          wapressure(i,j,bi,bj) = maskC(i,j,1,bi,bj)
          wrunoff   (i,j,bi,bj) = maskC(i,j,1,bi,bj)
          wsst      (i,j,bi,bj) = maskC(i,j,1,bi,bj)
          wsss      (i,j,bi,bj) = maskC(i,j,1,bi,bj)
#endif
         enddo
        enddo
       enddo
      enddo

      _BARRIER

c--   Summarize the cost function setup.
      _BEGIN_MASTER( myThid )
      call ctrl_summary( myThid )
      _END_MASTER( myThid )

      return
      end
