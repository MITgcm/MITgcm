#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine grdchk_getadxx(
     I                       icvrec,
     I                       itile,
     I                       jtile,
     I                       layer,
     I                       itilepos,
     I                       jtilepos,
     I                       xx_comp,
     I                       ierr,
     I                       mythid )

c     ==================================================================
c     SUBROUTINE grdchk_getadxx
c     ==================================================================
c
c     o Set component a component of the control vector; xx(loc)
c
c     started: Christian Eckert eckert@mit.edu 08-Mar-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_getadxx
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "CTRL_OBCS.h"
#include "optim.h"
#include "grdchk.h"

c     == routine arguments ==
      integer icvrec
      integer itile
      integer jtile
      integer layer
      integer itilepos
      integer jtilepos
      _RL     xx_comp
      integer ierr
      integer mythid

#ifdef ALLOW_GRDCHK
c--   == external ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer iarr
      integer il
      integer dumiter
      _RL     dumtime
      _RL     dummy
      logical doglobalread
      logical ladinit
#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      _RL tmpfldxz (1-olx:snx+olx,nr,nsx,nsy)
#endif
#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      _RL tmpfldyz (1-oly:sny+oly,nr,nsx,nsy)
#endif
      _RL loctmp2d (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL loctmp3d (1-olx:snx+olx,1-oly:sny+oly,Nr,nsx,nsy)
      character*(80) fname

c--   == end of interface ==

      doglobalread = .false.
      ladinit      = .false.
      dumiter      = 0
      dumtime      = 0. _d 0

      if ( grdchkvarindex .eq. 0 ) then
         STOP 'GRDCHK INDEX 0 NOT ALLOWED'

#ifdef ECCO_CTRL_DEPRECATED

#ifdef ALLOW_THETA0_CONTROL
      else if ( grdchkvarindex .eq. 1 ) then
         il=ilnblnk( xx_theta_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_theta_file(1:il),'.',optimcycle
#endif /* ALLOW_THETA0_CONTROL */

#ifdef ALLOW_SALT0_CONTROL
      else if ( grdchkvarindex .eq. 2 ) then
         il=ilnblnk( xx_salt_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_salt_file(1:il),'.',optimcycle
#endif /* ALLOW_SALT0_CONTROL */

#ifdef ALLOW_UVEL0_CONTROL
      else if ( grdchkvarindex .eq. 27 ) then
         il=ilnblnk( xx_uvel_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_uvel_file(1:il),'.',optimcycle
#endif /* ALLOW_UVEL0_CONTROL */

#ifdef ALLOW_VVEL0_CONTROL
      else if ( grdchkvarindex .eq. 28 ) then
         il=ilnblnk( xx_vvel_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_vvel_file(1:il),'.',optimcycle
#endif /* ALLOW_VVEL0_CONTROL */

#ifdef ALLOW_HFLUX_CONTROL
      else if ( grdchkvarindex .eq. 3 ) then
         il=ilnblnk( xx_hflux_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_hflux_file(1:il),'.',optimcycle
#endif /* ALLOW_HFLUX_CONTROL */

#ifdef ALLOW_SFLUX_CONTROL
      else if ( grdchkvarindex .eq. 4 ) then
         il=ilnblnk( xx_sflux_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_sflux_file(1:il),'.',optimcycle
#endif /* ALLOW_SFLUX_CONTROL */

#if (defined (ALLOW_USTRESS_CONTROL) || defined (ALLOW_TAUU0_CONTROL))
      else if ( grdchkvarindex .eq. 5 ) then
         il=ilnblnk( xx_tauu_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_tauu_file(1:il),'.',optimcycle
#endif /* ALLOW_USTRESS_CONTROL */

#if (defined (ALLOW_VSTRESS_CONTROL) || defined (ALLOW_TAUV0_CONTROL))
      else if ( grdchkvarindex .eq. 6 ) then
         il=ilnblnk( xx_tauv_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_tauv_file(1:il),'.',optimcycle
#endif /* ALLOW_VSTRESS_CONTROL */

#ifdef ALLOW_ATEMP_CONTROL
      else if ( grdchkvarindex .eq. 7 ) then
         il=ilnblnk( xx_atemp_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_atemp_file(1:il),'.',optimcycle
#endif /* ALLOW_ATEMP_CONTROL */

#ifdef ALLOW_AQH_CONTROL
      else if ( grdchkvarindex .eq. 8 ) then
         il=ilnblnk( xx_aqh_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_aqh_file(1:il),'.',optimcycle
#endif /* ALLOW_AQH_CONTROL */

#ifdef ALLOW_UWIND_CONTROL
      else if ( grdchkvarindex .eq. 9 ) then
         il=ilnblnk( xx_uwind_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_uwind_file(1:il),'.',optimcycle
#endif /* ALLOW_UWIND_CONTROL */

#ifdef ALLOW_VWIND_CONTROL
      else if ( grdchkvarindex .eq. 10 ) then
         il=ilnblnk( xx_vwind_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_vwind_file(1:il),'.',optimcycle
#endif /* ALLOW_VWIND_CONTROL */

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_OBCSN_CONTROL
      else if ( grdchkvarindex .eq. 11 ) then
         il=ilnblnk( xx_obcsn_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_obcsn_file(1:il),'.',optimcycle
#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL
      else if ( grdchkvarindex .eq. 12 ) then
         il=ilnblnk( xx_obcss_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_obcss_file(1:il),'.',optimcycle
#endif /* ALLOW_OBCSS_CONTROL */

#ifdef ALLOW_OBCSW_CONTROL
      else if ( grdchkvarindex .eq. 13 ) then
         il=ilnblnk( xx_obcsw_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_obcsw_file(1:il),'.',optimcycle
#endif /* ALLOW_OBCSW_CONTROL */

#ifdef ALLOW_OBCSE_CONTROL
      else if ( grdchkvarindex .eq. 14 ) then
         il=ilnblnk( xx_obcse_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_obcse_file(1:il),'.',optimcycle
#endif /* ALLOW_OBCSE_CONTROL */

#ifdef ECCO_CTRL_DEPRECATED

#ifdef ALLOW_DIFFKR_CONTROL
      else if ( grdchkvarindex .eq. 15 ) then
         il=ilnblnk( xx_diffkr_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_diffkr_file(1:il),'.',optimcycle
#endif /* ALLOW_DIFFKR_CONTROL */

#ifdef ALLOW_KAPGM_CONTROL
      else if ( grdchkvarindex .eq. 16 ) then
         il=ilnblnk( xx_kapgm_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_kapgm_file(1:il),'.',optimcycle
#endif /* ALLOW_KAPGM_CONTROL */

#ifdef ALLOW_KAPREDI_CONTROL
      else if ( grdchkvarindex .eq. 44 ) then
         il=ilnblnk( xx_kapredi_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_kapredi_file(1:il),'.',optimcycle
#endif /* ALLOW_KAPREDI_CONTROL */

#ifdef ALLOW_TR10_CONTROL
      else if ( grdchkvarindex .eq. 17 ) then
         il=ilnblnk( xx_tr1_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_tr1_file(1:il),'.',optimcycle
#endif /* ALLOW_TR10_CONTROL */

#if (defined (ALLOW_SST_CONTROL) || defined (ALLOW_SST0_CONTROL))
      else if ( grdchkvarindex .eq. 18 ) then
         il=ilnblnk( xx_sst_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_sst_file(1:il),'.',optimcycle
#endif /* ALLOW_SST0_CONTROL */

#if (defined (ALLOW_SSS_CONTROL) || defined (ALLOW_SSS0_CONTROL))
      else if ( grdchkvarindex .eq. 19 ) then
         il=ilnblnk( xx_sss_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_sss_file(1:il),'.',optimcycle
#endif /* ALLOW_SSS0_CONTROL */

#ifdef ALLOW_DEPTH_CONTROL
      else if ( grdchkvarindex .eq. 20 ) then
         il=ilnblnk( xx_depth_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_depth_file(1:il),'.',optimcycle
#endif /* ALLOW_DEPTH_CONTROL */

#ifdef ALLOW_EFLUXY0_CONTROL
      else if ( grdchkvarindex .eq. 21 ) then
         il=ilnblnk( xx_efluxy_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_efluxy_file(1:il),'.',optimcycle
#endif /* ALLOW_EFLUXY0_CONTROL */

#ifdef ALLOW_EFLUXP0_CONTROL
      else if ( grdchkvarindex .eq. 22 ) then
         il=ilnblnk( xx_efluxp_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_efluxp_file(1:il),'.',optimcycle
#endif /* ALLOW_EFLUXP0_CONTROL */

#ifdef ALLOW_BOTTOMDRAG_CONTROL
      else if ( grdchkvarindex .eq. 23 ) then
         il=ilnblnk( xx_bottomdrag_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_bottomdrag_file(1:il),'.',optimcycle
#endif /* ALLOW_HFLUXM_CONTROL */

#ifdef ALLOW_HFLUXM_CONTROL
      else if ( grdchkvarindex .eq. 24 ) then
         il=ilnblnk( xx_hfluxm_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_hfluxm_file(1:il),'.',optimcycle
#endif /* ALLOW_HFLUXM_CONTROL */

#ifdef ALLOW_GEN2D_CONTROL
      else if ( grdchkvarindex .eq. 30 ) then
         il=ilnblnk( xx_gen2d_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_gen2d_file(1:il),'.',optimcycle
#endif /* ALLOW_GEN2D_CONTROL */

#ifdef ALLOW_GEN3D_CONTROL
      else if ( grdchkvarindex .eq. 31 ) then
         il=ilnblnk( xx_gen3d_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_gen3d_file(1:il),'.',optimcycle
#endif /* ALLOW_GEN3D_CONTROL */

#ifdef ALLOW_PRECIP_CONTROL
      else if ( grdchkvarindex .eq. 32 ) then
         il=ilnblnk( xx_precip_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_precip_file(1:il),'.',optimcycle
#endif /* ALLOW_PRECIP_CONTROL */

#ifdef ALLOW_SWFLUX_CONTROL
      else if ( grdchkvarindex .eq. 33 ) then
         il=ilnblnk( xx_swflux_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_swflux_file(1:il),'.',optimcycle
#endif /* ALLOW_SWFLUX_CONTROL */

#ifdef ALLOW_SWDOWN_CONTROL
      else if ( grdchkvarindex .eq. 34 ) then
         il=ilnblnk( xx_swdown_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_swdown_file(1:il),'.',optimcycle
#endif /* ALLOW_SWDOWN_CONTROL */

#ifdef ALLOW_LWFLUX_CONTROL
      else if ( grdchkvarindex .eq. 35 ) then
         il=ilnblnk( xx_lwflux_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_lwflux_file(1:il),'.',optimcycle
#endif /* ALLOW_LWFLUX_CONTROL */

#ifdef ALLOW_LWDOWN_CONTROL
      else if ( grdchkvarindex .eq. 36 ) then
         il=ilnblnk( xx_lwdown_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_lwdown_file(1:il),'.',optimcycle
#endif /* ALLOW_LWDOWN_CONTROL */

#ifdef ALLOW_EVAP_CONTROL
      else if ( grdchkvarindex .eq. 37 ) then
         il=ilnblnk( xx_evap_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_evap_file(1:il),'.',optimcycle
#endif /* ALLOW_EVAP_CONTROL */

#ifdef ALLOW_SNOWPRECIP_CONTROL
      else if ( grdchkvarindex .eq. 38 ) then
         il=ilnblnk( xx_snowprecip_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_snowprecip_file(1:il),'.',optimcycle
#endif /* ALLOW_SNOWPRECIP_CONTROL */

#ifdef ALLOW_APRESSURE_CONTROL
      else if ( grdchkvarindex .eq. 39 ) then
         il=ilnblnk( xx_apressure_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_apressure_file(1:il),'.',optimcycle
#endif /* ALLOW_APRESSURE_CONTROL */

#ifdef ALLOW_RUNOFF_CONTROL
      else if ( grdchkvarindex .eq. 40 ) then
         il=ilnblnk( xx_runoff_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_runoff_file(1:il),'.',optimcycle
#endif /* ALLOW_RUNOFF_CONTROL */

#ifdef ALLOW_SIAREA_CONTROL
      else if ( grdchkvarindex .eq. 41 ) then
         il=ilnblnk( xx_siarea_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_siarea_file(1:il),'.',optimcycle
#endif /* ALLOW_SIAREA_CONTROL */

#ifdef ALLOW_SIHEFF_CONTROL
      else if ( grdchkvarindex .eq. 42 ) then
         il=ilnblnk( xx_siheff_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_siheff_file(1:il),'.',optimcycle
#endif /* ALLOW_SIHEFF_CONTROL */

#ifdef ALLOW_SIHSNOW_CONTROL
      else if ( grdchkvarindex .eq. 43 ) then
         il=ilnblnk( xx_sihsnow_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_sihsnow_file(1:il),'.',optimcycle
#endif /* ALLOW_SIHSNOW_CONTROL */

#ifdef ALLOW_ETAN0_CONTROL
      else if ( grdchkvarindex .eq. 29 ) then
         il=ilnblnk( xx_etan_file )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_etan_file(1:il),'.',optimcycle
#endif /* ALLOW_ETAN0_CONTROL */

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_GENARR2D_CONTROL
      else if ( grdchkvarindex .ge. 101 .and.
     &          grdchkvarindex .le. 100+maxCtrlArr2D ) then
       do iarr = 1, maxCtrlArr2D
        if ( grdchkvarindex .eq. 100+iarr ) then
         il=ilnblnk( xx_genarr2d_file(iarr) )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_genarr2d_file(iarr)(1:il),'.',optimcycle
        endif
       enddo
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
      else if ( grdchkvarindex .ge. 201 .and.
     &          grdchkvarindex .le. 200+maxCtrlArr3D ) then
       do iarr = 1, maxCtrlArr3D
        if ( grdchkvarindex .eq. 200+iarr ) then
         il=ilnblnk( xx_genarr3d_file(iarr) )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_genarr3d_file(iarr)(1:il),'.',optimcycle
        endif
       enddo
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
      else if ( grdchkvarindex .ge. 301 .and.
     &          grdchkvarindex .le. 300+maxCtrlTim2D ) then
       do iarr = 1, maxCtrlTim2D
        if ( grdchkvarindex .eq. 300+iarr ) then
         il=ilnblnk( xx_gentim2d_file(iarr) )
         write(fname(1:80),'(80a)') ' '
         write(fname(1:80),'(3a,i10.10)')
     &        yadmark, xx_gentim2d_file(iarr)(1:il),'.',optimcycle
        endif
       enddo
#endif /* ALLOW_GENTIM2D_CONTROL */

      else
         STOP 'grdchk_getadxx: grdchkvarindex not implemented'
      endif

      if (ierr .EQ. 0 ) then
      if ( grdchkvarindex.EQ.1 .OR. grdchkvarindex.EQ.2 .OR.
     &     grdchkvarindex.EQ.27 .OR. grdchkvarindex.EQ.28 .OR.
     &     grdchkvarindex.EQ.15 .OR. grdchkvarindex.EQ.16 .OR.
     &     grdchkvarindex.EQ.17 .OR. grdchkvarindex.EQ.21 .OR.
     &     grdchkvarindex.EQ.22 .OR. grdchkvarindex.EQ.31 .OR.
     &     grdchkvarindex.EQ.44
#ifdef ALLOW_GENARR3D_CONTROL
     &     .OR. ( grdchkvarindex .ge. 201 .and.
     &     grdchkvarindex .le. 200+maxCtrlArr3D )
#endif
     &                                            ) then

         call active_read_xyz( fname, loctmp3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp = loctmp3d( itilepos,jtilepos,layer,itile,jtile )

#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      elseif ( grdchkvarindex.EQ.11 .OR. grdchkvarindex.EQ.12) then
         call active_read_xz( fname, tmpfldxz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp = tmpfldxz( itilepos,layer,itile,jtile )
#endif

#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      elseif ( grdchkvarindex.EQ.13 .OR. grdchkvarindex.EQ.14) then
         call active_read_yz( fname, tmpfldyz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp = tmpfldyz( jtilepos,layer,itile,jtile )
#endif

      else

         call active_read_xy( fname, loctmp2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        mythid, dummy)
         if ( myProcId .EQ. grdchkwhichproc )
     &   xx_comp = loctmp2d( itilepos,jtilepos,itile,jtile )

      endif
      endif

#endif /* ALLOW_GRDCHK */

      return
      end
