C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_init_variables.F,v 1.19 2014/12/02 13:08:11 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

      subroutine ctrl_init_variables( myThid )

c     ==================================================================
c     SUBROUTINE ctrl_init_variables
c     ==================================================================
c
c     o Set parts of the vector of control variables and initialize the
c       rest to zero.
c
c     started: heimbach@mit.edu 25-Mar-2002
c
c     ==================================================================
c     SUBROUTINE ctrl_init_variables
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "CTRL_FIELDS.h"
# include "CTRL_GENARR.h"
#endif

c     == routine arguments ==

      integer myThid

#ifdef ALLOW_CTRL

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ntmp
      integer ivarindex

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1-OLy
      jmax = sNy+OLy
      imin = 1-OLx
      imax = sNx+OLx

#ifdef ALLOW_HFLUX_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_hflux0(i,j,bi,bj) = 0. _d 0
              xx_hflux1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SFLUX_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_sflux0(i,j,bi,bj) = 0. _d 0
              xx_sflux1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_USTRESS_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_tauu0(i,j,bi,bj) = 0. _d 0
              xx_tauu1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_VSTRESS_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_tauv0(i,j,bi,bj) = 0. _d 0
              xx_tauv1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_UWIND_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_uwind0(i,j,bi,bj) = 0. _d 0
              xx_uwind1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_VWIND_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_vwind0(i,j,bi,bj) = 0. _d 0
              xx_vwind1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_ATEMP_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_atemp0(i,j,bi,bj) = 0. _d 0
              xx_atemp1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_AQH_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_aqh0(i,j,bi,bj) = 0. _d 0
              xx_aqh1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_PRECIP_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_precip0(i,j,bi,bj) = 0. _d 0
              xx_precip1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SWFLUX_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_swflux0(i,j,bi,bj) = 0. _d 0
              xx_swflux1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SWDOWN_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_swdown0(i,j,bi,bj) = 0. _d 0
              xx_swdown1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_LWFLUX_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_lwflux0(i,j,bi,bj) = 0. _d 0
              xx_lwflux1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_LWDOWN_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_lwdown0(i,j,bi,bj) = 0. _d 0
              xx_lwdown1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_EVAP_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_evap0(i,j,bi,bj) = 0. _d 0
              xx_evap1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SNOWPRECIP_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_snowprecip0(i,j,bi,bj) = 0. _d 0
              xx_snowprecip1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_APRESSURE_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_apressure0(i,j,bi,bj) = 0. _d 0
              xx_apressure1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_RUNOFF_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_runoff0(i,j,bi,bj) = 0. _d 0
              xx_runoff1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SST_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_sst0(i,j,bi,bj) = 0. _d 0
              xx_sst1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SSS_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_sss0(i,j,bi,bj) = 0. _d 0
              xx_sss1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SHIFWFLX_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_shifwflx0(i,j,bi,bj) = 0. _d 0
              xx_shifwflx1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_BOTTOMDRAG_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              bottomDragFld(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_ATM_MEAN_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
# ifdef ALLOW_ATEMP_CONTROL
              xx_atemp_mean (i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_AQH_CONTROL
              xx_aqh_mean   (i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_UWIND_CONTROL
              xx_uwind_mean (i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_VWIND_CONTROL
              xx_vwind_mean (i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_PRECIP_CONTROL
              xx_precip_mean(i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_SWDOWN_CONTROL
              xx_swdown_mean(i,j,bi,bj) = 0. _d 0
# endif
            enddo
          enddo
        enddo
      enddo
#endif /* ALLOW_ATM_MEAN_CONTROL */

#ifdef ALLOW_OBCS_CONTROL
      IF (useOBCS) CALL CTRL_INIT_OBCS_VARIABLES( myThid )
#endif

C--   Map the control variables onto the model state.
C-    Needs to be called after few {PKG}_init_varia (e.g., PTRACERS_INIT_VARIA)
      IF ( .NOT. ctrlUseGen ) THEN
# ifdef ALLOW_ECCO
#  ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CTRL_MAP_INI_ECCO',myThid)
#  endif
      CALL CTRL_MAP_INI_ECCO( myThid )
# else
#  ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CTRL_MAP_INI',myThid)
#  endif
      CALL CTRL_MAP_INI( myThid )
# endif
      ENDIF !IF ( .NOT. ctrlUseGen ) THEN

      IF ( ctrlUseGen ) THEN
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL))
      CALL CTRL_MAP_INI_GENARR( myThid )
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      CALL CTRL_MAP_INI_GENTIM2D( myThid )
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k=1,maxCtrlTim2D
            objf_gentim2d(bi,bj,k)    = 0. _d 0
            num_gentim2d(bi,bj,k)    = 0. _d 0
          enddo
        enddo
      enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k=1,maxCtrlArr2D
            objf_genarr2d(bi,bj,k)    = 0. _d 0
            num_genarr2d(bi,bj,k)    = 0. _d 0
          enddo
        enddo
      enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k=1,maxCtrlArr3D
            objf_genarr3d(bi,bj,k)    = 0. _d 0
            num_genarr3d(bi,bj,k)    = 0. _d 0
          enddo
        enddo
      enddo
#endif
      ENDIF !IF ( .NOT. ctrlUseGen ) THEN

#ifdef ALLOW_SEAICE
      CALL SEAICE_CTRL_MAP_INI( myThid )
#endif

#endif /* ALLOW_CTRL */

      RETURN
      END
