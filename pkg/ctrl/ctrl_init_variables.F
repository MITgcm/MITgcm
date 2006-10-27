
#include "CTRL_CPPOPTIONS.h"

      subroutine ctrl_init_variables( mythid )

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
#include "GRID.h"
#include "ctrl.h"

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

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

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

#ifdef ALLOW_OBCS_CONTROL
      IF (useOBCS) CALL CTRL_INIT_OBCS_VARIABLES( mythid )
#endif

      return
      end

