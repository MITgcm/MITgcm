#include "ECCO_OPTIONS.h"

      subroutine cost_sla_read_yd(
     I                sla_file, sla_startdate,
c     I                sla_startdate, sla_period,
c     I                sla_intercept, sla_slope,
     O                sla_obs, sla_mask,
     I                year, day, myThid )

c     ==================================================================
c     SUBROUTINE cost_sla_read_yd
c     ==================================================================
c
c     o Read a given record of the SLA data.
c
c     started: Gael Forget 20-Oct-2009
c
c     ==================================================================
c     SUBROUTINE cost_sla_read_yd
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"

c     == routine arguments ==
      character*(MAX_LEN_FNAM) sla_file
      integer sla_startdate(4)
c     _RL sla_period
c     _RL sla_intercept
c     _RL sla_slope
      _RL sla_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sla_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      integer year, day
      integer myThid

c     == external functions ==
      integer  ilnblnk
      external ilnblnk
      integer  cal_IsLeap
      external cal_IsLeap

c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      _RL spval
      _RL factor
cnew(
      integer  il
      character*(80) fnametmp
      logical exst
cnew)

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      factor = 0.01
      spval = -9990.

      if ( (day.GE.1).AND.( (
     & (cal_IsLeap(year,myThid).eq.2).AND.(day.LE.366)
     & ).OR.(day.LE.365) ) ) then

      il=ilnblnk(sla_file)

      write(fnametmp(1:80),'(2a,i4)')
     &     sla_file(1:il), '_', year
      inquire( file=fnametmp, exist=exst )
      if (.NOT. exst) then
         stop
      endif

      CALL READ_REC_3D_RL( fnametmp, cost_iprec, 1,
     &                   sla_obs, day, 1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax

              if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                 sla_mask(i,j,bi,bj) = 0. _d 0
              else
                 sla_mask(i,j,bi,bj) = 1. _d 0
              endif
              if (sla_obs(i,j,bi,bj) .le. spval) then
                sla_mask(i,j,bi,bj) = 0. _d 0
              endif
              if (abs(sla_obs(i,j,bi,bj)) .lt. 1.d-8 ) then
                 sla_mask(i,j,bi,bj) = 0. _d 0
              endif

#ifndef ALLOW_SHALLOW_ALTIMETRY
              if ( R_low(i,j,bi,bj) .GT. -200. ) then
                sla_mask(i,j,bi,bj) = 0. _d 0
              endif
#endif
#ifndef ALLOW_HIGHLAT_ALTIMETRY
              if ( abs(YC(i,j,bi,bj)) .GT. 66. ) then
                sla_mask(i,j,bi,bj) = 0. _d 0
              endif
#endif

              sla_mask(i,j,bi,bj) = sla_mask(i,j,bi,bj)*frame(i,j)
              sla_obs(i,j,bi,bj)  = sla_mask(i,j,bi,bj)*factor*
     &             sla_obs(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

      else !if ( (day.GE.1).AND...

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
                 sla_obs(i,j,bi,bj) = 0. _d 0
                 sla_mask(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      endif !if ( (day.GE.1).AND...

      RETURN
      END
