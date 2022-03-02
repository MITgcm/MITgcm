#include "ECCO_OPTIONS.h"

      subroutine cost_sla_read(
     I                sla_file, sla_startdate, sla_period,
     I                sla_intercept, sla_slope,
     O                sla_obs, sla_mask,
     I                irec, myThid )

c     ==================================================================
c     SUBROUTINE cost_sla_read
c     ==================================================================
c
c     o Read a given record of the SLA data.
c
c     started: Gael Forget 20-Oct-2009
c
c     ==================================================================
c     SUBROUTINE cost_sla_read
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
      _RL sla_period
      _RL sla_intercept
      _RL sla_slope
      _RL sla_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sla_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      integer irec
      integer myThid

c     == external functions ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer sshrec
      integer difftime(4)
      integer tempDate_1
      integer middate(4)
      _RL diffsecs
      _RL spval
      _RL factor
cnew(
      integer  il
      _RL daytime
      integer dayiter
      integer daydate(4)
      integer yday, ymod
      integer md, dd, sd, ld, wd
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

c select data record to read
      daytime = FLOAT(secondsperday*(irec-1)) + modelstart
      dayiter = hoursperday*(irec-1)+modeliter0
      call cal_getdate( dayiter, daytime, daydate, myThid )
      call cal_convdate( daydate,yday,md,dd,sd,ld,wd,myThid )
      ymod = sla_startdate(1)/10000

      if ( ymod .GE. yday ) then
         call cal_FullDate( sla_startdate(1), 0, middate, myThid)
      else
         tempDate_1 = yday*10000+100+1
         call cal_FullDate( tempDate_1, 0, middate, myThid)
      endif

      call cal_TimePassed( middate, daydate, difftime, myThid )
      call cal_ToSeconds( difftime, diffsecs, myThid )
c      sshrec = floor(diffsecs/sla_period) + 1
      sshrec = int(diffsecs/sla_period) + 1

      il=ilnblnk(sla_file)
      write(fnametmp(1:80),'(2a,i4)')
     &     sla_file(1:il), '_', yday
      inquire( file=fnametmp, exist=exst )

c read data:
      if ( (sshrec .GT. 0).AND.(exst) ) then
       CALL READ_REC_3D_RL( fnametmp, cost_iprec, 1,
     &                      sla_obs, sshrec, 1, myThid )
      else
       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
            sla_obs(i,j,bi,bj) = spval
          enddo
         enddo
        enddo
       enddo
      endif

c mask data:
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
     &             ( sla_obs(i,j,bi,bj) -
     &               ( sla_intercept + sla_slope*irec*hoursperday ) )
            enddo
          enddo
        enddo
      enddo

      RETURN
      END
