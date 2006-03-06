C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readgfo.F,v 1.2 2006/03/06 19:16:53 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_readgfo( irec, mythid )

c     ==================================================================
c     SUBROUTINE cost_readgfo
c     ==================================================================
c
c     o Read a given record of the GFO SSH data.
c
c     started: Christian Eckert eckert@mit.edu 25-May-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_readgfo
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ecco_cost.h"

c     == routine arguments ==

      integer irec
      integer mythid

#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer sshrec
      integer difftime(4)
      integer middate(4)
      integer noffset
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

c     == external functions ==
 
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      factor = 0.01
      spval = -9990. 

cnew(
      daytime = FLOAT(secondsperday*(irec-1))
      dayiter = hoursperday*(irec-1)
      call cal_getdate( dayiter, daytime, daydate, mythid )
      call cal_convdate( daydate,yday,md,dd,sd,ld,wd,mythid )
      ymod = gfostartdate(1)/10000

      if ( ymod .EQ. yday ) then
         middate(1) = modelstartdate(1)
      else
         middate(1) = yday*10000+100+1
      endif
      middate(2) = 0
      middate(3) = modelstartdate(3)
      middate(4) = modelstartdate(4)

      call cal_TimePassed( middate, daydate, difftime, mythid )
      call cal_ToSeconds( difftime, diffsecs, mythid )
      sshrec = int(diffsecs/gfoperiod) + 1

      il=ilnblnk(gfofile)
      write(fnametmp(1:80),'(2a,i4)')
     &     gfofile(1:il), '_', yday
      inquire( file=fnametmp, exist=exst )
      if (.NOT. exst) then
         write(fnametmp(1:80),'(a)') gfofile(1:il)
         sshrec = irec
      endif
 
cnew)


      call mdsreadfield( fnametmp, cost_iprec, cost_yftype, 1, gfoobs, 
     &                   sshrec, mythid )


      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
              if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                 gfomask(i,j,bi,bj) = 0. _d 0
              else
                 gfomask(i,j,bi,bj) = 1. _d 0
              endif
              if (gfoobs(i,j,bi,bj) .le. spval) then
                gfomask(i,j,bi,bj) = 0. _d 0
              endif
              if (abs(gfoobs(i,j,bi,bj)) .lt. 1.d-8 ) then
                 gfomask(i,j,bi,bj) = 0. _d 0
              endif

cph(
cph               print *, 'WARNING: SPECIFIC SETUP FOR ECCO'
cph               below statement could be replaced by following
cph               to make it independnet of Nr:
cph
cph               if ( rC(K) .GT. -1000. ) then
cph)
c             set tpmask=0 in areas shallower than 1000m 
              if (_hFacC(i,j,13,bi,bj) .eq. 0.) then
                gfomask(i,j,bi,bj) = 0. _d 0
              endif

              gfomask(i,j,bi,bj) = gfomask(i,j,bi,bj)*frame(i,j)
              gfoobs(i,j,bi,bj)  = gfomask(i,j,bi,bj)*factor
     &             ( gfoobs(i,j,bi,bj) - 
     &               ( gfointercept + gfoslope*irec*hoursperday ) )
            enddo
          enddo
        enddo
      enddo

#endif

      return
      end

