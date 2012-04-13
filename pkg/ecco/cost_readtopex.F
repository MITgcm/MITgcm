C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readtopex.F,v 1.12 2012/04/13 18:04:30 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_readtopex( irec, mythid )

c     ==================================================================
c     SUBROUTINE cost_readtopex
c     ==================================================================
c
c     o Read a given record of the TOPEX SSH data.
c
c     started: Christian Eckert eckert@mit.edu 25-May-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_readtopex
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

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
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

#ifdef ALLOW_ECCO_DEBUG
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ioUnit
#endif

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

#ifdef ALLOW_ECCO_DEBUG
      ioUnit=standardMessageUnit
#endif

      factor = 0.01 _d 0
      spval = -9990. _d 0

cnew(
      daytime = FLOAT(secondsperday*(irec-1)) + modelstart
      dayiter = hoursperday*(irec-1)+modeliter0
      call cal_getdate( dayiter, daytime, daydate, mythid )
      call cal_convdate( daydate,yday,md,dd,sd,ld,wd,mythid )
      ymod = topexstartdate(1)/10000

      if ( ymod .GE. yday ) then
         call cal_FullDate( topexstartdate(1), 0, middate, mythid)
      else
         tempDate_1 = yday*10000+100+1
         call cal_FullDate( tempDate_1, 0, middate, mythid)
      endif

      call cal_TimePassed( middate, daydate, difftime, mythid )
      call cal_ToSeconds( difftime, diffsecs, mythid )
c      sshrec = floor(diffsecs/topexperiod) + 1
      sshrec = int(diffsecs/topexperiod) + 1

      il=ilnblnk(topexfile)
      write(fnametmp(1:80),'(2a,i4)')
     &     topexfile(1:il), '_', yday
#ifdef ALLOW_ECCO_DEBUG
        WRITE(msgBuf,'(A,I4,A,I4,A,I10,A,1PE15.2)') 'tpx reading ',
     &      yday,' ',ymod,' ',sshrec,' ',diffsecs
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c
        CALL CAL_PRINTDATE(middate,mythid)
        CALL CAL_PRINTDATE(daydate,mythid)
        CALL CAL_PRINTDATE(difftime,mythid)
#endif
      inquire( file=fnametmp, exist=exst )
      if (.NOT. exst) then
         write(fnametmp(1:80),'(a)') topexfile(1:il)
         call cal_TimePassed(topexstartdate,daydate,difftime,mythid)
         call cal_ToSeconds( difftime, diffsecs, mythid )
c         sshrec = floor(diffsecs/topexperiod) + 1
         sshrec = int(diffsecs/topexperiod) + 1
         inquire( file=fnametmp, exist=exst )
#ifdef ALLOW_ECCO_DEBUG
        WRITE(msgBuf,'(A,I4,A,I4,A,I10,A,1PE15.2)') 'tpx now reading ',
     &      yday,' ',ymod,' ',sshrec,' ',diffsecs
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c
        CALL CAL_PRINTDATE(middate,mythid)
        CALL CAL_PRINTDATE(daydate,mythid)
        CALL CAL_PRINTDATE(difftime,mythid)
#endif
      endif

cnew)

c read data:
      if ( (sshrec .GT. 0).AND.(exst) ) then
      call mdsreadfield( fnametmp, cost_iprec, cost_yftype, 1, tpobs,
     &                   sshrec, mythid )
      else
       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
            tpobs(i,j,bi,bj) = spval
          enddo
         enddo
        enddo
       enddo
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
              if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                tpmask(i,j,bi,bj) = 0. _d 0
              else
                tpmask(i,j,bi,bj) = 1. _d 0
              endif
              if (tpobs(i,j,bi,bj) .le. spval) then
                tpmask(i,j,bi,bj) = 0. _d 0
              endif
              if (abs(tpobs(i,j,bi,bj)) .lt. 1.d-8 ) then
                 tpmask(i,j,bi,bj) = 0. _d 0
              endif

              if ( R_low(i,j,bi,bj) .GT. -200. ) then
                tpmask(i,j,bi,bj) = 0. _d 0
              endif

              tpmask(i,j,bi,bj) = tpmask(i,j,bi,bj)*frame(i,j)
              tpobs(i,j,bi,bj)  = tpmask(i,j,bi,bj)*factor*
     &             ( tpobs(i,j,bi,bj) -
     &               ( topexintercept + topexslope*irec*hoursperday ) )
            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_SSH_TPANOM_COST_CONTRIBUTION */

      return
      end
