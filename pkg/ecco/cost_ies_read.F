C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_ies_read.F,v 1.2 2012/08/10 19:45:26 jmc Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


      subroutine cost_ies_read(
     I                               irec,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE cost_ies_read
c     ==================================================================
c
c     o Read a given record of inverted echo sounder data.
c
c     started: Matt Mazloff May-2010
c
c     ==================================================================
c     SUBROUTINE cost_ies_read
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

#ifdef ALLOW_IESTAU_COST_CONTRIBUTION

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nobs
      integer iesrec
      integer beginies
      integer beginrun

      _RL spval
      _RL vartile

cnew(
      integer  il
c      integer mody, modm
c      integer iyear, imonth
      character*(80) fnametmp
c      logical exst
cnew)

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      parameter (spval = -998. )
ce    --> there is certainly a better place for this.

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c      mody = modelstartdate(1)/10000
c      modm = modelstartdate(1)/100 - mody*100
c      iyear = mody + INT((modm-1+irec-1)/12)
c      imonth = 1 + MOD(modm-1+irec-1,12)

       il=ilnblnk(iesdatfile)
c      write(fnametmp(1:80),'(2a,i4)')
c     &     iesdatfile(1:il), '_', iyear
c      inquire( file=fnametmp, exist=exst )
c      if (.NOT. exst) then
         write(fnametmp(1:80),'(a)') iesdatfile(1:il)
c         imonth = irec
c      endif

      call mdsreadfield( fnametmp, cost_iprec, cost_yftype, 1,
     &                   iesdat, irec, mythid )
CMM debug     &                   iesdat, (irec+570), mythid )

      nobs = 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
              if (maskC(i,j,k,bi,bj) .eq. 0.) then
                iesmask(i,j,bi,bj) = 0. _d 0
              else
                iesmask(i,j,bi,bj) = 1. _d 0
              endif
              if (iesdat(i,j,bi,bj) .lt. spval) then
                iesmask(i,j,bi,bj) = 0. _d 0
              elseif (iesdat(i,j,bi,bj) .eq. 0. _d 0) then
                iesmask(i,j,bi,bj) = 0. _d 0
              else
                iesmask(i,j,bi,bj) = 1. _d 0
              endif
              iesdat(i,j,bi,bj) = iesdat(i,j,bi,bj)*
     &                              iesmask(i,j,bi,bj)
              nobs = nobs + int(iesmask(i,j,bi,bj))
            enddo
          enddo
        enddo
      enddo

#endif

      return
      end

