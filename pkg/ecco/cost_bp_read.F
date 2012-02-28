C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_bp_read.F,v 1.3 2012/02/28 00:51:07 gforget Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_bp_read(
     I                               irec,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE cost_bp_read
c     ==================================================================
c
c     o Read a given record of the GRACE data.
c
c     started: Gael Forget Oct-2009
c
c     ==================================================================
c     SUBROUTINE cost_bp_read
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

#ifdef ALLOW_BP_COST_CONTRIBUTION

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nobs
      integer bprec
      integer beginbp
      integer beginrun

      INTEGER beginlocal, beginmodel, obsrec

      _RL spval
      _RL vartile

cnew(
      integer  il
      integer mody, modm
      integer iyear, imonth
      character*(80) fnametmp
      logical exst
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


       beginlocal = bpstartdate(1)/10000
       beginmodel = modelstartdate(1)/10000
       obsrec =  ( beginmodel - beginlocal )*nmonthyear
     &         + ( mod(modelstartdate(1)/100,100)
     &            -mod(bpstartdate(1)/100,100) )
     &         + irec

      mody = modelstartdate(1)/10000
      modm = modelstartdate(1)/100 - mody*100
      iyear = mody + INT((modm-1+irec-1)/12)
      imonth = 1 + MOD(modm-1+irec-1,12)

      il=ilnblnk(bpdatfile)
      write(fnametmp(1:80),'(2a,i4)')
     &     bpdatfile(1:il), '_', iyear
      inquire( file=fnametmp, exist=exst )
      if (.NOT. exst) then
         write(fnametmp(1:80),'(a)') bpdatfile(1:il)
         imonth = obsrec
      endif

      if ( (obsrec.GT.0).AND.(imonth.GT.0) ) then
        call mdsreadfield( fnametmp, cost_iprec, cost_yftype, 1,
     &                   bpdat, imonth, mythid )
      else
        do bj = jtlo,jthi
         do bi = itlo,ithi
          do j = jmin,jmax
           do i = imin,imax
            bpdat(i,j,bi,bj) = spval
           enddo
          enddo
         enddo
        enddo
      endif

      nobs = 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c              if (maskC(i,j,k,bi,bj) .eq. 0.) then
c                bpmask(i,j,bi,bj) = 0. _d 0
c              else
c                bpmask(i,j,bi,bj) = 1. _d 0
c              endif
              if (bpdat(i,j,bi,bj) .le. spval) then
                bpmask(i,j,bi,bj) = 0. _d 0
              else
                bpmask(i,j,bi,bj) = 1. _d 0
              endif
              bpdat(i,j,bi,bj) = bpdat(i,j,bi,bj)*
     &                              bpmask(i,j,bi,bj)
              nobs = nobs + int(bpmask(i,j,bi,bj))
            enddo
          enddo
        enddo
      enddo

#endif

      return
      end

