C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readsstfields.F,v 1.2 2004/10/11 16:38:53 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ReadSSTFields(
     I                               irec,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE cost_ReadSSTFields
c     ==================================================================
c
c     o Read a given record of the SST data.
c
c     started: Christian Eckert eckert@mit.edu 25-May-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_ReadSSTFields
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

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nobs
      integer sstrec
      integer beginsst
      integer beginrun

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

      parameter (spval = -1.8 ) 
ce    --> there is certainly a better place for this.

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c      beginsst = sststartdate(1)/10000
c      beginrun = modelstartdate(1)/10000
c      if ( beginsst .eq. beginrun ) then
c        sstrec = mod(modelstartdate(1)/100,100) -
c     &           mod(sststartdate(1)/100,100) + irec
c      else
c        sstrec = ( beginrun - beginsst - 1)*nmonthyear +
c     &           (nmonthyear - mod(sststartdate(1)/100,100) +
c     &            1) + mod(modelstartdate(1)/100,100) - 1 + irec
c      endif

cnew(
      mody = modelstartdate(1)/10000
      modm = modelstartdate(1)/100 - mody*100
      iyear = mody + INT((modm-1+irec-1)/12)
      imonth = 1 + MOD(modm-1+irec-1,12)

      il=ilnblnk(sstdatfile)
      write(fnametmp(1:80),'(2a,i4)') 
     &     sstdatfile(1:il), '_', iyear
      inquire( file=fnametmp, exist=exst )
      if (.NOT. exst) then
         write(fnametmp(1:80),'(a)') sstdatfile(1:il)
         imonth = irec
      endif

      print *, 'ph-cost-t SST ', irec, imonth, iyear, fnametmp
      
      call mdsreadfield( fnametmp, cost_iprec, cost_yftype, 1, sstdat,
     &                   imonth, mythid )
cnew)

      nobs = 0
      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax

              if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                sstmask(i,j,bi,bj) = 0. _d 0
              else if (_hFacC(i,j,13,bi,bj) .eq. 0.) then
                sstmask(i,j,bi,bj) = 0. _d 0
              else
                sstmask(i,j,bi,bj) = 1. _d 0
              endif
              if (sstdat(i,j,bi,bj) .lt. spval) then
                sstmask(i,j,bi,bj) = 0. _d 0
              endif
              if (sstdat(i,j,bi,bj) .eq. 0. _d 0 ) then
                 sstmask(i,j,bi,bj) = 0. _d 0
              endif

              sstmask(i,j,bi,bj) = sstmask(i,j,bi,bj)*frame(i,j)
              sstdat(i,j,bi,bj) = sstdat(i,j,bi,bj)*sstmask(i,j,bi,bj)
              nobs = nobs + int(sstmask(i,j,bi,bj))

            enddo
          enddo
        enddo
      enddo

c--   Calculate the field variance for present subdomain.
c--   One could of course do a global sum here.
      vartile = 0. _d 0
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              vartile = vartile + sstdat(i,j,bi,bj)*sstdat(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo

      if (nobs .gt. 0) then
        vartile = vartile/float(nobs)
      else
        vartile = 0.0
      endif

      return
      end

