C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readsstfields.F,v 1.1 2003/11/06 22:10:07 heimbach Exp $

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

      beginsst = sststartdate(1)/10000
      beginrun = modelstartdate(1)/10000

      if ( beginsst .eq. beginrun ) then
        sstrec = mod(modelstartdate(1)/100,100) -
     &           mod(sststartdate(1)/100,100) + irec
      else
        sstrec = ( beginrun - beginsst - 1)*nmonthyear +
     &           (nmonthyear - mod(sststartdate(1)/100,100) +
     &            1) + mod(modelstartdate(1)/100,100) - 1 + irec
      endif

      if ( sstrec.gt.0 .and. sstdatfile .ne. ' ' ) then
        call mdsreadfield( sstdatfile, cost_iprec, cost_yftype, 1,
     &                   sstdat, sstrec, mythid )
      else
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do j = jmin,jmax
                  do i = imin,imax
                     sstdat(i,j,bi,bj)=spval
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

              if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                sstmask(i,j,bi,bj) = 0. _d 0
              else
                sstmask(i,j,bi,bj) = 1. _d 0
              endif
              if (sstdat(i,j,bi,bj) .le. spval) then
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
        vartile = spval
      endif

      return
      end

