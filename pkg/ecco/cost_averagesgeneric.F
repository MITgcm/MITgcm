C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_averagesgeneric.F,v 1.1 2005/09/01 05:32:56 heimbach Exp $

#include "COST_CPPOPTIONS.h"

      subroutine cost_averagesgeneric(
     &     localbarfile, 
     &     localbar, localfld, xx_localbar_mean_dummy,
     &     first, last, startofloc, endofloc, inloc,
     &     sum1loc, locrec, nnz, mythid )

c     ==================================================================
c     SUBROUTINE cost_averagesgeneric
c     ==================================================================
c
c     o Compute time averages of cost variables
c
c     ==================================================================
c     SUBROUTINE cost_averagesgeneric
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_COST
# include "optim.h"
# include "ecco_cost.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==

      integer mythid
      integer nnz
      integer locrec
      integer sum1loc

      _RL localbar(1-olx:snx+olx,1-oly:sny+oly,nnz,nsx,nsy)
      _RL localfld(1-olx:snx+olx,1-oly:sny+oly,nnz,nsx,nsy)
      _RL xx_localbar_mean_dummy

      logical first
      logical last
      logical startofloc
      logical endofloc
      logical inloc

      character*(128) localbarfile

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer il

      character*(128) fname

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

c--   First, do the daily averages.
      if (first .or. startofloc) then
c--     Assign the first value to the array holding the average.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnz
              do j = jmin,jmax
                do i =  imin,imax
                  localbar(i,j,k,bi,bj) = localfld(i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
      else if (last .or. endofloc) then
c--     Add the last value and devide by the number of accumulated records.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnz
              do j = jmin,jmax
                do i = imin,imax
                  localbar(i,j,k,bi,bj) = 
     &                  (localbar(i,j,k,bi,bj) 
     &                  +localfld(i,j,k,bi,bj))/
     &                  float(sum1loc)
                enddo
              enddo
            enddo
          enddo
        enddo
c--     Save ...bar on file.
        write(fname(1:128),'(80a)') ' '
        il=ilnblnk( localbarfile )
        write(fname,'(2a,i10.10)') 
     &       localbarfile(1:il), '.', optimcycle
        if ( nnz .EQ. 1 ) then
           call active_write_xy( fname, localbar, locrec, optimcycle,
     &          mythid, xx_localbar_mean_dummy )
        else
           call active_write_xyz( fname, localbar, locrec, optimcycle,
     &          mythid, xx_localbar_mean_dummy )
        endif
      else if (       (       inloc        ) .and.
     &          .not. (first .or. startofloc) .and.
     &          .not. (last  .or. endofloc  )       ) then
c--     Accumulate the array holding the average.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do k = 1,nnz
                do i = imin,imax
                  localbar(i,j,k,bi,bj) = 
     &                  localbar(i,j,k,bi,bj) + localfld(i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
      else
        stop   'in cost_averagesgeneric'
      endif

      return
      end

