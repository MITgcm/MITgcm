C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readtopex.F,v 1.1 2003/11/06 22:10:07 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ReadTopex(
     I                           irec,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cost_ReadTopex
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
c     SUBROUTINE cost_ReadTopex
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
      integer nobs
      integer difftime(4)
      integer middate(4)
      integer noffset
      _RL diffsecs
      _RL spval
      _RL factor
      _RL vartile

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

      middate(1) = modelstartdate(1)
      middate(2) = 120000
      middate(3) = modelstartdate(3)
      middate(4) = modelstartdate(4)

      call cal_TimePassed( topexstartdate, middate, difftime, mythid )
      call cal_ToSeconds( difftime, diffsecs, mythid )

      noffset = int(diffsecs/topexperiod)
      sshrec  = noffset + irec
      nobs    = 0

      if ( sshrec.gt.0 .and. topexfile .ne. ' ' ) then
        call mdsreadfield( topexfile, cost_iprec, cost_yftype, 1, tpobs, 
     &                   sshrec, mythid )
      else
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do j = jmin,jmax
                  do i = imin,imax
                     tpobs(i,j,bi,bj)=spval
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

cph(
cph               print *, 'WARNING: SPECIFIC SETUP FOR ECCO'
cph               below statement could be replaced by following
cph               to make it independnet of Nr:
cph
cph               if ( rC(K) .GT. -1000. ) then
cph)
c             set tpmask=0 in areas shallower than 1000m 
              if (_hFacC(i,j,13,bi,bj) .eq. 0.) then
                tpmask(i,j,bi,bj) = 0. _d 0
              endif

              tpmask(i,j,bi,bj) = tpmask(i,j,bi,bj)*frame(i,j)
              tpobs(i,j,bi,bj)  = tpobs(i,j,bi,bj)*
     &                            tpmask(i,j,bi,bj)*
     &                            factor
              nobs = nobs + int(tpmask(i,j,bi,bj))
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
              vartile = vartile + tpobs(i,j,bi,bj)*
     &                            tpobs(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo

      if (nobs .gt. 0) then
        vartile = vartile/float(nobs)
      else
        vartile = spval
      endif

#endif /* ALLOW_SSH_TPANOM_COST_CONTRIBUTION */

      return
      end

