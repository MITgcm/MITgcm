C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readscatxfields.F,v 1.1 2003/11/06 22:10:07 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ReadscatxFields(
     I                               irec,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE cost_ReadscatxFields
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
c     SUBROUTINE cost_ReadscatxFields
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
      integer scatxrec
      integer beginscatx
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

      beginscatx = scatxstartdate(1)/10000
      beginrun = modelstartdate(1)/10000
      if ( beginscatx .eq. beginrun ) then
        scatxrec = mod(modelstartdate(1)/100,100) -
     &           mod(scatxstartdate(1)/100,100) + irec
      else
        scatxrec = ( beginrun - beginscatx - 1)*nmonthyear +
     &           (nmonthyear - mod(scatxstartdate(1)/100,100) +
     &            1) + mod(modelstartdate(1)/100,100) - 1 + irec
      endif

      if ( scatxrec .gt. 0 .and. scatxdatfile .ne. ' ' ) then
        call mdsreadfield( scatxdatfile, cost_iprec, cost_yftype, 1, 
     &                   scatxdat, scatxrec, mythid )
      else
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do j = jmin,jmax
                  do i = imin,imax
                     scatxdat(i,j,bi,bj)=0. _d 0
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
                scatxmask(i,j,bi,bj) = 0. _d 0
              else
                scatxmask(i,j,bi,bj) = 1. _d 0
              endif
              if (scatxdat(i,j,bi,bj) .lt. spval) then
                scatxmask(i,j,bi,bj) = 0. _d 0
              endif
              if (scatxdat(i,j,bi,bj) .eq. 0. _d 0 ) then
                 scatxmask(i,j,bi,bj) = 0. _d 0
              endif
              scatxmask(i,j,bi,bj) = scatxmask(i,j,bi,bj)*frame(i,j)
              scatxdat(i,j,bi,bj) = scatxdat(i,j,bi,bj)*
     &                              scatxmask(i,j,bi,bj)
              nobs = nobs + int(scatxmask(i,j,bi,bj))
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
            vartile=vartile+scatxdat(i,j,bi,bj)*scatxdat(i,j,bi,bj)
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

