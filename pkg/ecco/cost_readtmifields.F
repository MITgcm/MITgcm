C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readtmifields.F,v 1.1 2003/11/06 22:10:07 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ReadTMIFields(
     I                               irec,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE cost_ReadTMIFields
c     ==================================================================
c
c     o Read a given record of the TMI SST data.
c
c     started: ARmin Koehl akoehl@ucsd.edu
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_ReadTMIFields
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
      integer tmirec
      integer begintmi
      integer beginrun

      _RL spval
      _RL vartile

c     == end of interface ==

      parameter (spval = -9999. ) 
ce    --> there is certainly a better place for this.

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      begintmi = tmistartdate(1)/10000
      beginrun = modelstartdate(1)/10000
      if ( begintmi .eq. beginrun ) then
        tmirec = mod(modelstartdate(1)/100,100) -
     &           mod(tmistartdate(1)/100,100) + irec
      else
        tmirec = ( beginrun - begintmi - 1)*nmonthyear +
     &           (nmonthyear - mod(tmistartdate(1)/100,100) +
     &            1) + mod(modelstartdate(1)/100,100) - 1 + irec
      endif
      if (tmirec.gt.0) then
      call mdsreadfield( tmidatfile, cost_iprec, cost_yftype, 1, tmidat,
     &                   tmirec, mythid )
      else         
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do j = jmin,jmax
                  do i = imin,imax
                     tmidat(i,j,bi,bj)=spval
                  enddo
               enddo
            enddo
         enddo
      endif
      return
      end

