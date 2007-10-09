C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readargos.F,v 1.2 2007/10/09 00:02:50 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_Readargos(
     I                               irec,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE cost_Readargos
c     ==================================================================
c
c     o Read a given record of the
c
c     started:Armin Koehl akoehl@ucsd.edu
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_Readargos
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
      integer argosrec
      integer beginargos
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

      beginargos = argosstartdate(1)/10000
      beginrun = modelstartdate(1)/10000
      if ( beginargos .eq. beginrun ) then
        argosrec = mod(modelstartdate(1)/100,100) -
     &           mod(argosstartdate(1)/100,100) + irec
      else
        argosrec = ( beginrun - beginargos - 1)*nmonthyear +
     &           (nmonthyear - mod(argosstartdate(1)/100,100) +
     &            1) + mod(modelstartdate(1)/100,100) - 1 + irec
      endif
      if (argosrec.gt.0) then
      call mdsreadfield( argosfile, cost_iprec, cost_yftype, nr,
     &        argosobs,argosrec, mythid )
      else
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,nr
               do j = jmin,jmax
                  do i = imin,imax
                     argosobs(i,j,k,bi,bj)=spval
                  enddo
               enddo
               enddo
            enddo
         enddo
      endif
      return
      end

