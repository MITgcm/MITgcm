C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_customize.F,v 1.6 2013/03/28 17:24:41 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

      subroutine cost_gencost_customize( mythid )

c     ==================================================================
c     SUBROUTINE cost_gencost_assignperiod
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

#include "optim.h"
#include "ecco_cost.h"
#ifdef ALLOW_SEAICE
#  include "SEAICE_SIZE.h"
#  include "SEAICE.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      do bj = jtlo,jthi
       do bi = itlo,ithi
        do j = jmin,jmax
         do i =  imin,imax
c #1:
c          gencost_modfld(i,j,bi,bj,1) = qnet(i,j,bi,bj)
c #2:
          gencost_modfld(i,j,bi,bj,1) = empmr(i,j,bi,bj)
c #3:
c          gencost_modfld(i,j,bi,bj,3) = fu(i,j,bi,bj)
c #4:
c          gencost_modfld(i,j,bi,bj,4) = fv(i,j,bi,bj)
c

#ifdef ALLOW_GENCOST_SEAICEV4
      do k=1,NGENCOST
        if (gencost_name(k).EQ.'siv4-conc') gencost_modfld(i,j,bi,bj,k)
     &        = area(i,j,bi,bj)*maskC(i,j,1,bi,bj) 
        if (gencost_name(k).EQ.'siv4-sst') gencost_modfld(i,j,bi,bj,k)
     &        = theta(i,j,1,bi,bj)*maskC(i,j,1,bi,bj)
        if (gencost_name(k).EQ.'siv4-vol') gencost_modfld(i,j,bi,bj,k)  
     &        = heff(i,j,bi,bj)*maskC(i,j,1,bi,bj)
      enddo
#endif

         enddo
        enddo
       enddo
      enddo

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
