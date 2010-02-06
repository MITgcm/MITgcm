C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_customize.F,v 1.1 2010/02/06 02:43:03 heimbach Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

      subroutine cost_gencost_customize( mythid )
      
c     ==================================================================
c     SUBROUTINE cost_gencost_assignperiod
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

#include "optim.h"
#include "ecco_cost.h"
#include "ctrl_dummy.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

c     == end of interface ==

      do bj = jtlo,jthi
       do bi = itlo,ithi
        do j = jmin,jmax
         do i =  imin,imax
c #1:
          gencost_modfld(i,j,bi,bi,1) = qnet(i,j,bi,bi)
c #2:
          gencost_modfld(i,j,bi,bi,2) = empmr(i,j,bi,bi)
c #3:
          gencost_modfld(i,j,bi,bi,3) = fu(i,j,bi,bi)
c #4:
          gencost_modfld(i,j,bi,bi,4) = fv(i,j,bi,bi)
c
         enddo
        enddo
       enddo
      enddo

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
