C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_all.F,v 1.5 2010/02/16 02:08:16 gforget Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_gencost_all( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_gencost_all
c     ==================================================================
c
c     o Evaluate user defined penalty terms
c
c     ==================================================================
c     SUBROUTINE cost_gencost_all
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "ecco_cost.h"

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer var_num
      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer ilps

      logical doglobalread
      logical ladinit

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifdef ALLOW_GENCOSTBUG
      do k = 1, NGENCOST

         write(msgbuf,'(a,x,i2.2)') 'ph-cost gencost #', k
         call print_message( msgbuf, standardmessageunit,
     &        SQUEEZE_RIGHT , mythid)
         nnzbar = 1
         nnzobs = 1
         call cost_generic(
     &        nnzbar, gencost_barfile(k), 
     &        gencost_barfld(1-Olx,1-Oly,1,1,k), 
     &        xx_genbar_dummy(k),
     &        nnzobs, gencost_datafile(k), mult_gencost(k),
     &        gencost_nrec(k), modelstartdate, gencost_period(k),
     &        gencost_spzero(k),
===+++>>>     &        gencost_mask(k), wsst,
     &        gencost_spmin(k), gencost_spmax(k), 
     &        objf_gencost(1,1,k), num_gencost(1,1,k),
     &        myiter, mytime, mythid )

      end do
#endif /* ALLOW_GENCOSTBUG */

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
