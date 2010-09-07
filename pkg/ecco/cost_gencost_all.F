C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_all.F,v 1.7 2010/09/07 21:20:39 gforget Exp $
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

      integer i,j,k
      character*(max_len_mbuf) msgbuf

      integer nnzbar
      integer nnzobs
      integer nrecloc

c     == external functions ==

c     == end of interface ==

      do k = 1, NGENCOST

         if ( (gencost_name(k).NE.'sshv4-mdt').AND.
     &        (gencost_name(k).NE.'sshv4-tp').AND.
     &        (gencost_name(k).NE.'sshv4-ers').AND.
     &        (gencost_name(k).NE.'sshv4-gfo').AND.
     &        (gencost_name(k).NE.'sshv4-lsc').AND.
     &        (gencost_name(k).NE.'sstv4-amsre').AND.
     &        (gencost_name(k).NE.'sstv4-amsre-lsc') ) then

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
     &        gencost_mask(k), gencost_weight(1-Olx,1-Oly,1,1,k),
     &        gencost_spmin(k), gencost_spmax(k), 
     &        gencost_spzero(k),
     &        objf_gencost(1,1,k), num_gencost(1,1,k),
     &        myiter, mytime, mythid )

         endif

      end do

# if (defined(ALLOW_GENCOST_SSHV4))
      call cost_gencost_sshv4( myiter, mytime, mythid )
#endif /* ALLOW_GENCOST_SSHV4 */

# if (defined(ALLOW_GENCOST_SSTV4)) 
      call cost_gencost_sstv4( myiter, mytime, mythid )
#endif /* ALLOW_GENCOST_SSTV4 */

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
