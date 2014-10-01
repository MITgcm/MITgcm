C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_all.F,v 1.15 2014/10/01 12:54:03 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


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
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      character*(max_len_mbuf) msgbuf
      integer nnzbar
      integer nnzobs
      integer k, kk

c     == external functions ==

c     == end of interface ==

      do k = 1, NGENCOST

         if ( (using_gencost(k)).AND.( gencost_flag(k).EQ.1 )
     &   .AND.( .NOT.gencost_is3d(k) ) ) then

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
         write(msgbuf,'(a,x,i2.2)') 'gencost #', k
         call print_message( msgbuf, standardmessageunit,
     &        SQUEEZE_RIGHT , mythid)
      ENDIF
#endif

         nnzbar = 1
         nnzobs = 1
         call cost_generic(
     &     nnzbar, gencost_barfile(k),
     &     gencost_barfld(1-Olx,1-Oly,1,1,k),
     &     gencost_dummy(k),
     &     nnzobs, gencost_datafile(k), mult_gencost(k),
     &     gencost_nrec(k), gencost_startdate(1,k), gencost_period(k),
     &     gencost_mask(k), gencost_weight(1-Olx,1-Oly,1,1,k),
     &     gencost_spmin(k), gencost_spmax(k),
     &     gencost_spzero(k), gencost_preproc(k),
     &     objf_gencost(1,1,k), num_gencost(1,1,k),
     &     myiter, mytime, mythid )

#ifdef ALLOW_GENCOST3D
         elseif ( (using_gencost(k)).AND.( gencost_flag(k).EQ.1 )
     &   .AND.( gencost_is3d(k) ) ) then

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
         write(msgbuf,'(a,x,i2.2)') 'gencost #', k
         call print_message( msgbuf, standardmessageunit,
     &        SQUEEZE_RIGHT , mythid)
      ENDIF
#endif

         nnzbar = nr
         nnzobs = nr
         kk=gencost_pointer3d(k)
         call cost_generic(
     &     nnzbar, gencost_barfile(k),
     &     gencost_bar3d(1-Olx,1-Oly,1,1,1,kk),
     &     gencost_dummy(k),
     &     nnzobs, gencost_datafile(k), mult_gencost(k),
     &     gencost_nrec(k), gencost_startdate(1,k), gencost_period(k),
     &     gencost_mask(k), gencost_wei3d(1-Olx,1-Oly,1,1,1,kk),
     &     gencost_spmin(k), gencost_spmax(k),
     &     gencost_spzero(k), gencost_preproc(k),
     &     objf_gencost(1,1,k), num_gencost(1,1,k),
     &     myiter, mytime, mythid )
#endif

         endif

      end do

      call cost_gencost_boxmean( mythid )
      call cost_gencost_bpv4( mythid )
      IF (using_cost_altim) call cost_gencost_sshv4( mythid )
      IF (using_cost_sst) call cost_gencost_sstv4( mythid )
      IF (using_cost_seaice) call cost_gencost_seaicev4( mythid )

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
