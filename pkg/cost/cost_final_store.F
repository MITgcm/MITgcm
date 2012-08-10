C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final_store.F,v 1.4 2012/08/10 19:36:02 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"


      subroutine cost_final_store ( mythid, lastdiva )

c     ==================================================================
c     SUBROUTINE cost_final_store
c     ==================================================================
c
c     Store cost function value
c     This is required in conjunction with DIVA
c     heimbach@mit.edu 5-Mar-2003
c
c     ==================================================================
c     SUBROUTINE cost_final_store
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cost.h"

c     == routine arguments ==

      integer mythid
      logical lastdiva

# ifdef ALLOW_DIVIDED_ADJOINT

c     == local variables ==

c     == end of interface ==

c--   Each process has calculated the global part for itself.
      lastdiva = .FALSE.

      IF (myProcId .eq. 0) THEN
         _BEGIN_MASTER( mythid )
           open(unit=76,file='costfinal')
           write(76,*) fc
           close(76)
         _END_MASTER( mythid )
      ENDIF
      _BARRIER

#endif

      end
