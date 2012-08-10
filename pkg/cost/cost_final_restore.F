C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final_restore.F,v 1.6 2012/08/10 19:36:02 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"


      subroutine cost_final_restore ( mythid, lastdiva )

c     ==================================================================
c     SUBROUTINE cost_final_restore
c     ==================================================================
c
c     Restore cost function value
c     This is required in conjunction with DIVA
c     heimbach@mit.edu 5-Mar-2003
c
c     ==================================================================
c     SUBROUTINE cost_final_restore
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
      logical exst

      integer idivbeg, idivend
c     == end of interface ==

c--   Each process has calculated the global part for itself.
cph      IF (myProcId .eq. 0) THEN
         _BEGIN_MASTER( mythid )
c
           inquire(file='divided.ctrl',exist=exst)
           if (exst) then
              open(unit=76,file='divided.ctrl',form='formatted')
              read(unit=76,fmt=*) idivbeg,idivend
              close(unit=76)
           else
              idivbeg = -2
           endif
c
           if ( idivbeg .EQ. 0 ) then
              lastdiva = .TRUE.
              open(unit=76,file='costfinal')
              read(76,*) fc
              close(76)
           else
              lastdiva = .FALSE.
           endif
c
         _END_MASTER( mythid )
cph      ENDIF
      _BARRIER

#endif

      end
