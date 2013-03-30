C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_cost_final.F,v 1.1 2013/03/30 01:23:58 heimbach Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      subroutine thsice_cost_final( mythid )

c     ==================================================================
c     SUBROUTINE thsice_cost_final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "THSICE_PARAMS.h"
#ifdef ALLOW_COST
#include "THSICE_COST.h"
#include "cost.h"
#include "ctrl.h"
#include "optim.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi

      _RL f_thsice
      _RL no_thsice

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      print *, 'ph-2 in thsice_cost_final'

      f_thsice      = 0. _d 0
      no_thsice     = 0. _d 0

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          fc = fc
     &          + mult_thsice        * objf_thsice(bi,bj)

          f_thsice = f_thsice + objf_thsice(bi,bj)
          no_thsice = no_thsice + num_thsice(bi,bj)

        enddo
      enddo

c--   Do global summation.
cph this is done only in cost_final!
cph      _GLOBAL_SUM_RL( fc , myThid )

c--   Do global summation for each part of the cost function

cc      _GLOBAL_SUM_RL( f_thsice , myThid )
cc      _GLOBAL_SUM_RL( no_thsice , myThid )

      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_thsice     = ', f_thsice

      THSICE_diagFreq = 0.
      THSICE_taveFreq = 0.

#endif /* ALLOW_COST */

      return
      end
