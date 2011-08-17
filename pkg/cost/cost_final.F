C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final.F,v 1.27 2011/08/17 10:19:29 heimbach Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

      subroutine cost_final( mythid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "cost.h"
#ifdef ALLOW_CTRL
# include "ctrl.h"
#endif
#ifdef ALLOW_DIC
# include "DIC_COST.h"
#endif
#ifdef ALLOW_COST_SHELFICE
# include "SHELFICE_COST.h"
#endif


c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST
c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

#ifdef ALLOW_SEAICE
      CALL SEAICE_COST_FINAL (myThid)
#endif

#ifdef ALLOW_SHELFICE
      CALL SHELFICE_COST_FINAL (myThid)
#endif

#if (defined (ALLOW_ECCO))
      CALL ECCO_COST_FINAL (myThid)

#elif (defined (ALLOW_COST_VECTOR))
      CALL COST_VECTOR (myThid)

#elif (defined (ALLOW_COST_STATE_FINAL))
      CALL COST_STATE_FINAL (myThid)

#endif /* above stuff undef */

#ifndef ALLOW_ECCO

# ifdef ALLOW_COST_TEST
      CALL COST_TEST (myThid)
# endif
# ifdef ALLOW_COST_ATLANTIC_HEAT
      CALL COST_ATLANTIC_HEAT (myThid)
# endif
#ifdef ALLOW_COST_HFLUXM
      CALL COST_HFLUX (myThid)
#endif
#ifdef ALLOW_COST_TEMP
      CALL COST_TEMP (myThid)
#endif

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_test(bi,bj)   = ', objf_test(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &         ' --> objf_tracer(bi,bj) = ', objf_tracer(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &         ' --> objf_atl(bi,bj)    = ', objf_atl(bi,bj)
#ifdef ALLOW_COST_TEMP
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_temp_tut(bi,bj)   = ', objf_temp_tut(bi,bj)
#endif
#ifdef ALLOW_COST_HFLUXM
          write(standardmessageunit,'(A,D22.15)')
     &         ' --> objf_hflux_tut(bi,bj) = ', objf_hflux_tut(bi,bj)
#endif
#ifdef ALLOW_COST_TRANSPORT
          write(standardmessageunit,'(A,D22.15)')
     &         ' --> objf_transport(bi,bj) = ', objf_transport(bi,bj)
#endif

          fc = fc
     &            + mult_test   * objf_test(bi,bj)
     &            + mult_tracer * objf_tracer(bi,bj)
     &            + mult_atl    * objf_atl(bi,bj)
#ifdef ALLOW_COST_TRANSPORT
     &            + mult_transport * objf_transport(bi,bj)
#endif
#ifdef ALLOW_COST_TEMP
     &            + mult_temp_tut  * objf_temp_tut(bi,bj)
#endif
#ifdef ALLOW_COST_HFLUXM
     &            + mult_hflux_tut * objf_hflux_tut(bi,bj)
#endif
        enddo
      enddo

      write(standardmessageunit,'(A,D22.15)') '  local fc = ', fc

c--   Do global summation.
      _GLOBAL_SUM_RL( fc , myThid )

#ifdef ALLOW_DIC_COST
cph-- quickly for testing
      fc = totcost
#endif

      write(standardmessageunit,'(A,D22.15)') ' global fc = ', fc

#endif /* ALLOW_ECCO */

c--   to avoid re-write of output in reverse checkpointing loops,
c--   switch off output flag :
      CALL TURNOFF_MODEL_IO( 0, myThid )

#endif /* ALLOW_COST */

      return
      end
