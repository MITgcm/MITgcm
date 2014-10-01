C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final.F,v 1.34 2014/10/01 12:55:58 gforget Exp $
C $Name:  $

#include "COST_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE COST_FINAL( myThid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================

      IMPLICIT NONE

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
      INTEGER myThid

#ifdef ALLOW_COST
c     == local variables ==
      INTEGER bi,bj
      _RL glob_fc, loc_fc

c     == end of interface ==

#ifdef ALLOW_SEAICE
      if (useSEAICE) CALL SEAICE_COST_FINAL (myThid)
#endif

#ifdef ALLOW_SHELFICE
      CALL SHELFICE_COST_FINAL (myThid)
#endif

c     print *, 'ph-1 in thsice_cost_final'
#ifdef ALLOW_THSICE
      IF (useThSIce) CALL THSICE_COST_FINAL (myThid)
#endif
c     print *, 'ph-3 in thsice_cost_final'

#ifdef ALLOW_ECCO
      CALL ECCO_COST_FINAL (myThid)
#endif

#ifdef ALLOW_COST_STATE_FINAL
      CALL COST_STATE_FINAL (myThid)
cgf : effectively using this in adjoint requires the
c     use of adjoint_state_final. That will activate the
c     objf_state_final vector in place of the fc scalar.
c     objf_state_final is therefore not added to fc.
#endif

#ifdef ALLOW_COST_VECTOR
cgf : same idea as for ALLOW_COST_STATE_FINAL
      CALL COST_VECTOR (myThid)
#endif

# ifdef ALLOW_COST_TEST
      CALL COST_TEST (myThid)
# endif

# ifdef ALLOW_COST_ATLANTIC_HEAT
      CALL COST_ATLANTIC_HEAT (myThid)
# endif

#ifdef ALLOW_COST_HFLUXM
cgf : to compile previous line user is expected to provide cost_hflux.F
      CALL COST_HFLUX (myThid)
#endif

#ifdef ALLOW_COST_TEMP
      CALL COST_TEMP (myThid)
cgf : to compile previous line user is expected to provide cost_temp.F
#endif

      write(standardmessageunit,'(A,D22.15)') '  early fc = ', fc

c--   Sum up all contributions.
      loc_fc = 0.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_COST_TEST
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_test(bi,bj)   = ', objf_test(bi,bj)
#endif
#ifdef ALLOW_COST_TRACER
          write(standardmessageunit,'(A,D22.15)')
     &         ' --> objf_tracer(bi,bj) = ', objf_tracer(bi,bj)
#endif
#if ( !defined (ALLOW_ECCO) || !defined (ALLOW_COST_ATLANTIC) )
# ifdef ALLOW_COST_ATLANTIC_HEAT
          write(standardmessageunit,'(A,D22.15)')
     &         ' --> objf_atl(bi,bj)    = ', objf_atl(bi,bj)
# endif
#endif
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

         tile_fc(bi,bj) = tile_fc(bi,bj)
#ifdef ALLOW_COST_TEST
     &            + mult_test   * objf_test(bi,bj)
#endif
#ifdef ALLOW_COST_TRACER
     &            + mult_tracer * objf_tracer(bi,bj)
#endif
#if ( !defined (ALLOW_ECCO) || !defined (ALLOW_COST_ATLANTIC) )
# ifdef ALLOW_COST_ATLANTIC_HEAT
     &            + mult_atl    * objf_atl(bi,bj)
# endif
#endif
#ifdef ALLOW_COST_TRANSPORT
     &            + mult_transport * objf_transport(bi,bj)
#endif
#ifdef ALLOW_COST_TEMP
     &            + mult_temp_tut  * objf_temp_tut(bi,bj)
#endif
#ifdef ALLOW_COST_HFLUXM
     &            + mult_hflux_tut * objf_hflux_tut(bi,bj)
#endif
         loc_fc = loc_fc + tile_fc(bi,bj)
       ENDDO
      ENDDO

      write(standardmessageunit,'(A,D22.15)') '  local fc = ', loc_fc

c--   Do global summation.
      CALL GLOBAL_SUM_TILE_RL( tile_fc, glob_fc, myThid )
      _BEGIN_MASTER( myThid )
      fc = fc + glob_fc
      _END_MASTER( myThid )

c--   Add contributions from global mean constraints
      _BEGIN_MASTER( myThid )
      fc = fc + glofc
      _END_MASTER( myThid )

#ifdef ALLOW_DIC_COST
cph-- quickly for testing
      fc = totcost
#endif

      write(standardmessageunit,'(A,D22.15)') ' global fc = ', fc

c--   to avoid re-write of output in reverse checkpointing loops,
c--   switch off output flag :
      CALL TURNOFF_MODEL_IO( 0, myThid )

#endif /* ALLOW_COST */

      return
      end
