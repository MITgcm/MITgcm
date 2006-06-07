C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final.F,v 1.15 2006/06/07 01:55:13 heimbach Exp $

#include "PACKAGES_CONFIG.h"
#include "COST_CPPOPTIONS.h"

      subroutine cost_final( mythid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c     heimbach@mit.edu 05-Nov-2003 Modularize cost package
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

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST
c     == local variables ==

      integer i,j,k
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

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          write(standardmessageunit,'(A,D22.15)') 
     &          ' --> objf_test(bi,bj)   = ', objf_test(bi,bj)
          write(standardmessageunit,'(A,D22.15)') 
     &         ' --> objf_tracer(bi,bj) = ', objf_tracer(bi,bj)
          write(standardmessageunit,'(A,D22.15)') 
     &         ' --> objf_atl(bi,bj)    = ', objf_atl(bi,bj)
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
        enddo
      enddo

      write(standardmessageunit,'(A,D22.15)') '  local fc = ', fc

c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

      write(standardmessageunit,'(A,D22.15)') ' global fc = ', fc

#endif /* ALLOW_ECCO */

c--   set averaging freq. to zero to avoid re-write of
c--   averaged fields in reverse checkpointing loops
      taveFreq    = 0.
      dumpFreq    = 0.
      pChkptFreq  = 0.
      monitorFreq = 0.

#endif /* ALLOW_COST */

      return
      end

