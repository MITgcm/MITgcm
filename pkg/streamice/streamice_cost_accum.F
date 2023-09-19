#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

      subroutine streamice_cost_accum ( myIter, myThid )
C     *==========================================================*
C     | subroutine cost_test                                     |
C     | o this routine accumulates the STREAMICE cost func-      |
C     |   tion at each time step, called from cost_tile          |
C     *==========================================================*
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif

#ifdef ALLOW_COST
# include "cost.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myIter, myThid

#ifdef ALLOW_COST
C     == Local variables
      _RL thetaRef, HAF

      integer i, j, k, bi, bj
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
      integer il
      integer itime
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
c       k=1

c#ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE surf_el_streamice = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
cCADJ STORE H_streamice_prev  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
cCADJ STORE H_streamice       = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
c#endif

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j=1,sNy
            do i=1,sNx

             if (streamice_cost_mask(i,j,bi,bj).eq.1.0) THEN
              cost_func1_streamice (bi,bj) =
     &        cost_func1_streamice (bi,bj) + 0.0
             endif

            end do
          end do
        end do
      end do

#ifdef ALLOW_STREAMICE_TC_COST
      IF ( STREAMICE_do_timedep_cost ) THEN
       CALL STREAMICE_COST_REG_ACCUM (myIter, myThid)
       DO itime = 1,streamiceMaxCostLevel
        IF (myIter==streamice_surf_cost_timesteps(itime)) THEN
         CALL STREAMICE_COST_SURF_ACCUM (myIter, myThid)
        ENDIF
        IF (myIter==streamice_vel_cost_timesteps(itime)) THEN
         CALL STREAMICE_COST_VEL_ACCUM (myIter, myThid)
        ENDIF
       ENDDO
      ENDIF
#endif

#endif

      RETURN
      END
