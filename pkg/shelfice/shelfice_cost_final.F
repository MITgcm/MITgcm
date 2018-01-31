#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SHELFICE_COST_FINAL
C     !INTERFACE:
      SUBROUTINE SHELFICE_COST_FINAL( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | subroutine shelfice_cost_final                           |
C     | o final cost evalualtion                                 |
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SHELFICE.h"
#include "SHELFICE_COST.h"

#ifdef ALLOW_COST
# include "cost.h"
# include "optim.h"
#endif
#if  (defined ALLOW_SHIFWFLX_CONTROL && \
      defined ALLOW_SHIFWFLX_COST_CONTRIBUTION)
# include "ctrl.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_COST
C     ! FUNCTIONS:
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER ifc
      CHARACTER*(25) cfname

      _RL f_shelfice
      _RL f_shifwflx
cML      _RL no_shelfice
      _RL no_shifwflx

#ifndef ALLOW_ECCO
      INTEGER i, j
#endif
CEOP

      f_shelfice = 0. _d 0
      f_shifwflx = 0. _d 0
cML      no_shelfice= 0. _d 0
      no_shifwflx= 0. _d 0

#ifdef ALLOW_ECCO
#if  (defined ALLOW_SHIFWFLX_CONTROL && \
      defined ALLOW_SHIFWFLX_COST_CONTRIBUTION)
      CALL SHELFICE_COST_SHIFWFLX( myThid )
#endif /* ALLOW_SHIFWFLX_CONTROL and ALLOW_SHIFWFLX_COST_CONTRIBUTION */
#else
C--   Calculate cost function on tile of this instance
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
ccc        do klev = 1, Nr
          DO j=1,sNy
           DO i=1,sNx

             objf_shelfice(bi,bj) = objf_shelfice(bi,bj)
     &            + cMeanSHIforS(i,j,bi,bj)/lastinterval/rhoConstFresh
     &            *_rA(i,j,bi,bj)

           ENDDO
          ENDDO
ccc        end do
       ENDDO
      ENDDO
#endif /* ALLOW_ECCO */

C--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        write(standardmessageunit,'(A,D22.15)')
     &       ' --> objf_shelfice(bi,bj) = ', objf_shelfice(bi,bj)
        write(standardmessageunit,'(A,D22.15)')
     &       ' --> objf_shifwflx(bi,bj) = ', objf_shifwflx(bi,bj)
        tile_fc(bi,bj) = tile_fc(bi,bj)
     &       + mult_shelfice * objf_shelfice(bi,bj)
     &       + mult_shifwflx * objf_shifwflx(bi,bj)

       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( objf_shelfice, f_shelfice, myThid )
      CALL GLOBAL_SUM_TILE_RL( objf_shifwflx, f_shifwflx, myThid )
cML      CALL GLOBAL_SUM_TILE_RL( num_shelfice, no_shelfice, myThid )
      CALL GLOBAL_SUM_TILE_RL( num_shifwflx, no_shifwflx, myThid )

# ifndef ALLOW_OPENAD
C--   Each process has calculated the global part for itself.
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

       ifc = 30
       WRITE(cfname,'(A,i4.4)') 'costfunction_shelfice',optimcycle
       OPEN(unit=ifc,file=cfname)

       WRITE(ifc,*) 'fc =', fc
cML       WRITE(ifc,*) 'f_shelfice =', f_shelfice, no_shelfice
       WRITE(ifc,*) 'f_shelfice =', f_shelfice
       WRITE(ifc,*) 'f_shifwflx =', f_shifwflx, no_shifwflx

       CLOSE(ifc)

      ENDIF
# endif /* ALLOW_OPENAD */

#endif /* ALLOW_COST */

      RETURN
      END
