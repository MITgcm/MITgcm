#include "STIC_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: STIC_COST_FINAL
C     !INTERFACE:
      SUBROUTINE STIC_COST_FINAL( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | subroutine stic_cost_final                           |
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
#include "STIC.h"
#include "STIC_COST.h"

#ifdef ALLOW_COST
# include "cost.h"
# include "optim.h"
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

      _RL f_stic

      INTEGER i, j
CEOP

      f_stic = 0. _d 0

      IF ( .NOT. useECCO ) THEN
C--   Calculate cost function on tile of this instance
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
ccc        do klev = 1, Nr
          DO j=1,sNy
           DO i=1,sNx

             objf_stic(bi,bj) = objf_stic(bi,bj)
     &            + cMeanSHIforS(i,j,bi,bj)/lastinterval/rhoConstFresh
     &            *_rA(i,j,bi,bj)

           ENDDO
          ENDDO
ccc        end do
        ENDDO
       ENDDO
      ENDIF

C--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        write(standardmessageunit,'(A,D22.15)')
     &       ' --> objf_stic(bi,bj) = ', objf_stic(bi,bj)
        tile_fc(bi,bj) = tile_fc(bi,bj)
     &       + mult_stic * objf_stic(bi,bj)

       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( objf_stic, f_stic, myThid )

# ifndef ALLOW_OPENAD
C--   Each process has calculated the global part for itself.
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

       ifc = 30
       WRITE(cfname,'(A,i4.4)') 'costfunction_stic',optimcycle
       OPEN(unit=ifc,file=cfname)

       WRITE(ifc,*) 'fc =', fc
       WRITE(ifc,*) 'f_stic =', f_stic

       CLOSE(ifc)

      ENDIF
# endif /* ALLOW_OPENAD */

#endif /* ALLOW_COST */

      RETURN
      END
