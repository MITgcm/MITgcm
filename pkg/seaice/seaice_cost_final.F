#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_COST
#include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
#include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_COST_FINAL
C     !INTERFACE:
      SUBROUTINE SEAICE_COST_FINAL( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE SEAICE_COST_FINAL
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_COST.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL.h"
# include "OPTIMCYCLE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

#if (defined ALLOW_COST && defined ALLOW_COST_ICE)
C     ! FUNCTIONS:
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER ifc
      _RL f_ice
      _RL no_ice
      CHARACTER*23 cfname
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      ifc = 30

      f_ice      = 0. _d 0
      no_ice     = 0. _d 0

#ifdef ALLOW_SEAICE_COST_EXPORT
      CALL SEAICE_COST_EXPORT( myThid )
#endif

C--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

          tile_fc(bi,bj) = tile_fc(bi,bj)
     &          + mult_ice_export * objf_ice_export(bi,bj)
     &          + mult_ice        * objf_ice(bi,bj)

       ENDDO
      ENDDO

C--   Note: global summation (tile_fc --> fc) is done only in cost_final

C--   Do global summation for each part of the cost function

      CALL GLOBAL_SUM_TILE_RL( objf_ice,     f_ice,     myThid )
      CALL GLOBAL_SUM_TILE_RL( num_ice,     no_ice,     myThid )

      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_ice     =',f_ice

C--   Each process has calculated the global part for itself.
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

        WRITE(cfname,'(A,i4.4)') 'costfunction_seaice',optimcycle
        OPEN(unit=ifc,file=cfname)

        WRITE(ifc,*) 'fc =', fc
        WRITE(ifc,*) 'f_ice   =', f_ice, no_ice

        CLOSE(ifc)

      ENDIF

#endif /* ALLOW_COST and ALLOW_COST_ICE */

      RETURN
      END
