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
      SUBROUTINE SEAICE_COST_FINAL( ifc, optimcycle, myThid )

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
# include "CTRL_SIZE.h"
# include "CTRL.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     ifc        :: file unit for costfunction_seaiceXXXX
C     optimcycle :: cycle number of the off-line optimization.
C     myThid     :: my Thread Id number
      INTEGER ifc
      INTEGER optimcycle
      INTEGER myThid

#if (defined ALLOW_COST && defined ALLOW_COST_ICE)
C     !LOCAL VARIABLES:
      INTEGER bi, bj
      _RL f_ice
      _RL no_ice
      CHARACTER*23 cfname
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

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

      WRITE(standardMessageUnit,'(A,1PE22.14,1PE22.14,1X,1PE9.2)')
     &     ' --> f_ice     =',f_ice, no_ice, mult_ice

C--   Each process has calculated the global part for itself.
      IF ( ifc .NE. -1 ) THEN

        WRITE(cfname,'(A,i4.4)') 'costfunction_seaice',optimcycle
        OPEN(unit=ifc,file=cfname)

C     makes little sense to print this now, because it is zero at this point
c       WRITE(ifc,*) 'fc =', fc
        WRITE(ifc,'(A,1PE22.14,1PE22.14,1X,1PE9.2)')
     &       'f_ice   =', f_ice, no_ice, mult_ice

        CLOSE(ifc)

      ENDIF

#endif /* ALLOW_COST and ALLOW_COST_ICE */

      RETURN
      END
