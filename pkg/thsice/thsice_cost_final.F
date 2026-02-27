#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_COST_FINAL
C     !INTERFACE:
      SUBROUTINE THSICE_COST_FINAL( ifc, optimcycle, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE THSICE_COST_FINAL
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "THSICE_PARAMS.h"
#ifdef ALLOW_COST
# include "THSICE_COST.h"
# include "cost.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     ifc        :: file unit for costfunction_thsiceXXXX
C     optimcycle :: cycle number of the off-line optimization.
C     myThid     :: my Thread Id number
      INTEGER ifc
      INTEGER optimcycle
      INTEGER myThid

#ifdef ALLOW_COST
C     !LOCAL VARIABLES:
      INTEGER bi,bj
      _RL f_thsice
      _RL no_thsice
c     CHARACTER*23 cfname
CEOP

c     print *, 'ph-2 in thsice_cost_final'

      f_thsice      = 0. _d 0
      no_thsice     = 0. _d 0

C--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

          tile_fc(bi,bj) = tile_fc(bi,bj)
     &          + mult_thsice        * objf_thsice(bi,bj)

          f_thsice = f_thsice + objf_thsice(bi,bj)

       ENDDO
      ENDDO

C--   Do global summation for each part of the cost function
c     CALL GLOBAL_SUM_TILE_RL( objf_thsice, f_thsice, myThid )
c     CALL GLOBAL_SUM_TILE_RL( num_thsice, no_thsice, myThid )

      WRITE(standardMessageUnit,'(A,1PE22.14)')
     &     ' --> f_thsice     = ', f_thsice

c     IF ( ifc .NE. -1 ) THEN
c       WRITE(cfname,'(A,i4.4)') 'costfunction_thsice',optimcycle
c       OPEN(unit=ifc,file=cfname)
c       WRITE(ifc,'(A,1PE22.14,1PE22.14,1X,1PE9.2)')
c    &       'f_thsice =', f_thsice, no_thsice, mult_thsice
c       CLOSE(ifc)
c     ENDIF

#endif /* ALLOW_COST */

      RETURN
      END
