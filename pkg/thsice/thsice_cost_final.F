C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_cost_final.F,v 1.2 2013/11/06 19:04:36 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_COST_FINAL
C     !INTERFACE:
      SUBROUTINE THSICE_COST_FINAL( myThid )

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
#include "THSICE_COST.h"
#include "cost.h"
#include "ctrl.h"
#include "optim.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

#ifdef ALLOW_COST
C     !LOCAL VARIABLES:
      INTEGER bi,bj
      _RL f_thsice
      _RL no_thsice
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

      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_thsice     = ', f_thsice

#endif /* ALLOW_COST */

      RETURN
      END
