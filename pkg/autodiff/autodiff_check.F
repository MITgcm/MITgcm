C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_check.F,v 1.1 2004/10/08 17:25:37 heimbach Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: AUTODIFF_CHECK
C     !INTERFACE:
      SUBROUTINE AUTODIFF_CHECK( myThid )

C     !DESCRIPTION: \bv
C     \ev

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF
#include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

#ifdef ALLOW_AUTODIFF_TAMC

#ifdef AUTODIFF_2_LEVEL_CHECKPOINT
      if (nchklev_1*nchklev_2 .lt. nTimeSteps) then
        print*, ' the_main_loop: TAMC checkpointing parameters'
        print*, '            nchklev_1*nchklev_2 = ',
     &       nchklev_1*nchklev_2
        print*, '            are not consistent with nTimeSteps = ',
     &       nTimeSteps
        stop    ' ... stopped in autodiff_check'
      endif
#else
c--   Check the choice of the checkpointing parameters in relation
c--   to nTimeSteps: (nchklev_1*nchklev_2*nchklev_3 .ge. nTimeSteps)
      if (nchklev_1*nchklev_2*nchklev_3 .lt. nTimeSteps) then
        print*, ' the_main_loop: TAMC checkpointing parameters'
        print*, '            nchklev_1*nchklev_2*nchklev_3 = ',
     &       nchklev_1*nchklev_2*nchklev_3
        print*, '            are not consistent with nTimeSteps = ',
     &       nTimeSteps
        stop    ' ... stopped in autodiff_check'
      endif
#endif

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
