C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_inadmode_unset_g.F,v 1.3 2012/07/04 20:21:10 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      SUBROUTINE G_AUTODIFF_INADMODE_UNSET( myThid )
C     *==========================================================*
C     | SUBROUTINE G_AUTODIFF_INADMODE_UNSET
C     *==========================================================*

      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"

C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid

cph   inAdMode = inAdFalse
      inAdMode = .FALSE.

      RETURN
      END
