C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_finalize_petsc.F,v 1.2 2015/02/16 16:46:45 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_FINALIZE_PETSC
C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#ifdef ALLOW_PETSC
#include "finclude/petsc.h"
#endif


C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid

#ifdef ALLOW_STREAMICE

#ifdef ALLOW_PETSC

      PetscErrorCode ierr

      if (streamice_use_petsc) then
      call PetscFinalize(ierr)
      endif

#endif

#endif
      RETURN
      END

