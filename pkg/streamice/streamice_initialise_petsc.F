#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_INITIALIZE_PETSC 
C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/

C     === Global variables ===
#ifdef ALLOW_PETSC
#ifdef STREAMICE_PETSC_ARCHER2
#include "petsc/finclude/petsc.h"
#include "petsc/finclude/petscvec.h"
      use petscvec
#else
#include "finclude/petsc.h"
#endif
#endif


C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid

#ifdef ALLOW_STREAMICE

#ifdef ALLOW_PETSC
      PetscErrorCode ierr
      call petscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

#endif
      RETURN
      END

