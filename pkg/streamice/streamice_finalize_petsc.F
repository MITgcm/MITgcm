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
#ifdef ALLOW_PETSC
#ifdef STREAMICE_PETSC_ARCHER2
C assumes paths 
C {petsc_dir}/include/petsc/finclude/ 
C {petsc_dir}/include
C in the include statement
#include "petsc/finclude/petsc.h"
#include "petsc/finclude/petscvec.h"
      use petscvec
#include "petsc/finclude/petscmat.h"
      use petscmat
#include "petsc/finclude/petscksp.h"
      use petscksp
#include "petsc/finclude/petscpc.h"
      use petscpc
      use STREAMICE_PETSC_mod
# else
#include "finclude/petsc.h"
#include "STREAMICE_PETSC.h"
#endif
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

