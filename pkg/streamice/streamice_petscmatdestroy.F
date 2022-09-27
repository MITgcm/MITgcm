#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_PETSCMATDESTROY(
     I                               myThid )

C     *============================================================*
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     *============================================================*

#ifdef ALLOW_PETSC
#ifdef STREAMICE_PETSC_3_8
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
      IMPLICIT NONE
# else
      IMPLICIT NONE
#include "finclude/petsc.h"
#include "STREAMICE_PETSC.h"
#endif
#endif

C     === Global variables ===

C     !INPUT/OUTPUT ARGUMENTS
C     cg_Uin, cg_Vin - input and output velocities
C     cg_Bu, cg_Bv - driving stress
      INTEGER myThid
#ifdef ALLOW_PETSC
      PetscErrorCode ierr
      call KSPDestroy (ksp, ierr)
      call MatDestroy (matrix, ierr)
#endif

      RETURN
      END
