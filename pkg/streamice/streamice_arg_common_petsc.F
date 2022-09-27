#include "STREAMICE_OPTIONS.h"

      MODULE STREAMICE_PETSC_mod
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE
#ifdef ALLOW_PETSC
#ifdef STREAMICE_PETSC_3_8

#include "petsc/finclude/petsc.h"
#include "petsc/finclude/petscvec.h"
      use petscvec
#include "petsc/finclude/petscmat.h"
      use petscmat
#include "petsc/finclude/petscksp.h"
      use petscksp
#include "petsc/finclude/petscpc.h"
      use petscpc

      Mat matrix
      Mat mumpsFac
C      COMMON /STREAMICE_PETSC_MATRIX/
C     & matrix, mumpsFac

      KSP ksp
C      COMMON /STREAMICE_PETSC_KSP/
C     & ksp

      PC  pc
C      COMMON /STREAMICE_PETSC_PC/
C     & pc

#endif /* ALLOW_STREAMICE */
#endif
#endif
       END MODULE

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
