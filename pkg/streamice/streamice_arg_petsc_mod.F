#include "STREAMICE_OPTIONS.h"

      MODULE STREAMICE_ARG_PETSC_mod
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE
#ifdef ALLOW_PETSC
#ifdef STREAMICE_PETSC_3_8

C     THE FOLLOWING VARIABLES ARE OF DEFINED TYPES IN THE PETSC 
C     LIBRARY, USED BY STREAMICE WHEN AVAILABLE TO ACCELERATE 
C     SOLVES OF LINEAR SYSTEMS

C     THE STORING OF PETSC OBJECTS IN COMMON BLOCKS IS 
C     FORBIDDEN BY PETSC 3.8.x AND ABOVE, SO A MODULE IS USED

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
      KSP ksp
      PC  pc

#endif /* ALLOW_STREAMICE */
#endif
#endif
       END MODULE

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
