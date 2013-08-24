C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_initialise_petsc.F,v 1.1 2013/08/24 20:32:03 dgoldberg Exp $
C $Name:  $

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
      IMPLICIT NONE

C     === Global variables ===
#ifdef ALLOW_PETSC
#include "finclude/petsc.h"
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

