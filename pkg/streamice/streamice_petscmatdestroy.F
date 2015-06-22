C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_petscmatdestroy.F,v 1.1 2015/06/22 16:23:32 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_PETSCMATDESTROY( 
     I                               myThid )

C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE




#ifdef ALLOW_PETSC
#include "finclude/petsc.h"
#include "STREAMICE_PETSC.h"
! UNCOMMENT IF V3.0
!#include "finclude/petscvec.h"
!#include "finclude/petscmat.h"
!#include "finclude/petscksp.h"
!#include "finclude/petscpc.h"
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
