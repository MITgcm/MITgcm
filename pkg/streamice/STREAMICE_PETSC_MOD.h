C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE
#ifdef ALLOW_PETSC
#ifdef STREAMICE_PETSC_3_8
       use STREAMICE_ARG_PETSC_MOD
#else

C     THE FOLLOWING VARIABLES ARE OF DEFINED TYPES IN THE PETSC
C     LIBRARY, USED BY STREAMICE WHEN AVAILABLE TO ACCELERATE
C     SOLVES OF LINEAR SYSTEMS

C     THE STORING OF PETSC OBJECTS IN COMMON BLOCKS IS
C     FORBIDDEN BY PETSC 3.8.x AND ABOVE

      COMMON /STREAMICE_PETSC_MATRIX/
     & matrix, mumpsFac
      Mat matrix
      Mat mumpsFac

      COMMON /STREAMICE_PETSC_KSP/
     & ksp
      KSP ksp

      COMMON /STREAMICE_PETSC_PC/
     & pc
      PC  pc
#endif
#endif
#endif /* ALLOW_STREAMICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
