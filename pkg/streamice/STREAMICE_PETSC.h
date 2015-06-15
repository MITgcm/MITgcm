C $Header: /u/gcmpack/MITgcm/pkg/streamice/STREAMICE_PETSC.h,v 1.1 2015/06/15 14:32:44 dgoldberg Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE

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

#endif /* ALLOW_STREAMICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
