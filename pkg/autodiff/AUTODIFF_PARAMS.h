C $Header: /u/gcmpack/MITgcm/pkg/autodiff/AUTODIFF_PARAMS.h,v 1.1 2012/07/04 20:16:43 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: AUTODIFF_PARAMS.h
C     !INTERFACE:
C     #include AUTODIFF_PARAMS.h

C     !DESCRIPTION:
C     Header file defining pkg/autodiff "parameters".  The values
C     from the pkg input file are stored into the variables held
C     here. Notes describing the parameters can also be found here.

CEOP

C--   COMMON /AUTODIFF_PARM_L/ Logical valued parameters used by the pkg.
C     inAdMode  :: enable approximate computation in backward sweep
C     inAdTrue  :: value of inAdMode flag during backward sweep
C     inAdFalse :: value of inAdMode flag during forward  sweep
C     inAdExact :: get an exact adjoint (no approximation)
      LOGICAL inAdMode, inAdTrue, inAdFalse, inAdExact

C-    Logical flags for turning off parts of the code in adjoint mode
      LOGICAL useKPPinAdMode,    useKPPinFwdMode
      LOGICAL useGMRediInAdMode, useGMRediInFwdMode
      LOGICAL useSEAICEinAdMode, useSEAICEinFwdMode

      COMMON /AUTODIFF_PARM_L/
     &       inAdMode, inAdTrue, inAdFalse, inAdExact,
     &       useKPPinAdMode,    useKPPinFwdMode,
     &       useGMRediInAdMode, useGMRediInFwdMode,
     &       useSEAICEinAdMode, useSEAICEinFwdMode

C--   COMMON /AUTODIFF_PARM_I/ Integer valued parameters used by the pkg.

C--   COMMON /AUTODIFF_PARM_R/ "Real" valued parameters used by the pkg.

C--   COMMON /AUTODIFF_PARM_C/ Character valued parameters used by the pkg.

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
