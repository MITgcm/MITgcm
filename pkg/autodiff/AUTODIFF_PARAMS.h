C $Header: /u/gcmpack/MITgcm/pkg/autodiff/AUTODIFF_PARAMS.h,v 1.9 2014/06/28 22:41:00 gforget Exp $
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
C     SEAICEuseFREEDRIFTswitchInAd :: switch on/off Free-Drift in adjoint mode (def=F)
C     SEAICEuseDYNAMICSswitchInAd  :: switch on/off seaice Dyn in adjoint mode (def=F)
      LOGICAL useKPPinAdMode,    useKPPinFwdMode
      LOGICAL useGMRediInAdMode, useGMRediInFwdMode
      LOGICAL useSEAICEinAdMode, useSEAICEinFwdMode
      LOGICAL useGGL90inAdMode,    useGGL90inFwdMode
      LOGICAL useSALT_PLUMEinAdMode, useSALT_PLUMEInFwdMode
      LOGICAL SEAICEuseFREEDRIFTswitchInAd, SEAICEuseFREEDRIFTinFwdMode
      LOGICAL SEAICEuseDYNAMICSswitchInAd, SEAICEuseDYNAMICSinFwdMode
      LOGICAL useSmoothCorrel2DinAdMode, useSmoothCorrel2DinFwdMode

C-    Logical for ad dump format (if true then write all records
C       to one file per variable; else write one file per record)
      LOGICAL dumpAdByRec

      COMMON /AUTODIFF_PARM_L/
     &       inAdMode, inAdTrue, inAdFalse, inAdExact,
     &       useKPPinAdMode,    useKPPinFwdMode,
     &       useGMRediInAdMode, useGMRediInFwdMode,
     &       useSEAICEinAdMode, useSEAICEinFwdMode,
     &       useGGL90inAdMode,    useGGL90inFwdMode,
     &       useSALT_PLUMEinAdMode, useSALT_PLUMEInFwdMode,
     &       SEAICEuseFREEDRIFTswitchInAd, SEAICEuseFREEDRIFTinFwdMode,
     &       SEAICEuseDYNAMICSswitchInAd, SEAICEuseDYNAMICSinFwdMode,
     &       useSmoothCorrel2DinAdMode, useSmoothCorrel2DinFwdMode,
     &       dumpAdByRec

C--   COMMON /AUTODIFF_PARM_I/ Integer valued parameters used by the pkg.
C     dumpAdVarExch :: control ad-variables exchange before dumping output
C     mon_AdVarExch :: control ad-variables exchange before monitor output
C      - for both   :: =0 : no exch ; =1 : apply adexch ;
C      *AdVarExch - :: =2 : do adexch on a local copy.
C     SEAICEapproxLevInAd :: level of approximation in seaice adjoint
C       -1 (and .NOT.useSEAICEinAdMode) : use seaice_fake adjoint
C       0 (and .NOT.useSEAICEinAdMode)  : omit all of seaice thermo adjoint 
C       0 (and useSEAICEinAdMode)       : use all of seaice thermo adjoint
C       >= 1 (and useSEAICEinAdMode)    : omit pieces of seaice thermo adjoint 
      INTEGER dumpAdVarExch
      INTEGER mon_AdVarExch
      INTEGER SEAICEapproxLevInAd
      COMMON /AUTODIFF_PARM_I/
     &       dumpAdVarExch, mon_AdVarExch, SEAICEapproxLevInAd

C--   COMMON /AUTODIFF_PARM_R/ "Real" valued parameters used by the pkg.
C     viscFacInAd :: viscosity factor for adjoint
      _RL viscFacInAd
      COMMON /AUTODIFF_PARM_R/ viscFacInAd

C--   COMMON /AUTODIFF_PARM_C/ Character valued parameters used by the pkg.

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
