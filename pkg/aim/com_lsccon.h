C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_lsccon.h,v 1.3 2002/09/27 20:01:57 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C--   COMMON /LSCCON/: Constants for large-scale condendation 
C                       (initial. in INPHYS)
C      RHLSC  = Relative humidity threshold
C      TRLSC  = Relaxation time (in hours) for supersat. specific humidity 

      COMMON /LSCCON/ RHLSC, TRLSC
      _RL RHLSC, TRLSC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
