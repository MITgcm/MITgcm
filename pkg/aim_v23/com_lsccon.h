#ifdef ALLOW_AIM

C--   COMMON /LSCCON/: Constants for large-scale condendation 
C                       (initial. in INPHYS)
C      TRLSC  = Relaxation time (in hours) for specific humidity
C      RHLSC  = Maximum relative humidity threshold (at sigma=1)
C      DRHLSC = Vertical range of relative humidity threshold
C      QSMAX  = used to define the maximum latent heat release 

      COMMON /LSCCON/ TRLSC, RHLSC, DRHLSC, QSMAX 
      _RL             TRLSC, RHLSC, DRHLSC, QSMAX 

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
