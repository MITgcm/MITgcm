C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_radcon.h,v 1.4 2002/09/27 20:01:57 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C--   COMMON /RADCON/: Radiation constants (initial. in INPHYS)
C      ABSSW  = shortwave absorptivity for dry air (per dp = 10^5 Pa)
C      ABSLW  =  longwave absorptivity for dry air (per dp = 10^5 Pa)
C      ABWSW  = shortwave absorptivity for water vapour (per dq = 1 g/kg)
C      ABWLW  =  longwave absorptivity for water vapour (per dq = 1 g/kg)
C      ABCSW  = shortwave absorptivity for cloud fraction
C      ABCLW  =  longwave absorptivity for cloud fraction
C      EPSSW  = fraction of incoming solar radiation absorbed by ozone
C      EPSLW  = fraction of surface LW radiation emitted directly to space
C      ALBCL  = cloud albedo (for cloud cover = 1)
C      RHCL1  = relative hum. corresponding to cloud cover = 0
C      RHCL2  = relative hum. corresponding to cloud cover = 1
C      QACL   = specific hum. threshold for cloud cover
      COMMON /RADCON/ ABSSW, ABSLW, ABWSW, ABWLW,
     &                ABCSW, ABCLW, EPSSW, EPSLW, 
     &                ALBCL, RHCL1, RHCL2, QACL
      _RL ABSSW, ABSLW, ABWSW, ABWLW
      _RL ABCSW, ABCLW, EPSSW, EPSLW 
      _RL ALBCL, RHCL1, RHCL2, QACL

C--   COMMON /FLDRAD/: Transmissivity and blackbody radiation (workspace)
C      TAU    = transmissivity of atmospheric layers
C      ST4A   = blackbody emission from full and half atmospheric levels 
      COMMON /RADFLD/ TAUORIG, ST4AORIG
      _RL TAUORIG (NGP,NLEV)
      _RL ST4AORIG(NGP,NLEV,2)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
