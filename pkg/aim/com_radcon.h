C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_radcon.h,v 1.3 2001/05/29 19:28:53 cnh Exp $
C $Name:  $

C--
C--   /RADCON/: Radiation constants (initial. in INPHYS)
C--    ABSSW  = shortwave absorptivity for dry air (per dp = 10^5 Pa)
C--    ABSLW  =  longwave absorptivity for dry air (per dp = 10^5 Pa)
C--    ABWSW  = shortwave absorptivity for water vapour (per dq = 1 g/kg)
C--    ABWLW  =  longwave absorptivity for water vapour (per dq = 1 g/kg)
C--    ABCSW  = shortwave absorptivity for cloud fraction
C--    ABCLW  =  longwave absorptivity for cloud fraction
C--    EPSSW  = fraction of incoming solar radiation absorbed by ozone
C--    EPSLW  = fraction of surface LW radiation emitted directly to space
C--    ALBCL  = cloud albedo (for cloud cover = 1)
C--    RHCL1  = relative hum. corresponding to cloud cover = 0
C--    RHCL2  = relative hum. corresponding to cloud cover = 1
C--    QACL   = specific hum. threshold for cloud cover

      COMMON /RADCON/ ABSSW, ABSLW, ABWSW, ABWLW,
     *                ABCSW, ABCLW, EPSSW, EPSLW, 
     *                ALBCL, RHCL1, RHCL2, QACL

C--
C--   /FLDRAD/: Transmissivity and blackbody radiation (workspace)
C--    TAU    = transmissivity of atmospheric layers
C--    ST4A   = blackbody emission from full and half atmospheric levels 

      COMMON /RADFLD/ TAUORIG(NGP,NLEV),ST4AORIG(NGP,NLEV,2)
