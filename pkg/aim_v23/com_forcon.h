C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_forcon.h,v 1.1 2002/11/22 17:16:06 jmc Exp $
C $Name:  $
C $Namer: $

#ifdef ALLOW_AIM

C--   COMMON /FORCON/: Constants for forcing fields (initial. in INPHYS)
C      SOLC   = Solar constant (area averaged) in W/m^2
C      ALBSEA = Albedo over sea
C      ALBICE = Albedo over sea ice (for ice fraction = 1)
C      ALBSN  = Albedo over snow (for snow depth > SDALB)
C      SDALB  = Snow depth (mm water) corresponding to maximum albedo
C      SWCAP  = Soil wetness at field capacity (volume fraction)
C      SWWIL  = Soil wetness at wilting point  (volume fraction)

      COMMON /FORCON/ SOLC, ALBSEA, ALBICE, ALBSN, SDALB,
     &                SWCAP, SWWIL
      _RL SOLC, ALBSEA, ALBICE, ALBSN, SDALB, SWCAP, SWWIL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
