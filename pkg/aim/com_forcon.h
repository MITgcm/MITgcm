C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_forcon.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Namer: $

C--
C--   /FORCON/: Constants for forcing fields (initial. in INPHYS)
C--    SOLC   = Solar constant (area averaged) in W/m^2
C--    ALBSEA = Albedo over sea 
C--    ALBICE = Albedo over sea ice (for ice fraction = 1)
C--    ALBSN  = Albedo over snow (for snow depth > SDALB)
C--    SDALB  = Snow depth corresponding to maximum albedo
C--    SWCAP  = Soil wetness capacity
C--    SWWIL  = Soil wetness at wilting point

      COMMON /FORCON/ SOLC, ALBSEA, ALBICE, ALBSN, SDALB,
     &                SWCAP, SWWIl
