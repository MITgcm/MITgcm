C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_cnvcon.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Namer: $

C--
C--   /CNVCON/: Convection constants (init. in INPHYS)
C--    RHBL   = relative hum. threshold in the boundary (lowest) layer
C--    TRCNV  = time of relaxation (in hours) towards neutral equilibrium
C--    ENTMAX = max. entrainment as a fraction of cloud-base mass flux
 
      COMMON /CNVCON/ RHBL, TRCNV, ENTMAX
C      $Id: com_cnvcon.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
