C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_const.h,v 1.2 2004/05/14 16:08:38 jmc Exp $
C $Name:  $

C--   Constants for physical parametrization routines:

C--   Constants for boundary forcing (common FORCON):

      SOLC   = 342. _d 0

      ALBSEA = 0.07 _d 0
      ALBICE = 0.60 _d 0
      ALBSN  = 0.60 _d 0
      SDALB  = 60.  _d 0

      SWCAP = 0.30 _d 0
      SWWIL = 0.17 _d 0
      hSnowWetness = 1. _d -2

C--   Constants for convection (common CNVCON):

      PSMIN  =  0.7 _d 0
      TRCNV  =  6.  _d 0
      QBL    = 15.  _d 0
      RHBL   =  0.8 _d 0
      RHIL   =  0.7 _d 0
      ENTMAX =  0.5 _d 0
      SMF    =  0.5 _d 0

C--   Constants for large-scale condensation (common LSCCON):

      TRLSC  = 4.  _d 0
      RHLSC  = 0.9 _d 0
      DRHLSC = 0.1 _d 0
      QSMAX  = 50. _d 0

C--   Constants for radiation (common RADCON):

      RHCL1 = 0.45 _d 0
      RHCL2 = 0.85 _d 0
      QACL1 = 0.10 _d 0
      QACL2 = 1.00 _d 0
      ALBCL = 0.40 _d 0

      EPSSW  =  0.015 _d 0
      EPSLW  =  0.00  _d 0
      EMISFC =  0.98  _d 0

      ABSDRY =  0.033 _d 0
      ABSAER =  0.033 _d 0
      ABSWV1 =  0.022 _d 0
      ABSWV2 = 15.000 _d 0
      ABSCL1 =  0.0   _d 0
      ABSCL2 =  0.010 _d 0

      ABLWIN =  0.7 _d 0
      ABLCO2 =  4.0 _d 0
      ABLWV1 =  0.7 _d 0
      ABLWV2 = 50.0 _d 0
      ABLCL1 = 12.0 _d 0
      ABLCL2 =  0.0 _d 0


C--   Constants for surface fluxes (common SFLCON):

      FWIND0 = 0.6 _d 0
      FTEMP0 = 1.  _d 0
      FHUM0  = 1.  _d 0

      CDL = 1.8 _d -3
      CDS = 0.8 _d -3
      CHL = 1.2 _d -3
      CHS = 0.8 _d -3

      VGUST  = 5. _d 0
      CTDAY  = 1. _d -2
      DTHETA = 3. _d 0
      FSTAB  = 0.67 _d 0
      HDRAG  = 2000. _d 0
      FHDRAG = 0.8 _d 0

C--   Constants for vertical diffusion and sh. conv. (common VDICON):

      TRSHC  = 24. _d 0
      TRVDI  = 40. _d 0
      TRVDS  =  5. _d 0
      RHGRAD = 0.5 _d 0
      SEGRAD = 0.1 _d 0
