C $Header: /u/gcmpack/MITgcm/model/inc/Attic/INI_DEPTHS.h,v 1.2 2001/05/29 14:01:36 adcroft Exp $
C $Name:  $
C
C     /==========================================================\
C     | INI_DEPTHS.h                                             |
C     | o Globals used by Fortran depth map initialization       |
C     \==========================================================/
      COMMON / INIDEP_COMMON_RS / H
      _RS H(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
