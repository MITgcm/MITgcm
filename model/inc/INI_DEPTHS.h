C $Header: /u/gcmpack/MITgcm/model/inc/Attic/INI_DEPTHS.h,v 1.3 2001/09/21 15:13:31 cnh Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: INI_DEPTHS.h
C    !INTERFACE:
C    include INI_DEPTHS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | INI_DEPTHS.h                                              
C     | o Globals used by Fortran depth map initialization        
C     *==========================================================*
C     \ev
CEOP
      COMMON / INIDEP_COMMON_RS / H
      _RS H(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
