C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM_FFIELDS.h,v 1.1 2002/11/22 17:16:06 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     /==========================================================\
C     | AIM_FFIELDS.h                                            |
C     | o AIM forcing fields.                                    |
C     \==========================================================/


C     aim_albedo    - Holds surface albedo ( 0-1 )
C     aim_surfTemp  - Holds surface temperature ( K )
C     aim_soilWater - Holds soil water availability ( 0-1 )
C
      COMMON /AIM_FFIELDS_R/
     &                   aim_albedo,
     &                   aim_surfTemp,
     &                   aim_soilWater
      _RS  aim_albedo   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_surfTemp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_soilWater(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_AIM */
