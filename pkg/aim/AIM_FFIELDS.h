C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/AIM_FFIELDS.h,v 1.1 2001/05/29 19:28:53 cnh Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     /==========================================================\
C     | AIM_FFIELDS.h                                            |
C     | o AIM forcing fields.                                    |
C     \==========================================================/

C     aim_NxIO - Extent of input dataset in X
C     aim_NyIO - Extent of input dataset in Y
C                AIM climatologies need converting to MDSIO 
C                compatible datasets.
      INTEGER aim_NxIO
      INTEGER aim_NyIO
      PARAMETER ( aim_NxIO = 128,
     &            aim_NyIO =  64 )

C     aim_albedo       - Holds surface albedo ( 0-1 )
C     aim_surfTemp     - Holds surface temperature ( K )
C     aim_soilMoisture - Holds soil moisture ( 0-20 )
C
      COMMON /AIM_FFIELDS_R/
     &                   aim_albedo,
     &                   aim_surfTemp,
     &                   aim_soilMoisture
      REAL*4  aim_albedo      (aim_NxIO, aim_NyIO)
      REAL*4  aim_surfTemp    (aim_NxIO, aim_NyIO)
      REAL*4  aim_soilMoisture(aim_NxIO, aim_NyIO)

#endif /* ALLOW_AIM */
