C $Header:

#ifdef ALLOW_SBO

C     /==========================================================\
C     | SBO.h                                                    |
C     | o Basic header for SBO                                   |
C     \==========================================================/

C     xoamc        - x-comp oam due to currents        (kg-m**2/s)
C     yoamc        - y-comp oam due to currents        (kg-m**2/s)
C     zoamc        - z-comp oam due to currents        (kg-m**2/s)
C     xoamp        - x-comp oam due to pressure        (kg-m**2/s)
C     yoamp        - y-comp oam due to pressure        (kg-m**2/s)
C     zoamp        - z-comp oam due to pressure        (kg-m**2/s)
C     mass         - mass of oceans                           (kg)
C     xcom         - x-comp of center-of-mass of oceans        (m)
C     ycom         - y-comp of center-of-mass of oceans        (m)
C     zcom         - z-comp of center-of-mass of oceans        (m)
C     obp          - ocean-bottom pressure               (Pascals)
C
      _RL xoamc, yoamc, zoamc, xoamp, yoamp, zoamp          
      _RL mass, xcom, ycom, zcom
      _RL obp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      common /sbo/ xoamc, yoamc, zoamc, xoamp, yoamp, zoamp,
     &               mass, xcom, ycom, zcom, obp

C     sbo_taveFreq - SBO time-averaging frequency              (s)
C
      _RL sbo_taveFreq
      COMMON /sbo_r/ sbo_taveFreq

#ifdef ALLOW_TIMEAVE

C----------------------------------------------------------------
C     sbo_drctrecTave - next record to dump for SBO averaging files
C----------------------------------------------------------------

      INTEGER sbo_drctrecTave
      COMMON /SBO_RECORDNUM/ sbo_drctrecTave

C----------------------------------------------------------------
C     sbo_TimeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL sbo_TimeAve(Nr,nSx,nSy)
      COMMON /SBO_TAVE/ sbo_TimeAve

C----------------------------------------------------------------
C     OBPtave      - time-averaged ocean-bottom pressure (Pascals)
C----------------------------------------------------------------

      _RL OBPtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /SBO_TAVE_DIAGS/ OBPtave

#endif ALLOW_TIMEAVE

#endif ALLOW_SBO
