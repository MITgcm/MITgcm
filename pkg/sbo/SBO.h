C $Header: /u/gcmpack/MITgcm/pkg/sbo/SBO.h,v 1.9 2014/05/18 23:55:51 jmc Exp $
C $Name:  $

#ifdef ALLOW_SBO

C     *==========================================================*
C     | SBO.h
C     | o Basic header for SBO
C     *==========================================================*

C     xoamc       :: x-comp oam due to currents        (kg-m**2/s)
C     yoamc       :: y-comp oam due to currents        (kg-m**2/s)
C     zoamc       :: z-comp oam due to currents        (kg-m**2/s)
C     xoamp       :: x-comp oam due to pressure        (kg-m**2/s)
C     yoamp       :: y-comp oam due to pressure        (kg-m**2/s)
C     zoamp       :: z-comp oam due to pressure        (kg-m**2/s)
C     mass        :: mass of oceans                           (kg)
C     xcom        :: x-comp of center-of-mass of oceans        (m)
C     ycom        :: y-comp of center-of-mass of oceans        (m)
C     zcom        :: z-comp of center-of-mass of oceans        (m)
C     sboarea     :: surface ocean area                     (m**2)
C
      _RL xoamc, yoamc, zoamc, xoamp, yoamp, zoamp
      _RL mass, xcom, ycom, zcom
      _RL sboarea
      common /sbo/ xoamc, yoamc, zoamc, xoamp, yoamp, zoamp,
     &             mass, xcom, ycom, zcom,
     &             sboarea

C components due to real freshwater flux
      _RL xoamp_fw, yoamp_fw, zoamp_fw
      _RL mass_fw, xcom_fw, ycom_fw, zcom_fw
      common /sbo_fw/ xoamp_fw, yoamp_fw, zoamp_fw,
     &             mass_fw, xcom_fw, ycom_fw, zcom_fw

C components due to seaice motion
      _RL xoamc_si, yoamc_si, zoamc_si
      _RL mass_si
      common /sbo_si/ xoamc_si, yoamc_si, zoamc_si,
     &             mass_si

C components due to Greatbatch correction
      _RL mass_gc
      common /sbo_gc/ mass_gc

C sbo_monFreq :: SBO monitor frequency           (s)
      _RL sbo_monFreq
      COMMON /sbo_params_r/ sbo_monFreq

#endif /* ALLOW_SBO */
