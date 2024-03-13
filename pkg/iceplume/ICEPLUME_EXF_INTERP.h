#ifdef ALLOW_ICEPLUME
#ifdef ALLOW_EXF
C     the following variables are used in conjunction
C     with pkg/exf to specify sub-glacial runoff

      COMMON /ICEPLUME_EXF_PARM03_I/
     &       runoffQsgstartdate1, runoffQsgstartdate2
#ifdef USE_EXF_INTERPOLATION
     &      ,runoffQsg_nlon,runoffQsg_nlat
     &      ,runoffQsg_interpMethod
#endif 

      INTEGER runoffQsgstartdate1
      INTEGER runoffQsgstartdate2
#ifdef USE_EXF_INTERPOLATION
      INTEGER runoffQsg_nlon
      INTEGER runoffQsg_nlat
      INTEGER runoffQsg_interpMethod
#endif

      COMMON /ICEPLUME_EXF_PARM03_RL/
     &       runoffQsgperiod, runoffQsgStartTime,
     &       runoffQsgconst, runoffQsg_inscal, 
     &       runoffQsg_remov_intercept, runoffQsg_remov_slope,
     &       runoffQsgRepCycle
#ifdef USE_EXF_INTERPOLATION
     &      ,runoffQsg_lon0,runoffQsg_lon_inc
     &      ,runoffQsg_lat0,runoffQsg_lon_inc
#endif 

      _RL    runoffQsgperiod, runoffQsgStartTime,
     &       runoffQsgconst, runoffQsg_inscal,
     &       runoffQsg_remov_intercept, runoffQsg_remov_slope,
     &       runoffQsgRepCycle
#ifdef USE_EXF_INTERPOLATION
     &      ,runoffQsg_lon0, runoffQsg_lon_inc,
     &       runoffQsg_lat0
c MAX_LAT_INC=1279 in exf_interp_size
      _RL     runoffQsg_lat_inc(MAX_LAT_INC)
#endif /* USE_EXF_INTERPOLATION */

c runoffQsgmask will be 'c'
      COMMON /ICEPLUME_EXF_PAR_C/ runoffQsgmask
      CHARACTER*1 runoffQsgmask

#endif /* ALLOW_EXF */
#endif /* ALLOW_ICEPLUME */

