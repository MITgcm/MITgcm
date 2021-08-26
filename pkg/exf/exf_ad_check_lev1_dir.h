c
c     store directives for checkpoint level 1
c
c     created: heimbach@mit.edu 10-Jan-2002
c
#ifdef ALLOW_EXF
C     most of these fields are here for safety only, because it should
C     always be possible to save the field hflux instead of hflux0 and
C     hflux1 in the innermost checkpoint level

CADJ STORE hflux0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE hflux1    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE sflux0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE sflux1    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE ustress0  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE ustress1  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vstress0  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vstress1  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE wspeed0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE wspeed1   = comlev1, key=ikey_dynamics, kind=isbyte

# ifdef ALLOW_ATM_TEMP

CADJ STORE aqh0      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE aqh1      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE atemp0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE atemp1    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE precip0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE precip1   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE lwflux0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE lwflux1   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE swflux0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE swflux1   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE snowprecip0 = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE snowprecip1 = comlev1, key=ikey_dynamics, kind=isbyte
#  ifdef ALLOW_READ_TURBFLUXES
CADJ STORE hs0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE hs1   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE hl0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE hl1   = comlev1, key=ikey_dynamics, kind=isbyte
#  endif /* ALLOW_READ_TURBFLUXES */
#  ifdef EXF_READ_EVAP
CADJ STORE evap0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE evap1     = comlev1, key=ikey_dynamics, kind=isbyte
#  else
CADJ STORE evap      = comlev1, key=ikey_dynamics, kind=isbyte
#  endif /* EXF_READ_EVAP */
#  ifdef ALLOW_DOWNWARD_RADIATION
CADJ STORE swdown0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE swdown1   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE lwdown0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE lwdown1   = comlev1, key=ikey_dynamics, kind=isbyte
#  endif

# else /* ALLOW_ATM_TEMP undef */

#  ifdef SHORTWAVE_HEATING
CADJ STORE swflux0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE swflux1   = comlev1, key=ikey_dynamics, kind=isbyte
#  endif

# endif /* ALLOW_ATM_TEMP */

CADJ STORE uwind0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uwind1    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vwind0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vwind1    = comlev1, key=ikey_dynamics, kind=isbyte

# if (defined ALLOW_BULKFORMULAE) || (defined ALLOW_DOWNWARD_RADIATION)
CADJ STORE theta     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
CADJ STORE climsss0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE climsss1   = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
CADJ STORE climsst0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE climsst1   = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_CLIMSALT_RELAXATION
cph not used so far
cphCADJ STORE climsalt0  = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE climsalt1  = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_CLIMTEMP_RELAXATION
cph not used so far
cphCADJ STORE climtemp0  = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE climtemp1  = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE apressure0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE apressure1    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE siceload      = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_RUNOFF
CADJ STORE runoff0       = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE runoff1       = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SALTFLX
CADJ STORE saltflx0      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE saltflx1      = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef EXF_SEAICE_FRACTION
CADJ STORE areamask0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE areamask1     = comlev1, key=ikey_dynamics, kind=isbyte
# endif

# ifdef ECCO_CTRL_DEPRECATED
# ifdef ALLOW_HFLUX_CONTROL
CADJ STORE xx_hflux0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_hflux1     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SFLUX_CONTROL
CADJ STORE xx_sflux0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_sflux1     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_USTRESS_CONTROL
CADJ STORE xx_tauu0      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_tauu1      = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_VSTRESS_CONTROL
CADJ STORE xx_tauv0      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_tauv1      = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_UWIND_CONTROL
CADJ STORE xx_uwind0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_uwind1     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_VWIND_CONTROL
CADJ STORE xx_vwind0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_vwind1     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_ATEMP_CONTROL
CADJ STORE xx_atemp0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_atemp1     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_AQH_CONTROL
CADJ STORE xx_aqh0       = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_aqh1       = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_PRECIP_CONTROL
CADJ STORE xx_precip0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_precip1    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SWFLUX_CONTROL
CADJ STORE xx_swflux0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_swflux1    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SWDOWN_CONTROL
CADJ STORE xx_swdown0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_swdown1    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SNOWPRECIP_CONTROL
CADJ STORE xx_snowprecip0 = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_snowprecip1 = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_LWFLUX_CONTROL
CADJ STORE xx_lwflux0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_lwflux1    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_LWDOWN_CONTROL
CADJ STORE xx_lwdown0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_lwdown1    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_EVAP_CONTROL
CADJ STORE xx_evap0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_evap1    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_APRESSURE_CONTROL
CADJ STORE xx_apressure0 = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_apressure1 = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_RUNOFF_CONTROL
CADJ STORE xx_runoff0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_runoff1    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SST_CONTROL
CADJ STORE xx_sst0       = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_sst1       = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SSS_CONTROL
CADJ STORE xx_sss0       = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_sss1       = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# endif /* ECCO_CTRL_DEPRECATED */

#endif /* ALLOW_EXF */
