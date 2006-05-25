#ifdef ALLOW_EXF

CADJ STORE hflux0    = tapelev3, key = ilev_3
CADJ STORE hflux1    = tapelev3, key = ilev_3
CADJ STORE sflux0    = tapelev3, key = ilev_3
CADJ STORE sflux1    = tapelev3, key = ilev_3
CADJ STORE ustress0  = tapelev3, key = ilev_3
CADJ STORE ustress1  = tapelev3, key = ilev_3
CADJ STORE vstress0  = tapelev3, key = ilev_3
CADJ STORE vstress1  = tapelev3, key = ilev_3
CADJ STORE wspeed0   = tapelev3, key = ilev_3
CADJ STORE wspeed1   = tapelev3, key = ilev_3

# ifdef ALLOW_ATM_TEMP

CADJ STORE aqh0      = tapelev3, key = ilev_3
CADJ STORE aqh1      = tapelev3, key = ilev_3
CADJ STORE atemp0    = tapelev3, key = ilev_3
CADJ STORE atemp1    = tapelev3, key = ilev_3
CADJ STORE precip0   = tapelev3, key = ilev_3
CADJ STORE precip1   = tapelev3, key = ilev_3
CADJ STORE lwflux0   = tapelev3, key = ilev_3
CADJ STORE lwflux1   = tapelev3, key = ilev_3
CADJ STORE swflux0   = tapelev3, key = ilev_3
CADJ STORE swflux1   = tapelev3, key = ilev_3
#  ifdef EXF_READ_EVAP
CADJ STORE evap0     = tapelev3, key = ilev_3
CADJ STORE evap1     = tapelev3, key = ilev_3
#  else
CADJ STORE evap      = tapelev3, key = ilev_3
#  endif /* EXF_READ_EVAP */
#  ifdef ALLOW_DOWNWARD_RADIATION
CADJ STORE swdown0   = tapelev3, key = ilev_3
CADJ STORE swdown1   = tapelev3, key = ilev_3
CADJ STORE lwdown0   = tapelev3, key = ilev_3
CADJ STORE lwdown1   = tapelev3, key = ilev_3
#  endif

# else /* ALLOW_ATM_TEMP undef */

#  ifdef SHORTWAVE_HEATING
CADJ STORE swflux0   = tapelev3, key = ilev_3
CADJ STORE swflux1   = tapelev3, key = ilev_3
#  endif

# endif /* ALLOW_ATM_TEMP */

# ifdef ALLOW_ATM_WIND

CADJ STORE uwind0    = tapelev3, key = ilev_3
CADJ STORE uwind1    = tapelev3, key = ilev_3
CADJ STORE vwind0    = tapelev3, key = ilev_3
CADJ STORE vwind1    = tapelev3, key = ilev_3

# else /* ALLOW_ATM_WIND undef */

# endif  /* ALLOW_ATM_WIND */

# ifdef ALLOW_CLIMSSS_RELAXATION
CADJ STORE climsss0   = tapelev3, key = ilev_3
CADJ STORE climsss1   = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
CADJ STORE climsst0   = tapelev3, key = ilev_3
CADJ STORE climsst1   = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_CLIMSALT_RELAXATION
cph not used so far
cphCADJ STORE climsalt0  = tapelev3, key = ilev_3
cphCADJ STORE climsalt1  = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_CLIMTEMP_RELAXATION
cph not used so far
cphCADJ STORE climtemp0  = tapelev3, key = ilev_3
cphCADJ STORE climtemp1  = tapelev3, key = ilev_3
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE apressure0    = tapelev3, key = ilev_3
CADJ STORE apressure1    = tapelev3, key = ilev_3
# endif

# ifdef ALLOW_HFLUX_CONTROL
CADJ STORE xx_hflux0     = tapelev3, key = ilev_3
CADJ STORE xx_hflux1     = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_SFLUX_CONTROL
CADJ STORE xx_sflux0     = tapelev3, key = ilev_3
CADJ STORE xx_sflux1     = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_USTRESS_CONTROL
CADJ STORE xx_tauu0      = tapelev3, key = ilev_3
CADJ STORE xx_tauu1      = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_VSTRESS_CONTROL
CADJ STORE xx_tauv0      = tapelev3, key = ilev_3
CADJ STORE xx_tauv1      = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_UWIND_CONTROL
CADJ STORE xx_uwind0     = tapelev3, key = ilev_3
CADJ STORE xx_uwind1     = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_VWIND_CONTROL
CADJ STORE xx_vwind0     = tapelev3, key = ilev_3
CADJ STORE xx_vwind1     = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_ATEMP_CONTROL
CADJ STORE xx_atemp0     = tapelev3, key = ilev_3
CADJ STORE xx_atemp1     = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_AQH_CONTROL
CADJ STORE xx_aqh0       = tapelev3, key = ilev_3
CADJ STORE xx_aqh1       = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_PRECIP_CONTROL
CADJ STORE xx_precip0    = tapelev3, key = ilev_3
CADJ STORE xx_precip1    = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_SWFLUX_CONTROL
CADJ STORE xx_swflux0    = tapelev3, key = ilev_3
CADJ STORE xx_swflux1    = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_SWDOWN_CONTROL
CADJ STORE xx_swdown0    = tapelev3, key = ilev_3
CADJ STORE xx_swdown1    = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_SST_CONTROL
CADJ STORE xx_sst0    = tapelev3, key = ilev_3
CADJ STORE xx_sst1    = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_SSS_CONTROL
CADJ STORE xx_sss0    = tapelev3, key = ilev_3
CADJ STORE xx_sss1    = tapelev3, key = ilev_3
# endif

#endif /* ALLOW_EXF */
