#ifdef ALLOW_EXF

# ifdef EXF_SEAICE_FRACTION
CADJ STORE areamask0    = tapelev4, key = ilev_4
CADJ STORE areamask1    = tapelev4, key = ilev_4
# endif

# ifdef ECCO_CTRL_DEPRECATED
# ifdef ALLOW_HFLUX_CONTROL
CADJ STORE xx_hflux0     = tapelev4, key = ilev_4
CADJ STORE xx_hflux1     = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_SFLUX_CONTROL
CADJ STORE xx_sflux0     = tapelev4, key = ilev_4
CADJ STORE xx_sflux1     = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_USTRESS_CONTROL
CADJ STORE xx_tauu0      = tapelev4, key = ilev_4
CADJ STORE xx_tauu1      = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_VSTRESS_CONTROL
CADJ STORE xx_tauv0      = tapelev4, key = ilev_4
CADJ STORE xx_tauv1      = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_SST_CONTROL
CADJ STORE xx_sst0      = tapelev4, key = ilev_4
CADJ STORE xx_sst1      = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_SSS_CONTROL
CADJ STORE xx_sss0      = tapelev4, key = ilev_4
CADJ STORE xx_sss1      = tapelev4, key = ilev_4
# endif
# endif /* ECCO_CTRL_DEPRECATED */

#ifdef AUTODIFF_USE_STORE_RESTORE
CADJ STORE StoreEXF1   = tapelev4, key = ilev_4
CADJ STORE StoreEXF2   = tapelev4, key = ilev_4
CADJ STORE StoreCTRLS1 = tapelev4, key = ilev_4
#else
CADJ STORE hflux0   = tapelev4, key = ilev_4
CADJ STORE hflux1   = tapelev4, key = ilev_4
CADJ STORE sflux0   = tapelev4, key = ilev_4
CADJ STORE sflux1   = tapelev4, key = ilev_4
CADJ STORE ustress0 = tapelev4, key = ilev_4
CADJ STORE ustress1 = tapelev4, key = ilev_4
CADJ STORE vstress0 = tapelev4, key = ilev_4
CADJ STORE vstress1 = tapelev4, key = ilev_4
CADJ STORE wspeed0  = tapelev4, key = ilev_4
CADJ STORE wspeed1  = tapelev4, key = ilev_4
# ifdef SHORTWAVE_HEATING
CADJ STORE swflux0  = tapelev4, key = ilev_4
CADJ STORE swflux1  = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_RUNOFF
CADJ STORE runoff0  = tapelev4, key = ilev_4
CADJ STORE runoff1  = tapelev4, key = ilev_4
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE apressure0=tapelev4, key = ilev_4
CADJ STORE apressure1=tapelev4, key = ilev_4
CADJ STORE siceload = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
CADJ STORE climsss0 = tapelev4, key = ilev_4
CADJ STORE climsss1 = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
CADJ STORE climsst0 = tapelev4, key = ilev_4
CADJ STORE climsst1 = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_SALTFLX
CADJ STORE saltflx0 = tapelev4, key = ilev_4
CADJ STORE saltflx1 = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_ATM_TEMP
CADJ STORE aqh0        = tapelev4, key = ilev_4
CADJ STORE aqh1        = tapelev4, key = ilev_4
CADJ STORE atemp0      = tapelev4, key = ilev_4
CADJ STORE atemp1      = tapelev4, key = ilev_4
CADJ STORE precip0     = tapelev4, key = ilev_4
CADJ STORE precip1     = tapelev4, key = ilev_4
CADJ STORE lwflux0     = tapelev4, key = ilev_4
CADJ STORE lwflux1     = tapelev4, key = ilev_4
CADJ STORE snowprecip0 = tapelev4, key = ilev_4
CADJ STORE snowprecip1 = tapelev4, key = ilev_4
#  ifdef ALLOW_READ_TURBFLUXES
CADJ STORE hs0         = tapelev4, key = ilev_4
CADJ STORE hs1         = tapelev4, key = ilev_4
CADJ STORE hl0         = tapelev4, key = ilev_4
CADJ STORE hl1         = tapelev4, key = ilev_4
#  endif
#  ifdef EXF_READ_EVAP
CADJ STORE evap0       = tapelev4, key = ilev_4
CADJ STORE evap1       = tapelev4, key = ilev_4
#  endif /* EXF_READ_EVAP */
#  ifdef ALLOW_DOWNWARD_RADIATION
CADJ STORE swdown0     = tapelev4, key = ilev_4
CADJ STORE swdown1     = tapelev4, key = ilev_4
CADJ STORE lwdown0     = tapelev4, key = ilev_4
CADJ STORE lwdown1     = tapelev4, key = ilev_4
#  endif
# endif /* ALLOW_ATM_TEMP */
CADJ STORE uwind0      = tapelev4, key = ilev_4
CADJ STORE uwind1      = tapelev4, key = ilev_4
CADJ STORE vwind0      = tapelev4, key = ilev_4
CADJ STORE vwind1      = tapelev4, key = ilev_4
#endif /* AUTODIFF_USE_STORE_RESTORE */
#endif /* ALLOW_EXF */
