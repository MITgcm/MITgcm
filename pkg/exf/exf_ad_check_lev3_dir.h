#ifdef ALLOW_EXF

# ifdef EXF_SEAICE_FRACTION
CADJ STORE areamask0    = tapelev3, key = ilev_3
CADJ STORE areamask1    = tapelev3, key = ilev_3
# endif

# ifdef ECCO_CTRL_DEPRECATED
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
# ifdef ALLOW_SST_CONTROL
CADJ STORE xx_sst0      = tapelev3, key = ilev_3
CADJ STORE xx_sst1      = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_SSS_CONTROL
CADJ STORE xx_sss0      = tapelev3, key = ilev_3
CADJ STORE xx_sss1      = tapelev3, key = ilev_3
# endif
# endif /* ECCO_CTRL_DEPRECATED */

#ifdef AUTODIFF_USE_STORE_RESTORE
CADJ STORE StoreEXF1   = tapelev3, key = ilev_3
CADJ STORE StoreEXF2   = tapelev3, key = ilev_3
CADJ STORE StoreCTRLS1 = tapelev3, key = ilev_3
#else
CADJ STORE hflux0   = tapelev3, key = ilev_3
CADJ STORE hflux1   = tapelev3, key = ilev_3
CADJ STORE sflux0   = tapelev3, key = ilev_3
CADJ STORE sflux1   = tapelev3, key = ilev_3
CADJ STORE ustress0 = tapelev3, key = ilev_3
CADJ STORE ustress1 = tapelev3, key = ilev_3
CADJ STORE vstress0 = tapelev3, key = ilev_3
CADJ STORE vstress1 = tapelev3, key = ilev_3
CADJ STORE wspeed0  = tapelev3, key = ilev_3
CADJ STORE wspeed1  = tapelev3, key = ilev_3
# ifdef SHORTWAVE_HEATING
CADJ STORE swflux0  = tapelev3, key = ilev_3
CADJ STORE swflux1  = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_RUNOFF
CADJ STORE runoff0  = tapelev3, key = ilev_3
CADJ STORE runoff1  = tapelev3, key = ilev_3
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE apressure0=tapelev3, key = ilev_3
CADJ STORE apressure1=tapelev3, key = ilev_3
CADJ STORE siceload = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
CADJ STORE climsss0 = tapelev3, key = ilev_3
CADJ STORE climsss1 = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
CADJ STORE climsst0 = tapelev3, key = ilev_3
CADJ STORE climsst1 = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_SALTFLX
CADJ STORE saltflx0 = tapelev3, key = ilev_3
CADJ STORE saltflx1 = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_ATM_TEMP
CADJ STORE aqh0        = tapelev3, key = ilev_3
CADJ STORE aqh1        = tapelev3, key = ilev_3
CADJ STORE atemp0      = tapelev3, key = ilev_3
CADJ STORE atemp1      = tapelev3, key = ilev_3
CADJ STORE precip0     = tapelev3, key = ilev_3
CADJ STORE precip1     = tapelev3, key = ilev_3
CADJ STORE lwflux0     = tapelev3, key = ilev_3
CADJ STORE lwflux1     = tapelev3, key = ilev_3
CADJ STORE snowprecip0 = tapelev3, key = ilev_3
CADJ STORE snowprecip1 = tapelev3, key = ilev_3
#  ifdef ALLOW_READ_TURBFLUXES
CADJ STORE hs0         = tapelev3, key = ilev_3
CADJ STORE hs1         = tapelev3, key = ilev_3
CADJ STORE hl0         = tapelev3, key = ilev_3
CADJ STORE hl1         = tapelev3, key = ilev_3
#  endif
#  ifdef EXF_READ_EVAP
CADJ STORE evap0       = tapelev3, key = ilev_3
CADJ STORE evap1       = tapelev3, key = ilev_3
#  endif /* EXF_READ_EVAP */
#  ifdef ALLOW_DOWNWARD_RADIATION
CADJ STORE swdown0     = tapelev3, key = ilev_3
CADJ STORE swdown1     = tapelev3, key = ilev_3
CADJ STORE lwdown0     = tapelev3, key = ilev_3
CADJ STORE lwdown1     = tapelev3, key = ilev_3
#  endif
# endif /* ALLOW_ATM_TEMP */
CADJ STORE uwind0      = tapelev3, key = ilev_3
CADJ STORE uwind1      = tapelev3, key = ilev_3
CADJ STORE vwind0      = tapelev3, key = ilev_3
CADJ STORE vwind1      = tapelev3, key = ilev_3
#endif /* AUTODIFF_USE_STORE_RESTORE */
#endif /* ALLOW_EXF */
