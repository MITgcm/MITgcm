c
c     store directives for checkpoint level 1
c
c     created: heimbach@mit.edu 10-Jan-2002
c
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
# ifdef ALLOW_ATM_TEMP
CADJ STORE aqh0      = comlev1, key = ikey_dynamics
CADJ STORE aqh1      = comlev1, key = ikey_dynamics
CADJ STORE atemp0    = comlev1, key = ikey_dynamics
CADJ STORE atemp1    = comlev1, key = ikey_dynamics
CADJ STORE precip0   = comlev1, key = ikey_dynamics
CADJ STORE precip1   = comlev1, key = ikey_dynamics
CADJ STORE lwflux0   = comlev1, key = ikey_dynamics
CADJ STORE lwflux1   = comlev1, key = ikey_dynamics
CADJ STORE swflux0   = comlev1, key = ikey_dynamics
CADJ STORE swflux1   = comlev1, key = ikey_dynamics
#  ifdef EXF_READ_EVAP
CADJ STORE evap0     = comlev1, key = ikey_dynamics
CADJ STORE evap1     = comlev1, key = ikey_dynamics
#  else
CADJ STORE evap      = comlev1, key = ikey_dynamics
#  endif
#  ifdef ALLOW_DOWNWARD_RADIATION
CADJ STORE swdown0   = comlev1, key = ikey_dynamics
CADJ STORE swdown1   = comlev1, key = ikey_dynamics
CADJ STORE lwdown0   = comlev1, key = ikey_dynamics
CADJ STORE lwdown1   = comlev1, key = ikey_dynamics
#  endif
# else
CADJ STORE hflux0    = comlev1, key = ikey_dynamics
CADJ STORE hflux1    = comlev1, key = ikey_dynamics
CADJ STORE sflux0    = comlev1, key = ikey_dynamics
CADJ STORE sflux1    = comlev1, key = ikey_dynamics
#  ifdef SHORTWAVE_HEATING
CADJ STORE swflux0   = comlev1, key = ikey_dynamics
CADJ STORE swflux1   = comlev1, key = ikey_dynamics
#  endif
# endif /* ALLOW_ATM_TEMP */
# ifdef ALLOW_ATM_WIND
CADJ STORE uwind0    = comlev1, key = ikey_dynamics
CADJ STORE uwind1    = comlev1, key = ikey_dynamics
CADJ STORE vwind0    = comlev1, key = ikey_dynamics
CADJ STORE vwind1    = comlev1, key = ikey_dynamics
# else
CADJ STORE ustress0  = comlev1, key = ikey_dynamics
CADJ STORE ustress1  = comlev1, key = ikey_dynamics
CADJ STORE vstress0  = comlev1, key = ikey_dynamics
CADJ STORE vstress1  = comlev1, key = ikey_dynamics
# endif  /* ALLOW_ATM_WIND */
# ifdef ALLOW_BULKFORMULAE
CADJ STORE theta     = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
CADJ STORE climsss0   = comlev1, key = ikey_dynamics
CADJ STORE climsss1   = comlev1, key = ikey_dynamics
# endif
#ifdef ALLOW_CLIMSST_RELAXATION
CADJ STORE climsst0   = comlev1, key = ikey_dynamics
CADJ STORE climsst1   = comlev1, key = ikey_dynamics
# endif
#ifdef ALLOW_CLIMSALT_RELAXATION
cph not used so far
cphCADJ STORE climsalt0  = comlev1, key = ikey_dynamics
cphCADJ STORE climsalt1  = comlev1, key = ikey_dynamics
# endif
#ifdef ALLOW_CLIMTEMP_RELAXATION
cph not used so far
cphCADJ STORE climtemp0  = comlev1, key = ikey_dynamics
cphCADJ STORE climtemp1  = comlev1, key = ikey_dynamics
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE apressure0    = comlev1, key = ikey_dynamics
CADJ STORE apressure1    = comlev1, key = ikey_dynamics
# endif

# ifdef ALLOW_HFLUX_CONTROL
CADJ STORE xx_hflux0     = comlev1, key = ikey_dynamics
CADJ STORE xx_hflux1     = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_SFLUX_CONTROL
CADJ STORE xx_sflux0     = comlev1, key = ikey_dynamics
CADJ STORE xx_sflux1     = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_USTRESS_CONTROL
CADJ STORE xx_tauu0      = comlev1, key = ikey_dynamics
CADJ STORE xx_tauu1      = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_VSTRESS_CONTROL
CADJ STORE xx_tauv0      = comlev1, key = ikey_dynamics
CADJ STORE xx_tauv1      = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_UWIND_CONTROL
CADJ STORE xx_uwind0     = comlev1, key = ikey_dynamics
CADJ STORE xx_uwind1     = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_VWIND_CONTROL
CADJ STORE xx_vwind0     = comlev1, key = ikey_dynamics
CADJ STORE xx_vwind1     = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_ATEMP_CONTROL
CADJ STORE xx_atemp0     = comlev1, key = ikey_dynamics
CADJ STORE xx_atemp1     = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_AQH_CONTROL
CADJ STORE xx_aqh0       = comlev1, key = ikey_dynamics
CADJ STORE xx_aqh1       = comlev1, key = ikey_dynamics
# endif

#else /* INCLUDE_EXTERNAL_FORCING_PACKAGE undef */

CADJ STORE taux0   = comlev1, key = ikey_dynamics
CADJ STORE taux1   = comlev1, key = ikey_dynamics
CADJ STORE tauy0   = comlev1, key = ikey_dynamics
CADJ STORE tauy1   = comlev1, key = ikey_dynamics
CADJ STORE Qnet0   = comlev1, key = ikey_dynamics
CADJ STORE Qnet1   = comlev1, key = ikey_dynamics
CADJ STORE EmPmR0  = comlev1, key = ikey_dynamics
CADJ STORE EmPmR1  = comlev1, key = ikey_dynamics
CADJ STORE SST0    = comlev1, key = ikey_dynamics
CADJ STORE SST1    = comlev1, key = ikey_dynamics
CADJ STORE SSS0    = comlev1, key = ikey_dynamics
CADJ STORE SSS1    = comlev1, key = ikey_dynamics
#ifdef SHORTWAVE_HEATING
CADJ STORE Qsw0    = comlev1, key = ikey_dynamics
CADJ STORE Qsw1    = comlev1, key = ikey_dynamics
#endif
#ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0  = comlev1, key = ikey_dynamics
CADJ STORE pload1  = comlev1, key = ikey_dynamics
#endif

#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */

cph The following storing may not be needed anymore
cph but cannot tell for sure, so leave them.
cph 
#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNt    = comlev1, key = ikey_dynamics
CADJ STORE OBNs    = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSt    = comlev1, key = ikey_dynamics
CADJ STORE OBSs    = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEt    = comlev1, key = ikey_dynamics
CADJ STORE OBEs    = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWt    = comlev1, key = ikey_dynamics
CADJ STORE OBWs    = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_WEST */
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
CADJ STORE area          = comlev1, key = ikey_dynamics
CADJ STORE heff          = comlev1, key = ikey_dynamics
CADJ STORE hsnow         = comlev1, key = ikey_dynamics
CADJ STORE tice          = comlev1, key = ikey_dynamics
CADJ STORE runoff        = comlev1, key = ikey_dynamics
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uvel          = comlev1, key = ikey_dynamics
CADJ STORE vvel          = comlev1, key = ikey_dynamics
CADJ STORE uice          = comlev1, key = ikey_dynamics
CADJ STORE vice          = comlev1, key = ikey_dynamics
# endif
#endif

