c
c     store directives for checkpoint level 2
c
c     created: heimbach@mit.edu 10-Jan-2002
c
CADJ STORE gsnm1     = tapelev2, key = ilev_2
CADJ STORE gtnm1     = tapelev2, key = ilev_2
CADJ STORE gunm1     = tapelev2, key = ilev_2
CADJ STORE gvnm1     = tapelev2, key = ilev_2
CADJ STORE theta     = tapelev2, key = ilev_2
CADJ STORE salt      = tapelev2, key = ilev_2
CADJ STORE uvel      = tapelev2, key = ilev_2
CADJ STORE vvel      = tapelev2, key = ilev_2
CADJ STORE wvel      = tapelev2, key = ilev_2
CADJ STORE etan      = tapelev2, key = ilev_2
CADJ STORE gu        = tapelev2, key = ilev_2
CADJ STORE gv        = tapelev2, key = ilev_2
CADJ STORE totphihyd = tapelev2, key = ilev_2
CADJ STORE empmr     = tapelev2, key = ilev_2

#ifdef INCLUDE_CD_CODE
CADJ STORE uveld     = tapelev2, key = ilev_2
CADJ STORE vveld     = tapelev2, key = ilev_2
CADJ STORE etanm1    = tapelev2, key = ilev_2
CADJ STORE unm1      = tapelev2, key = ilev_2
CADJ STORE vnm1      = tapelev2, key = ilev_2
CADJ STORE gucd      = tapelev2, key = ilev_2
CADJ STORE gvcd      = tapelev2, key = ilev_2
#endif /* INCLUDE_CD_CODE */

#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1       = tapelev2, key = ilev_2
CADJ STORE gtr1nm1   = tapelev2, key = ilev_2
#endif /* ALLOW_PASSIVE_TRACER */

#ifdef EXACT_CONSERV
CADJ STORE etaH      = tapelev2, key = ilev_2
CADJ STORE hDivFlow  = tapelev2, key = ilev_2
#endif /* EXACT_CONSERV */

#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
# ifdef ALLOW_ATM_TEMP
CADJ STORE aqh0      = tapelev2, key = ilev_2
CADJ STORE aqh1      = tapelev2, key = ilev_2
CADJ STORE atemp0    = tapelev2, key = ilev_2
CADJ STORE atemp1    = tapelev2, key = ilev_2
CADJ STORE precip0   = tapelev2, key = ilev_2
CADJ STORE precip1   = tapelev2, key = ilev_2
CADJ STORE lwflux0   = tapelev2, key = ilev_2
CADJ STORE lwflux1   = tapelev2, key = ilev_2
CADJ STORE swflux0   = tapelev2, key = ilev_2
CADJ STORE swflux1   = tapelev2, key = ilev_2
#  ifdef EXF_READ_EVAP
CADJ STORE evap0     = tapelev2, key = ilev_2
CADJ STORE evap1     = tapelev2, key = ilev_2
#  else /* EXF_READ_EVAP undefined */
CADJ STORE evap      = tapelev2, key = ilev_2
#  endif /* EXF_READ_EVAP */
#  ifdef ALLOW_DOWNWARD_RADIATION
CADJ STORE swdown0   = tapelev2, key = ilev_2
CADJ STORE swdown1   = tapelev2, key = ilev_2
CADJ STORE lwdown0   = tapelev2, key = ilev_2
CADJ STORE lwdown1   = tapelev2, key = ilev_2
#  endif /* ALLOW_DOWNWARD_RADIATION */
# else /* ALLOW_ATM_TEMP */
CADJ STORE hflux0    = tapelev2, key = ilev_2
CADJ STORE hflux1    = tapelev2, key = ilev_2
CADJ STORE sflux0    = tapelev2, key = ilev_2
CADJ STORE sflux1    = tapelev2, key = ilev_2
#  ifdef SHORTWAVE_HEATING
CADJ STORE swflux0   = tapelev2, key = ilev_2
CADJ STORE swflux1   = tapelev2, key = ilev_2
#  endif /* SHORTWAVE_HEATING */
# endif /* ALLOW_ATM_TEMP */
# ifdef ALLOW_ATM_WIND
CADJ STORE uwind0    = tapelev2, key = ilev_2
CADJ STORE uwind1    = tapelev2, key = ilev_2
CADJ STORE vwind0    = tapelev2, key = ilev_2
CADJ STORE vwind1    = tapelev2, key = ilev_2
# else /* ALLOW_ATM_WIND undefined */
CADJ STORE ustress0  = tapelev2, key = ilev_2
CADJ STORE ustress1  = tapelev2, key = ilev_2
CADJ STORE vstress0  = tapelev2, key = ilev_2
CADJ STORE vstress1  = tapelev2, key = ilev_2
# endif /* ALLOW_ATM_WIND */
# ifdef ALLOW_CLIMSSS_RELAXATION
CADJ STORE climsss0   = tapelev2, key = ilev_2
CADJ STORE climsss1   = tapelev2, key = ilev_2
# endif
#ifdef ALLOW_CLIMSST_RELAXATION
CADJ STORE climsst0   = tapelev2, key = ilev_2
CADJ STORE climsst1   = tapelev2, key = ilev_2
# endif
#ifdef ALLOW_CLIMSALT_RELAXATION
cph not used so far
cphCADJ STORE climsalt0  = tapelev2, key = ilev_2
cphCADJ STORE climsalt1  = tapelev2, key = ilev_2
# endif
#ifdef ALLOW_CLIMTEMP_RELAXATION
cph not used so far
cphCADJ STORE climtemp0  = tapelev2, key = ilev_2
cphCADJ STORE climtemp1  = tapelev2, key = ilev_2
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE apressure0    = tapelev2, key = ilev_2
CADJ STORE apressure1    = tapelev2, key = ilev_2
# endif

# ifdef ALLOW_HFLUX_CONTROL
CADJ STORE xx_hflux0     = tapelev2, key = ilev_2
CADJ STORE xx_hflux1     = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_SFLUX_CONTROL
CADJ STORE xx_sflux0     = tapelev2, key = ilev_2
CADJ STORE xx_sflux1     = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_USTRESS_CONTROL
CADJ STORE xx_tauu0      = tapelev2, key = ilev_2
CADJ STORE xx_tauu1      = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_VSTRESS_CONTROL
CADJ STORE xx_tauv0      = tapelev2, key = ilev_2
CADJ STORE xx_tauv1      = tapelev2, key = ilev_2
#endif

#else /* INCLUDE_EXTERNAL_FORCING_PACKAGE undef */

CADJ STORE taux0   = tapelev2, key = ilev_2
CADJ STORE taux1   = tapelev2, key = ilev_2
CADJ STORE tauy0   = tapelev2, key = ilev_2
CADJ STORE tauy1   = tapelev2, key = ilev_2
CADJ STORE Qnet0   = tapelev2, key = ilev_2
CADJ STORE Qnet1   = tapelev2, key = ilev_2
CADJ STORE EmPmR0  = tapelev2, key = ilev_2
CADJ STORE EmPmR1  = tapelev2, key = ilev_2
CADJ STORE SST0    = tapelev2, key = ilev_2
CADJ STORE SST1    = tapelev2, key = ilev_2
CADJ STORE SSS0    = tapelev2, key = ilev_2
CADJ STORE SSS1    = tapelev2, key = ilev_2
#ifdef SHORTWAVE_HEATING
CADJ STORE Qsw0    = tapelev2, key = ilev_2
CADJ STORE Qsw1    = tapelev2, key = ilev_2
#endif
#ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0  = tapelev2, key = ilev_2
CADJ STORE pload1  = tapelev2, key = ilev_2
#endif

#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */

#ifdef ALLOW_OBCS

#if  (defined (BAROTROPIC_OBVEL_CONTROL) || \
      defined (BALANCE_CONTROL_VOLFLUX_GLOBAL))      
CADJ STORE shiftvel = tapelev2, key = ilev_2
#endif
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNt     = tapelev2, key = ilev_2
CADJ STORE OBNs     = tapelev2, key = ilev_2
CADJ STORE OBNu0    = tapelev2, key = ilev_2
CADJ STORE OBNv0    = tapelev2, key = ilev_2
CADJ STORE OBNt0    = tapelev2, key = ilev_2
CADJ STORE OBNs0    = tapelev2, key = ilev_2
CADJ STORE OBNu1    = tapelev2, key = ilev_2
CADJ STORE OBNv1    = tapelev2, key = ilev_2
CADJ STORE OBNt1    = tapelev2, key = ilev_2
CADJ STORE OBNs1    = tapelev2, key = ilev_2
#ifdef ALLOW_OBCSN_CONTROL
CADJ STORE xx_obcsn0      = tapelev2, key = ilev_2
CADJ STORE xx_obcsn1      = tapelev2, key = ilev_2
#endif
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSt     = tapelev2, key = ilev_2
CADJ STORE OBSs     = tapelev2, key = ilev_2
CADJ STORE OBSu0    = tapelev2, key = ilev_2
CADJ STORE OBSv0    = tapelev2, key = ilev_2
CADJ STORE OBSt0    = tapelev2, key = ilev_2
CADJ STORE OBSs0    = tapelev2, key = ilev_2
CADJ STORE OBSu1    = tapelev2, key = ilev_2
CADJ STORE OBSv1    = tapelev2, key = ilev_2
CADJ STORE OBSt1    = tapelev2, key = ilev_2
CADJ STORE OBSs1    = tapelev2, key = ilev_2
#ifdef ALLOW_OBCSS_CONTROL
CADJ STORE xx_obcss0      = tapelev2, key = ilev_2
CADJ STORE xx_obcss1      = tapelev2, key = ilev_2
#endif
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEt     = tapelev2, key = ilev_2
CADJ STORE OBEs     = tapelev2, key = ilev_2
CADJ STORE OBEu0    = tapelev2, key = ilev_2
CADJ STORE OBEv0    = tapelev2, key = ilev_2
CADJ STORE OBEt0    = tapelev2, key = ilev_2
CADJ STORE OBEs0    = tapelev2, key = ilev_2
CADJ STORE OBEu1    = tapelev2, key = ilev_2
CADJ STORE OBEv1    = tapelev2, key = ilev_2
CADJ STORE OBEt1    = tapelev2, key = ilev_2
CADJ STORE OBEs1    = tapelev2, key = ilev_2
#ifdef ALLOW_OBCSE_CONTROL
CADJ STORE xx_obcse0      = tapelev2, key = ilev_2
CADJ STORE xx_obcse1      = tapelev2, key = ilev_2
#endif
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWt     = tapelev2, key = ilev_2
CADJ STORE OBWs     = tapelev2, key = ilev_2
CADJ STORE OBWu0    = tapelev2, key = ilev_2
CADJ STORE OBWv0    = tapelev2, key = ilev_2
CADJ STORE OBWt0    = tapelev2, key = ilev_2
CADJ STORE OBWs0    = tapelev2, key = ilev_2
CADJ STORE OBWu1    = tapelev2, key = ilev_2
CADJ STORE OBWv1    = tapelev2, key = ilev_2
CADJ STORE OBWt1    = tapelev2, key = ilev_2
CADJ STORE OBWs1    = tapelev2, key = ilev_2
#ifdef ALLOW_OBCSW_CONTROL
CADJ STORE xx_obcsw0      = tapelev2, key = ilev_2
CADJ STORE xx_obcsw1      = tapelev2, key = ilev_2
#endif
#endif /* ALLOW_OBCS_WEST */

#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
CADJ STORE area     = tapelev2, key = ilev_2
CADJ STORE heff     = tapelev2, key = ilev_2
CADJ STORE hsnow    = tapelev2, key = ilev_2
CADJ STORE tice     = tapelev2, key = ilev_2
CADJ STORE runoff   = tapelev2, key = ilev_2
#endif /* ALLOW_SEAICE */

