c
c     store directives for checkpoint level 3
c
c     created: heimbach@mit.edu 10-Jan-2002
c
CADJ STORE gsnm1     = tapelev3, key = ilev_3
CADJ STORE gtnm1     = tapelev3, key = ilev_3
CADJ STORE gunm1     = tapelev3, key = ilev_3
CADJ STORE gvnm1     = tapelev3, key = ilev_3
CADJ STORE theta     = tapelev3, key = ilev_3
CADJ STORE salt      = tapelev3, key = ilev_3
CADJ STORE uvel      = tapelev3, key = ilev_3
CADJ STORE vvel      = tapelev3, key = ilev_3
CADJ STORE wvel      = tapelev3, key = ilev_3
CADJ STORE etan      = tapelev3, key = ilev_3
CADJ STORE gu        = tapelev3, key = ilev_3
CADJ STORE gv        = tapelev3, key = ilev_3

#ifdef INCLUDE_CD_CODE
CADJ STORE uveld     = tapelev3, key = ilev_3
CADJ STORE vveld     = tapelev3, key = ilev_3
CADJ STORE etanm1    = tapelev3, key = ilev_3
CADJ STORE unm1      = tapelev3, key = ilev_3
CADJ STORE vnm1      = tapelev3, key = ilev_3
CADJ STORE gucd      = tapelev3, key = ilev_3
CADJ STORE gvcd      = tapelev3, key = ilev_3
#endif /* INCLUDE_CD_CODE */

#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1       = tapelev3, key = ilev_3
CADJ STORE gtr1nm1   = tapelev3, key = ilev_3
#endif /*  */

#ifdef EXACT_CONSERV
CADJ STORE etaH      = tapelev3, key = ilev_3
CADJ STORE hDivFlow  = tapelev3, key = ilev_3
#endif /*  */

#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
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
# else
CADJ STORE hflux0    = tapelev3, key = ilev_3
CADJ STORE hflux1    = tapelev3, key = ilev_3
CADJ STORE sflux0    = tapelev3, key = ilev_3
CADJ STORE sflux1    = tapelev3, key = ilev_3
#  ifdef ALLOW_KPP
CADJ STORE swflux0   = tapelev3, key = ilev_3
CADJ STORE swflux1   = tapelev3, key = ilev_3
#  endif
# endif /* ALLOW_ATM_TEMP */
# ifdef ALLOW_ATM_WIND
CADJ STORE uwind0    = tapelev3, key = ilev_3
CADJ STORE uwind1    = tapelev3, key = ilev_3
CADJ STORE vwind0    = tapelev3, key = ilev_3
CADJ STORE vwind1    = tapelev3, key = ilev_3
# else
CADJ STORE ustress0  = tapelev3, key = ilev_3
CADJ STORE ustress1  = tapelev3, key = ilev_3
CADJ STORE vstress0  = tapelev3, key = ilev_3
CADJ STORE vstress1  = tapelev3, key = ilev_3
# endif  /* ALLOW_ATM_WIND */

#else /* INCLUDE_EXTERNAL_FORCING_PACKAGE undef */

CADJ STORE taux0   = tapelev3, key = ilev_3
CADJ STORE taux1   = tapelev3, key = ilev_3
CADJ STORE tauy0   = tapelev3, key = ilev_3
CADJ STORE tauy1   = tapelev3, key = ilev_3
CADJ STORE Qnet0   = tapelev3, key = ilev_3
CADJ STORE Qnet1   = tapelev3, key = ilev_3
CADJ STORE EmPmR0  = tapelev3, key = ilev_3
CADJ STORE EmPmR1  = tapelev3, key = ilev_3
CADJ STORE SST0    = tapelev3, key = ilev_3
CADJ STORE SST1    = tapelev3, key = ilev_3
CADJ STORE SSS0    = tapelev3, key = ilev_3
CADJ STORE SSS1    = tapelev3, key = ilev_3
CADJ STORE Qsw0    = tapelev3, key = ilev_3
CADJ STORE Qsw1    = tapelev3, key = ilev_3

#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNu    = tapelev3, key = ilev_3
CADJ STORE OBNv    = tapelev3, key = ilev_3
CADJ STORE OBNt    = tapelev3, key = ilev_3
CADJ STORE OBNs    = tapelev3, key = ilev_3
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSu    = tapelev3, key = ilev_3
CADJ STORE OBSv    = tapelev3, key = ilev_3
CADJ STORE OBSt    = tapelev3, key = ilev_3
CADJ STORE OBSs    = tapelev3, key = ilev_3
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEu    = tapelev3, key = ilev_3
CADJ STORE OBEv    = tapelev3, key = ilev_3
CADJ STORE OBEt    = tapelev3, key = ilev_3
CADJ STORE OBEs    = tapelev3, key = ilev_3
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWu    = tapelev3, key = ilev_3
CADJ STORE OBWv    = tapelev3, key = ilev_3
CADJ STORE OBWt    = tapelev3, key = ilev_3
CADJ STORE OBWs    = tapelev3, key = ilev_3
#endif /* ALLOW_OBCS_WEST */
#endif  /* ALLOW_OBCS */

