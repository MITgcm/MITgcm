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

#ifdef INCLUDE_CD_CODE
CADJ STORE uveld     = tapelev2, key = ilev_2
CADJ STORE vveld     = tapelev2, key = ilev_2
CADJ STORE etanm1    = tapelev2, key = ilev_2
CADJ STORE unm1      = tapelev2, key = ilev_2
CADJ STORE vnm1      = tapelev2, key = ilev_2
CADJ STORE gucd      = tapelev2, key = ilev_2
CADJ STORE gvcd      = tapelev2, key = ilev_2
#endif

#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1       = tapelev2, key = ilev_2
CADJ STORE gtr1nm1   = tapelev2, key = ilev_2
#endif

#ifdef EXACT_CONSERV
CADJ STORE etaH      = tapelev2, key = ilev_2
CADJ STORE hDivFlow  = tapelev2, key = ilev_2
#endif

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
# else
CADJ STORE hflux0    = tapelev2, key = ilev_2
CADJ STORE hflux1    = tapelev2, key = ilev_2
CADJ STORE sflux0    = tapelev2, key = ilev_2
CADJ STORE sflux1    = tapelev2, key = ilev_2
#  ifdef ALLOW_KPP
CADJ STORE swflux0   = tapelev2, key = ilev_2
CADJ STORE swflux1   = tapelev2, key = ilev_2
#  endif
# endif /* ALLOW_ATM_TEMP */
# ifdef ALLOW_ATM_WIND
CADJ STORE uwind0    = tapelev2, key = ilev_2
CADJ STORE uwind1    = tapelev2, key = ilev_2
CADJ STORE vwind0    = tapelev2, key = ilev_2
CADJ STORE vwind1    = tapelev2, key = ilev_2
# else
CADJ STORE ustress0  = tapelev2, key = ilev_2
CADJ STORE ustress1  = tapelev2, key = ilev_2
CADJ STORE vstress0  = tapelev2, key = ilev_2
CADJ STORE vstress1  = tapelev2, key = ilev_2
# endif  /* ALLOW_ATM_WIND */
#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNu    = tapelev2, key = ilev_2
CADJ STORE OBNv    = tapelev2, key = ilev_2
CADJ STORE OBNt    = tapelev2, key = ilev_2
CADJ STORE OBNs    = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSu    = tapelev2, key = ilev_2
CADJ STORE OBSv    = tapelev2, key = ilev_2
CADJ STORE OBSt    = tapelev2, key = ilev_2
CADJ STORE OBSs    = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEu    = tapelev2, key = ilev_2
CADJ STORE OBEv    = tapelev2, key = ilev_2
CADJ STORE OBEt    = tapelev2, key = ilev_2
CADJ STORE OBEs    = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWu    = tapelev2, key = ilev_2
CADJ STORE OBWv    = tapelev2, key = ilev_2
CADJ STORE OBWt    = tapelev2, key = ilev_2
CADJ STORE OBWs    = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_WEST */
#endif  /* ALLOW_OBCS */

