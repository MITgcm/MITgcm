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
# else
CADJ STORE hflux0    = comlev1, key = ikey_dynamics
CADJ STORE hflux1    = comlev1, key = ikey_dynamics
CADJ STORE sflux0    = comlev1, key = ikey_dynamics
CADJ STORE sflux1    = comlev1, key = ikey_dynamics
#  ifdef ALLOW_KPP
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
CADJ STORE Qsw0    = comlev1, key = ikey_dynamics
CADJ STORE Qsw1    = comlev1, key = ikey_dynamics


#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */

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

