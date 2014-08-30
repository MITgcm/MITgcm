C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_ad_check_lev1_dir.h,v 1.12 2014/08/30 21:50:35 jmc Exp $
C $Name:  $

#ifdef ALLOW_OBCS

#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNu    = comlev1, key = ikey_dynamics
CADJ STORE OBNv    = comlev1, key = ikey_dynamics
CADJ STORE OBNt    = comlev1, key = ikey_dynamics
CADJ STORE OBNs    = comlev1, key = ikey_dynamics
CADJ STORE obnu0,obnu1  = comlev1, key = ikey_dynamics
CADJ STORE obnv0,obnv1  = comlev1, key = ikey_dynamics
CADJ STORE obnt0,obnt1  = comlev1, key = ikey_dynamics
CADJ STORE obns0,obns1  = comlev1, key = ikey_dynamics
#ifdef NONLIN_FRSURF
CADJ STORE obneta0,obneta1 = comlev1, key = ikey_dynamics
#endif
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBNtStevens = comlev1, key = ikey_dynamics
CADJ STORE OBNsStevens = comlev1, key = ikey_dynamics
CADJ STORE OBNvStevens = comlev1, key = ikey_dynamics
# endif /* ALLOW_OBCS_STEVENS */
# ifdef ALLOW_OBCSN_CONTROL
CADJ STORE xx_obcsn0,xx_obcsn1 = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSu    = comlev1, key = ikey_dynamics
CADJ STORE OBSv    = comlev1, key = ikey_dynamics
CADJ STORE OBSt    = comlev1, key = ikey_dynamics
CADJ STORE OBSs    = comlev1, key = ikey_dynamics
CADJ STORE obsu0,obsu1  = comlev1, key = ikey_dynamics
CADJ STORE obsv0,obsv1  = comlev1, key = ikey_dynamics
CADJ STORE obst0,obst1  = comlev1, key = ikey_dynamics
CADJ STORE obss0,obss1  = comlev1, key = ikey_dynamics
#ifdef NONLIN_FRSURF
CADJ STORE obseta0,obseta1 = comlev1, key = ikey_dynamics
#endif
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBStStevens = comlev1, key = ikey_dynamics
CADJ STORE OBSsStevens = comlev1, key = ikey_dynamics
CADJ STORE OBSvStevens = comlev1, key = ikey_dynamics
# endif /* ALLOW_OBCS_STEVENS */
# ifdef ALLOW_OBCSS_CONTROL
CADJ STORE xx_obcss0,xx_obcss1 = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_OBCS_SOUTH */

#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEu    = comlev1, key = ikey_dynamics
CADJ STORE OBEv    = comlev1, key = ikey_dynamics
CADJ STORE OBEt    = comlev1, key = ikey_dynamics
CADJ STORE OBEs    = comlev1, key = ikey_dynamics
CADJ STORE obeu0,obeu1  = comlev1, key = ikey_dynamics
CADJ STORE obev0,obev1  = comlev1, key = ikey_dynamics
CADJ STORE obet0,obet1  = comlev1, key = ikey_dynamics
CADJ STORE obes0,obes1  = comlev1, key = ikey_dynamics
#ifdef NONLIN_FRSURF
CADJ STORE obeeta0,obeeta1 = comlev1, key = ikey_dynamics
#endif
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBEtStevens = comlev1, key = ikey_dynamics
CADJ STORE OBEsStevens = comlev1, key = ikey_dynamics
CADJ STORE OBEuStevens = comlev1, key = ikey_dynamics
# endif /* ALLOW_OBCS_STEVENS */
# ifdef ALLOW_OBCSE_CONTROL
CADJ STORE xx_obcse0,xx_obcse1 = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWu    = comlev1, key = ikey_dynamics
CADJ STORE OBWv    = comlev1, key = ikey_dynamics
CADJ STORE OBWt    = comlev1, key = ikey_dynamics
CADJ STORE OBWs    = comlev1, key = ikey_dynamics
CADJ STORE obwu0,obwu1  = comlev1, key = ikey_dynamics
CADJ STORE obwv0,obwv1  = comlev1, key = ikey_dynamics
CADJ STORE obwt0,obwt1  = comlev1, key = ikey_dynamics
CADJ STORE obws0,obws1  = comlev1, key = ikey_dynamics
#ifdef NONLIN_FRSURF
CADJ STORE obweta0,obweta1 = comlev1, key = ikey_dynamics
#endif
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBWtStevens = comlev1, key = ikey_dynamics
CADJ STORE OBWsStevens = comlev1, key = ikey_dynamics
CADJ STORE OBWuStevens = comlev1, key = ikey_dynamics
# endif /* ALLOW_OBCS_STEVENS */
# ifdef ALLOW_OBCSW_CONTROL
CADJ STORE xx_obcsw0,xx_obcsw1 = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_OBCS_WEST */

# ifdef ALLOW_PTRACERS

#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNptr  = comlev1, key = ikey_dynamics
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNptr0 = comlev1, key = ikey_dynamics
CADJ STORE OBNptr1 = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSptr  = comlev1, key = ikey_dynamics
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSptr0 = comlev1, key = ikey_dynamics
CADJ STORE OBSptr1 = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEptr  = comlev1, key = ikey_dynamics
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEptr0 = comlev1, key = ikey_dynamics
CADJ STORE OBEptr1 = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWptr  = comlev1, key = ikey_dynamics
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWptr0 = comlev1, key = ikey_dynamics
CADJ STORE OBWptr1 = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */

# endif  /* ALLOW_PTRACERS */

# ifdef ALLOW_SEAICE

#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNh  = comlev1, key = ikey_dynamics
CADJ STORE OBNa  = comlev1, key = ikey_dynamics
CADJ STORE OBNsn = comlev1, key = ikey_dynamics
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBNsl = comlev1, key = ikey_dynamics
CADJ STORE OBNsl0,OBNsl1  = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNh0,OBNh1  = comlev1, key = ikey_dynamics
CADJ STORE OBNa0,OBNa1  = comlev1, key = ikey_dynamics
CADJ STORE OBNsn0,OBNsn1  = comlev1, key = ikey_dynamics
# endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSh  = comlev1, key = ikey_dynamics
CADJ STORE OBSa  = comlev1, key = ikey_dynamics
CADJ STORE OBSsn = comlev1, key = ikey_dynamics
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBSsl = comlev1, key = ikey_dynamics
CADJ STORE OBSsl0,OBSsl1  = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSh0,OBSh1  = comlev1, key = ikey_dynamics
CADJ STORE OBSa0,OBSa1  = comlev1, key = ikey_dynamics
CADJ STORE OBSsn0,OBSsn1  = comlev1, key = ikey_dynamics
# endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEh  = comlev1, key = ikey_dynamics
CADJ STORE OBEa  = comlev1, key = ikey_dynamics
CADJ STORE OBEsn = comlev1, key = ikey_dynamics
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBEsl = comlev1, key = ikey_dynamics
CADJ STORE OBEsl0,OBEsl1  = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEh0,OBEh1  = comlev1, key = ikey_dynamics
CADJ STORE OBEa0,OBEa1  = comlev1, key = ikey_dynamics
CADJ STORE OBEsn0,OBEsn1  = comlev1, key = ikey_dynamics
# endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWh  = comlev1, key = ikey_dynamics
CADJ STORE OBWa  = comlev1, key = ikey_dynamics
CADJ STORE OBWsn = comlev1, key = ikey_dynamics
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBWsl = comlev1, key = ikey_dynamics
CADJ STORE OBWsl0,OBWsl1  = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWh0,OBWh1  = comlev1, key = ikey_dynamics
CADJ STORE OBWa0,OBWa1  = comlev1, key = ikey_dynamics
CADJ STORE OBWsn0,OBWsn1  = comlev1, key = ikey_dynamics
# endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */

# endif /* ALLOW_SEAICE */

#endif  /* ALLOW_OBCS */
