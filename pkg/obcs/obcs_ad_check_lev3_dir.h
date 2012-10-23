C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_ad_check_lev3_dir.h,v 1.9 2012/10/23 20:15:32 heimbach Exp $
C $Name:  $

#ifdef ALLOW_OBCS
#
# ifdef ALLOW_OBCS_NORTH
CADJ STORE StoreOBCSN     = tapelev3, key = ilev_3
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBNtStevens    = tapelev3, key = ilev_3
CADJ STORE OBNsStevens    = tapelev3, key = ilev_3
CADJ STORE OBNvStevens    = tapelev3, key = ilev_3
# endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_NORTH */
#
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE StoreOBCSS     = tapelev3, key = ilev_3
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBStStevens    = tapelev3, key = ilev_3
CADJ STORE OBSsStevens    = tapelev3, key = ilev_3
CADJ STORE OBSvStevens    = tapelev3, key = ilev_3
# endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_SOUTH */
#
# ifdef ALLOW_OBCS_EAST
CADJ STORE StoreOBCSE     = tapelev3, key = ilev_3
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBEtStevens    = tapelev3, key = ilev_3
CADJ STORE OBEsStevens    = tapelev3, key = ilev_3
CADJ STORE OBEuStevens    = tapelev3, key = ilev_3
# endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_EAST */
#
# ifdef ALLOW_OBCS_WEST
CADJ STORE StoreOBCSW     = tapelev3, key = ilev_3
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBWtStevens    = tapelev3, key = ilev_3
CADJ STORE OBWsStevens    = tapelev3, key = ilev_3
CADJ STORE OBWuStevens    = tapelev3, key = ilev_3
# endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_WEST */
#
# ifdef ALLOW_PTRACERS
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNptr  = tapelev3, key = ilev_3
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNptr0 = tapelev3, key = ilev_3
CADJ STORE OBNptr1 = tapelev3, key = ilev_3
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSptr  = tapelev3, key = ilev_3
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSptr0 = tapelev3, key = ilev_3
CADJ STORE OBSptr1 = tapelev3, key = ilev_3
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEptr  = tapelev3, key = ilev_3
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEptr0 = tapelev3, key = ilev_3
CADJ STORE OBEptr1 = tapelev3, key = ilev_3
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWptr  = tapelev3, key = ilev_3
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWptr0 = tapelev3, key = ilev_3
CADJ STORE OBWptr1 = tapelev3, key = ilev_3
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */
#
# endif  /* ALLOW_PTRACERS */
#
# ifdef ALLOW_SEAICE
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNh   = tapelev3, key = ilev_3
CADJ STORE OBNa   = tapelev3, key = ilev_3
CADJ STORE OBNsn  = tapelev3, key = ilev_3
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBNsl = tapelev3, key = ilev_3
CADJ STORE OBNsl0 = tapelev3, key = ilev_3
CADJ STORE OBNsl1 = tapelev3, key = ilev_3
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNh0  = tapelev3, key = ilev_3
CADJ STORE OBNh1  = tapelev3, key = ilev_3
CADJ STORE OBNa0  = tapelev3, key = ilev_3
CADJ STORE OBNa1  = tapelev3, key = ilev_3
CADJ STORE OBNsn0 = tapelev3, key = ilev_3
CADJ STORE OBNsn1 = tapelev3, key = ilev_3
#ifdef NONLIN_FRSURF
CADJ STORE obneta0,obneta1 = tapelev3, key = ilev_3
#endif
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSh   = tapelev3, key = ilev_3
CADJ STORE OBSa   = tapelev3, key = ilev_3
CADJ STORE OBSsn  = tapelev3, key = ilev_3
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBSsl = tapelev3, key = ilev_3
CADJ STORE OBSsl0 = tapelev3, key = ilev_3
CADJ STORE OBSsl1 = tapelev3, key = ilev_3
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSh0  = tapelev3, key = ilev_3
CADJ STORE OBSh1  = tapelev3, key = ilev_3
CADJ STORE OBSa0  = tapelev3, key = ilev_3
CADJ STORE OBSa1  = tapelev3, key = ilev_3
CADJ STORE OBSsn0 = tapelev3, key = ilev_3
CADJ STORE OBSsn1 = tapelev3, key = ilev_3
#ifdef NONLIN_FRSURF
CADJ STORE obseta0,obseta1 = tapelev3, key = ilev_3
#endif
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEh   = tapelev3, key = ilev_3
CADJ STORE OBEa   = tapelev3, key = ilev_3
CADJ STORE OBEsn  = tapelev3, key = ilev_3
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBEsl = tapelev3, key = ilev_3
CADJ STORE OBEsl0 = tapelev3, key = ilev_3
CADJ STORE OBEsl1 = tapelev3, key = ilev_3
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEh0  = tapelev3, key = ilev_3
CADJ STORE OBEh1  = tapelev3, key = ilev_3
CADJ STORE OBEa0  = tapelev3, key = ilev_3
CADJ STORE OBEa1  = tapelev3, key = ilev_3
CADJ STORE OBEsn0 = tapelev3, key = ilev_3
CADJ STORE OBEsn1 = tapelev3, key = ilev_3
#ifdef NONLIN_FRSURF
CADJ STORE obeeta0,obeeta1 = tapelev3, key = ilev_3
#endif
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWh   = tapelev3, key = ilev_3
CADJ STORE OBWa   = tapelev3, key = ilev_3
CADJ STORE OBWsn  = tapelev3, key = ilev_3
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBWsl = tapelev3, key = ilev_3
CADJ STORE OBWsl0 = tapelev3, key = ilev_3
CADJ STORE OBWsl1 = tapelev3, key = ilev_3
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWh0  = tapelev3, key = ilev_3
CADJ STORE OBWh1  = tapelev3, key = ilev_3
CADJ STORE OBWa0  = tapelev3, key = ilev_3
CADJ STORE OBWa1  = tapelev3, key = ilev_3
CADJ STORE OBWsn0 = tapelev3, key = ilev_3
CADJ STORE OBWsn1 = tapelev3, key = ilev_3
#ifdef NONLIN_FRSURF
CADJ STORE obweta0,obweta1 = tapelev3, key = ilev_3
#endif
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */
#
# endif /* ALLOW_SEAICE */
#
#endif  /* ALLOW_OBCS */
