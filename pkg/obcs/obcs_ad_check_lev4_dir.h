C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_ad_check_lev4_dir.h,v 1.9 2012/10/23 20:15:32 heimbach Exp $
C $Name:  $

#ifdef ALLOW_OBCS
#
# ifdef ALLOW_OBCS_NORTH
CADJ STORE StoreOBCSN     = tapelev4, key = ilev_4
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBNtStevens    = tapelev4, key = ilev_4
CADJ STORE OBNsStevens    = tapelev4, key = ilev_4
CADJ STORE OBNvStevens    = tapelev4, key = ilev_4
# endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_NORTH */
#
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE StoreOBCSS     = tapelev4, key = ilev_4
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBStStevens    = tapelev4, key = ilev_4
CADJ STORE OBSsStevens    = tapelev4, key = ilev_4
CADJ STORE OBSvStevens    = tapelev4, key = ilev_4
# endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_SOUTH */
#
# ifdef ALLOW_OBCS_EAST
CADJ STORE StoreOBCSE     = tapelev4, key = ilev_4
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBEtStevens    = tapelev4, key = ilev_4
CADJ STORE OBEsStevens    = tapelev4, key = ilev_4
CADJ STORE OBEuStevens    = tapelev4, key = ilev_4
# endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_EAST */
#
# ifdef ALLOW_OBCS_WEST
CADJ STORE StoreOBCSW     = tapelev4, key = ilev_4
# ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBWtStevens    = tapelev4, key = ilev_4
CADJ STORE OBWsStevens    = tapelev4, key = ilev_4
CADJ STORE OBWsStevens    = tapelev4, key = ilev_4
# endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_WEST */
#
# ifdef ALLOW_PTRACERS
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNptr  = tapelev4, key = ilev_4
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNptr0 = tapelev4, key = ilev_4
CADJ STORE OBNptr1 = tapelev4, key = ilev_4
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSptr  = tapelev4, key = ilev_4
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSptr0 = tapelev4, key = ilev_4
CADJ STORE OBSptr1 = tapelev4, key = ilev_4
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEptr  = tapelev4, key = ilev_4
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEptr0 = tapelev4, key = ilev_4
CADJ STORE OBEptr1 = tapelev4, key = ilev_4
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWptr  = tapelev4, key = ilev_4
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWptr0 = tapelev4, key = ilev_4
CADJ STORE OBWptr1 = tapelev4, key = ilev_4
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */
#
# endif  /* ALLOW_PTRACERS */
#
# ifdef ALLOW_SEAICE
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNh   = tapelev4, key = ilev_4
CADJ STORE OBNa   = tapelev4, key = ilev_4
CADJ STORE OBNsn  = tapelev4, key = ilev_4
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBNsl = tapelev4, key = ilev_4
CADJ STORE OBNsl0 = tapelev4, key = ilev_4
CADJ STORE OBNsl1 = tapelev4, key = ilev_4
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNh0  = tapelev4, key = ilev_4
CADJ STORE OBNh1  = tapelev4, key = ilev_4
CADJ STORE OBNa0  = tapelev4, key = ilev_4
CADJ STORE OBNa1  = tapelev4, key = ilev_4
CADJ STORE OBNsn0 = tapelev4, key = ilev_4
CADJ STORE OBNsn1 = tapelev4, key = ilev_4
#ifdef NONLIN_FRSURF
CADJ STORE obneta0,obneta1 = tapelev4, key = ilev_4
#endif
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSh   = tapelev4, key = ilev_4
CADJ STORE OBSa   = tapelev4, key = ilev_4
CADJ STORE OBSsn  = tapelev4, key = ilev_4
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBSsl = tapelev4, key = ilev_4
CADJ STORE OBSsl0 = tapelev4, key = ilev_4
CADJ STORE OBSsl1 = tapelev4, key = ilev_4
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSh0  = tapelev4, key = ilev_4
CADJ STORE OBSh1  = tapelev4, key = ilev_4
CADJ STORE OBSa0  = tapelev4, key = ilev_4
CADJ STORE OBSa1  = tapelev4, key = ilev_4
CADJ STORE OBSsn0 = tapelev4, key = ilev_4
CADJ STORE OBSsn1 = tapelev4, key = ilev_4
#ifdef NONLIN_FRSURF
CADJ STORE obseta0,obseta1 = tapelev4, key = ilev_4
#endif
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEh   = tapelev4, key = ilev_4
CADJ STORE OBEa   = tapelev4, key = ilev_4
CADJ STORE OBEsn  = tapelev4, key = ilev_4
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBEsl = tapelev4, key = ilev_4
CADJ STORE OBEsl0 = tapelev4, key = ilev_4
CADJ STORE OBEsl1 = tapelev4, key = ilev_4
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEh0  = tapelev4, key = ilev_4
CADJ STORE OBEh1  = tapelev4, key = ilev_4
CADJ STORE OBEa0  = tapelev4, key = ilev_4
CADJ STORE OBEa1  = tapelev4, key = ilev_4
CADJ STORE OBEsn0 = tapelev4, key = ilev_4
CADJ STORE OBEsn1 = tapelev4, key = ilev_4
#ifdef NONLIN_FRSURF
CADJ STORE obeeta0,obeeta1 = tapelev4, key = ilev_4
#endif
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWh   = tapelev4, key = ilev_4
CADJ STORE OBWa   = tapelev4, key = ilev_4
CADJ STORE OBWsn  = tapelev4, key = ilev_4
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBWsl = tapelev4, key = ilev_4
CADJ STORE OBWsl0 = tapelev4, key = ilev_4
CADJ STORE OBWsl1 = tapelev4, key = ilev_4
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWh0  = tapelev4, key = ilev_4
CADJ STORE OBWh1  = tapelev4, key = ilev_4
CADJ STORE OBWa0  = tapelev4, key = ilev_4
CADJ STORE OBWa1  = tapelev4, key = ilev_4
CADJ STORE OBWsn0 = tapelev4, key = ilev_4
CADJ STORE OBWsn1 = tapelev4, key = ilev_4
#ifdef NONLIN_FRSURF
CADJ STORE obweta0,obweta1 = tapelev4, key = ilev_4
#endif
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */
#
# endif /* ALLOW_SEAICE */
#
#endif  /* ALLOW_OBCS */
