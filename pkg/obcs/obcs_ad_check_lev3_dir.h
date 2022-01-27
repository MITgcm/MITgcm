#
# ifdef AUTODIFF_USE_STORE_RESTORE_OBCS
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE StoreOBCSN     = tapelev3, key = ilev_3
#  endif
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE StoreOBCSS     = tapelev3, key = ilev_3
#  endif
#  ifdef ALLOW_OBCS_EAST
CADJ STORE StoreOBCSE     = tapelev3, key = ilev_3
#  endif
#  ifdef ALLOW_OBCS_WEST
CADJ STORE StoreOBCSW     = tapelev3, key = ilev_3
#  endif
# else
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNu           = tapelev3, key = ilev_3
CADJ STORE OBNv           = tapelev3, key = ilev_3
CADJ STORE OBNt           = tapelev3, key = ilev_3
CADJ STORE OBNs           = tapelev3, key = ilev_3
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNu0, OBNu1   = tapelev3, key = ilev_3
CADJ STORE OBNv0, OBNv1   = tapelev3, key = ilev_3
CADJ STORE OBNt0, OBNt1   = tapelev3, key = ilev_3
CADJ STORE OBNs0, OBNs1   = tapelev3, key = ilev_3
#   endif
#   ifdef ALLOW_OBCSN_CONTROL
CADJ STORE xx_obcsn0      = tapelev3, key = ilev_3
CADJ STORE xx_obcsn1      = tapelev3, key = ilev_3
#   endif
#  endif /* ALLOW_OBCS_NORTH */
#
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSu           = tapelev3, key = ilev_3
CADJ STORE OBSv           = tapelev3, key = ilev_3
CADJ STORE OBSt           = tapelev3, key = ilev_3
CADJ STORE OBSs           = tapelev3, key = ilev_3
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSu0, OBSu1   = tapelev3, key = ilev_3
CADJ STORE OBSv0, OBSv1   = tapelev3, key = ilev_3
CADJ STORE OBSt0, OBSt1   = tapelev3, key = ilev_3
CADJ STORE OBSs0, OBSs1   = tapelev3, key = ilev_3
#   endif
#   ifdef ALLOW_OBCSS_CONTROL
CADJ STORE xx_obcss0      = tapelev3, key = ilev_3
CADJ STORE xx_obcss1      = tapelev3, key = ilev_3
#   endif
#  endif /* ALLOW_OBCS_SOUTH */
#
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEu           = tapelev3, key = ilev_3
CADJ STORE OBEv           = tapelev3, key = ilev_3
CADJ STORE OBEt           = tapelev3, key = ilev_3
CADJ STORE OBEs           = tapelev3, key = ilev_3
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEu0, OBEu1   = tapelev3, key = ilev_3
CADJ STORE OBEv0, OBEv1   = tapelev3, key = ilev_3
CADJ STORE OBEt0, OBEt1   = tapelev3, key = ilev_3
CADJ STORE OBEs0, OBEs1   = tapelev3, key = ilev_3
#   endif
#   ifdef ALLOW_OBCSE_CONTROL
CADJ STORE xx_obcse0      = tapelev3, key = ilev_3
CADJ STORE xx_obcse1      = tapelev3, key = ilev_3
#   endif
#  endif /* ALLOW_OBCS_EAST */
#
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWu           = tapelev3, key = ilev_3
CADJ STORE OBWv           = tapelev3, key = ilev_3
CADJ STORE OBWt           = tapelev3, key = ilev_3
CADJ STORE OBWs           = tapelev3, key = ilev_3
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWu0, OBWu1   = tapelev3, key = ilev_3
CADJ STORE OBWv0, OBWv1   = tapelev3, key = ilev_3
CADJ STORE OBWt0, OBWt1   = tapelev3, key = ilev_3
CADJ STORE OBWs0, OBWs1   = tapelev3, key = ilev_3
#   endif
#   ifdef ALLOW_OBCSW_CONTROL
CADJ STORE xx_obcsw0      = tapelev3, key = ilev_3
CADJ STORE xx_obcsw1      = tapelev3, key = ilev_3
#   endif
#  endif /* ALLOW_OBCS_WEST */
# endif /* AUTODIFF_USE_STORE_RESTORE_OBCS */
#
# ifdef ALLOW_OBCS_STEVENS
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNtStevens    = tapelev3, key = ilev_3
CADJ STORE OBNsStevens    = tapelev3, key = ilev_3
CADJ STORE OBNvStevens    = tapelev3, key = ilev_3
#  endif
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBStStevens    = tapelev3, key = ilev_3
CADJ STORE OBSsStevens    = tapelev3, key = ilev_3
CADJ STORE OBSvStevens    = tapelev3, key = ilev_3
#  endif
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEtStevens    = tapelev3, key = ilev_3
CADJ STORE OBEsStevens    = tapelev3, key = ilev_3
CADJ STORE OBEuStevens    = tapelev3, key = ilev_3
#  endif
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWtStevens    = tapelev3, key = ilev_3
CADJ STORE OBWsStevens    = tapelev3, key = ilev_3
CADJ STORE OBWuStevens    = tapelev3, key = ilev_3
#  endif
# endif /* ALLOW_OBCS_STEVENS */
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
CADJ STORE OBNsl  = tapelev3, key = ilev_3
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
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSh   = tapelev3, key = ilev_3
CADJ STORE OBSa   = tapelev3, key = ilev_3
CADJ STORE OBSsn  = tapelev3, key = ilev_3
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBSsl  = tapelev3, key = ilev_3
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
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEh   = tapelev3, key = ilev_3
CADJ STORE OBEa   = tapelev3, key = ilev_3
CADJ STORE OBEsn  = tapelev3, key = ilev_3
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBEsl  = tapelev3, key = ilev_3
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
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWh   = tapelev3, key = ilev_3
CADJ STORE OBWa   = tapelev3, key = ilev_3
CADJ STORE OBWsn  = tapelev3, key = ilev_3
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBWsl  = tapelev3, key = ilev_3
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
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */
#
# endif /* ALLOW_SEAICE */

# if (defined ALLOW_SEAICE || defined ALLOW_SHELFICE)
#  if (defined NONLIN_FRSURF && defined ALLOW_OBCS_PRESCRIBE)
CADJ STORE obneta0,obneta1 = tapelev3, key = ilev_3
CADJ STORE obseta0,obseta1 = tapelev3, key = ilev_3
CADJ STORE obeeta0,obeeta1 = tapelev3, key = ilev_3
CADJ STORE obweta0,obweta1 = tapelev3, key = ilev_3
#  endif
# endif
