#
# ifdef AUTODIFF_USE_STORE_RESTORE_OBCS
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE StoreOBCSN     = tapelev4, key = ilev_4
#  endif
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE StoreOBCSS     = tapelev4, key = ilev_4
#  endif
#  ifdef ALLOW_OBCS_EAST
CADJ STORE StoreOBCSE     = tapelev4, key = ilev_4
#  endif
#  ifdef ALLOW_OBCS_WEST
CADJ STORE StoreOBCSW     = tapelev4, key = ilev_4
#  endif
# else
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNu           = tapelev4, key = ilev_4
CADJ STORE OBNv           = tapelev4, key = ilev_4
CADJ STORE OBNt           = tapelev4, key = ilev_4
CADJ STORE OBNs           = tapelev4, key = ilev_4
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNu0, OBNu1   = tapelev4, key = ilev_4
CADJ STORE OBNv0, OBNv1   = tapelev4, key = ilev_4
CADJ STORE OBNt0, OBNt1   = tapelev4, key = ilev_4
CADJ STORE OBNs0, OBNs1   = tapelev4, key = ilev_4
#   endif
#   ifdef ALLOW_OBCSN_CONTROL
CADJ STORE xx_obcsn0      = tapelev4, key = ilev_4
CADJ STORE xx_obcsn1      = tapelev4, key = ilev_4
#   endif
#  endif /* ALLOW_OBCS_NORTH */
#
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSu           = tapelev4, key = ilev_4
CADJ STORE OBSv           = tapelev4, key = ilev_4
CADJ STORE OBSt           = tapelev4, key = ilev_4
CADJ STORE OBSs           = tapelev4, key = ilev_4
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSu0, OBSu1   = tapelev4, key = ilev_4
CADJ STORE OBSv0, OBSv1   = tapelev4, key = ilev_4
CADJ STORE OBSt0, OBSt1   = tapelev4, key = ilev_4
CADJ STORE OBSs0, OBSs1   = tapelev4, key = ilev_4
#   endif
#   ifdef ALLOW_OBCSS_CONTROL
CADJ STORE xx_obcss0      = tapelev4, key = ilev_4
CADJ STORE xx_obcss1      = tapelev4, key = ilev_4
#   endif
#  endif /* ALLOW_OBCS_SOUTH */
#
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEu           = tapelev4, key = ilev_4
CADJ STORE OBEv           = tapelev4, key = ilev_4
CADJ STORE OBEt           = tapelev4, key = ilev_4
CADJ STORE OBEs           = tapelev4, key = ilev_4
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEu0, OBEu1   = tapelev4, key = ilev_4
CADJ STORE OBEv0, OBEv1   = tapelev4, key = ilev_4
CADJ STORE OBEt0, OBEt1   = tapelev4, key = ilev_4
CADJ STORE OBEs0, OBEs1   = tapelev4, key = ilev_4
#   endif
#   ifdef ALLOW_OBCSE_CONTROL
CADJ STORE xx_obcse0      = tapelev4, key = ilev_4
CADJ STORE xx_obcse1      = tapelev4, key = ilev_4
#   endif
#  endif /* ALLOW_OBCS_EAST */
#
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWu           = tapelev4, key = ilev_4
CADJ STORE OBWv           = tapelev4, key = ilev_4
CADJ STORE OBWt           = tapelev4, key = ilev_4
CADJ STORE OBWs           = tapelev4, key = ilev_4
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWu0, OBWu1   = tapelev4, key = ilev_4
CADJ STORE OBWv0, OBWv1   = tapelev4, key = ilev_4
CADJ STORE OBWt0, OBWt1   = tapelev4, key = ilev_4
CADJ STORE OBWs0, OBWs1   = tapelev4, key = ilev_4
#   endif
#   ifdef ALLOW_OBCSW_CONTROL
CADJ STORE xx_obcsw0      = tapelev4, key = ilev_4
CADJ STORE xx_obcsw1      = tapelev4, key = ilev_4
#   endif
#  endif /* ALLOW_OBCS_WEST */
# endif /* AUTODIFF_USE_STORE_RESTORE_OBCS */
#
# ifdef ALLOW_OBCS_STEVENS
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNtStevens    = tapelev4, key = ilev_4
CADJ STORE OBNsStevens    = tapelev4, key = ilev_4
CADJ STORE OBNvStevens    = tapelev4, key = ilev_4
#  endif
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBStStevens    = tapelev4, key = ilev_4
CADJ STORE OBSsStevens    = tapelev4, key = ilev_4
CADJ STORE OBSvStevens    = tapelev4, key = ilev_4
#  endif
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEtStevens    = tapelev4, key = ilev_4
CADJ STORE OBEsStevens    = tapelev4, key = ilev_4
CADJ STORE OBEuStevens    = tapelev4, key = ilev_4
#  endif
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWtStevens    = tapelev4, key = ilev_4
CADJ STORE OBWsStevens    = tapelev4, key = ilev_4
CADJ STORE OBWuStevens    = tapelev4, key = ilev_4
#  endif
# endif /* ALLOW_OBCS_STEVENS */
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
CADJ STORE OBNsl  = tapelev4, key = ilev_4
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
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSh   = tapelev4, key = ilev_4
CADJ STORE OBSa   = tapelev4, key = ilev_4
CADJ STORE OBSsn  = tapelev4, key = ilev_4
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBSsl  = tapelev4, key = ilev_4
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
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEh   = tapelev4, key = ilev_4
CADJ STORE OBEa   = tapelev4, key = ilev_4
CADJ STORE OBEsn  = tapelev4, key = ilev_4
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBEsl  = tapelev4, key = ilev_4
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
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWh   = tapelev4, key = ilev_4
CADJ STORE OBWa   = tapelev4, key = ilev_4
CADJ STORE OBWsn  = tapelev4, key = ilev_4
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBWsl  = tapelev4, key = ilev_4
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
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */
#
# endif /* ALLOW_SEAICE */

# if (defined ALLOW_SEAICE || defined ALLOW_SHELFICE)
#  if (defined NONLIN_FRSURF && defined ALLOW_OBCS_PRESCRIBE)
CADJ STORE obneta0,obneta1 = tapelev4, key = ilev_4
CADJ STORE obseta0,obseta1 = tapelev4, key = ilev_4
CADJ STORE obeeta0,obeeta1 = tapelev4, key = ilev_4
CADJ STORE obweta0,obweta1 = tapelev4, key = ilev_4
#  endif
# endif
