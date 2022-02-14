#
# ifdef AUTODIFF_USE_STORE_RESTORE_OBCS
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE StoreOBCSN     = tapelev2, key = ilev_2
#  endif
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE StoreOBCSS     = tapelev2, key = ilev_2
#  endif
#  ifdef ALLOW_OBCS_EAST
CADJ STORE StoreOBCSE     = tapelev2, key = ilev_2
#  endif
#  ifdef ALLOW_OBCS_WEST
CADJ STORE StoreOBCSW     = tapelev2, key = ilev_2
#  endif
# else
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNu           = tapelev2, key = ilev_2
CADJ STORE OBNv           = tapelev2, key = ilev_2
CADJ STORE OBNt           = tapelev2, key = ilev_2
CADJ STORE OBNs           = tapelev2, key = ilev_2
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNu0, OBNu1   = tapelev2, key = ilev_2
CADJ STORE OBNv0, OBNv1   = tapelev2, key = ilev_2
CADJ STORE OBNt0, OBNt1   = tapelev2, key = ilev_2
CADJ STORE OBNs0, OBNs1   = tapelev2, key = ilev_2
#   endif
#   ifdef ALLOW_OBCSN_CONTROL
CADJ STORE xx_obcsn0      = tapelev2, key = ilev_2
CADJ STORE xx_obcsn1      = tapelev2, key = ilev_2
#   endif
#  endif /* ALLOW_OBCS_NORTH */
#
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSu           = tapelev2, key = ilev_2
CADJ STORE OBSv           = tapelev2, key = ilev_2
CADJ STORE OBSt           = tapelev2, key = ilev_2
CADJ STORE OBSs           = tapelev2, key = ilev_2
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSu0, OBSu1   = tapelev2, key = ilev_2
CADJ STORE OBSv0, OBSv1   = tapelev2, key = ilev_2
CADJ STORE OBSt0, OBSt1   = tapelev2, key = ilev_2
CADJ STORE OBSs0, OBSs1   = tapelev2, key = ilev_2
#   endif
#   ifdef ALLOW_OBCSS_CONTROL
CADJ STORE xx_obcss0      = tapelev2, key = ilev_2
CADJ STORE xx_obcss1      = tapelev2, key = ilev_2
#   endif
#  endif /* ALLOW_OBCS_SOUTH */
#
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEu           = tapelev2, key = ilev_2
CADJ STORE OBEv           = tapelev2, key = ilev_2
CADJ STORE OBEt           = tapelev2, key = ilev_2
CADJ STORE OBEs           = tapelev2, key = ilev_2
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEu0, OBEu1   = tapelev2, key = ilev_2
CADJ STORE OBEv0, OBEv1   = tapelev2, key = ilev_2
CADJ STORE OBEt0, OBEt1   = tapelev2, key = ilev_2
CADJ STORE OBEs0, OBEs1   = tapelev2, key = ilev_2
#   endif
#   ifdef ALLOW_OBCSE_CONTROL
CADJ STORE xx_obcse0      = tapelev2, key = ilev_2
CADJ STORE xx_obcse1      = tapelev2, key = ilev_2
#   endif
#  endif /* ALLOW_OBCS_EAST */
#
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWu           = tapelev2, key = ilev_2
CADJ STORE OBWv           = tapelev2, key = ilev_2
CADJ STORE OBWt           = tapelev2, key = ilev_2
CADJ STORE OBWs           = tapelev2, key = ilev_2
#   ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWu0, OBWu1   = tapelev2, key = ilev_2
CADJ STORE OBWv0, OBWv1   = tapelev2, key = ilev_2
CADJ STORE OBWt0, OBWt1   = tapelev2, key = ilev_2
CADJ STORE OBWs0, OBWs1   = tapelev2, key = ilev_2
#   endif
#   ifdef ALLOW_OBCSW_CONTROL
CADJ STORE xx_obcsw0      = tapelev2, key = ilev_2
CADJ STORE xx_obcsw1      = tapelev2, key = ilev_2
#   endif
#  endif /* ALLOW_OBCS_WEST */
# endif /* AUTODIFF_USE_STORE_RESTORE_OBCS */
#
# ifdef ALLOW_OBCS_STEVENS
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNtStevens    = tapelev2, key = ilev_2
CADJ STORE OBNsStevens    = tapelev2, key = ilev_2
CADJ STORE OBNvStevens    = tapelev2, key = ilev_2
#  endif
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBStStevens    = tapelev2, key = ilev_2
CADJ STORE OBSsStevens    = tapelev2, key = ilev_2
CADJ STORE OBSvStevens    = tapelev2, key = ilev_2
#  endif
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEtStevens    = tapelev2, key = ilev_2
CADJ STORE OBEsStevens    = tapelev2, key = ilev_2
CADJ STORE OBEuStevens    = tapelev2, key = ilev_2
#  endif
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWtStevens    = tapelev2, key = ilev_2
CADJ STORE OBWsStevens    = tapelev2, key = ilev_2
CADJ STORE OBWuStevens    = tapelev2, key = ilev_2
#  endif
# endif /* ALLOW_OBCS_STEVENS */
#
# ifdef ALLOW_PTRACERS
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNptr  = tapelev2, key = ilev_2
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNptr0 = tapelev2, key = ilev_2
CADJ STORE OBNptr1 = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSptr  = tapelev2, key = ilev_2
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSptr0 = tapelev2, key = ilev_2
CADJ STORE OBSptr1 = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEptr  = tapelev2, key = ilev_2
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEptr0 = tapelev2, key = ilev_2
CADJ STORE OBEptr1 = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWptr  = tapelev2, key = ilev_2
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWptr0 = tapelev2, key = ilev_2
CADJ STORE OBWptr1 = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */
#
# endif  /* ALLOW_PTRACERS */
#
# ifdef ALLOW_SEAICE
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNh   = tapelev2, key = ilev_2
CADJ STORE OBNa   = tapelev2, key = ilev_2
CADJ STORE OBNsn  = tapelev2, key = ilev_2
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBNsl  = tapelev2, key = ilev_2
CADJ STORE OBNsl0 = tapelev2, key = ilev_2
CADJ STORE OBNsl1 = tapelev2, key = ilev_2
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBNh0  = tapelev2, key = ilev_2
CADJ STORE OBNh1  = tapelev2, key = ilev_2
CADJ STORE OBNa0  = tapelev2, key = ilev_2
CADJ STORE OBNa1  = tapelev2, key = ilev_2
CADJ STORE OBNsn0 = tapelev2, key = ilev_2
CADJ STORE OBNsn1 = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSh   = tapelev2, key = ilev_2
CADJ STORE OBSa   = tapelev2, key = ilev_2
CADJ STORE OBSsn  = tapelev2, key = ilev_2
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBSsl = tapelev2, key = ilev_2
CADJ STORE OBSsl0 = tapelev2, key = ilev_2
CADJ STORE OBSsl1 = tapelev2, key = ilev_2
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBSh0  = tapelev2, key = ilev_2
CADJ STORE OBSh1  = tapelev2, key = ilev_2
CADJ STORE OBSa0  = tapelev2, key = ilev_2
CADJ STORE OBSa1  = tapelev2, key = ilev_2
CADJ STORE OBSsn0 = tapelev2, key = ilev_2
CADJ STORE OBSsn1 = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEh   = tapelev2, key = ilev_2
CADJ STORE OBEa   = tapelev2, key = ilev_2
CADJ STORE OBEsn  = tapelev2, key = ilev_2
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBEsl  = tapelev2, key = ilev_2
CADJ STORE OBEsl0 = tapelev2, key = ilev_2
CADJ STORE OBEsl1 = tapelev2, key = ilev_2
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBEh0  = tapelev2, key = ilev_2
CADJ STORE OBEh1  = tapelev2, key = ilev_2
CADJ STORE OBEa0  = tapelev2, key = ilev_2
CADJ STORE OBEa1  = tapelev2, key = ilev_2
CADJ STORE OBEsn0 = tapelev2, key = ilev_2
CADJ STORE OBEsn1 = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWh   = tapelev2, key = ilev_2
CADJ STORE OBWa   = tapelev2, key = ilev_2
CADJ STORE OBWsn  = tapelev2, key = ilev_2
#ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBWsl  = tapelev2, key = ilev_2
CADJ STORE OBWsl0 = tapelev2, key = ilev_2
CADJ STORE OBWsl1 = tapelev2, key = ilev_2
#endif
#ifdef ALLOW_OBCS_PRESCRIBE
CADJ STORE OBWh0  = tapelev2, key = ilev_2
CADJ STORE OBWh1  = tapelev2, key = ilev_2
CADJ STORE OBWa0  = tapelev2, key = ilev_2
CADJ STORE OBWa1  = tapelev2, key = ilev_2
CADJ STORE OBWsn0 = tapelev2, key = ilev_2
CADJ STORE OBWsn1 = tapelev2, key = ilev_2
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */
#
# endif /* ALLOW_SEAICE */

# if (defined ALLOW_SEAICE || defined ALLOW_SHELFICE)
#  if (defined NONLIN_FRSURF && defined ALLOW_OBCS_PRESCRIBE)
CADJ STORE obneta0,obneta1 = tapelev2, key = ilev_2
CADJ STORE obseta0,obseta1 = tapelev2, key = ilev_2
CADJ STORE obeeta0,obeeta1 = tapelev2, key = ilev_2
CADJ STORE obweta0,obweta1 = tapelev2, key = ilev_2
#  endif
# endif
