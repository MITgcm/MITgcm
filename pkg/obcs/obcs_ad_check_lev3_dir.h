#ifdef ALLOW_OBCS
#
CADJ STORE shiftvel = tapelev3, key = ilev_3
#
# ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNt     = tapelev3, key = ilev_3
CADJ STORE OBNs     = tapelev3, key = ilev_3
CADJ STORE OBNu0    = tapelev3, key = ilev_3
CADJ STORE OBNv0    = tapelev3, key = ilev_3
CADJ STORE OBNt0    = tapelev3, key = ilev_3
CADJ STORE OBNs0    = tapelev3, key = ilev_3
CADJ STORE OBNu1    = tapelev3, key = ilev_3
CADJ STORE OBNv1    = tapelev3, key = ilev_3
CADJ STORE OBNt1    = tapelev3, key = ilev_3
CADJ STORE OBNs1    = tapelev3, key = ilev_3
#  ifdef ALLOW_OBCSN_CONTROL
CADJ STORE xx_obcsn0      = tapelev3, key = ilev_3
CADJ STORE xx_obcsn1      = tapelev3, key = ilev_3
#  endif
# endif /* ALLOW_OBCS_NORTH */
#
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSt     = tapelev3, key = ilev_3
CADJ STORE OBSs     = tapelev3, key = ilev_3
CADJ STORE OBSu0    = tapelev3, key = ilev_3
CADJ STORE OBSv0    = tapelev3, key = ilev_3
CADJ STORE OBSt0    = tapelev3, key = ilev_3
CADJ STORE OBSs0    = tapelev3, key = ilev_3
CADJ STORE OBSu1    = tapelev3, key = ilev_3
CADJ STORE OBSv1    = tapelev3, key = ilev_3
CADJ STORE OBSt1    = tapelev3, key = ilev_3
CADJ STORE OBSs1    = tapelev3, key = ilev_3
#  ifdef ALLOW_OBCSS_CONTROL
CADJ STORE xx_obcss0      = tapelev3, key = ilev_3
CADJ STORE xx_obcss1      = tapelev3, key = ilev_3
#  endif
# endif /* ALLOW_OBCS_SOUTH */
#
# ifdef ALLOW_OBCS_EAST
CADJ STORE OBEt     = tapelev3, key = ilev_3
CADJ STORE OBEs     = tapelev3, key = ilev_3
CADJ STORE OBEu0    = tapelev3, key = ilev_3
CADJ STORE OBEv0    = tapelev3, key = ilev_3
CADJ STORE OBEt0    = tapelev3, key = ilev_3
CADJ STORE OBEs0    = tapelev3, key = ilev_3
CADJ STORE OBEu1    = tapelev3, key = ilev_3
CADJ STORE OBEv1    = tapelev3, key = ilev_3
CADJ STORE OBEt1    = tapelev3, key = ilev_3
CADJ STORE OBEs1    = tapelev3, key = ilev_3
#  ifdef ALLOW_OBCSE_CONTROL
CADJ STORE xx_obcse0      = tapelev3, key = ilev_3
CADJ STORE xx_obcse1      = tapelev3, key = ilev_3
#  endif
# endif /* ALLOW_OBCS_EAST */
#
# ifdef ALLOW_OBCS_WEST
CADJ STORE OBWt     = tapelev3, key = ilev_3
CADJ STORE OBWs     = tapelev3, key = ilev_3
CADJ STORE OBWu0    = tapelev3, key = ilev_3
CADJ STORE OBWv0    = tapelev3, key = ilev_3
CADJ STORE OBWt0    = tapelev3, key = ilev_3
CADJ STORE OBWs0    = tapelev3, key = ilev_3
CADJ STORE OBWu1    = tapelev3, key = ilev_3
CADJ STORE OBWv1    = tapelev3, key = ilev_3
CADJ STORE OBWt1    = tapelev3, key = ilev_3
CADJ STORE OBWs1    = tapelev3, key = ilev_3
#  ifdef ALLOW_OBCSW_CONTROL
CADJ STORE xx_obcsw0      = tapelev3, key = ilev_3
CADJ STORE xx_obcsw1      = tapelev3, key = ilev_3
#  endif
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
#endif  /* ALLOW_OBCS */
