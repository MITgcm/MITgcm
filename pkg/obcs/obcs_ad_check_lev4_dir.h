#ifdef ALLOW_OBCS
#
CADJ STORE shiftvel = tapelev4, key = ilev_4
#
# ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNt     = tapelev4, key = ilev_4
CADJ STORE OBNs     = tapelev4, key = ilev_4
CADJ STORE OBNu0    = tapelev4, key = ilev_4
CADJ STORE OBNv0    = tapelev4, key = ilev_4
CADJ STORE OBNt0    = tapelev4, key = ilev_4
CADJ STORE OBNs0    = tapelev4, key = ilev_4
CADJ STORE OBNu1    = tapelev4, key = ilev_4
CADJ STORE OBNv1    = tapelev4, key = ilev_4
CADJ STORE OBNt1    = tapelev4, key = ilev_4
CADJ STORE OBNs1    = tapelev4, key = ilev_4
#  ifdef ALLOW_OBCSN_CONTROL
CADJ STORE xx_obcsn0      = tapelev4, key = ilev_4
CADJ STORE xx_obcsn1      = tapelev4, key = ilev_4
#  endif
# endif /* ALLOW_OBCS_NORTH */
#
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSt     = tapelev4, key = ilev_4
CADJ STORE OBSs     = tapelev4, key = ilev_4
CADJ STORE OBSu0    = tapelev4, key = ilev_4
CADJ STORE OBSv0    = tapelev4, key = ilev_4
CADJ STORE OBSt0    = tapelev4, key = ilev_4
CADJ STORE OBSs0    = tapelev4, key = ilev_4
CADJ STORE OBSu1    = tapelev4, key = ilev_4
CADJ STORE OBSv1    = tapelev4, key = ilev_4
CADJ STORE OBSt1    = tapelev4, key = ilev_4
CADJ STORE OBSs1    = tapelev4, key = ilev_4
#  ifdef ALLOW_OBCSS_CONTROL
CADJ STORE xx_obcss0      = tapelev4, key = ilev_4
CADJ STORE xx_obcss1      = tapelev4, key = ilev_4
#  endif
# endif /* ALLOW_OBCS_SOUTH */
#
# ifdef ALLOW_OBCS_EAST
CADJ STORE OBEt     = tapelev4, key = ilev_4
CADJ STORE OBEs     = tapelev4, key = ilev_4
CADJ STORE OBEu0    = tapelev4, key = ilev_4
CADJ STORE OBEv0    = tapelev4, key = ilev_4
CADJ STORE OBEt0    = tapelev4, key = ilev_4
CADJ STORE OBEs0    = tapelev4, key = ilev_4
CADJ STORE OBEu1    = tapelev4, key = ilev_4
CADJ STORE OBEv1    = tapelev4, key = ilev_4
CADJ STORE OBEt1    = tapelev4, key = ilev_4
CADJ STORE OBEs1    = tapelev4, key = ilev_4
#  ifdef ALLOW_OBCSE_CONTROL
CADJ STORE xx_obcse0      = tapelev4, key = ilev_4
CADJ STORE xx_obcse1      = tapelev4, key = ilev_4
#  endif
# endif /* ALLOW_OBCS_EAST */
#
# ifdef ALLOW_OBCS_WEST
CADJ STORE OBWt     = tapelev4, key = ilev_4
CADJ STORE OBWs     = tapelev4, key = ilev_4
CADJ STORE OBWu0    = tapelev4, key = ilev_4
CADJ STORE OBWv0    = tapelev4, key = ilev_4
CADJ STORE OBWt0    = tapelev4, key = ilev_4
CADJ STORE OBWs0    = tapelev4, key = ilev_4
CADJ STORE OBWu1    = tapelev4, key = ilev_4
CADJ STORE OBWv1    = tapelev4, key = ilev_4
CADJ STORE OBWt1    = tapelev4, key = ilev_4
CADJ STORE OBWs1    = tapelev4, key = ilev_4
#  ifdef ALLOW_OBCSW_CONTROL
CADJ STORE xx_obcsw0      = tapelev4, key = ilev_4
CADJ STORE xx_obcsw1      = tapelev4, key = ilev_4
#  endif
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
#endif  /* ALLOW_OBCS */
