#ifdef ALLOW_OBCS
#
CADJ STORE shiftvel = tapelev2, key = ilev_2
#
# ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNt     = tapelev2, key = ilev_2
CADJ STORE OBNs     = tapelev2, key = ilev_2
CADJ STORE OBNu0    = tapelev2, key = ilev_2
CADJ STORE OBNv0    = tapelev2, key = ilev_2
CADJ STORE OBNt0    = tapelev2, key = ilev_2
CADJ STORE OBNs0    = tapelev2, key = ilev_2
CADJ STORE OBNu1    = tapelev2, key = ilev_2
CADJ STORE OBNv1    = tapelev2, key = ilev_2
CADJ STORE OBNt1    = tapelev2, key = ilev_2
CADJ STORE OBNs1    = tapelev2, key = ilev_2
#  ifdef ALLOW_OBCSN_CONTROL
CADJ STORE xx_obcsn0      = tapelev2, key = ilev_2
CADJ STORE xx_obcsn1      = tapelev2, key = ilev_2
#  endif
# endif /* ALLOW_OBCS_NORTH */
#
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSt     = tapelev2, key = ilev_2
CADJ STORE OBSs     = tapelev2, key = ilev_2
CADJ STORE OBSu0    = tapelev2, key = ilev_2
CADJ STORE OBSv0    = tapelev2, key = ilev_2
CADJ STORE OBSt0    = tapelev2, key = ilev_2
CADJ STORE OBSs0    = tapelev2, key = ilev_2
CADJ STORE OBSu1    = tapelev2, key = ilev_2
CADJ STORE OBSv1    = tapelev2, key = ilev_2
CADJ STORE OBSt1    = tapelev2, key = ilev_2
CADJ STORE OBSs1    = tapelev2, key = ilev_2
#  ifdef ALLOW_OBCSS_CONTROL
CADJ STORE xx_obcss0      = tapelev2, key = ilev_2
CADJ STORE xx_obcss1      = tapelev2, key = ilev_2
#  endif
# endif /* ALLOW_OBCS_SOUTH */
#
# ifdef ALLOW_OBCS_EAST
CADJ STORE OBEt     = tapelev2, key = ilev_2
CADJ STORE OBEs     = tapelev2, key = ilev_2
CADJ STORE OBEu0    = tapelev2, key = ilev_2
CADJ STORE OBEv0    = tapelev2, key = ilev_2
CADJ STORE OBEt0    = tapelev2, key = ilev_2
CADJ STORE OBEs0    = tapelev2, key = ilev_2
CADJ STORE OBEu1    = tapelev2, key = ilev_2
CADJ STORE OBEv1    = tapelev2, key = ilev_2
CADJ STORE OBEt1    = tapelev2, key = ilev_2
CADJ STORE OBEs1    = tapelev2, key = ilev_2
#  ifdef ALLOW_OBCSE_CONTROL
CADJ STORE xx_obcse0      = tapelev2, key = ilev_2
CADJ STORE xx_obcse1      = tapelev2, key = ilev_2
#  endif
# endif /* ALLOW_OBCS_EAST */
#
# ifdef ALLOW_OBCS_WEST
CADJ STORE OBWt     = tapelev2, key = ilev_2
CADJ STORE OBWs     = tapelev2, key = ilev_2
CADJ STORE OBWu0    = tapelev2, key = ilev_2
CADJ STORE OBWv0    = tapelev2, key = ilev_2
CADJ STORE OBWt0    = tapelev2, key = ilev_2
CADJ STORE OBWs0    = tapelev2, key = ilev_2
CADJ STORE OBWu1    = tapelev2, key = ilev_2
CADJ STORE OBWv1    = tapelev2, key = ilev_2
CADJ STORE OBWt1    = tapelev2, key = ilev_2
CADJ STORE OBWs1    = tapelev2, key = ilev_2
#  ifdef ALLOW_OBCSW_CONTROL
CADJ STORE xx_obcsw0      = tapelev2, key = ilev_2
CADJ STORE xx_obcsw1      = tapelev2, key = ilev_2
#  endif
# endif /* ALLOW_OBCS_WEST */
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
#endif  /* ALLOW_OBCS */
