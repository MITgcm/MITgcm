#ifdef ALLOW_OBCS
#
CADJ STORE shiftvel = tapelev3, key = ilev_3
#
# ifdef ALLOW_OBCS_NORTH
CADJ STORE StoreOBCSN     = tapelev3, key = ilev_3
# endif /* ALLOW_OBCS_NORTH */
#
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE StoreOBCSS     = tapelev3, key = ilev_3
# endif /* ALLOW_OBCS_SOUTH */
#
# ifdef ALLOW_OBCS_EAST
CADJ STORE StoreOBCSE     = tapelev3, key = ilev_3
# endif /* ALLOW_OBCS_EAST */
#
# ifdef ALLOW_OBCS_WEST
CADJ STORE StoreOBCSW     = tapelev3, key = ilev_3
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
