#ifdef ALLOW_OBCS
#
CADJ STORE shiftvel = tapelev2, key = ilev_2
#
# ifdef ALLOW_OBCS_NORTH
CADJ STORE StoreOBCSN     = tapelev2, key = ilev_2
# endif /* ALLOW_OBCS_NORTH */
#
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE StoreOBCSS     = tapelev2, key = ilev_2
# endif /* ALLOW_OBCS_SOUTH */
#
# ifdef ALLOW_OBCS_EAST
CADJ STORE StoreOBCSE     = tapelev2, key = ilev_2
# endif /* ALLOW_OBCS_EAST */
#
# ifdef ALLOW_OBCS_WEST
CADJ STORE StoreOBCSW     = tapelev2, key = ilev_2
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
