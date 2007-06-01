#ifdef ALLOW_OBCS
#
CADJ STORE shiftvel = tapelev4, key = ilev_4
#
# ifdef ALLOW_OBCS_NORTH
CADJ STORE StoreOBCSN     = tapelev4, key = ilev_4
# endif /* ALLOW_OBCS_NORTH */
#
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE StoreOBCSS     = tapelev4, key = ilev_4
# endif /* ALLOW_OBCS_SOUTH */
#
# ifdef ALLOW_OBCS_EAST
CADJ STORE StoreOBCSE     = tapelev4, key = ilev_4
# endif /* ALLOW_OBCS_EAST */
#
# ifdef ALLOW_OBCS_WEST
CADJ STORE StoreOBCSW     = tapelev4, key = ilev_4
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
