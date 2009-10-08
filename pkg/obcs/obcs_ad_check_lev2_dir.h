C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_ad_check_lev2_dir.h,v 1.5 2009/10/08 09:39:31 mlosch Exp $
C $Name:  $

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
# ifdef ALLOW_SEAICE
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNh   = tapelev2, key = ilev_2
CADJ STORE OBNa   = tapelev2, key = ilev_2
CADJ STORE OBNsn  = tapelev2, key = ilev_2
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
#
#endif  /* ALLOW_OBCS */
