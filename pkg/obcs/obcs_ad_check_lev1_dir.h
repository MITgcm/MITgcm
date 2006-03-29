#ifdef ALLOW_OBCS
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNt    = comlev1, key = ikey_dynamics
CADJ STORE OBNs    = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSt    = comlev1, key = ikey_dynamics
CADJ STORE OBSs    = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEt    = comlev1, key = ikey_dynamics
CADJ STORE OBEs    = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWt    = comlev1, key = ikey_dynamics
CADJ STORE OBWs    = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_WEST */
#
# ifdef ALLOW_PTRACERS
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNptr  = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSptr  = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEptr  = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWptr  = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_WEST */
#
# endif  /* ALLOW_PTRACERS */
#
#endif  /* ALLOW_OBCS */
