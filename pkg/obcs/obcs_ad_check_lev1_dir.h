#ifdef ALLOW_OBCS
#
#ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNt    = comlev1, key = ikey_dynamics
CADJ STORE OBNs    = comlev1, key = ikey_dynamics
CADJ STORE obns0,obns1  = comlev1, key = ikey_dynamics
CADJ STORE obnt0,obnt1  = comlev1, key = ikey_dynamics
CADJ STORE obnu0,obnu1  = comlev1, key = ikey_dynamics
CADJ STORE obnv0,obnv1  = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSt    = comlev1, key = ikey_dynamics
CADJ STORE OBSs    = comlev1, key = ikey_dynamics
CADJ STORE obss0,obss1  = comlev1, key = ikey_dynamics
CADJ STORE obst0,obst1  = comlev1, key = ikey_dynamics
CADJ STORE obsu0,obsu1  = comlev1, key = ikey_dynamics
CADJ STORE obsv0,obsv1  = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
CADJ STORE OBEt    = comlev1, key = ikey_dynamics
CADJ STORE OBEs    = comlev1, key = ikey_dynamics
CADJ STORE obes0,obes1  = comlev1, key = ikey_dynamics
CADJ STORE obet0,obet1  = comlev1, key = ikey_dynamics
CADJ STORE obeu0,obeu1  = comlev1, key = ikey_dynamics
CADJ STORE obev0,obev1  = comlev1, key = ikey_dynamics
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
CADJ STORE OBWt    = comlev1, key = ikey_dynamics
CADJ STORE OBWs    = comlev1, key = ikey_dynamics
CADJ STORE obws0,obws1  = comlev1, key = ikey_dynamics
CADJ STORE obwt0,obwt1  = comlev1, key = ikey_dynamics
CADJ STORE obwu0,obwu1  = comlev1, key = ikey_dynamics
CADJ STORE obwv0,obwv1  = comlev1, key = ikey_dynamics
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
