#ifdef ALLOW_DIC

C common CARBON_NEEDS
CADJ STORE pH                 = comlev1, key = ikey_dynamics
CADJ STORE pCO2               = comlev1, key = ikey_dynamics
CADJ STORE fIce               = comlev1, key = ikey_dynamics
CADJ STORE silicaSurf         = comlev1, key = ikey_dynamics
CADJ STORE atmosPCO2          = comlev1, key = ikey_dynamics

#ifdef DIC_BIOTIC
CADJ STORE omegaC             = comlev1, key = ikey_dynamics
#endif

CADJ STORE gsm_s              = comlev1, key = ikey_dynamics
CADJ STORE co2atmos           = comlev1, key = ikey_dynamics
CADJ STORE total_atmos_carbon = comlev1, key = ikey_dynamics

C common dic_load
CADJ STORE dicwind0, dicwind1       = comlev1, key = ikey_dynamics
CADJ STORE ice0, ice1               = comlev1, key = ikey_dynamics
CADJ STORE atmosp0, atmosp1         = comlev1, key = ikey_dynamics
CADJ STORE silicaSurf0, silicaSurf1 = comlev1, key = ikey_dynamics
#ifdef ALLOW_FE
CADJ STORE feinput0, feinput1       = comlev1, key = ikey_dynamics
#endif

#ifdef ALLOW_COST
CADJ STORE totcost = comlev1, key = ikey_dynamics
#endif

#endif /* ALLOW_DIC */
