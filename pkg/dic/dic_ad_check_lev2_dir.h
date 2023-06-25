#ifdef ALLOW_DIC

C common CARBON_NEEDS
CADJ STORE pH                 = tapelev2, key = ilev_2
CADJ STORE pCO2               = tapelev2, key = ilev_2
CADJ STORE fIce               = tapelev2, key = ilev_2
CADJ STORE silicaSurf         = tapelev2, key = ilev_2
CADJ STORE atmosPCO2          = tapelev2, key = ilev_2

CADJ STORE gsm_s              = tapelev2, key = ilev_2
CADJ STORE co2atmos           = tapelev2, key = ilev_2
CADJ STORE total_atmos_carbon = tapelev2, key = ilev_2

C common dic_load
CADJ STORE dicwind0, dicwind1       = tapelev2, key = ilev_2
CADJ STORE ice0, ice1               = tapelev2, key = ilev_2
CADJ STORE atmosp0, atmosp1         = tapelev2, key = ilev_2
CADJ STORE silicaSurf0, silicaSurf1 = tapelev2, key = ilev_2
#ifdef ALLOW_FE
CADJ STORE feinput0, feinput1       = tapelev2, key = ilev_2
#endif

#ifdef ALLOW_COST
CADJ STORE totcost = tapelev2, key = ilev_2
#endif

#endif /* ALLOW_DIC */
