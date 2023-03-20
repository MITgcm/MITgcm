#ifdef ALLOW_DIC

C common CARBON_NEEDS
CADJ STORE pH                 = tapelev3, key = ilev_3
CADJ STORE pCO2               = tapelev3, key = ilev_3
CADJ STORE fIce               = tapelev3, key = ilev_3
CADJ STORE silicaSurf         = tapelev3, key = ilev_3
CADJ STORE atmosPCO2          = tapelev3, key = ilev_3

CADJ STORE gsm_s              = tapelev3, key = ilev_3
CADJ STORE co2atmos           = tapelev3, key = ilev_3
CADJ STORE total_atmos_carbon = tapelev3, key = ilev_3

C common dic_load
CADJ STORE dicwind0, dicwind1       = tapelev3, key = ilev_3
CADJ STORE ice0, ice1               = tapelev3, key = ilev_3
CADJ STORE atmosp0, atmosp1         = tapelev3, key = ilev_3
CADJ STORE silicaSurf0, silicaSurf1 = tapelev3, key = ilev_3
#ifdef ALLOW_FE
CADJ STORE feinput0, feinput1       = tapelev3, key = ilev_3
#endif

#ifdef ALLOW_COST
CADJ STORE totcost = tapelev3, key = ilev_3
#endif

#endif /* ALLOW_DIC */
