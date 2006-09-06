      COMMON/ATM_GRID/atm_yC,atm_yG
       REAL*4 atm_yC(jm0)    ! center points of atmos grid
       REAL*4 atm_yG(jm0)    ! southern boundary of atmos grid cells

      COMMON/ATM2OCEAN/hfluxo,hfluxi,dhfodtg,dhfodtgeq,
     & dhfidtg,dhfidtgeq,precip,evao,evai,devodtg,devodtgeq,
     & devidtg,devidtgeq,tauu,tauv,arunoff,solarinc_ice,solarnet_ice,
     & solarinc_ocean,solarnet_ocean,wsocean,co24ocean,ps4ocean,tempr
    
       REAL*8 hfluxo(jm0)    ! ocean surf. heat flux (W/m2) (+=downward)
       REAL*8 hfluxi(jm0)    ! seaice surf. heat flux (W/m2) (+=downward)
       REAL*8 dhfodtg(jm0)   ! dF/dT ocean, consistent with above units (-)
       REAL*8 dhfodtgeq(jm0) ! dF/dT ocean, alternative formulation
       REAL*8 dhfidtg(jm0)   ! dF/dT seaice
       REAL*8 dhfidtgeq(jm0) ! dF/dT seaice alternative formulation
       REAL*8 precip(jm0)    ! precip (mm/day) (+=into ocean)
       REAL*8 evao(jm0)      ! ocean evap. (mm/day) (+=out of ocean)
       REAL*8 evai(jm0)      ! seaice evap. (mm/day) (+=out of ocean)
       REAL*8 devodtg(jm0)   ! dL/dT ocean, consistent with above units (+)
       REAL*8 devodtgeq(jm0) ! dL/dT ocean, alternative formulation
       REAL*8 devidtg(jm0)   ! dL/dT ice
       REAL*8 devidtgeq(jm0) ! dL/dT ice, alternative formulation
       REAL*8 tauu(jm0)      ! zonal mom flux at lower boundary (N/m2)
       REAL*8 tauv(jm0)      ! merid. mom flux at lower boundary (N/m2)
       REAL*8 arunoff(jm0)   ! runoff (kg/day) (+=into ocean)
       REAL*8 solarinc_ice(jm0)    ! solar incoming (+=into ocean)
       REAL*8 solarnet_ice(jm0)    ! net solar radation to ice (not used)
       REAL*8 solarinc_ocean(jm0)  ! solar incoming (+=into ocean)(unused)
       REAL*8 solarnet_ocean(jm0)  ! net radiation solar to ocean
       REAL*8 wsocean(jm0)   ! windspeed at ocean surface (m/s)
       REAL*8 co24ocean(jm0) ! atmospheric CO2  (ppm)
       REAL*8 ps4ocean(jm0)  ! surf pres (mb), to be normalized to sea level
       REAL*8 tempr(jm0)     ! precipitation temp (used over seaice) (C)


      COMMON/ATM_PROF/sigfl,qyz4ocean,tyz4ocean
       REAL*8 sigfl(lm0)     ! sigma levels of atmospheric model
       REAL*8 qyz4ocean(jm0,lm0)  ! atmos mixing ratio structure (kg/kg)
       REAL*8 tyz4ocean(jm0,lm0)  ! atmos temp structure (K)
       
      COMMON/OCDATA/mmsst,mmfice,mmtice,mmco2flux,cflan,mmsAlb,
&                   mmicem, mmtice1, mmtice2, mmsnowm
       REAL*8 mmsst(jm0)    ! zonal mean SST (C)
       REAL*8 mmfice(jm0)   ! fraction of ocean area with seaice cover
       REAL*8 mmtice(jm0)   ! zonal mean seaice TSurf (C)
       REAL*8 mmco2flux(jm0)! total ocean->atmos CO2 flux (units?)
       REAL*8 cflan(jm0)    ! fraction of land in latitude circle
       REAL*8 mmsAlb(jm0)   ! zonal mean seaice albedo
       REAL*8 mmicem(jm0)
       REAL*8 mmtice1(jm0)
       REAL*8 mmtice2(jm0)
       REAL*8 mmsnowm(jm0)

#ifdef ML_2D
      common/OCEAN2ATM/osst(jm0),aoice(jm0),foice(jm0),
     & snowice(jm0),tice1(jm0),tice2(jm0)

      common /TIME4ML/TOFDAYML,TAUML,IDAYM,JDAYM,JMONTHM,JYEARM,KOCEANM
     &  ,JDATEM
      character *4 JMONTHM

      common/mltemp/osst2(jm0),osst3(jm0),rseaice(jm0)
     & ,CLAND4OCEAN(io0,jm0)
      common/atmnth/nmonth
      character *4 nmonth

#endif

