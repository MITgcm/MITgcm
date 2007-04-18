      INTEGER nForcingPer
      PARAMETER ( nForcingPer = 12)

      COMMON/TMSTPS4OCN/dtatmo,dtocno,dtcouplo
       _RS dtatmo             ! atmospheric time step (s)
       _RS dtocno             ! ocean time step (s)
       _RS dtcouplo           ! time of coupling period (s)


C     Files: mean 2D atmos fields used for wind anomaly coupling
      COMMON /MEANWIND/ 
     &        atmosTauuFile, atmosTauvFile,atmosWindFile,
     &        atau, atav, awind
       CHARACTER*(MAX_LEN_FNAM) atmosTauuFile
       CHARACTER*(MAX_LEN_FNAM) atmosTauvFile
       CHARACTER*(MAX_LEN_FNAM) atmosWindFile
       _RL atau(jm0,nForcingPer) ! zonal wind stress
       _RL atav(jm0,nForcingPer) ! meridional wind stress
       _RL awind(jm0,nForcingPer)! (total) wind speed


C     Files: basic state 3D wind fields, and E,P, and qnet prescibed fluxes (if used)
      COMMON /OCEAN_2D_FILES/
     &        tauuFile, tauvFile, windFile,
     &        qnetFile, evapFile, precipFile

       CHARACTER*(MAX_LEN_FNAM) tauuFile
       CHARACTER*(MAX_LEN_FNAM) tauvFile
       CHARACTER*(MAX_LEN_FNAM) windFile
       CHARACTER*(MAX_LEN_FNAM) qnetFile
       CHARACTER*(MAX_LEN_FNAM) evapFile
       CHARACTER*(MAX_LEN_FNAM) precipFile


      COMMON /OCEAN_RELAX_FILES/
     &        thetaRelaxFile, saltRelaxFile,
     &        tauThetaRelax, tauSaltRelax,
     &        r_tauThetaRelax, r_tauSaltRelax,
     &        nttyperelax, nstyperelax


       CHARACTER*(MAX_LEN_FNAM) thetaRelaxFile
       CHARACTER*(MAX_LEN_FNAM) saltRelaxFile
       _RL          tauThetaRelax   ! relaxation time (s) for temp
       _RL          tauSaltRelax    ! relaxation time (s) for salt
       _RL          r_tauThetaRelax ! reciprocal of above
       _RL          r_tauSaltRelax  ! reciprocal of above
       INTEGER      ntTypeRelax     ! method of relaxation for temp, non-zero is tapered
       INTEGER      nsTypeRelax     ! method of relaxation for salt, non-zero is tapered


      COMMON /RUNOFF_DATA/ 
     &        runoffFile, runoffMapFile,
     &        runoffVal, numBands, rband, runIndex

       CHARACTER*(MAX_LEN_FNAM) runoffFile
       CHARACTER*(MAX_LEN_FNAM) runoffMapFile
       _RL          runoffVal (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER      numBands      ! number of runoff bands
       INTEGER      rband(sNy)    ! first band j=1,rband(1); next j=rband(1)+1,rband(2) etc.
       INTEGER      runIndex(sNy) ! for specific latitude, corresponding runoff band

      COMMON /OPTIONS_1DTO2D/
     &        useObsEmP, useObsRunoff, useAltDeriv
       LOGICAL         useObsEmP    ! if true, do not pass 2D atmos fluxes to ocean
       LOGICAL         useObsRunoff ! if true, do do not pass 2S atmos runoff to ocean
       LOGICAL         useAltDeriv  ! use alternate derivative formulation (smaller derivates)
                                    ! not used for thSice surface temp solver

      COMMON /ATMOS4OCN/ atm_tauu, atm_tauv, atm_Tair,
     &           atm_precip, atm_runoff, 
     &           atm_evap_ice, atm_evap_ocn,
     &           atm_qnet_ice, atm_qnet_ocn,
     &           atm_dLdt_ice, atm_dLdt_ocn,
     &           atm_dFdt_ice, atm_dFdt_ocn,
     &           atm_dLdt_iceq, atm_dLdt_ocnq,
     &           atm_dFdt_iceq, atm_dFdt_ocnq,
     &           atm_solarinc, atm_solar_ice,
     &           atm_solar_ocn, atm_windspeed,
     &           atm_slp, atm_pco2
       _RL atm_tauu(jm0)      ! zonal mom flux at lower boundary (N/m2)
       _RL atm_tauv(jm0)      ! merid. mom flux at lower boundary (N/m2)
       _RL atm_Tair(jm0)      ! precipitation temp (used over seaice) (C)
       _RL atm_precip(jm0)    ! precip (m/s) (+=out of ocean, - definite)
       _RL atm_runoff(jm0)    ! runoff (m3/s) (+=out of ocean, normally - )
       _RL atm_evap_ice(jm0)  ! seaice evap. (m/s) (-=out of ocean, normally -)
       _RL atm_evap_ocn(jm0)  ! ocean evap. (m/s) (-=out of ocean, normally -) 
       _RL atm_qnet_ice(jm0)  ! seaice surf. heat flux (W/m2) (+=upward)
       _RL atm_qnet_ocn(jm0)  ! ocean surf. heat flux (W/m2) (+=upward)
       _RL atm_dLdt_ice(jm0)  ! dL/dT ice (m/s/K) (-)
       _RL atm_dLdt_ocn(jm0)  ! dL/dT ocean (m/s/K) (-)
       _RL atm_dFdt_ice(jm0)  ! dF/dT ice (W/m2/K) (+)
       _RL atm_dFdt_ocn(jm0)  ! dF/dT ocean (W/m2/K) (+) 
       _RL atm_dLdt_iceq(jm0) ! dL/dT ice, alternate form (m/s/K) (-)
       _RL atm_dLdt_ocnq(jm0) ! dL/dT ocean, alternate form (m/s/K) (-)
       _RL atm_dFdt_iceq(jm0) ! dF/dT ice, alternate form (W/m2/K) (+)
       _RL atm_dFdt_ocnq(jm0) ! dF/dT ocean, alternate form (W/m2/K) (+) 
       _RL atm_solarinc(jm0)  ! solar incoming (+=out of ocean)
       _RL atm_solar_ice(jm0) ! net solar radation to ice (+=into ocean)
       _RL atm_solar_ocn(jm0) ! solar incoming to ocean (+=into ocean)
       _RL atm_windspeed(jm0) ! windspeed at ocean surface (m/s)
       _RL atm_slp(jm0)       ! SLP (mb) 
       _RL atm_pco2(jm0)      ! atmospheric pCO2  (units?)  

c
c ocean data zonal means
      COMMON/OCN_ZONALMN/ ctocn, ctice, cfice, csAlb, ocnArea, cco2flux
       _RL ctocn(jm0)    ! zonal mean ice-free ocean temp
       _RL ctice(jm0)    ! zonal mean seaice surface temp
       _RL cfice(jm0)    ! zonal mean ice fraction
       _RL csAlb(jm0)    ! zonal mean seaice albedo
       _RL ocnArea(jm0)  ! ocean area of latitude strip on atm grid (m2)
       _RL cco2flux(jm0) ! zonally integrated flux of CO2 from ocean->atm (units?)

c
c OCN fluxes after conversion from 1D to 2D, whether by flux adj.,
c restoring, or by manipulating 1D atmos values for ocean model
c
       COMMON/FLUXES_2D_OCN/ qneto_2D, evapo_2D, runoff_2D,
     &                   precipo_2D, fu_2D, fv_2D, solarnet_ocn_2D,
     &                   slp_2D, pCO2_2D, wspeed_2D
       _RL qneto_2D(1:sNx,1:sNy)   ! ocean surf. heat flux (W/m2) (+=upward)
       _RL evapo_2D(1:sNx,1:sNy)   ! ocean evap. (m/s) (-=out of ocean) 
       _RL runoff_2D(1:sNx,1:sNy)  ! runoff (m/s over gridcell) (+=out of ocean)
       _RL precipo_2D(1:sNx,1:sNy) ! precip (m/s) (+=out of ocean, - definite)
       _RL fu_2D(1:sNx,1:sNy)      ! zonal mom flux at lower boundary (N/m2)
       _RL fv_2D(1:sNx,1:sNy)      ! merid. mom flux at lower boundary (N/m2)
       _RL solarnet_ocn_2D(1:sNx,1:sNy) ! solar incoming to ocean (+=into ocean)
       _RL slp_2D(1:sNx,1:sNy)     ! SLP (mb)
       _RL pCO2_2D(1:sNx,1:sNy)    ! atmospheric pCO2  (units?) 
       _RL wspeed_2D(1:sNx,1:sNy)  ! windspeed at ocean surface (m/s)

c
c ICE fluxes after conversion from 1D to 2D, whether by flux adj.,
c restoring, or by manipulating 1D atmos values for seaice model
c
      COMMON/FLUXES_2D_ICE/ qneti_2D, evapi_2D, precipi_2D,  
     &                   solarinc_2D, dFdT_ice_2D, Tair_2D
       _RL qneti_2D(1:sNx,1:sNy)   ! seaice surf. heat flux (W/m2) (+=upward)
       _RL evapi_2D(1:sNx,1:sNy)   ! ocean evap. (m/s) (-=out of ocean)
       _RL precipi_2D(1:sNx,1:sNy) ! precip (m/s) (+=out of ocean, - definite)
       _RL solarinc_2D(1:sNx,1:sNy)! solar incoming (+=out of ocean)
       _RL dFdT_ice_2D(1:sNx,1:sNy)! dF/dT ice (W/m^2/K) (+ given +=up HF)
       _RL Tair_2D(1:sNx,1:sNy)    ! precipitation temp (used over seaice) (C)

c
c lookup table for ocean gridcell to atmos grid cell. The weight
c is for that atmos grid cell; 1-weight is for atmos cell+1.
c    
      COMMON/LOOKUP_GRID/ atm_oc_ind, atm_oc_wgt, atm_oc_frac1, atm_oc_frac2,
     &                    endwgt1, endwgt2, rsumwgt
       INTEGER atm_oc_ind(sNy) ! lookup of (first) atm grid for given ocean lat
       _RL atm_oc_wgt(sNy)    ! weight of first atm grid for given ocean lat
       _RL atm_oc_frac1(sNy)  ! fraction of atm lat area covered by ocean lat
       _RL atm_oc_frac2(sNy)  ! fraction of atm+1 lat area covered by ocean lat
       _RL endwgt1            ! weighting of atmos polar cap
       _RL endwgt2            ! weighting of atmos cell next polar cap
       _RL rsumwgt            ! recip of sum of above

    
C     Fields use to sum 2D ocean fluxes over several atm timesteps
C     Also sum of atm E,P  for seaice growth step, and sum of seaice bottom fluxes->ocean
      COMMON/OCN_FLUXES_SUM/ sum_runoff, sum_precip, sum_evap, sum_qnet,
     &                       sum_fu, sum_fv, sum_wspeed, 
     &                       sum_solarnet, sum_slp, sum_pCO2,
     &                       sum_prcIce, sum_snowPrc, sum_evapIce,
     &                       sum_sHeat, sum_flxCnB
       _RL sum_runoff(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! sum of runoff (m/s) (+=out of ocean) 
       _RL sum_precip(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! sum ocean-only precip (m/s) (+=out of ocean, - def)
       _RL sum_evap(1-OLx:sNx+OLx,1-OLy:sNy+OLy)   ! sum ocean-only evap (m/s) (-=out of ocean)
       _RL sum_qnet(1-OLx:sNx+OLx,1-OLy:sNy+OLy)   ! sum ocean-only qnet (W/m2) (+=upward)
       _RL sum_fu(1-OLx:sNx+OLx,1-OLy:sNy+OLy)     ! sum of zonal wind stress applied to ocean (N/m2)
       _RL sum_fv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)     ! sum of merid wind stress applied to ocean (N/m2)
       _RL sum_wspeed(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! sum of wind speed applied to ocean (m/s)
       _RL sum_solarnet(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! sum of net solar into ocean, inc. thru ice (+=up)
       _RL sum_slp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    ! sum of SLP (mb)
       _RL sum_pCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)   ! sum of atmospheric pCO2  (units?) 
       _RL sum_prcIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! sum of total precip over ice (kg/m2/s, + def)
       _RL sum_snowPrc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)! sum of snow precip to ice (kg/m2/s, + def)
       _RL sum_evapIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)! total evap over ice (kg/m2/s, +=out of ocean)
       _RL sum_sHeat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)  ! surf heating left (post ice temp step) to melt ice/snow
       _RL sum_flxCnB(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! heat flux conducted through ice to bottom surface


C     These are the fluxes actually passed to the ocean model
      COMMON/OCN_FLUXES_PASS/ pass_runoff, pass_precip, pass_evap, pass_qnet,
     &                        pass_fu, pass_fv, pass_wspeed, 
     &                        pass_solarnet, pass_slp, pass_pCO2,
     &                        pass_sIceLoad, sFluxFromIce
       _RL pass_runoff(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! runoff -> ocean (m/s) (+=out of ocean)
       _RL pass_precip(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! precip -> ocean (m/s) (+=out of ocean, - def)
       _RL pass_evap(1-OLx:sNx+OLx,1-OLy:sNy+OLy)   ! evap -> ocean (m/s) (+=out of ocean), inc. seaice part
       _RL pass_qnet(1-OLx:sNx+OLx,1-OLy:sNy+OLy)   ! HF-> ocean (W/m2) (+=out of ocean), inc seaice part
       _RL pass_fu(1-OLx:sNx+OLx,1-OLy:sNy+OLy)     ! zonal wind stress -> ocean (N/m2)
       _RL pass_fv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)     ! merid wind stress -> ocean (N/m2
       _RL pass_wspeed(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! wind speed -> ocean (m/s)
       _RL pass_solarnet(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! total net solar -> ocean (+=up, - def)
       _RL pass_slp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    ! slp -> ocean (mb)
       _RL pass_pCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)   ! atmos pCO2 -> ocean (units?)
       _RL pass_sIceLoad(1-OLx:sNx+OLx,1-OLy:sNy+OLy)  ! seaice mass loading -> ocean (to be done) 
       _RL sFluxFromIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! upward salt flux->ocean (psu.kg/m^2/s, or g/m^2/s?)

     
      COMMON/ICE_FLUXES_PASS/ incSW, sFlx, dTsurf, pass_prcAtm
       _RL incSW(sNx,sNy)    ! shortwave flux to ice (W/m2) (+= down), init. total, then net
       _RL sFlx(sNx,sNy,0:2) ! input variables to seaice temp solver:
			     ! 0: flux needed to raise Tsurf to melting (Ts=0) (>= 0 W/m2)
			     ! 1: surface heat flux to ice (Ts=Ts^n) (W/m2) (+=down)
			     ! 2: dF/dT (over ice), (- def, as for +=down HF)
       _RL dTsurf(sNx,sNy)   ! surf temp adjustment Ts^n+1 - Ts^n
       _RL pass_prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)  ! total precip -> seaice top ((kg/m2/s, +=precip to ice)


C     Variables used to sum and compute atm2d diagnostic outputs
      COMMON/DIAGS_1DTO2D/ qnet_atm, evap_atm, precip_atm, runoff_atm,
     &                     sum_qrel, sum_frel, 
     &                     sum_iceMask, sum_iceHeight, sum_iceTime,
     &                     sum_oceMxLT, sum_oceMxLS,
     &                     qnet_atm_ta, evap_atm_ta, 
     &                     precip_atm_ta, runoff_atm_ta,
     &                     sum_qrel_ta, sum_frel_ta,
     &                     sum_iceMask_ta, sum_iceHeight_ta, 
     &                     sum_iceTime_ta,
     &                     sum_oceMxLT_ta, sum_oceMxLS_ta,
     &                     sum_tauu_ta, sum_tauv_ta, sum_wsocean_ta,
     &                     sum_ps4ocean_ta
       _RL qnet_atm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL evap_atm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL precip_atm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)  
       _RL runoff_atm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)  
       _RL sum_qrel(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL sum_frel(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL sum_iceMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL sum_iceHeight(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL sum_iceTime(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL sum_oceMxLT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL sum_oceMxLS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
       _RL qnet_atm_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer)    
       _RL evap_atm_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer)    
       _RL precip_atm_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer)  
       _RL runoff_atm_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer)  
       _RL sum_qrel_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer)    
       _RL sum_frel_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer)    
       _RL sum_iceMask_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer) 
       _RL sum_iceHeight_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer) 
       _RL sum_iceTime_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer) 
       _RL sum_oceMxLT_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer) 
       _RL sum_oceMxLS_ta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nForcingPer) 
       _RL sum_tauu_ta(jm0,nForcingPer)
       _RL sum_tauv_ta(jm0,nForcingPer)
       _RL sum_wsocean_ta(jm0,nForcingPer)
       _RL sum_ps4ocean_ta(jm0,nForcingPer)
       

C      Variables passed from ocean model
      COMMON/FROM_OCN/ sstFromOcn, sssFromOcn, fluxCO2, mlDepth
       _RL sstFromOcn(1:sNx,1:sNy)
       _RL sssFromOcn(1:sNx,1:sNy)
       _RL fluxCO2(1:sNx,1:sNy)
       _RL mlDepth(1:sNx,1:sNy)    ! at present, simply the depth of top ocean grid cell (m)

