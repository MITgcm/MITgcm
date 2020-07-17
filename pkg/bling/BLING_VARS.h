C ==========================================================
C   Carbon chemistry variables
C ==========================================================

       COMMON /CARBON_NEEDS/
     &                      apCO2, AtmosP, pH, pCO2, FluxCO2,
     &                      wind, FIce, Silica
#ifdef ALLOW_EXF
     &                      , apco20, apco21
#endif
      _RL  apCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  AtmosP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  pH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  pCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FluxCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wind(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Silica(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_EXF
      _RL  apco20(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  apco21(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

C ==========================================================
C   Carbon and oxygen chemistry parameters
C ==========================================================

       COMMON /CARBON_CHEM/
     &                     ak0,ak1,ak2,akw,akb,aks,akf,
     &                     ak1p,ak2p,ak3p,aksi,fugf,
     &                     ff,ft,st,bt,
     &                     Ksp_TP_Calc,Ksp_TP_Arag
      _RL  ak0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ak1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ak2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  akw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  akb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aks(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  akf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ak1p(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ak2p(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ak3p(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aksi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  fugf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ft(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  st(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  bt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Ksp_TP_Calc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Ksp_TP_Arag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef CARBONCHEM_SOLVESAPHE
C If using Solvesaphe routines (Munhoven, 2013) then in adittion,
C  cat  = total calcium concentration, Ref.: Culkin (1965)
C   akn = the dissociation constant of ammonium [H][NH3]/[NH4]
C           Ref.: Yao and Millero (1995)
C   akhs = the dissociation constant of hydrogen sulfide [H][HS]/[H2S]
C           Ref.: Millero et al. (1988)
C          (cited by Millero (1995) and Yao and Millero (1995))
C  aphscale = pH scale conversion factor ; convert from the total to the free
C          scale for solvesaphe calculations;  Ref.: Munhoven, 2013
C   Ksp_TP_Arag = solubility product for aragonite, Ref.: Mucci (1983)
C----
C  selectBTconst :: estimates borate concentration from salinity:
C     =1 :: use default formulation of Uppström (1974)(same as S/R CARBON_COEFFS)
C     =2 :: use new formulation from Lee et al (2010)
C
C  selectFTconst :: estimates fluoride concentration from salinity:
C     =1 :: use default formulation of Riley (1965) (same as S/R CARBON_COEFFS)
C     =2 :: use new formulation from Culkin (1965)
C
C  selectHFconst :: sets the first dissociation constant for hydrogen fluoride:
C     =1 :: use default  Dickson and Riley (1979) (same as S/R CARBON_COEFFS)
C     =2 :: use new formulation of Perez and Fraga (1987)
C
C  selectK1K2const :: sets the 1rst & 2nd dissociation constants of carbonic acid:
C     =1 :: use default formulation of Millero (1995) with data
C            from Mehrbach et al. (1973) (same as S/R CARBON_COEFFS)
C     =2 :: use formulation of Roy et al. (1993)
C     =3 :: use "combination" formulation of Millero (1995)
C     =4 :: use formulation of Luecker et al. (2000)
C     =5 :: use formulation of Millero (2010, Mar. Fresh Wat. Res.)
C     =6 :: use formulation of Waters, Millero, Woosley (2014, Mar. Chem.)
C  selectPHsolver :: sets the pH solver to use:
C     =1 :: use the GENERAL solver ;  =2 :: use SEC solver ;
C     =3 :: use FAST solver routine.

       COMMON /CARBONCHEM_SOLVESAPHE_ARIANE/
cav     &                     cat, akn, akhs, aphscale, Ksp_TP_Arag,
     &                     cat, akn, akhs, aphscale,
     &                     at_maxniter,
     &                     selectBTconst,selectFTconst,
     &                     selectHFconst,selectK1K2const,
     &                     selectPHsolver

      _RL  cat(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  akn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  akhs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aphscale(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
cav      _RL  Ksp_TP_Arag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER at_maxniter
      INTEGER selectBTconst
      INTEGER selectFTconst
      INTEGER selectHFconst
      INTEGER selectK1K2const
      INTEGER selectPHsolver
#endif /* CARBONCHEM_SOLVESAPHE */

       COMMON /OXYGEN_CHEM/
     &                     oA0,oA1,oA2,oA3,oA4,oA5,
     &                     oB0,oB1,oB2,oB3,
     &                     oC0
      _RL oA0,oA1,oA2,oA3,oA4,oA5
      _RL oB0,oB1,oB2,oB3
      _RL oC0

       COMMON /GLOBAL_SURF_MEAN/
     &                          permil,Pa2Atm,epsln

C      permil : is conversion factor for mol/m3 to mol/kg
C               assumes uniform (surface) density
C      Pa2Atm : for conversion of atmospheric pressure
C               when coming from atmospheric model

      _RL  permil
      _RL  Pa2Atm
      _RL  epsln

       COMMON /SCHMIDT_NO/
     &                    sca1, sca2, sca3, sca4,
     &                    sox1, sox2, sox3, sox4

C      Schmidt number coefficients

      _RL  sca1,sca2,sca3,sca4
      _RL  sox1,sox2,sox3,sox4

#ifdef ALLOW_EXF
      integer apco2startdate1
      integer apco2startdate2
      _RL     apco2StartTime
      _RL     apco2period
      _RL     apco2RepCycle
      _RL     apco2const
      _RL     apco2_exfremo_intercept
      _RL     apco2_exfremo_slope
      CHARACTER*1 apco2mask
#endif

C ==========================================================
C   Bling inputs (specified in data.bling)
C ==========================================================

       COMMON /BLING_INPUTS/
     &        bling_windFile, bling_atmospFile, bling_iceFile,
     &        bling_ironFile, bling_silicaFile,
     &        bling_psmFile, bling_plgFile, bling_pdiazFile,
     &        bling_forcingPeriod, bling_forcingCycle,
     &        bling_pCO2,
     &        river_conc_po4, river_dom_to_nut,
     &        bling_Pc_2dFile, bling_Pc_2d_diazFile,
     &        bling_alpha_photo2dFile,bling_phi_DOM2dFile,
     &        bling_k_Fe2dFile, bling_k_Fe_diaz2dFile,
     &        bling_gamma_POM2dFile, bling_wsink0_2dFile,
     &        bling_phi_lg2dFile, bling_phi_sm2dFile
#ifdef ALLOW_EXF
     &       ,apco2startdate1,apco2startdate2,
     &        apco2StartTime, apco2period, apco2RepCycle,
     &        apco2const,
     &        apco2_exfremo_intercept,
     &        apco2_exfremo_slope,
     &        apco2file, apco2mask
#endif

C      bling_windFile      :: file name of wind speeds
C      bling_atmospFile    :: file name of atmospheric pressure
C      bling_iceFile       :: file name of sea ice fraction
C      bling_ironFile      :: file name of aeolian iron flux
C      bling_silicaFile    :: file name of surface silica
C      bling_psmFile       :: file name of init small phyto biomass
C      bling_plgFile       :: file name of init lg phyto biomass
C      bling_pdiazFile     :: file name of init diaz biomass
C      bling_forcingPeriod :: period of forcing for biogeochemistry (seconds)
C      bling_forcingCycle  :: periodic forcing parameter for biogeochemistry
C      bling_pCO2          :: Atmospheric pCO2 to be read in data.bling
C      river_conc_trac     :: River concentration, bgc tracers
C      apco2               :: Atmospheric pCO2 to be read in with exf pkg

      CHARACTER*(MAX_LEN_FNAM) bling_windFile
      CHARACTER*(MAX_LEN_FNAM) bling_atmospFile
      CHARACTER*(MAX_LEN_FNAM) bling_iceFile
      CHARACTER*(MAX_LEN_FNAM) bling_ironFile
      CHARACTER*(MAX_LEN_FNAM) bling_silicaFile
      CHARACTER*(MAX_LEN_FNAM) bling_psmFile
      CHARACTER*(MAX_LEN_FNAM) bling_plgFile
      CHARACTER*(MAX_LEN_FNAM) bling_pdiazFile
      CHARACTER*(MAX_LEN_FNAM) bling_Pc_2dFile
      CHARACTER*(MAX_LEN_FNAM) bling_Pc_2d_diazFile
      CHARACTER*(MAX_LEN_FNAM) bling_alpha_photo2dFile
      CHARACTER*(MAX_LEN_FNAM) bling_k_Fe2dFile
      CHARACTER*(MAX_LEN_FNAM) bling_k_Fe_diaz2dFile
      CHARACTER*(MAX_LEN_FNAM) bling_gamma_POM2dFile
      CHARACTER*(MAX_LEN_FNAM) bling_wsink0_2dFile
      CHARACTER*(MAX_LEN_FNAM) bling_phi_DOM2dFile
      CHARACTER*(MAX_LEN_FNAM) bling_phi_lg2dFile
      CHARACTER*(MAX_LEN_FNAM) bling_phi_sm2dFile
#ifdef ALLOW_EXF
      CHARACTER*(MAX_LEN_FNAM) apco2file
#endif
      _RL     bling_forcingPeriod
      _RL     bling_forcingCycle
      _RL     bling_pCO2
      _RL     river_conc_po4
      _RL     river_dom_to_nut

C ==========================================================
C   EXF input/output scaling factors for unit conversion if needed
C ==========================================================
#ifdef ALLOW_EXF
      _RL     exf_inscal_apco2
      _RL     exf_outscal_apco2
      COMMON /BLG_PARAM_SCAL/
     &                    exf_inscal_apco2,
     &                    exf_outscal_apco2
#endif

C ==========================================================
C   EXF interpolation needs
C ==========================================================
#ifdef ALLOW_EXF
#ifdef USE_EXF_INTERPOLATION
      _RL apco2_lon0, apco2_lon_inc
      _RL apco2_lat0, apco2_lat_inc(MAX_LAT_INC)
      INTEGER apco2_nlon, apco2_nlat, apco2_interpMethod

      COMMON /BLG_EXF_INTERPOLATION/
     &        apco2_lon0, apco2_lon_inc,
     &        apco2_lat0, apco2_lat_inc,
     &        apco2_nlon, apco2_nlat,apco2_interpMethod
#endif
#endif

C ==========================================================
C   Ecosystem variables and parameters
C ==========================================================

      COMMON /BIOTIC_NEEDS/
     &                     InputFe,
     &                     omegaC,
     &                     omegaAr,
     &                     irr_mem,
     &                     phyto_lg,
     &                     phyto_sm,
     &                     chl,
     &                     poc,
     &                     Pc_0_2d,
     &                     k_Fe_2d,
     &                     wsink0_2d,
     &                     gamma_POM_2d,
     &                     phi_DOM_2d,
     &                     phi_sm_2d,
     &                     phi_lg_2d,
#ifndef USE_BLING_V1
     &                     phyto_diaz,
     &                     Pc_0_diaz_2d,
     &                     k_Fe_diaz_2d,
     &                     alpha_photo_2d,
     &                     Pc_0_diaz,
     &                     alpha_photo,
     &                     gamma_DON,
     &                     k_Fe_diaz,
     &                     k_NO3,
     &                     k_PtoN,
     &                     k_FetoN,
     &                     PtoN_min,
     &                     PtoN_max,
     &                     FetoN_min,
     &                     FetoN_max,
     &                     kappa_eppley_diaz,
     &                     phi_dvm,
     &                     sigma_dvm,
#ifdef USE_SIBLING
     &                     k_Si,
     &                     gamma_Si_0,
     &                     kappa_remin_Si,
     &                     wsink_Si,
     &                     SitoN_uptake_min,
     &                     SitoN_uptake_max,
     &                     SitoN_uptake_scale,
     &                     SitoN_uptake_exp,
     &                     q_SitoN_diss,
#endif
#else
     &                     alpha_max,
     &                     alpha_min,
     &                     gamma_biomass,
     &                     k_FetoP,
     &                     FetoP_max,
     &                     Fe_lim_min,
#endif
     &                     CtoN,
     &                     CtoP,
     &                     NtoP,
     &                     HtoC,
     &                     NO3toN,
     &                     O2toN,
     &                     O2toP,
     &                     CatoN,
     &                     CatoP,
     &                     masstoN,
     &                     pivotal,
     &                     Pc_0,
     &                     lambda_0,
     &                     resp_frac,
     &                     chl_min,
     &                     theta_Fe_max_hi,
     &                     theta_Fe_max_lo,
     &                     gamma_irr_mem,
     &                     gamma_DOP,
     &                     gamma_POM,
     &                     k_O2,
     &                     k_Fe,
     &                     k_PO4,
     &                     kFe_eq_lig_max,
     &                     kFe_eq_lig_min,
     &                     kFe_eq_lig_Femin,
     &                     kFe_eq_lig_irr,
     &                     kFe_org,
     &                     kFe_inorg,
     &                     FetoC_sed,
     &                     remin_min,
     &                     oxic_min,
     &                     ligand,
     &                     kappa_eppley,
     &                     kappa_remin,
     &                     ca_remin_depth,
     &                     phi_DOM,
     &                     phi_sm,
     &                     phi_lg,
     &                     wsink0,
     &                     wsink0z,
     &                     wsinkacc,
     &                     parfrac,
     &                     alpfe,
     &                     k0,
     &                     MLmix_max

      _RL InputFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL omegaC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL omegaAr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL irr_mem(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phyto_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phyto_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL chl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL poc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Pc_0_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL k_Fe_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wsink0_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gamma_POM_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL phi_DOM_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL phi_sm_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL phi_lg_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifndef USE_BLING_V1
      _RL phyto_diaz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Pc_0_diaz_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL k_Fe_diaz_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL alpha_photo_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Pc_0_diaz
      _RL alpha_photo
      _RL gamma_DON
      _RL k_Fe_diaz
      _RL k_NO3
      _RL k_PtoN
      _RL k_FetoN
      _RL PtoN_min
      _RL PtoN_max
      _RL FetoN_min
      _RL FetoN_max
      _RL kappa_eppley_diaz
      _RL phi_dvm
      _RL sigma_dvm
#ifdef USE_SIBLING
      _RL k_Si
      _RL gamma_Si_0
      _RL kappa_remin_Si
      _RL wsink_Si
      _RL SitoN_uptake_min
      _RL SitoN_uptake_max
      _RL SitoN_uptake_scale
      _RL SitoN_uptake_exp
      _RL q_SitoN_diss
#endif
#else
      _RL alpha_max
      _RL alpha_min
      _RL gamma_biomass
      _RL k_FetoP
      _RL FetoP_max
      _RL Fe_lim_min
#endif
      _RL CtoN
      _RL CtoP
      _RL NtoP
      _RL HtoC
      _RL NO3toN
      _RL O2toN
      _RL O2toP
      _RL CatoN
      _RL CatoP
      _RL masstoN
      _RL pivotal
      _RL Pc_0
      _RL lambda_0
      _RL resp_frac
      _RL chl_min
      _RL theta_Fe_max_hi
      _RL theta_Fe_max_lo
      _RL gamma_irr_mem
      _RL gamma_DOP
      _RL gamma_POM
      _RL k_O2
      _RL k_Fe
      _RL k_PO4
      _RL kFe_eq_lig_max
      _RL kFe_eq_lig_min
      _RL kFe_eq_lig_Femin
      _RL kFe_eq_lig_irr
      _RL kFe_org
      _RL kFe_inorg
      _RL FetoC_sed
      _RL remin_min
      _RL oxic_min
      _RL ligand
      _RL kappa_eppley
      _RL kappa_remin
      _RL ca_remin_depth
      _RL phi_DOM
      _RL phi_sm
      _RL phi_lg
      _RL wsink0
      _RL wsink0z
      _RL wsinkacc
      _RL parfrac
      _RL alpfe
      _RL k0
      _RL MLmix_max

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
