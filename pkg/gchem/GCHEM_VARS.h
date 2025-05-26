CMLC ==========================================================
CMLC   Carbon chemistry variables
CMLC ==========================================================
CML
CML       COMMON /CARBON_NEEDS/
CML     &                      apCO2, AtmosP, pH, pCO2, FluxCO2,
CML     &                      wind, FIce, Silica
CML#ifdef ALLOW_EXF
CML     &                      , apco20, apco21
CML#endif
CML      _RL  apCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  AtmosP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  pH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL  pCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  FluxCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  wind(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  FIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  Silica(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML#ifdef ALLOW_EXF
CML      _RL  apco20(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  apco21(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML#endif
CML
C Store dissociation and carbon chemistry coefficients for
C    pCO2 solvers (see carbon_chem.F).
C ak0 =  [H2CO2]/pCO2
C   [Weiss 1974]
C ak1 =  [H][HCO3]/[H2CO3]
C   [Millero p.664 (1995) using Mehrbach et al. data on seawater scale]
C ak2 =  [H][CO3]/[HCO3]
C   [Millero p.664 (1995) using Mehrbach et al. data on seawater scale]
C akw =  [H][OH]
C   [Millero p.670 (1995) using composite data]
C akb =  [H][BO2]/[HBO2]
C   [Millero p.669 (1995) using data from dickson (1990)]
C aks =  [H][SO4]/[HSO4]
C   [dickson (1990, J. chem. Thermodynamics 22, 113)]
C akf =  [H][F]/[HF]
C   [dickson and Riley (1979)]
C ak1p = [H][H2PO4]/[H3PO4]
C   [DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)]
C ak2p = [H][HPO4]/[H2PO4]
C   [DOE(1994) eq 7.2.23 with footnote using data from Millero (1974)]
C ak3p = [H][PO4]/[HPO4]
C   [DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)]
C aksi = [H][SiO(OH)3]/[Si(OH)4]
C   [Millero p.671 (1995) using data from Yao and Millero (1995)]
C ft  = estimated fluoride concentration
C   [Riley (1965)]
C st  = estimated sulphate concentration
C   [Morris & Riley (1966)]
C bt  = estimated borate concentration
C   [Uppstrom (1974)]
C fugf :: correct for non-ideality in ocean
C   [Weiss (1974) Marine Chemistry]
C ff   :: used for water vapor and pressure correction
C   [Weiss & Price (1980, Mar. Chem., 8, 347-359; Eq 13 with table 6 values)]
C Ksp_TP_Calc :: solubility product for calcite
C   [Following Mucci (1983) with pressure dependence from Ingle (1975)]
C Ksp_TP_Arag :: solubility product for aragonite, Ref.: Mucci (1983)

       COMMON /CARBON_CHEM/
     &                     ak0,ak1,ak2,akw,akb,aks,akf,
     &                     ak1p,ak2p,ak3p,aksi, fugf,
     &                     ff,ft,st,bt, Ksp_TP_Calc
#ifdef ALLOW_BLING
     &                    ,Ksp_TP_Arag
#endif

      _RL ak0        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ak1        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ak2        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL akw        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL akb        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL aks        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL akf        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ak1p       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ak2p       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ak3p       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL aksi       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ff         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C Fugacity Factor added by Val Bennington Nov. 2010
      _RL fugf       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ft         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL st         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bt         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Ksp_TP_Calc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_BLING
      _RL Ksp_TP_Arag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

CML#ifdef CARBONCHEM_SOLVESAPHE
CMLC If using Solvesaphe routines (Munhoven, 2013) then in adittion,
CMLC  cat  = total calcium concentration, Ref.: Culkin (1965)
CMLC   akn = the dissociation constant of ammonium [H][NH3]/[NH4]
CMLC           Ref.: Yao and Millero (1995)
CMLC   akhs = the dissociation constant of hydrogen sulfide [H][HS]/[H2S]
CMLC           Ref.: Millero et al. (1988)
CMLC          (cited by Millero (1995) and Yao and Millero (1995))
CMLC  aphscale = pH scale conversion factor ; convert from the total to the free
CMLC          scale for solvesaphe calculations;  Ref.: Munhoven, 2013
CMLC   Ksp_TP_Arag = solubility product for aragonite, Ref.: Mucci (1983)
CMLC----
CMLC  selectBTconst :: estimates borate concentration from salinity:
CMLC     =1 :: use default formulation of Uppström (1974)(same as S/R CARBON_COEFFS)
CMLC     =2 :: use new formulation from Lee et al (2010)
CMLC
CMLC  selectFTconst :: estimates fluoride concentration from salinity:
CMLC     =1 :: use default formulation of Riley (1965) (same as S/R CARBON_COEFFS)
CMLC     =2 :: use new formulation from Culkin (1965)
CMLC
CMLC  selectHFconst :: sets the first dissociation constant for hydrogen fluoride:
CMLC     =1 :: use default  Dickson and Riley (1979) (same as S/R CARBON_COEFFS)
CMLC     =2 :: use new formulation of Perez and Fraga (1987)
CMLC
CMLC  selectK1K2const :: sets the 1rst & 2nd dissociation constants of carbonic acid:
CMLC     =1 :: use default formulation of Millero (1995) with data
CMLC            from Mehrbach et al. (1973) (same as S/R CARBON_COEFFS)
CMLC     =2 :: use formulation of Roy et al. (1993)
CMLC     =3 :: use "combination" formulation of Millero (1995)
CMLC     =4 :: use formulation of Luecker et al. (2000)
CMLC     =5 :: use formulation of Millero (2010, Mar. Fresh Wat. Res.)
CMLC     =6 :: use formulation of Waters, Millero, Woosley (2014, Mar. Chem.)
CMLC  selectPHsolver :: sets the pH solver to use:
CMLC     =1 :: use the GENERAL solver ;  =2 :: use SEC solver ;
CMLC     =3 :: use FAST solver routine.
CML
CML       COMMON /CARBONCHEM_SOLVESAPHE_ARIANE/
CMLcav     &                     cat, akn, akhs, aphscale, Ksp_TP_Arag,
CML     &                     cat, akn, akhs, aphscale,
CML     &                     at_maxniter,
CML     &                     selectBTconst,selectFTconst,
CML     &                     selectHFconst,selectK1K2const,
CML     &                     selectPHsolver
CML
CML      _RL  cat(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  akn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  akhs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL  aphscale(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CMLcav      _RL  Ksp_TP_Arag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML
CML      INTEGER at_maxniter
CML      INTEGER selectBTconst
CML      INTEGER selectFTconst
CML      INTEGER selectHFconst
CML      INTEGER selectK1K2const
CML      INTEGER selectPHsolver
CML#endif /* CARBONCHEM_SOLVESAPHE */
CML
CML       COMMON /OXYGEN_CHEM/
CML     &                     oA0,oA1,oA2,oA3,oA4,oA5,
CML     &                     oB0,oB1,oB2,oB3,
CML     &                     oC0
CML      _RL oA0,oA1,oA2,oA3,oA4,oA5
CML      _RL oB0,oB1,oB2,oB3
CML      _RL oC0
CML
CML       COMMON /GLOBAL_SURF_MEAN/
CML     &                          permil,Pa2Atm,epsln
CML
CMLC      permil : is conversion factor for mol/m3 to mol/kg
CMLC               assumes uniform (surface) density
CMLC      Pa2Atm : for conversion of atmospheric pressure
CMLC               when coming from atmospheric model
CML
CML      _RL  permil
CML      _RL  Pa2Atm
CML      _RL  epsln
CML
CML       COMMON /SCHMIDT_NO/
CML     &                    sca1, sca2, sca3, sca4,
CML     &                    sox1, sox2, sox3, sox4
CML
CMLC      Schmidt number coefficients
CML
CML      _RL  sca1,sca2,sca3,sca4
CML      _RL  sox1,sox2,sox3,sox4
CML
CML#ifdef ALLOW_EXF
CML      integer apco2startdate1
CML      integer apco2startdate2
CML      _RL     apco2StartTime
CML      _RL     apco2period
CML      _RL     apco2RepCycle
CML      _RL     apco2const
CML      _RL     apco2_exfremo_intercept
CML      _RL     apco2_exfremo_slope
CML      CHARACTER*1 apco2mask
CML#endif
CML
CMLC ==========================================================
CMLC   Bling inputs (specified in data.bling)
CMLC ==========================================================
CML
CML       COMMON /BLING_INPUTS/
CML     &        bling_windFile, bling_atmospFile, bling_iceFile,
CML     &        bling_ironFile, bling_silicaFile,
CML     &        bling_psmFile, bling_plgFile, bling_pdiazFile,
CML     &        bling_forcingPeriod, bling_forcingCycle,
CML     &        bling_pCO2,
CML     &        river_conc_po4, river_dom_to_nut,
CML     &        bling_Pc_2dFile, bling_Pc_2d_diazFile,
CML     &        bling_alpha_photo2dFile,bling_phi_DOM2dFile,
CML     &        bling_k_Fe2dFile, bling_k_Fe_diaz2dFile,
CML     &        bling_gamma_POM2dFile, bling_wsink0_2dFile,
CML     &        bling_phi_lg2dFile, bling_phi_sm2dFile
CML#ifdef ALLOW_EXF
CML     &       ,apco2startdate1,apco2startdate2,
CML     &        apco2StartTime, apco2period, apco2RepCycle,
CML     &        apco2const,
CML     &        apco2_exfremo_intercept,
CML     &        apco2_exfremo_slope,
CML     &        apco2file, apco2mask
CML#endif
CML
CMLC      bling_windFile      :: file name of wind speeds
CMLC      bling_atmospFile    :: file name of atmospheric pressure
CMLC      bling_iceFile       :: file name of sea ice fraction
CMLC      bling_ironFile      :: file name of aeolian iron flux
CMLC      bling_silicaFile    :: file name of surface silica
CMLC      bling_psmFile       :: file name of init small phyto biomass
CMLC      bling_plgFile       :: file name of init lg phyto biomass
CMLC      bling_pdiazFile     :: file name of init diaz biomass
CMLC      bling_forcingPeriod :: period of forcing for biogeochemistry (seconds)
CMLC      bling_forcingCycle  :: periodic forcing parameter for biogeochemistry
CMLC      bling_pCO2          :: Atmospheric pCO2 to be read in data.bling
CMLC      river_conc_trac     :: River concentration, bgc tracers
CMLC      apco2               :: Atmospheric pCO2 to be read in with exf pkg
CML
CML      CHARACTER*(MAX_LEN_FNAM) bling_windFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_atmospFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_iceFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_ironFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_silicaFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_psmFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_plgFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_pdiazFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_Pc_2dFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_Pc_2d_diazFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_alpha_photo2dFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_k_Fe2dFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_k_Fe_diaz2dFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_gamma_POM2dFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_wsink0_2dFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_phi_DOM2dFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_phi_lg2dFile
CML      CHARACTER*(MAX_LEN_FNAM) bling_phi_sm2dFile
CML#ifdef ALLOW_EXF
CML      CHARACTER*(MAX_LEN_FNAM) apco2file
CML#endif
CML      _RL     bling_forcingPeriod
CML      _RL     bling_forcingCycle
CML      _RL     bling_pCO2
CML      _RL     river_conc_po4
CML      _RL     river_dom_to_nut
CML
CMLC ==========================================================
CMLC   EXF input/output scaling factors for unit conversion if needed
CMLC ==========================================================
CML#ifdef ALLOW_EXF
CML      _RL     exf_inscal_apco2
CML      _RL     exf_outscal_apco2
CML      COMMON /BLG_PARAM_SCAL/
CML     &                    exf_inscal_apco2,
CML     &                    exf_outscal_apco2
CML#endif
CML
CMLC ==========================================================
CMLC   EXF interpolation needs
CMLC ==========================================================
CML#ifdef ALLOW_EXF
CML#ifdef USE_EXF_INTERPOLATION
CML      _RL apco2_lon0, apco2_lon_inc
CML      _RL apco2_lat0, apco2_lat_inc(MAX_LAT_INC)
CML      INTEGER apco2_nlon, apco2_nlat, apco2_interpMethod
CML
CML      COMMON /BLG_EXF_INTERPOLATION/
CML     &        apco2_lon0, apco2_lon_inc,
CML     &        apco2_lat0, apco2_lat_inc,
CML     &        apco2_nlon, apco2_nlat,apco2_interpMethod
CML#endif
CML#endif
CML
CMLC ==========================================================
CMLC   Ecosystem variables and parameters
CMLC ==========================================================
CMLC     irr_mem       :: Phyto irradiance memory
CMLC          this is a temporally smoothed field carried between timesteps,
CMLC          to represent photoadaptation.
CMLC   chlsat_locTimWindow(1:2) :: local-time window (in h) for
CMLC          satellite-equivalent chlorophyll diagnostic (and cost)
CML
CML      COMMON /BIOTIC_NEEDS/
CML     &                     InputFe,
CML     &                     omegaC,
CML     &                     omegaAr,
CML     &                     irr_mem,
CML     &                     phyto_lg,
CML     &                     phyto_sm,
CML     &                     chl,
CML     &                     chl_sat,
CML     &                     poc,
CML     &                     Pc_0_2d,
CML     &                     k_Fe_2d,
CML     &                     wsink0_2d,
CML     &                     gamma_POM_2d,
CML     &                     phi_DOM_2d,
CML     &                     phi_sm_2d,
CML     &                     phi_lg_2d,
CML#ifndef USE_BLING_V1
CML     &                     phyto_diaz,
CML     &                     Pc_0_diaz_2d,
CML     &                     k_Fe_diaz_2d,
CML     &                     alpha_photo_2d,
CML     &                     Pc_0_diaz,
CML     &                     alpha_photo,
CML     &                     gamma_DON,
CML     &                     k_Fe_diaz,
CML     &                     k_NO3,
CML     &                     k_NO3_sm,
CML     &                     k_NO3_lg,
CML     &                     k_PO4_sm,
CML     &                     k_PO4_lg,
CML     &                     k_Fe_sm,
CML     &                     k_Fe_lg,
CML     &                     k_PtoN,
CML     &                     k_FetoN,
CML     &                     PtoN_min,
CML     &                     PtoN_max,
CML     &                     FetoN_min,
CML     &                     FetoN_max,
CML     &                     kappa_eppley_diaz,
CML     &                     phi_dvm,
CML     &                     sigma_dvm,
CML#ifdef USE_SIBLING
CML     &                     k_Si,
CML     &                     gamma_Si_0,
CML     &                     kappa_remin_Si,
CML     &                     wsink_Si,
CML     &                     SitoN_uptake_min,
CML     &                     SitoN_uptake_max,
CML     &                     SitoN_uptake_scale,
CML     &                     SitoN_uptake_exp,
CML     &                     q_SitoN_diss,
CML#endif
CML#else
CML     &                     alpha_max,
CML     &                     alpha_min,
CML     &                     gamma_biomass,
CML     &                     k_FetoP,
CML     &                     FetoP_max,
CML     &                     Fe_lim_min,
CML#endif
CML     &                     CtoN,
CML     &                     CtoP,
CML     &                     NtoP,
CML     &                     HtoC,
CML     &                     NO3toN,
CML     &                     O2toN,
CML     &                     O2toP,
CML     &                     CatoN,
CML     &                     CatoP,
CML     &                     masstoN,
CML     &                     pivotal,
CML     &                     Pc_0,
CML     &                     lambda_0,
CML     &                     resp_frac,
CML     &                     chl_min,
CML     &                     theta_Fe_max_hi,
CML     &                     theta_Fe_max_lo,
CML     &                     gamma_irr_mem,
CML     &                     gamma_DOP,
CML     &                     gamma_POM,
CML     &                     k_O2,
CML     &                     k_Fe,
CML     &                     k_PO4,
CML     &                     kFe_eq_lig_max,
CML     &                     kFe_eq_lig_min,
CML     &                     kFe_eq_lig_Femin,
CML     &                     kFe_eq_lig_irr,
CML     &                     kFe_org,
CML     &                     kFe_inorg,
CML     &                     FetoC_sed,
CML     &                     remin_min,
CML     &                     oxic_min,
CML     &                     ligand,
CML     &                     kappa_eppley,
CML     &                     kappa_remin,
CML     &                     ca_remin_depth,
CML     &                     phi_DOM,
CML     &                     phi_sm,
CML     &                     phi_lg,
CML     &                     wsink0,
CML     &                     wsink0z,
CML     &                     wsinkacc,
CML     &                     parfrac,
CML     &                     alpfe,
CML     &                     k0,
CML     &                     MLmix_max,
CML     &                     chlsat_locTimWindow
CML
CML      _RL InputFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL omegaC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL omegaAr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL irr_mem(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL phyto_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL phyto_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL chl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL chl_sat(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL poc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL Pc_0_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL k_Fe_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL wsink0_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL gamma_POM_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL phi_DOM_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL phi_sm_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL phi_lg_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML#ifndef USE_BLING_V1
CML      _RL phyto_diaz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CML      _RL Pc_0_diaz_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL k_Fe_diaz_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL alpha_photo_2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CML      _RL Pc_0_diaz
CML      _RL alpha_photo
CML      _RL gamma_DON
CML      _RL k_Fe_diaz
CML      _RL k_NO3
CML      _RL k_NO3_sm
CML      _RL k_NO3_lg
CML      _RL k_PO4_sm
CML      _RL k_PO4_lg
CML      _RL k_Fe_sm
CML      _RL k_Fe_lg
CML      _RL k_PtoN
CML      _RL k_FetoN
CML      _RL PtoN_min
CML      _RL PtoN_max
CML      _RL FetoN_min
CML      _RL FetoN_max
CML      _RL kappa_eppley_diaz
CML      _RL phi_dvm
CML      _RL sigma_dvm
CML#ifdef USE_SIBLING
CML      _RL k_Si
CML      _RL gamma_Si_0
CML      _RL kappa_remin_Si
CML      _RL wsink_Si
CML      _RL SitoN_uptake_min
CML      _RL SitoN_uptake_max
CML      _RL SitoN_uptake_scale
CML      _RL SitoN_uptake_exp
CML      _RL q_SitoN_diss
CML#endif
CML#else
CML      _RL alpha_max
CML      _RL alpha_min
CML      _RL gamma_biomass
CML      _RL k_FetoP
CML      _RL FetoP_max
CML      _RL Fe_lim_min
CML#endif
CML      _RL CtoN
CML      _RL CtoP
CML      _RL NtoP
CML      _RL HtoC
CML      _RL NO3toN
CML      _RL O2toN
CML      _RL O2toP
CML      _RL CatoN
CML      _RL CatoP
CML      _RL masstoN
CML      _RL pivotal
CML      _RL Pc_0
CML      _RL lambda_0
CML      _RL resp_frac
CML      _RL chl_min
CML      _RL theta_Fe_max_hi
CML      _RL theta_Fe_max_lo
CML      _RL gamma_irr_mem
CML      _RL gamma_DOP
CML      _RL gamma_POM
CML      _RL k_O2
CML      _RL k_Fe
CML      _RL k_PO4
CML      _RL kFe_eq_lig_max
CML      _RL kFe_eq_lig_min
CML      _RL kFe_eq_lig_Femin
CML      _RL kFe_eq_lig_irr
CML      _RL kFe_org
CML      _RL kFe_inorg
CML      _RL FetoC_sed
CML      _RL remin_min
CML      _RL oxic_min
CML      _RL ligand
CML      _RL kappa_eppley
CML      _RL kappa_remin
CML      _RL ca_remin_depth
CML      _RL phi_DOM
CML      _RL phi_sm
CML      _RL phi_lg
CML      _RL wsink0
CML      _RL wsink0z
CML      _RL wsinkacc
CML      _RL parfrac
CML      _RL alpfe
CML      _RL k0
CML      _RL MLmix_max
CML      _RL chlsat_locTimWindow(2)
