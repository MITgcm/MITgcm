C $Header: /u/gcmpack/MITgcm/pkg/bling/BLING_VARS.h,v 1.3 2016/10/15 21:30:43 mmazloff Exp $
C $Name:  $


C ==========================================================
C   Carbon chemistry variables
C ==========================================================

       COMMON /CARBON_NEEDS/
     &                      AtmospCO2, AtmosP, pH, pCO2, FluxCO2,
     &                      wind, FIce, Silica
#ifdef USE_EXFCO2
     &                      ,apco2, apco20, apco21
#endif      
      _RL  AtmospCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  AtmosP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  pH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  pCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FluxCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wind(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Silica(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef USE_EXFCO2
      _RL apco2      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL apco20     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL apco21     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif 

C ==========================================================
C   Carbon and oxygen chemistry parameters
C ==========================================================

       COMMON /CARBON_CHEM/
     &                     ak0,ak1,ak2,akw,akb,aks,akf,
     &                     ak1p,ak2p,ak3p,aksi, fugf, 
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


       COMMON /OXYGEN_CHEM/
     &                     oA0,oA1,oA2,oA3,oA4,oA5,
     &                     oB0,oB1,oB2,oB3,
     &                     oC0
      _RL oA0,oA1,oA2,oA3,oA4,oA5
      _RL oB0,oB1,oB2,oB3
      _RL oC0


       COMMON /GLOBAL_SURF_MEAN/
     &                          permil,Pa2Atm

C      permil : is conversion factor for mol/m3 to mol/kg
C               assumes uniform (surface) density
C      Pa2Atm : for conversion of atmospheric pressure
C               when coming from atmospheric model

      _RL  permil
      _RL  Pa2Atm


       COMMON /SCHMIDT_NO/
     &                    sca1, sca2, sca3, sca4,
     &                    sox1, sox2, sox3, sox4

C      Schmidt number coefficients

      _RL  sca1,sca2,sca3,sca4
      _RL  sox1,sox2,sox3,sox4

#ifdef USE_EXFCO2
      integer apco2startdate1
      integer apco2startdate2
      _RL     apco2startdate
      _RL     apco2period
      _RL     apco2const
      _RL     apco2_exfremo_intercept
      _RL     apco2_exfremo_slope
      character*1 apco2mask
#endif

C ==========================================================
C   Bling inputs (specified in data.bling)
C ==========================================================

       COMMON /BLING_INPUTS/
     &        bling_windFile, bling_atmospFile, bling_iceFile,
     &        bling_ironFile, bling_silicaFile, 
     &        bling_forcingPeriod, bling_forcingCycle,
     &        bling_pCO2, 
     &        river_conc_trac
#ifdef USE_EXFCO2
     &       ,apco2startdate1,apco2startdate2,
     &        apco2period,      apco2startdate,
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
#ifdef USE_EXFCO2
      CHARACTER*(MAX_LEN_FNAM) apco2file
#endif
      _RL     bling_forcingPeriod
      _RL     bling_forcingCycle
      _RL     bling_pCO2
c      _RL     river_conc_trac(PTRACERS_num)
      _RL     river_conc_trac(8)

C ==========================================================
C   EXF input/output scaling factors for unit conversion if needed
C ==========================================================
#ifdef USE_EXFCO2
      _RL     exf_inscal_apco2
      _RL     exf_outscal_apco2
      COMMON /BLG_PARAM_SCAL/
     &                    exf_inscal_apco2,
     &                    exf_outscal_apco2
#endif

C ==========================================================
C   EXF interpolation needs 
C ==========================================================
#ifdef USE_EXFCO2
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
     &                     phyto_diaz,
     &                     chl,
     &                     pivotal,
     &                     Pc_0,
     &                     Pc_0_diaz,
     &                     lambda_0,
     &                     chl_min,
     &                     CtoN,
     &                     NO3toN,
     &                     HtoC,
     &                     O2toN,
     &                     CatoN,
     &                     masstoN,
     &                     alpha_photo,
     &                     theta_Fe_max_hi,
     &                     theta_Fe_max_lo,
     &                     gamma_irr_mem,
     &                     gamma_DON,
     &                     gamma_DOP,
     &                     gamma_POM,
     &                     k_Fe,
     &                     k_Fe_diaz,
     &                     k_O2,
     &                     k_NO3,
     &                     k_PO4,
     &                     k_PtoN,
     &                     k_FetoN,
     &                     kFe_eq_lig_max,
     &                     kFe_eq_lig_min,
     &                     kFe_eq_lig_Femin,
     &                     kFe_eq_lig_irr,    
     &                     kFe_org,
     &                     kFe_inorg,
     &                     PtoN_min,
     &                     PtoN_max,
     &                     FetoN_min,
     &                     FetoN_max,
     &                     FetoC_sed,
     &                     remin_min,
     &                     oxic_min,
     &                     ligand,
     &                     kappa_eppley,
     &                     kappa_eppley_diaz,
     &                     kappa_remin,
     &                     ca_remin_depth,
     &                     phi_DOM,
     &                     phi_sm,
     &                     phi_lg,
     &                     phi_dvm,
     &                     sigma_dvm,
     &                     wsink0z,
     &                     wsink0,
     &                     wsinkacc,
     &                     parfrac,
     &                     alpfe,
     &                     k0,
     &                     epsln,
     &                     QSW_underice

      _RL InputFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL omegaC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL omegaAr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL irr_mem(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phyto_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phyto_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phyto_diaz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL chl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL pivotal
      _RL Pc_0
      _RL Pc_0_diaz
      _RL lambda_0
      _RL chl_min
      _RL CtoN
      _RL NO3toN
      _RL HtoC
      _RL O2toN
      _RL CatoN 
      _RL masstoN 
      _RL alpha_photo
      _RL theta_Fe_max_hi
      _RL theta_Fe_max_lo
      _RL gamma_irr_mem
      _RL gamma_DON
      _RL gamma_DOP
      _RL gamma_POM
      _RL k_Fe
      _RL k_Fe_diaz
      _RL k_O2
      _RL k_NO3
      _RL k_PO4
      _RL k_PtoN
      _RL k_FetoN
      _RL kFe_eq_lig_max
      _RL kFe_eq_lig_min
      _RL kFe_eq_lig_Femin
      _RL kFe_eq_lig_irr    
      _RL kFe_org
      _RL kFe_inorg
      _RL PtoN_min
      _RL PtoN_max
      _RL FetoN_min
      _RL FetoN_max
      _RL FetoC_sed
      _RL remin_min
      _RL oxic_min
      _RL ligand
      _RL kappa_eppley
      _RL kappa_eppley_diaz
      _RL kappa_remin
      _RL ca_remin_depth
      _RL phi_DOM
      _RL phi_sm
      _RL phi_lg
      _RL phi_dvm
      _RL sigma_dvm
      _RL wsink0z
      _RL wsink0
      _RL wsinkacc
      _RL parfrac
      _RL alpfe
      _RL k0
      _RL epsln
      LOGICAL QSW_underice


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
