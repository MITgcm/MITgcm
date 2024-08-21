!-------------------------------------------------------------------------
! indices for state variables
!-------------------------------------------------------------------------
  integer, parameter :: iSPc=1,iSPn=2,iSPp=3,
 &                      iTRc=4,iTRn=5,iTRp=6,
 &                      iUNc=7,iUNn=8,iUNp=9,
 &                      iBAc=10,iBAn=11,iBAp=12,
 &                      iPRTc=13,iPRTn=14,iPRTp=15,
 &                      iMZc=16,iMZn=17,iMZp=18,
 &                      iLDOMc=19,iLDOMn=20,iLDOMp=21,
 &                      iSDOMc=22,iSDOMn=23,iSDOMp=24,
 &                      iDETc=25,iDETn=26,iDETp=27,
 &                      iNH4=28,iNO3=29,iPO4=30,
 &                      iSPchl=31, iTRchl = 32, iUNchl = 33,
 &                      iDVMZc=34, iDVMZn=35, iDVMZp=36

!-------------------------------------------------------------------------
! ***Required*** numstatevar - number of ecosystem scalars.  Should be
! equal to maximum index above.
!-------------------------------------------------------------------------
  integer, parameter :: numstatevar=36

!-------------------------------------------------------------------------
! indices for diagnostic variables
!-------------------------------------------------------------------------
  integer, parameter :: iPP=1, iprBAc=2, &
                   igrowSPc=3, igrowSPnh4=4, igrowSPno3=5, &
                   igrowSPn=6, igrowSPp=7, &
                   iexcrSP_1c=8, iexcrSP_1n=9, iexcrSP_1p=10, &
                   iexcrSP_2c=11, iexcrSP_2n=12, iexcrSP_2p=13, &
                   ipomSPc=14, ipomSPn=15, ipomSPp=16, &
                   igrazSPc=17, igrazSPn=18, igrazSPp=19, &
                   igrowTRc=20, igrowTRnh4=21, igrowTRno3=22, igrowTRnf=23, &
                   igrowTRn=24, igrowTRpo4=25, ipickTRpo4=26, igrowTRp=27, &
                   iexcrTR_1c=28, iexcrTR_1n=29, iexcrTR_1p=30, iexcrTR_nh4=31, &
                   iexcrTR_2c=32, iexcrTR_2n=33, iexcrTR_2p=34, &
                   ipomTRc=35, ipomTRn=36, ipomTRp=37, &
                   igrazTRc=38, igrazTRn=39, igrazTRp=40, &
                   igrowUNc=41, igrowUNnh4=42, igrowUNno3=43, igrowUNnf=44, &
                   igrowUNn=45, igrowUNp=46, &
                   iexcrUN_1c=47, iexcrUN_1n=48, iexcrUN_1p=49, iexcrUN_nh4=50, &
                   iexcrUN_2c=51, iexcrUN_2n=52, iexcrUN_2p=53, &
                   ipomUNc=54, ipomUNn=55, ipomUNp=56, &
                   igrazUNc=57, igrazUNn=58, igrazUNp=59, &
                   igrowBAldoc=60, igrowBAldon=61, igrowBAldop=62, &
                   igrowBAsdoc=63, igrowBAsdon=64, igrowBAsdop=65, &
                   igrowBAnh4=66, igrowBAno3=67, igrowBApo4=68, &
                   igrowBAc=69, igrowBAn=70, igrowBAp=71, irespBA=72, &
                   irefrBAc=73, irefrBAn=74, irefrBAp=75, &
                   iexcrBAc=76, iexcrBAn=77, iexcrBAp=78, &
                   iremiBAn=79, iremiBAp=80, &
                   igrazBAc=81, igrazBAn=82, igrazBAp=83, &
                   imortBAc=84, imortBAn=85, imortBAp=86, &
                   ifluxBAnh4=87, ifluxBApo4=88, &
                   igrowPRTc=89, igrowPRTn=90, igrowPRTp=91, irespPRT=92, &
                   iexcrPRTldomc=93, iexcrPRTldomn=94, iexcrPRTldomp=95, &
                   iexcrPRTsdomc=96, iexcrPRTsdomn=97, iexcrPRTsdomp=98, &
                   iexcrPRTsdom2c=99, iexcrPRTsdom2n=100, iexcrPRTsdom2p=101, &
                   iremiPRTn=102, iremiPRTp=103, &
                   ipomPRTc=104, ipomPRTn=105, ipomPRTp=106, &
                   igrazPRTc=107, igrazPRTn=108, igrazPRTp=109, &
                   igrowMZc=110, igrowMZn=111, igrowMZp=112, &
                   irespSP=113, irespTR=114, irespUN=115, irespMZ=116, &
                   iexcrMZldomc=117, iexcrMZldomn=118, iexcrMZldomp=119, &
                   iexcrMZsdomc=120, iexcrMZsdomn=121, iexcrMZsdomp=122, &
                   iexcrMZsdom2c=123, iexcrMZsdom2n=124, iexcrMZsdom2p=125, &
                   iremiMZn=126, iremiMZp=127, &
                   irefrMZc=128, irefrMZn=129, irefrMZp=130, &
                   ipomMZc=131, ipomMZn=132, ipomMZp=133, &
                   iremvMZc=134, iremvMZn=135, iremvMZp=136, &
                   ipomHZc=137, ipomHZn=138, ipomHZp=139, &
                   iexcrHZsdomc=140, iexcrHZsdomn=141, iexcrHZsdomp=142, &
                   iremiHZn=143, iremiHZp=144, &
                   idisDETc=145, idisDETn=146, idisDETp=147, &
                   initrf=148, &
                   irefrSDOMc=149, irefrSDOMn=150, irefrSDOMp=151, &
                   iexportc=152,iexportn=153,iexportp=154,&
                                   igrowBApoc=155,igrowBApon=156,igrowBApop=157,&
                                   igrazDETc=158,igrazDETn=159,igrazDETp=160,iHETres=161

!-------------------------------------------------------------------------
! ***Required*** numdiagvar - number of diagnostic variables.  Must be
! equal to the maximum index above
!-------------------------------------------------------------------------
  integer, parameter :: numdiagvar=161

!-------------------------------------------------------------------------
! indices for ecosystem parameters
!-------------------------------------------------------------------------
  integer, parameter :: &
       iae          = 1, &
       imu_SP          = iae    + 1, &
       ialpha_SP       = imu_SP          + 1, &
       ia_SP       = ialpha_SP          + 1, &
       iv_SPn     = ia_SP             + 1, &
       ik_nh4SP        = iv_SPn       + 1, &
       ik_no3SP        = ik_nh4SP        + 1, &
       iv_SPp            = ik_no3SP        + 1, &
       ik_po4SP        = iv_SPp        + 1, &
       izeta               = ik_po4SP     + 1, &
       itheta             = izeta        + 1, &
       ir_excrSP_1     = itheta        + 1, &
       ir_excrSP_2     = ir_excrSP_1     + 1, &
       ir_pomSP        = ir_excrSP_2     + 1, &
       imu_TR          = ir_pomSP        + 1, &
       ialpha_TR       = imu_TR          + 1, &
       ia_TR       = ialpha_TR         + 1, &
       iv_TRn     = ia_TR              + 1, &
       ik_nh4TR        = iv_TRn       + 1, &
       ik_no3TR        = ik_nh4TR        + 1, &
       iv_TRp             = ik_no3TR       + 1, &
       ik_po4TR        = iv_TRp        + 1, &
       imu_pickTRpo4 = ik_po4TR + 1, &
       izeta_nf            = imu_pickTRpo4        + 1, &
       ir_excrTR_1     = izeta_nf        + 1, &
       ir_excrTR_n   = ir_excrTR_1     + 1, &
       ir_excrTR_2     = ir_excrTR_n   + 1, &
       ir_pomTR        = ir_excrTR_2     + 1, &
       imu_UN          = ir_pomTR        + 1, &
       ialpha_UN       = imu_UN          + 1, &
       ik_DOM        = ialpha_UN        + 1, &
       !ib_SDONlabi     = ik_DOM        + 1, &
       !ib_SDOPlabi     = ib_SDONlabi     + 1, &
       ir_SDOM = ik_DOM + 1, &
       imu_BA          = ir_SDOM     + 1, &
       ib_BAresp       = imu_BA       + 1, &
       ir_BAadju       = ib_BAresp       + 1, &
       ir_BAremi       = ir_BAadju       + 1, &
       ir_BArefr       = ir_BAremi       + 1, &
       if_BAslct = ir_BArefr + 1, &
       ir_BAresp_1 = if_BAslct + 1, &
       ir_BAresp_min = ir_BAresp_1 + 1, &
       ir_BAresp_max = ir_BAresp_min  + 1, &
       ir_BAmort = ir_BAresp_max  + 1, &
       imu_PRT       = ir_BAmort        + 1, &
       ig_sp        = imu_PRT       + 1, &
       ig_ba        = ig_sp       + 1, &
       ir_PRTex        = ig_ba        + 1, &
       if_exPRTldom    = ir_PRTex      + 1, &
       ir_PRTresp_1    = if_exPRTldom    + 1, &
       ir_PRTresp_2    = ir_PRTresp_1    + 1, &
       ir_PRTadju      = ir_PRTresp_2    + 1, &
       ir_PRTremi      = ir_PRTadju      + 1, &
       ir_pomPRT       = ir_PRTremi      + 1, &
       imu_MZ       = ir_pomPRT       + 1, &
       ig_prt        = imu_MZ       + 1, &
       ig_tr         = ig_prt        + 1, &
       ir_MZex         = ig_tr     + 1, &
       if_exMZldom     = ir_MZex         + 1, &
       ir_MZresp_1     = if_exMZldom     + 1, &
       ir_MZresp_2     = ir_MZresp_1     + 1, &
       ir_MZadju       = ir_MZresp_2     + 1, &
       ir_MZremi       = ir_MZadju       + 1, &
       ir_MZpom        = ir_MZremi       + 1, &
       ir_MZrefr       = ir_MZpom        + 1, &
       ir_MZremv       = ir_MZrefr        + 1, &
       if_HZsdom       = ir_MZremv       + 1, &
       if_HZpom        = if_HZsdom       + 1, &
       ir_SDOMrefr     = if_HZpom        + 1, &
       iq_refrDOM_n = ir_SDOMrefr     + 1, &
       iq_refrDOM_p = iq_refrDOM_n + 1, &
       iq_POM_n        = iq_refrDOM_p     + 1, &
       iq_POM_p        = iq_POM_n        + 1, &
       ir_nitrf        = iq_POM_p        + 1, &
       iremin_prf_n    = ir_nitrf        + 1, &
       iremin_prf_p    = iremin_prf_n    + 1, &
       iwnsvo    = iremin_prf_p    + 1, &
       iremin          = iwnsvo    + 1, &
       ik_pom          = iremin    + 1, &
       ig_det          = ik_pom    + 1, &
       if_dvm          = ig_det    + 1

! mwC              molecular weight of Carbon (g/mol)
! mwN              molecular weight of Nitrogen (g/mol)
! mwN              molecular weight of Phosphorus (g/mol)
  double precision, parameter :: mwC=12.0,mwN=14.0,mwP=31

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
C     irr_mem       :: Phyto irradiance memory
C          this is a temporally smoothed field carried between timesteps,
C          to represent photoadaptation.
C   chlsat_locTimWindow(1:2) :: local-time window (in h) for
C          satellite-equivalent chlorophyll diagnostic (and cost)

      COMMON /BIOTIC_NEEDS/
     &                     InputFe,
     &                     omegaC,
     &                     omegaAr,
     &                     irr_mem,
     &                     phyto_lg,
     &                     phyto_sm,
     &                     chl,
     &                     chl_sat,
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
     &                     MLmix_max,
     &                     chlsat_locTimWindow

      _RL InputFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL omegaC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL omegaAr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL irr_mem(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phyto_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phyto_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL chl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL chl_sat(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      _RL chlsat_locTimWindow(2)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
