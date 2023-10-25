C     *==========================================================*
C     | DIC_VARS.h
C     | o Abiotic Carbon Variables
C     *==========================================================*

C     AtmospCO2   :: Atmospheric pCO2 (atm).
C     AtmosP      :: Atmospheric Pressure loaded from file (atm).
C     pH          :: surface ocean pH (acidity) for pCO2 calculations.
C     pCO2        :: surface ocean partial pressure of CO2 (atm).
C     FluxCO2     :: Air-sea flux of CO2 (mol/m2/s).
C     wind        :: Wind speed loaded from file for air-sea
C                       flux calculations (m/s).
C     fIce        :: Fraction of sea ice cover loaded from file
C                       (or set by thice/seaice)
C                       for air-sea flux calculations.
C     Kwexch_Pre  :: Common part of piston velocity used for
C                       for air-sea CO2 and O2 flux calculations.
C     silicaSurf  :: Surface ocean concentration of silicate for
C                       pCO2 calculations. Read in from file (mol/m3).
C     zca         :: Scale depth for CaCO3 remineralization [m]
C     useCalciteSaturation :: Dissolve calcium carbonate only below saturation
C                             horizon (needs DIC_CALCITE_SAT to be defined)
C     calcOmegaCalciteFreq :: Frequency at which 3D calcite saturation state,
C                             omegaC, is updated (s).
C     nIterCO3    :: Number of iterations of the Follows 3D pH solver to
C                       calculate deep carbonate ion concenetration (no
C                       effect when using the Munhoven/SolveSapHe solvers).
C     calciteDissolRate :: Rate constant (%) for calcite dissolution
C                       from Keir (1980) Geochem. Cosmochem. Acta.
C     calciteDissolExp  :: Rate exponent for calcite dissolution
C                       from Keir (1980) Geochem. Cosmochem. Acta.
C     WsinkPIC    :: sinking speed (m/s) of particulate inorganic carbon for
C                    calculation of calcite dissolution through the watercolumn
C     selectCalciteBottomRemin :: to either remineralize in bottom or top layer
C                       if flux reaches bottom layer; =0 : bottom, =1 : top
C  selectCalciteDissolution :: flag to control calcite dissolution rate method:
C          =0 : Constant dissolution rate;
C          =1 : Follows (default) ;
C          =2 : Keir (1980) Geochem. Cosmochem. Acta. ;
C          =3 : Naviaux et al. 2019, Marine Chemistry
C     pH_isLoaded(1) :: = T when surface pH is loaded from pickup file
C     pH_isLoaded(2) :: = T when   3-D   pH is loaded from pickup file

       COMMON /CARBON_NEEDS/
     &              AtmospCO2, AtmosP, pH, pCO2, FluxCO2,
     &              wind, fIce, Kwexch_Pre, silicaSurf,
     &              calciteDissolRate, calciteDissolExp,
     &              calcOmegaCalciteFreq, zca,
     &              WsinkPIC, selectCalciteBottomRemin,
     &              selectCalciteDissolution, nIterCO3,
     &              useCalciteSaturation, pH_isLoaded

      _RL  AtmospCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  AtmosP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  pCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  pH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FluxCO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wind(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  fIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Kwexch_Pre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  silicaSurf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  calciteDissolRate(2), calciteDissolExp(2)
      _RL  calcOmegaCalciteFreq
      _RL  zca
      _RL  WsinkPIC
      INTEGER selectCalciteBottomRemin
      INTEGER selectCalciteDissolution
      INTEGER nIterCO3
      LOGICAL useCalciteSaturation
      LOGICAL pH_isLoaded(2)

#ifdef DIC_CALCITE_SAT
C     silicaDeep  :: 3D-field of silicate concentration for pH and
C                    carbonate calculations. Read in from file (mol/m3).
C     omegaC      :: Local saturation state with respect to calcite
       COMMON /DIC_CALCITE_SAT_FIELDS/
     &              silicaDeep, omegaC
      _RL  silicaDeep(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  omegaC    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

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

       COMMON /CARBON_CHEM/
     &                     ak0,ak1,ak2,akw,akb,aks,akf,
     &                     ak1p,ak2p,ak3p,aksi, fugf,
     &                     ff,ft,st,bt, Ksp_TP_Calc

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
C Fugacity Factor added by Val Bennington Nov. 2010
      _RL  fugf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ft(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  st(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  bt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Ksp_TP_Calc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

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

       COMMON /CARBONCHEM_SOLVESAPHE/
     &                     cat, akn, akhs, aphscale, Ksp_TP_Arag,
     &                     at_maxniter,
     &                     selectBTconst,selectFTconst,
     &                     selectHFconst,selectK1K2const,
     &                     selectPHsolver

      _RL  cat(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  akn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  akhs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aphscale(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Ksp_TP_Arag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER at_maxniter
      INTEGER selectBTconst
      INTEGER selectFTconst
      INTEGER selectHFconst
      INTEGER selectK1K2const
      INTEGER selectPHsolver
#endif /* CARBONCHEM_SOLVESAPHE */

C Store dissociation coefficients for O2 solubility
       COMMON /OXYGEN_CHEM/
     &              oA0,oA1,oA2,oA3,oA4,oA5,
     &              oB0,oB1,oB2,oB3,
     &              oC0
      _RL oA0,oA1,oA2,oA3,oA4,oA5
      _RL oB0,oB1,oB2,oB3
      _RL oC0

C Store surface mean tracer values for virtual FW fluxes
C permil : is conversion factor for mol/m3 to mol/kg
C          assumes uniform (surface) density
C Pa2Atm : for conversion of atmospheric pressure
C          when coming from atmospheric model
       COMMON /GLOBAL_SURF_MEAN/
     &                          gsm_alk,gsm_s,gsm_t,gsm_dic,
     &                          gsm_c14,permil,Pa2Atm
      _RL  gsm_alk
      _RL  gsm_s
      _RL  gsm_t
      _RL  gsm_DIC
      _RL  gsm_C14
      _RL  permil
      _RL  Pa2Atm

C Schmidt number coefficients for air sea gas exchange of
C   carbon and oxygen
      COMMON /DIC_SCHMIDT_NO/
     &                    sca1, sca2, sca3, sca4,
     &                    sox1, sox2, sox3, sox4
      _RL  sca1
      _RL  sca2
      _RL  sca3
      _RL  sca4
      _RL  sox1
      _RL  sox2
      _RL  sox3
      _RL  sox4

C--   COMMON /DIC_FILENAMES/
C  DIC_windFile    :: file name of wind speeds
C  DIC_atmospFile  :: file name of atmospheric pressure
C  DIC_iceFile     :: file name of seaice fraction
C  DIC_ironFile    :: file name of aeolian iron flux
C  DIC_silicaFile  :: file name of surface silica
C  DIC_deepSilicaFile  :: file name of 3D silica fields
C  DIC_parFile     :: file name of photosynthetically available radiation (PAR)
C  DIC_chlaFile    :: file name of chlorophyll climatology
C  DIC_forcingPeriod :: periodic forcing parameter specific for dic (seconds)
C  DIC_forcingCycle  :: periodic forcing parameter specific for dic (seconds)
C  dic_int*          :: Handle the atmospheric boundary condition for pCO2
C  dic_int1:
C          0=use default pCO2 (278.d-6)
C          1=use constant value of dic_pCO2 read in from data.dic
C          2=read in from file named co2atmos.dat
C          3=use well mixed atmospheric box (can use dic_pCO2 as initial value)
C  dic_int2          :: number pCO2 entries to read from file
C  dic_int3          :: start timestep
C  dic_int4          :: timestep between file entries
C  dic_pCO2          :: atmospheric pCO2 to be read from data.dic
      COMMON /DIC_FILENAMES/
     &        DIC_windFile, DIC_atmospFile, DIC_silicaFile,
     &        DIC_deepSilicaFile,
     &        DIC_iceFile, DIC_parFile,
     &        DIC_chlaFile, DIC_ironFile,
     &        DIC_forcingPeriod, DIC_forcingCycle,
     &        dic_pCO2, dic_int1, dic_int2, dic_int3, dic_int4

      CHARACTER*(MAX_LEN_FNAM) DIC_windFile
      CHARACTER*(MAX_LEN_FNAM) DIC_atmospFile
      CHARACTER*(MAX_LEN_FNAM) DIC_silicaFile
      CHARACTER*(MAX_LEN_FNAM) DIC_deepSilicaFile
      CHARACTER*(MAX_LEN_FNAM) DIC_iceFile
      CHARACTER*(MAX_LEN_FNAM) DIC_parFile
      CHARACTER*(MAX_LEN_FNAM) DIC_chlaFile
      CHARACTER*(MAX_LEN_FNAM) DIC_ironFile
      _RL     DIC_forcingPeriod
      _RL     DIC_forcingCycle
      _RL     dic_pCO2
      INTEGER dic_int1
      INTEGER dic_int2
      INTEGER dic_int3
      INTEGER dic_int4

#ifdef DIC_BIOTIC
C     *==========================================================*
C     | o Biological Carbon Variables
C     *==========================================================*

C Values for biotic biogeochemistry
C     (many set in data.dic, defaults to values in dic_readparms.F)
C  par              :: photosynthetically active radiation (light available
C                       for phytoplankton growth) [W/m2]
C  alpha            :: maximum rate of biological activity [mol P/m3/s]
C  alphaUniform     :: read in alphaUniform to fill in 2d array alpha
C  rain_ratio       :: inorganic/organic particulate carbon rain ratio (PIC/POC)
C  rainRatioUniform :: read in rainRatioUniform to fill in 2d array rain_ratio
C  InputFe          :: aeolian deposition of TOTAL IRON in dust [mol/m2/s]
C  CHL              :: chlorophyll climatology data for self-shading effect [mg/m3]
C  Kpo4, KFE, lit0  :: half saturation constants for phosphate [mol P/m3],
C                       iron [mol Fe/m3] and light [W/m2]
C  DOPfraction      :: fraction of new production going to DOP
C  zcrit            :: Minimum Depth (m) over which biological activity is computed
C  nlev             :: level index just below -zcrit
C  KRemin           :: remineralization power law coeffient
C  KDOPremin        :: DOP remineralization rate [1/s]
C  R_op, R_cp       :: stochiometric ratios of nutrients
C  R_NP, R_FeP      :: stochiometric ratios of nutrients
C                       (assumption of stoichometry of plankton and
C                        particulate  and dissolved organic matter)
C  O2crit           :: critical oxygen level [mol/m3]
C  alpfe            :: solubility of aeolian fe [fraction]
C  KScav            :: iron scavenging rate [1/s]
C  ligand_stab      :: ligand-free iron stability constant [m3/mol]
C  ligand_tot       :: uniform, invariant total free ligand conc [mol/m3]
C  freefemax        :: max soluble free iron [mol/m3]
C  fesedflux_pcm    :: ratio of sediment iron to sinking organic matter
C  FeIntSec         :: Sediment Fe flux, intersect value in:
C                              Fe_flux = fesedflux_pcm*pflux + FeIntSec
C  parfrac          :: fraction of Qsw that is PAR
C  k0               :: light attentuation coefficient of water [1/m]
C  kchl             :: light attentuation coefficient of chlorophyll [m2/mg]
C  alphamax,alphamin :: not used (legacy adjoint param)
C  calpha            :: not used (legacy adjoint param)
C  crain_ratio       :: not used (legacy adjoint param)
C  cInputFe          :: not used (legacy adjoint param)
C  calpfe            :: not used (legacy adjoint param)
C  feload            :: not used (legacy adjoint param)
C  cfeload           :: not used (legacy adjoint param)
C  QSW_underice      :: is Qsw is masked by ice fraction?

      COMMON /BIOTIC_NEEDS/
     &     par, alpha, rain_ratio, InputFe, CHL,
     &     Kpo4, DOPfraction, zcrit, KRemin,
     &     KDOPremin, R_op, R_cp, R_NP, R_FeP,
     &     O2crit, alpfe, KScav, ligand_stab, ligand_tot, KFE,
     &     freefemax, fesedflux_pcm, FeIntSec,
     &     parfrac, k0, kchl, lit0,
     &     alphaUniform, rainRatioUniform,
     &     nlev, QSW_underice
c    &     alphamax, alphamin,
c    &     calpha, crain_ratio, cInputFe, calpfe, feload, cfeload,

      _RL par(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL alpha(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rain_ratio(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL InputFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CHL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Kpo4
      _RL DOPfraction
      _RL zcrit
      _RL KRemin
      _RL KDOPremin
      _RL R_op
      _RL R_cp
      _RL R_NP
      _RL R_FeP
      _RL O2crit
      _RL alpfe
      _RL fesedflux_pcm
      _RL FeIntSec
      _RL KScav
      _RL ligand_stab
      _RL ligand_tot
      _RL KFe
      _RL freefemax
      _RL k0, kchl, parfrac, lit0
      _RL alphaUniform
      _RL rainRatioUniform
c     _RL alphamax, alphamin
c     _RL calpha
c     _RL crain_ratio
c     _RL cInputFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL calpfe
c     _RL cfeload
c     _RL feload(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER nlev
      LOGICAL QSW_underice

#ifdef ALLOW_TIMEAVE
C For time-averages output using TIMEAVE pkg:
C Note: Although some of the Time-Ave arrays are for A-Biotic fields, all these
C       arrays are only used + filled within ifdef DIC_BIOTIC code, so we keep
C       them only if DIC_BIOTIC is defined.
C  BIOave     :: biological productivity [mol P/m3/s]
C  CARave     :: carbonate changes due to biological productivity
C                 and remineralization [mol C/m3/s]
C  SURave     :: tendency of DIC due to air-sea exchange
C                 and virtual flux [mol C/m3/s]
C  SUROave    :: tendency of O2 due to air-sea exchange [mol O2/m3/s]
C  pCO2ave    :: surface ocean pCO2 [uatm]
C  pHave      :: surface ocean pH
C  fluxCO2ave :: Air-sea flux of CO2 [mol C/m2/s]
C  pfluxave   :: changes to PO4 due to flux and remineralization [mol P/m3/s]
C  epfluxave  :: export flux of PO4 through each layer [mol P/m2/s]
C  cfluxave   :: carbonate changes due to flux and remineralization [mol C/m3/s]
C  DIC_timeAve  :: period over which DIC averages are calculated [s]

      COMMON /BIOTIC_TAVE/
     &     BIOave, CARave, SURave, SUROave, pCO2ave, pHave,
     &     fluxCO2ave, pfluxave, epfluxave, cfluxave,
     &     DIC_timeAve

      _RL BIOave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CARave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SURave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SUROave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pCO2ave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pHave     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fluxCO2ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pfluxave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL epfluxave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL cfluxave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL DIC_timeAve(nSx,nSy)
#endif /* ALLOW_TIMEAVE */

#endif /* DIC_BIOTIC */
