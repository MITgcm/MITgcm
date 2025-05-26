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
