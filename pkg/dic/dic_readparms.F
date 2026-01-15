#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"
CBOP
C !ROUTINE: DIC_READPARMS
C !INTERFACE: ==========================================================
      SUBROUTINE DIC_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIC_READPARMS
C     | o Initialise and read dic package parameters
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC

C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     iUnit     :: Work variable for IO unit number
C     errCount  :: error counter
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER errCount
#ifndef CARBONCHEM_SOLVESAPHE
      INTEGER selectBTconst, selectFTconst, selectHFconst
      INTEGER selectK1K2const, selectPHsolver
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Abiotic dic parameters:
C permil   :: set carbon mol/m3 <---> mol/kg conversion factor
C             default permil = 1024.5 kg/m3
C Pa2Atm   :: Conversion factor for atmospheric pressure anomaly pLoad (when
C             coupled to atmospheric model) into Atm.
C             Default assumes pLoad in Pascal: 1 Atm = 1.01325e5 Pa = 1013.25 mb
C  selectBTconst :: estimates borate concentration from salinity:
C     =1 :: use default formulation of Uppström (1974)(same as S/R CARBON_COEFFS)
C     =2 :: use new formulation from Lee et al (2010)
C  selectFTconst :: estimates fluoride concentration from salinity:
C     =1 :: use default formulation of Riley (1965) (same as S/R CARBON_COEFFS)
C     =2 :: use new formulation from Culkin (1965)
C  selectHFconst :: sets the first dissociation constant for hydrogen fluoride:
C     =1 :: use default  Dickson and Riley (1979) (same as S/R CARBON_COEFFS)
C     =2 :: use new formulation of Perez and Fraga (1987)
C  selectK1K2const :: sets the 1rst & 2nd dissociation constants of carbonic acid:
C     =1 :: use default formulation of Millero (1995) with data
C            from Mehrbach et al. (1973) (same as S/R CARBON_COEFFS)
C     =2 :: use formulation of Roy et al. (1993)
C     =3 :: use "combination" formulation of Millero (1995)
C     =4 :: use formulation of Luecker et al. (2000)
C     =5 :: use formulation of Millero (2010, Mar. Fresh Wat. Res.)
C     =6 :: use formulation of Waters, Millero, Woosley (2014, Mar. Chem.)
C  selectPHsolver :: sets the pH solver to use:
C     =0 :: use Follows et al., (2006) solver;
C     =1 :: use the GENERAL solver from Munhoven (2013);
C     =2 :: use SEC solver  from Munhoven (2013);
C     =3 :: use FAST solver from Munhoven (2013);
C  useCalciteSaturation :: dissolve calcium carbonate below the calcite
C                          saturation horizon following method by Karsten Friis
C  calcOmegaCalciteFreq :: Frequency that 3d calcite saturation state, omegaC,
C                          is calculated.
C  nIterCO3             :: Number of iterations of the Follows 3D pH solver to
C                          calculate deep carbonate ion concenetration (no
C                          effect when using the Munhoven/SolveSapHe solvers).
C  selectCalciteDissolution :: flag to control calcite dissolution rate method:
C          =0 : Constant dissolution rate
C          =1 : Follows sinking flux (default);
C          =2 : Keir (1980) Geochem. Cosmochem. Acta. ;
C          =3 : Naviaux et al. 2019, Marine Chemistry
C  calciteDissolRate :: Rate constant (%) for calcite dissolution from
C          e.g. Keir (1980) Geochem. Cosmochem. Acta. or Naviaux et al. (2019)
C  calciteDissolExp  :: Rate exponent for calcite dissolution from
C          e.g. Keir (1980) Geochem. Cosmochem. Acta. or Naviaux et al. (2019)
C  WsinkPIC             :: sinking speed (m/s) of particulate inorganic carbon
C                          for calcite dissolution through the watercolumn
C  selectCalciteBottomRemin :: to either remineralize in bottom or top layer
C                          if flux reaches bottom layer: =0 : bottom, =1 : top
C   zca         :: scale depth for CaCO3 remineralization power law (m)
C----

      NAMELIST /ABIOTIC_PARMS/
     &  permil, Pa2Atm,
     &  selectBTconst, selectFTconst,
     &  selectHFconst, selectK1K2const,
     &  selectPHsolver,
     &  useCalciteSaturation, calcOmegaCalciteFreq,
     &  nIterCO3, selectCalciteDissolution,
     &  WsinkPIC, selectCalciteBottomRemin,
     &  calciteDissolRate, calciteDissolExp,
     &  zca

#ifdef DIC_BIOTIC
C-- Biotic dic parameters:
C   DOPfraction :: fraction of new production going to DOP
C   KDOPRemin   :: DOP remineralization rate (1/s) = 1/(6 month)
C   KRemin      :: remin power law coeff
C   zcrit       :: Minimum Depth (m) over which biological activity
C                  is computed --> determines nlev as the indice of the
C                  first layer deeper than -zcrit
C   O2crit      :: critical oxygen level (mol/m3)
C   R_OP, R_CP  :: stochiometric ratios
C   R_NP, R_FeP
CC Parameters for light/nutrient limited bioac
C   parfrac     :: fraction of Qsw that is PAR
C   k0          :: light attentuation coefficient (1/m)
C   lit0        :: half saturation light constant (W/m2)
C   KPO4        :: half saturation phosphate constant (mol/m3)
C   KFE         :: half saturation fe constant (mol/m3)
CC Iron chemisty values
C   alpfe       :: solubility of aeolian fe
C   fesedflux_pcm :: ratio of sediment iron to sinking organic matter
C   FeIntSec    :: y-axis crossing for Fe_flux = fesedflux_pcm*pflux + FeIntSec
C   freefemax   :: max solubility of free iron (mol/m3)
CC Control variables
C   KScav       :: iron scavenging rate QQ
C   ligand_stab :: ligand-free iron stability constant (m3/mol)
C   ligand_tot  :: total free ligand  (mol/m3)
C   alpha       :: timescale for biological activity
C                  read in alphaUniform and filled in 2d array alpha
C   rain_ratio  :: inorganic/organic carbon rain ratio
C                  read in rainRatioUniform and filled in 2d array rain_ratio

      NAMELIST /BIOTIC_PARMS/
     & DOPfraction, KDOPRemin, KRemin, zcrit,
     & O2crit, R_OP, R_CP, R_NP, R_FeP,
     & parfrac, k0, lit0, KPO4, KFE, kchl,
     & alpfe, fesedflux_pcm, FeIntSec, freefemax,
     & KScav, ligand_stab, ligand_tot,
     & alphaUniform, rainRatioUniform
#endif

      NAMELIST /DIC_FORCING/
     &          DIC_windFile, DIC_atmospFile, DIC_silicaFile,
     &          DIC_deepSilicaFile, DIC_iceFile, DIC_parFile,
     &          DIC_chlaFile, DIC_ironFile,
     &          DIC_forcingPeriod, DIC_forcingCycle,
     &          dic_int1, dic_int2, dic_int3, dic_int4, dic_pCO2

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)
       errCount = 0

       permil      = 1. _d 0 / 1024.5 _d 0
       Pa2Atm      = 1.01325 _d 5
       selectBTconst   = UNSET_I
       selectFTconst   = UNSET_I
       selectHFconst   = UNSET_I
       selectK1K2const = UNSET_I
       selectPHsolver  = UNSET_I
       useCalciteSaturation = .FALSE.
C-- Parameters used in Calcite Saturation calculation (useCalciteSaturation=T):
C set calcite saturation calculation to every timestep - could be expensive
       calcOmegaCalciteFreq = deltaTClock
C number of iterations for the Follows 3d pH solver
       nIterCO3             = 10
C flag to either remineralize in bottom or top layer if flux
C reaches bottom layer 0=bottom, 1=top
       selectCalciteBottomRemin = 1
C selectCalciteDissolution :: flag to control calcite dissolution rate method:
       selectCalciteDissolution = 1
C Calcite dissolution rate constant (%/day, defaults set later if not read in)
       calciteDissolRate(1) = UNSET_RL
       calciteDissolRate(2) = UNSET_RL
C Calcite dissolution rate exponent (defaults set later if not read in)
       calciteDissolExp(1)  = UNSET_RL
       calciteDissolExp(2)  = UNSET_RL
C set nominal particulate sinking rate (m/s)
       WsinkPIC             = 100. _d 0/86400. _d 0
C-- setting default values for Calcite Saturation params ends here.
       zca                  = 3500. _d 0

#ifdef DIC_BIOTIC
       DOPfraction = 0.67 _d 0
       KDOPRemin   = 1. _d 0/(6. _d 0*30. _d 0*86400. _d 0)
       KRemin      = 0.9 _d 0
       zcrit       = 500. _d 0
       O2crit      = 4. _d -3
       R_OP        =-170. _d 0
       R_CP        = 117. _d 0
       R_NP        = 16. _d 0
       R_FeP       = 0.000468 _d 0
       parfrac     = 0.4 _d 0
       k0          = 0.02 _d 0
       kchl        = 0.02 _d 0
       lit0        = 30. _d 0
       KPO4        = 5. _d -4
       KFE         = 1.2 _d -7
       alpfe       = 0.01 _d 0
       fesedflux_pcm = 6.8 _d -4 * 106. _d 0
       FeIntSec    = 0.5 _d -6 / 86400. _d 0
       freefemax   = 3. _d -7
       KScav       = 0.19 _d 0/(360. _d 0*86400. _d 0)
       ligand_stab = 1. _d 8
       ligand_tot  = 1. _d -6
       alphaUniform     = 2. _d -3/(360. _d 0 * 86400. _d 0)
       rainRatioUniform = 7. _d -2
#endif
       DIC_windFile   = ' '
       DIC_atmospFile = ' '
       DIC_silicaFile = ' '
       DIC_deepSilicaFile = ' '
       DIC_iceFile    = ' '
       DIC_parFile    = ' '
       DIC_chlaFile   = ' '
       DIC_ironFile   = ' '
#ifdef GCHEM_ALLOW_FFIELDS
       dic_int1    = -1
#else
       dic_int1    = 0
#endif
       dic_int2    = 0
       dic_int3    = 0
       dic_int4    = 0
       dic_pCO2    = 278. _d -6
C default periodic forcing to same as for physics
       DIC_forcingPeriod = externForcingPeriod
       DIC_forcingCycle  = externForcingCycle

      WRITE(msgBuf,'(A)') ' DIC_READPARMS: opening data.dic'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data.dic', 'DIC_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Abiotic parameters
      READ(UNIT=iUnit,NML=ABIOTIC_PARMS)

#ifdef DIC_BIOTIC
C-    Biotic parameters
      READ(UNIT=iUnit,NML=BIOTIC_PARMS)
#endif

C-    forcing filenames and parameters
      READ(UNIT=iUnit,NML=DIC_FORCING)

      WRITE(msgBuf,'(A)')
     &   ' DIC_READPARMS: finished reading data.dic'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    derive other parameters:

C-    set parameter default values
      IF ( selectBTconst  .EQ.UNSET_I ) selectBTconst   = 1
      IF ( selectFTconst  .EQ.UNSET_I ) selectFTconst   = 1
      IF ( selectHFconst  .EQ.UNSET_I ) selectHFconst   = 1
      IF ( selectK1K2const.EQ.UNSET_I ) selectK1K2const = 1
      IF ( selectPHsolver .EQ.UNSET_I ) selectPHsolver  = 0

      IF ( selectCalciteDissolution .EQ. 3 ) THEN
C     Calcite dissolution rate constant for the Naviaux et al. 2019 scheme
C      Default values are slightly different from the paper, but consistent with
C      the Darwin model. Values include a ~1e5 cm2/mol conversion factor for
C      specific surface area (SSA) of CaCO3 (e.g. benthic forams or synthetic
C      calcite), for example, Subhas et al. 2018 (Marine Chemistry).
        IF ( calciteDissolRate(1).EQ.UNSET_RL )
     &       calciteDissolRate(1) = 5.22 _d -9
        IF ( calciteDissolRate(2).EQ.UNSET_RL )
     &       calciteDissolRate(2) = 1.65 _d -5
C     Calcite dissolution rate exponent for the Naviaux et al. 2019 scheme
        IF ( calciteDissolExp(1).EQ.UNSET_RL )
     &       calciteDissolExp(1) = 0.11 _d 0
        IF ( calciteDissolExp(2).EQ.UNSET_RL )
     &       calciteDissolExp(2) = 4.76 _d 0
      ELSE
C     use second set of Coeff only if select case =3
       calciteDissolRate(2) = 0.0 _d 0
       calciteDissolExp(2)  = 0.0 _d 0
C     set default for other cases:
       IF ( selectCalciteDissolution .EQ. 0 ) THEN
C     Calcite dissolution rate constant
        IF ( calciteDissolRate(1).EQ.UNSET_RL )
     &       calciteDissolRate(1) = 1 _d 0/(360 _d 0*86400.0 _d 0)
C     Calcite dissolution rate exponent not used for constant dissolution
        calciteDissolExp(1) = 0.0 _d 0
       ELSE
C     Calcite dissolution rate constant (%/day) for Keir dissolution
        IF ( calciteDissolRate(1).EQ.UNSET_RL )
     &       calciteDissolRate(1) = 7.177 _d 0
C     Calcite dissolution rate exponent for Keir dissolution (4.2 for aragonite)
        IF ( calciteDissolExp(1).EQ.UNSET_RL )
     &       calciteDissolExp(1) = 4.54 _d 0
       ENDIF
      ENDIF

#ifdef DIC_BIOTIC
       QSW_underice = .FALSE.
#ifdef USE_QSW_UNDERICE
       QSW_underice = .TRUE.
#elif (defined (USE_QSW))
C if using Qsw and seaice, then ice fraction is already
C taken into account
       IF ( useSEAICE ) QSW_underice = .TRUE.
       IF ( useThSIce ) QSW_underice = .TRUE.
#endif
#endif /* DIC_BIOTIC */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print out parameter values :

      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// DIC package parameters :'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)

#ifdef CARBONCHEM_SOLVESAPHE
C Record the solver to calculate pH and evaluate surface ocean pCO2
      IF ( selectPHsolver.GT.0 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Munhoven (2013) Solvesaphe for pH/pCO2'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectPHsolver.EQ.0 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Follows et al. (2006) for pH/pCO2'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
#endif /* CARBONCHEM_SOLVESAPHE */

C Record which carbonate coefficients are used and which pH/pCO2 solver
#ifdef CARBONCHEM_SOLVESAPHE
      WRITE(msgBuf,'(A)')
     &  'Using Munhoven (2013) Solvesaphe carbon coefficients'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
C Munhoven (2013)'s "Solvesaphe" coefficients have several options:
      IF ( selectK1K2const.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Millero (1995)/Mehrbach K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Roy et al. (1993) K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.3 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Millero (1995) "consensus" K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.4 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Luecker et al. (2000) K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.5 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Millero et al. (2010) K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectK1K2const.EQ.6 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Waters et al. (2014) K1 and K2 coefficients'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF

      IF ( selectHFconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Dickson and Riley (1979) KF coefficient'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectHFconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Perez and Fraga (1987) KF coefficient'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF

      IF ( selectBTconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     & 'Using Uppstrom (1974) BT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectBTconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Lee et al (2010) BT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF

      IF ( selectFTconst.EQ.1 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Riley (1965) FT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ELSEIF ( selectBTconst.EQ.2 ) THEN
         WRITE(msgBuf,'(A)')
     &  'Using Culkin (1965) FT estimation from salinity'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
#else /* CARBONCHEM_SOLVESAPHE */
C OCMIP2 Coefficients from S/R CARBON_COEFFS in CARBON_CHEM.F
      WRITE(msgBuf,'(A)')
     &  'Using Millero (1995)/Mehrbach K1 and K2 coefficients'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)')
     &  'Using Dickson and Riley (1979) KF coefficient'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)')
     &  'Using Uppstrom (1974) BT estimation from salinity'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(A)')
     &  'Using Riley (1965) FT estimation from salinity'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
#endif /* CARBONCHEM_SOLVESAPHE */

C- namelist ABIOTIC_PARMS
       CALL WRITE_0D_RL( permil, INDEX_NONE,'permil =',
     &  ' /* Ref. density to convert mol/m3 to mol/kg */')
       CALL WRITE_0D_RL( Pa2Atm, INDEX_NONE,'Pa2Atm =',
     &  ' /* Atmosph. pressure conversion coeff (to Atm) */')
       CALL WRITE_0D_RL( zca, INDEX_NONE,'zca =',
     &  ' /* Scale depth for CaCO3 remineralization (m) */')
       CALL WRITE_0D_L( useCalciteSaturation, INDEX_NONE,
     &  'useCalciteSaturation  =',
     &  '  /* Flag for omegaC calculation on/off */')
      IF ( useCalciteSaturation ) THEN
       CALL WRITE_0D_RL( calcOmegaCalciteFreq, INDEX_NONE,
     &  'calcOmegaCalciteFreq =',
     &  ' /* Frequency of calcite saturation calculation (s) */')
       IF ( selectCalciteDissolution .LT. 3 ) THEN
        IF ( selectCalciteDissolution .EQ. 0 )
     &  WRITE(msgBuf,'(A)') 'Using Constant dissolution rate method'
        IF ( selectCalciteDissolution .EQ. 1 )
     &  WRITE(msgBuf,'(A)') 'Using Follows default dissolution method'
        IF ( selectCalciteDissolution .EQ. 2 )
     &  WRITE(msgBuf,'(A)') 'Using Keir (1980) dissolution method'
        IF ( selectCalciteDissolution .GE. 0 ) THEN
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
         CALL WRITE_0D_RL( calciteDissolRate(1), INDEX_NONE,
     &   'calciteDissolRate =',
     &   ' /* Rate constant for calcite dissolution (%/day) */')
        ENDIF
        IF ( selectCalciteDissolution .GE. 1  ) THEN
         CALL WRITE_0D_RL( calciteDissolExp(1), INDEX_NONE,
     &   'calciteDissolExp = ',
     &   ' /* Rate exponent for calcite dissolution */')
        ENDIF
       ELSEIF ( selectCalciteDissolution .EQ. 3 ) THEN
        WRITE(msgBuf,'(A)')
     &               'Using Naviaux et al. (2019) dissolution rates'
        CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
        CALL WRITE_0D_RL( calciteDissolRate(1), INDEX_NONE,
     &   'calciteDissolRate(1) =',
     &   ' /* Rate constant for dissolution (%/day), omega > 0.8272 */')
        CALL WRITE_0D_RL( calciteDissolRate(2), INDEX_NONE,
     &   'calciteDissolRate(2) =',
     &   ' /* Rate constant for dissolution (%/day), omega < 0.8272 */')
        CALL WRITE_0D_RL( calciteDissolExp(1), INDEX_NONE,
     &   'calciteDissolExp(1) = ',
     &   ' /* Rate exponent for dissolution, omega > 0.8272 */')
        CALL WRITE_0D_RL( calciteDissolExp(2), INDEX_NONE,
     &   'calciteDissolExp(2) = ',
     &   ' /* Rate exponent for dissolution, omega < 0.8272 */')
       ENDIF
       CALL WRITE_0D_RL( WsinkPIC, INDEX_NONE, 'WsinkPIC =',
     &  ' /* Sinking speed of particulate inorganic carbon (m/s) */')
       CALL WRITE_0D_I( selectCalciteBottomRemin, INDEX_NONE,
     &  'selectCalciteBottomRemin =',
     &  ' /* Remineralize CO3 bottom flux: =0: here, =1: top layer */')
      ENDIF

#ifdef DIC_BIOTIC
C- namelist BIOTIC_PARMS
       CALL WRITE_0D_RL( DOPfraction, INDEX_NONE,'DOPfraction =',
     &  ' /* Fraction of new production going to DOP */')
       CALL WRITE_0D_RL( KDOPRemin, INDEX_NONE,'KDOPRemin =',
     &  ' /* DOP remineralization rate (1/s) */')
       CALL WRITE_0D_RL( KRemin, INDEX_NONE,'KRemin =',
     &  ' /* Remin power law coeff. */')
       CALL WRITE_0D_RL( zcrit, INDEX_NONE,'zcrit =',
     &  ' /* Minimum depth for biological activity (m) */')
       CALL WRITE_0D_RL( O2crit, INDEX_NONE,'O2crit =',
     &  ' /* Critical oxygen level (mol/m3) */')
       CALL WRITE_0D_RL( R_OP, INDEX_NONE,'R_OP =',
     &  ' /* Stochiometric ratio R_OP */')
       CALL WRITE_0D_RL( R_CP, INDEX_NONE,'R_CP =',
     &  ' /* Stochiometric ratio R_CP */')
       CALL WRITE_0D_RL( R_NP, INDEX_NONE,'R_NP =',
     &  ' /* Stochiometric ratio R_NP */')
       CALL WRITE_0D_RL( R_FeP, INDEX_NONE,'R_FeP =',
     &  ' /* Stochiometric ratio R_FeP */')
       CALL WRITE_0D_RL( parfrac, INDEX_NONE,'parfrac =',
     &  ' /* Fraction of Qsw that is PAR */')
       CALL WRITE_0D_RL( k0, INDEX_NONE,'k0 =',
     &  ' /* Light attentuation coefficient, water (1/m) */')
       CALL WRITE_0D_RL( kchl, INDEX_NONE,'kchl =',
     &  ' /* Light attentuation coefficient, chlorophyll (m2/mg) */')
       CALL WRITE_0D_RL( lit0, INDEX_NONE,'lit0 =',
     &  ' /* Half saturation light constant (W/m2) */')
       CALL WRITE_0D_RL( KPO4, INDEX_NONE,'KPO4 =',
     &  ' /* Half saturation phosphate constant (mol/m3) */')
       CALL WRITE_0D_RL( KFE, INDEX_NONE,'KFE =',
     &  ' /* Half saturation fe constant (mol/m3) */')
       CALL WRITE_0D_RL( alpfe, INDEX_NONE,'alpfe =',
     &  ' /* Solubility of aeolian fe */')
       CALL WRITE_0D_RL( fesedflux_pcm, INDEX_NONE,'fesedflux_pcm =',
     &  ' /* Sediment Fe flux = fesedflux_pcm*pflux+FeIntSec */')
       CALL WRITE_0D_RL( FeIntSec, INDEX_NONE,'FeIntSec =',
     &  ' /* Sediment Fe flux = fesedflux_pcm * pflux + FeIntSec */')
       CALL WRITE_0D_RL( freefemax, INDEX_NONE,'freefemax =',
     &  ' /* Max solubility of free iron (mol/m3) */')
       CALL WRITE_0D_RL( KScav, INDEX_NONE,'KScav =',
     &  ' /* Iron scavenging rate */')
       CALL WRITE_0D_RL( ligand_stab, INDEX_NONE,'ligand_stab =',
     &  ' /* Ligand-free iron stability constant (m3/mol) */')
       CALL WRITE_0D_RL( ligand_tot, INDEX_NONE,'ligand_tot =',
     &  ' /* Total free ligand  (mol/m3) */')
       CALL WRITE_0D_RL( alphaUniform, INDEX_NONE,'alphaUniform =',
     &  ' /* Timescale for biological activity */')
       CALL WRITE_0D_RL(rainRatioUniform,INDEX_NONE,'rainRatioUniform=',
     &  ' /* Inorganic/organic carbon rain ratio */')
       CALL WRITE_0D_L( QSW_underice, INDEX_NONE, 'QSW_underice  =',
     &  '  /* Flag for Qsw under Sea-Ice (i.e. SI fract included) */')
#endif /* DIC_BIOTIC */

C- namelist DIC_FORCING
       CALL WRITE_0D_C( DIC_windFile, -1, INDEX_NONE, 'DIC_windFile =',
     & '  /* File name of wind speeds */')
       CALL WRITE_0D_C( DIC_atmospFile, -1,INDEX_NONE,'DIC_atmospFile=',
     & '  /* File name of atmospheric pressure*/')
       CALL WRITE_0D_C( DIC_silicaFile, -1,INDEX_NONE,
     & 'DIC_silicaFile=','  /* File name of surface silica */')
      IF ( useCalciteSaturation ) THEN
       CALL WRITE_0D_C( DIC_deepSilicaFile, -1,INDEX_NONE,
     & 'DIC_deepSilicaFile=','  /* File name of 3d silica field */')
      ENDIF
       CALL WRITE_0D_C( DIC_iceFile, -1, INDEX_NONE, 'DIC_iceFile =',
     & '  /* File name of seaice fraction */')
       CALL WRITE_0D_C( DIC_parFile, -1,INDEX_NONE,'DIC_parFile=',
     & '  /* File name of photosynthetically available radiation */')
       CALL WRITE_0D_C( DIC_chlaFile, -1,INDEX_NONE,'DIC_chlaFile=',
     & '  /* File name of chlorophyll climatology */')
       CALL WRITE_0D_C( DIC_ironFile, -1, INDEX_NONE, 'DIC_ironFile =',
     & '  /* File name of aeolian iron flux */')
       CALL WRITE_0D_RL( DIC_forcingPeriod,
     &   INDEX_NONE,'DIC_forcingPeriod =',
     &  ' /* Periodic forcing parameter specific for DIC (s) */')
       CALL WRITE_0D_RL( DIC_forcingCycle,
     &   INDEX_NONE,'DIC_forcingCycle =',
     &  ' /* Periodic forcing parameter specific for DIC (s) */')
       CALL WRITE_0D_I( dic_int1, INDEX_NONE, 'dic_int1 =',
     &  '  /*  */')
       CALL WRITE_0D_I( dic_int2, INDEX_NONE, 'dic_int2 =',
     &  '  /*  */')
       CALL WRITE_0D_I( dic_int3, INDEX_NONE, 'dic_int3 =',
     &  '  /*  */')
       CALL WRITE_0D_I( dic_int4, INDEX_NONE, 'dic_int4 =',
     &  '  /*  */')
       CALL WRITE_0D_RL( dic_pCO2, INDEX_NONE,'dic_pCO2 =',
     &  ' /* Atmospheric pCO2 to be read in data.dic */')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( dic_int1.EQ.0 .AND. dic_pCO2.NE.278. _d -6 ) THEN
        WRITE(msgBuf,'(A)')
     &    'DIC_READPARMS: cannot change default dic_pCO2 if dic_int1=0'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#ifdef ALLOW_OLD_VIRTUALFLUX
      IF ( PTRACERS_EvPrRn(1).NE.UNSET_RL .OR.
     &     PTRACERS_EvPrRn(2).NE.UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'when ALLOW_OLD_VIRTUALFLUX is defined (in DIC_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid )
        IF ( PTRACERS_EvPrRn(1).NE.UNSET_RL ) THEN
         WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &   ' cannot set PTRACERS_EvPrRn(1) (in data.ptracers)'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( PTRACERS_EvPrRn(2).NE.UNSET_RL ) THEN
         WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &   ' cannot set PTRACERS_EvPrRn(2) (in data.ptracers)'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_OLD_VIRTUALFLUX */

#ifdef CARBONCHEM_SOLVESAPHE
      IF ( selectHFconst.LT.1 .OR. selectHFconst.GT.2 .OR.
     &     selectBTconst.LT.1 .OR. selectBTconst.GT.2 .OR.
     &     selectFTconst.LT.1 .OR. selectFTconst.GT.2 ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &        'Invalid value for selectHF, selectBT or selectFT'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectK1K2const.LT.1 .OR. selectK1K2const.GT.6 ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &        'Invalid value for selectK1K2const (< 1 or > 6)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectPHsolver.LT.0 .OR. selectPHsolver.GT.3 ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &        'Invalid value for selectPHsolver (< 0 or > 3)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#else /* CARBONCHEM_SOLVESAPHE */
      IF ( selectBTconst.NE.1 .OR. selectFTconst.NE.1 .OR.
     &     selectHFconst.NE.1 .OR. selectK1K2const.NE.1 ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: selectHF, selectBT, selectFT or selectK1K2const'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define CARBONCHEM_SOLVESAPHE" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectPHsolver.NE.0 ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: selectPHsolver > 0'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define CARBONCHEM_SOLVESAPHE" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* CARBONCHEM_SOLVESAPHE */

#ifdef DIC_CALCITE_SAT
C Issue an error and stop if DIC_CALCITE_SAT AND ALLOW_GCHEM_FFIELDS
C  but not GCHEM_3D_SILICA
#if defined (GCHEM_ALLOW_FFIELDS) && \
    !defined (GCHEM_3D_SILICA)
      IF ( useCalciteSaturation ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: useCalciteSaturation with ALLOW_GCHEM_FFIELDS'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define GCHEM_3D_SILICA" in "GCHEM_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif
C Issue an error and stop for invalid selectCalciteDissolution
      IF ( ( selectCalciteDissolution .LT. 0 .OR.
     &       selectCalciteDissolution .GT. 3 )
     &              .AND. useCalciteSaturation ) THEN
        WRITE(msgBuf,'(2A,I4,A)') 'DIC_READPARMS: ',
     &    'selectCalciteDissolution =', selectCalciteDissolution,
     &    ' : invalid value'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
C Issue a soft message if a deepSilicaFile is supplied
C    but will not be used because useCalciteSaturation is FALSE
      IF ( DIC_deepSilicaFile .NE. ' ' .AND.
     &                        .NOT. useCalciteSaturation ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: DIC_deepSilicaFile (3d silicate input)'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "useCalciteSaturation=.TRUE." in "data.dic"'
         CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,myThid)
      ENDIF
#else
C Issue an error and stop if useCalciteSaturation is TRUE but
C    DIC_CALCITE_SATURATION code is not compiled
      IF ( useCalciteSaturation ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to enable: useCalciteSaturation'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define DIC_CALCITE_SAT" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C Issue an error and stop if deepSilicaFile is supplied but
C    DIC_CALCITE_SATURATION code is not compiled
      IF ( DIC_deepSilicaFile .NE. ' '  ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: DIC_deepSilicaFile (3d silicate input)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define DIC_CALCITE_SAT" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* DIC_CALCITE_SAT */

C Issue an error and stop if GCHEM_ALLOW_FFIELDS conflicts
C   with interactive atmospheric box
#ifdef GCHEM_ALLOW_FFIELDS
      IF (dic_int1.GE.0 .AND. dic_int1.LT.3) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'GCHEM_ALLOW_FFIELDS conflicts with 0<=dic_int1<=2'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'only interactive atmosphere dic_int1=3 allowed'
        CALL PRINT_ERROR( msgBuf, myThid )

        IF (dic_int1.EQ.0 .OR. dic_int1.EQ.1) THEN
          WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &      'dic_int1=0 or 1: set gchem_apco2_const in data.gchem'
        ELSEIF (dic_int1.EQ.2) THEN
          WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &      'dic_int1=2: supply 2D gchem_apco2File in data.gchem'
        ENDIF
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
# ifndef DIC_ALLOW_READ_ATMOSPCO2
      IF (dic_int1.EQ.-1) THEN
        WRITE(msgBuf,'(3A)') '** warning ** DIC_READPARMS: ',
     &    '#define DIC_ALLOW_READ_ATMOSPCO2 is required ',
     &    'for dic_int1=-1 to work properly'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
# endif
#endif /* GCHEM_ALLOW_FFIELDS */

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'DIC_READPARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R DIC_READPARMS'
      ENDIF
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIC */

      RETURN
      END
