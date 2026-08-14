#include "DIC_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

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
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
# include "DIC_EXF.h"
# ifdef USE_EXF_INTERPOLATION
#  include "EXF_INTERP_SIZE.h"
#  include "EXF_INTERP_PARAM.h"
#  include "DIC_INTERP_PARAM.h"
# endif
#endif

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
#ifdef ALLOW_EXF
C     Local helper parameters that can be set in case we want to use
C       pkg/exf machinery for dic fields with dic_useEXF=T but useEXF=F 
C.      (physical model not using pkg/exf)
      INTEGER dic_exf_iprec, dic_exf_debugLev
      LOGICAL dic_useExfYearlyFields, dic_twoDigitYear
      LOGICAL prtBlkLn, addBlkLn
# ifdef USE_EXF_INTERPOLATION
      LOGICAL dic_exf_output_interp
      INTEGER j
# endif
#endif
CEOP
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
     &     DIC_useEXF,
     &     DIC_forcingPeriod, DIC_forcingCycle,
     &     DIC_windFile, DIC_atmospFile, DIC_silicaFile,
     &     DIC_silicaDeepFile, DIC_iceFile, DIC_parFile,
     &     DIC_chlaFile, DIC_ironFile,
     &     DIC_atmospCO2File, DIC_SSTFile, DIC_SSSFile,
     &     dic_int1, dic_int2, dic_int3, dic_int4, dic_pCO2,
     &     dic_SilicaSurf_const, dic_SilicaDeep_const, dic_par_const,
     &     dic_Iron_const, dic_atmospCO2_const, dic_wind_const,
     &     dic_atmosp_const, dic_ice_const, dic_chl_const,
     &     dic_SST_const, dic_SSS_const
# ifdef ALLOW_EXF
     &     , dic_exf_iprec, dic_exf_debugLev,
     &     dic_useExfYearlyFields, dic_twoDigitYear,
C
     &     dic_SilicaSurfPeriod, dic_SilicaSurfRepCycle, 
     &     dic_SilicaSurfStartTime,
     &     dic_SilicaSurfStartDate1, dic_SilicaSurfStartDate2,
     &     dic_SilicaSurf_exfremo_intercept, 
     &     dic_SilicaSurf_exfremo_slope,
     &     dic_SilicaSurfMask, dic_inscal_SilicaSurf,
C
     &     dic_SilicaDeepPeriod, dic_SilicaDeepRepCycle, 
     &     dic_SilicaDeepStartTime,
     &     dic_SilicaDeepStartDate1, dic_SilicaDeepStartDate2,
     &     dic_SilicaDeep_exfremo_intercept, 
     &     dic_SilicaDeep_exfremo_slope,
     &     dic_SilicaDeepMask, dic_inscal_SilicaDeep,
C
     &     dic_parPeriod, dic_parRepCycle, dic_parStartTime,
     &     dic_parStartDate1, dic_parStartDate2,
     &     dic_par_exfremo_intercept, dic_par_exfremo_slope,
     &     dic_parMask, dic_inscal_par,
C
     &     dic_IronPeriod, dic_IronRepCycle, dic_IronStartTime,
     &     dic_IronStartDate1, dic_IronStartDate2,
     &     dic_Iron_exfremo_intercept, dic_Iron_exfremo_slope,
     &     dic_IronMask, dic_inscal_Iron,
C
     &     dic_atmospCO2Period, dic_atmospCO2RepCycle, 
     &     dic_atmospCO2StartTime,
     &     dic_atmospCO2StartDate1, dic_atmospCO2StartDate2,
     &     dic_atmospCO2_exfremo_intercept, dic_atmospCO2_exfremo_slope,
     &     dic_atmospCO2Mask, dic_inscal_atmospCO2,
C
     &     dic_windPeriod, dic_windRepCycle, dic_windStartTime,
     &     dic_windStartDate1, dic_windStartDate2,
     &     dic_wind_exfremo_intercept, dic_wind_exfremo_slope,
     &     dic_windMask, dic_inscal_wind,
C
     &     dic_icePeriod, dic_iceRepCycle, dic_iceStartTime,
     &     dic_iceStartDate1, dic_iceStartDate2,
     &     dic_ice_exfremo_intercept, dic_ice_exfremo_slope,
     &     dic_iceMask, dic_inscal_ice,
C
     &     dic_atmospPeriod, dic_atmospRepCycle, dic_atmospStartTime,
     &     dic_atmospStartDate1, dic_atmospStartDate2,
     &     dic_atmosp_exfremo_intercept, dic_atmosp_exfremo_slope,
     &     dic_atmospMask, dic_inscal_atmosp,
C
     &     dic_chlperiod, dic_chlRepCycle, dic_chlStartTime,
     &     dic_chlstartdate1, dic_chlstartdate2,
     &     dic_chl_exfremo_intercept, dic_chl_exfremo_slope,
     &     dic_chlMask, dic_inscal_chl,
C
     &     dic_SSTPeriod, dic_SSTRepCycle, dic_SSTStartTime,
     &     dic_SSTStartDate1, dic_SSTStartDate2,
     &     dic_SST_exfremo_intercept, dic_SST_exfremo_slope,
     &     dic_SSTMask, dic_inscal_SST,
C
     &     dic_SSSPeriod, dic_SSSRepCycle, dic_SSSStartTime,
     &     dic_SSSStartDate1, dic_SSSStartDate2,
     &     dic_SSS_exfremo_intercept, dic_SSS_exfremo_slope,
     &     dic_SSSMask, dic_inscal_SSS
#  ifdef USE_EXF_INTERPOLATION
     &     , dic_exf_output_interp,
C
     &     SilicaSurf_lon0, SilicaSurf_lat0, SilicaSurf_nlon, 
     &     SilicaSurf_nlat, SilicaSurf_lon_inc, 
     &     SilicaSurf_lat_inc, SilicaSurf_interpMethod,
C
     &     SilicaDeep_lon0, SilicaDeep_lat0, SilicaDeep_nlon,
     &     SilicaDeep_nlat, SilicaDeep_lon_inc, 
     &     SilicaDeep_lat_inc, SilicaDeep_interpMethod,
C Additional parameters for interpolation of 3D silica fields     
     &     SilicaDeep_nzin, SilicaDeep_dzin,
C
     &     par_lon0, par_lat0, par_nlon, par_nlat,
     &     par_lon_inc, par_lat_inc, par_interpMethod,
C
     &     Fe_lon0, Fe_lat0, Fe_nlon, Fe_nlat,
     &     Fe_lon_inc, Fe_lat_inc, Fe_interpMethod,
C
     &     apco2_lon0, apco2_lat0, apco2_nlon, apco2_nlat,
     &     apco2_lon_inc, apco2_lat_inc, apco2_interpMethod,
C
     &     wind_lon0, wind_lat0, wind_nlon, wind_nlat,
     &     wind_lon_inc, wind_lat_inc, wind_interpMethod,
C
     &     ice_lon0, ice_lat0, ice_nlon, ice_nlat,
     &     ice_lon_inc, ice_lat_inc, ice_interpMethod,
C
     &     atmosp_lon0, atmosp_lat0, atmosp_nlon, atmosp_nlat,
     &     atmosp_lon_inc, atmosp_lat_inc, atmosp_interpMethod,
C
     &     chl_lon0, chl_lat0, chl_nlon, chl_nlat,
     &     chl_lon_inc, chl_lat_inc, chl_interpMethod,
C
     &     SST_lon0, SST_lat0, SST_nlon, SST_nlat,
     &     SST_lon_inc, SST_lat_inc, SST_interpMethod,
C
     &     SSS_lon0, SSS_lat0, SSS_nlon, SSS_nlat,
     &     SSS_lon_inc, SSS_lat_inc, SSS_interpMethod
#  endif
# endif /* ALLOW_EXF */
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
       DOPfraction   = 0.67 _d 0
       KDOPRemin     = 1. _d 0/(6. _d 0*30. _d 0*86400. _d 0)
       KRemin        = 0.9 _d 0
       zcrit         = 500. _d 0
       O2crit        = 4. _d -3
       R_OP          =-170. _d 0
       R_CP          = 117. _d 0
       R_NP          = 16. _d 0
       R_FeP         = 0.000468 _d 0
       parfrac       = 0.4 _d 0
       k0            = 0.02 _d 0
       kchl          = 0.02 _d 0
       lit0          = 30. _d 0
       KPO4          = 5. _d -4
       KFE           = 1.2 _d -7
       alpfe         = 0.01 _d 0
       fesedflux_pcm = 6.8 _d -4 * 106. _d 0
       FeIntSec      = 0.5 _d -6 / 86400. _d 0
       freefemax     = 3. _d -7
       KScav         = 0.19 _d 0/(360. _d 0*86400. _d 0)
       ligand_stab   = 1. _d 8
       ligand_tot    = 1. _d -6
       alphaUniform     = 2. _d -3/(360. _d 0 * 86400. _d 0)
       rainRatioUniform = 7. _d -2
#endif
       dic_useEXF         = .FALSE.
       DIC_windFile       = ' '
       DIC_atmospFile     = ' '
       DIC_silicaFile     = ' '
       DIC_silicaDeepFile = ' '
       DIC_iceFile         = ' '
       DIC_parFile         = ' '
       DIC_atmospCO2File   = ' '
       DIC_atmospFile      = ' '
       DIC_chlaFile        = ' '
       DIC_ironFile        = ' '
       DIC_SSTFile         = ' '
       DIC_SSSFile         = ' '
       dic_int1            = 0
       dic_int2            = 0
       dic_int3            = 0
       dic_int4            = 0
       dic_pCO2            = 278. _d -6

C     some sensible defaults
      dic_ice_const        = 0.      _d 0
      dic_atmosp_const     = 1.01325 _d 5
      dic_silicaSurf_const = 7.6838  _d -3
      dic_silicaDeep_const = 3.      _d -2
      dic_atmospCO2_const  = dic_pCO2
      dic_wind_const       = 5.      _d 0
      dic_par_const        = 100.    _d 0
      dic_iron_const       = 1.      _d -11
      dic_chl_const        = 1. _d -2
      dic_SST_const        = 24.     _d 0
      dic_SSS_const        = 34.5    _d 0

C default periodic forcing to same as for physics
       DIC_forcingPeriod = externForcingPeriod
       DIC_forcingCycle  = externForcingCycle

C  default based on 360 day year or related to calendar settings
       dic_secondsPerYear   = 360. _d 0 * 86400. _d 0
#ifdef ALLOW_CAL
       IF ( useCAL ) THEN
        IF ( theCalendar .EQ. 'gregorian') THEN
         dic_secondsPerYear = 365.25 _d 0*86400. _d 0
        ELSEIF ( theCalendar .EQ. 'noLeapYear') THEN
         dic_secondsPerYear = 365.00 _d 0*86400. _d 0
        ENDIF
       ENDIF
#endif

      WRITE(msgBuf,'(A)') ' DIC_READPARMS: opening data.dic'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data.dic', 'DIC_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Abiotic parameters
      READ(UNIT=iUnit, NML=ABIOTIC_PARMS)

#ifdef DIC_BIOTIC
C-    Biotic parameters
      READ(UNIT=iUnit, NML=BIOTIC_PARMS)
#endif

C-    forcing filenames and parameters
      READ(UNIT=iUnit, NML=DIC_FORCING)

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
# ifdef ALLOW_EXF
      IF ( dic_useEXF ) THEN
       dic_SilicaSurfPeriod               = 0.0 _d 0
       dic_SilicaSurfRepCycle             = DIC_forcingPeriod
       dic_SilicaSurfStartTime            = UNSET_RL
       dic_SilicaSurfStartDate1           = 0
       dic_SilicaSurfStartDate2           = 0
       dic_SilicaSurf_exfremo_intercept   = 0.0 _d 0
       dic_SilicaSurf_exfremo_slope       = 0.0 _d 0
       dic_SilicaSurfMask                 = 'c'
       dic_inscal_SilicaSurf              = 1. _d 0

       dic_SilicaDeepPeriod               = 0.0 _d 0
       dic_SilicaDeepRepCycle             = DIC_forcingPeriod
       dic_SilicaDeepStartTime            = UNSET_RL
       dic_SilicaDeepStartDate1           = 0
       dic_SilicaDeepStartDate2           = 0
       dic_SilicaDeep_exfremo_intercept   = 0.0 _d 0
       dic_SilicaDeep_exfremo_slope       = 0.0 _d 0
       dic_SilicaDeepMask                 = 'c'
       dic_inscal_SilicaDeep              = 1. _d 0

       dic_parPeriod                      = 0.0 _d 0
       dic_parRepCycle                    = DIC_forcingPeriod
       dic_parStartTime                   = UNSET_RL
       dic_parStartDate1                  = 0
       dic_parStartDate2                  = 0
       dic_par_exfremo_intercept          = 0.0 _d 0
       dic_par_exfremo_slope              = 0.0 _d 0
       dic_parMask                        = 'c'
       dic_inscal_par                     = 1. _d 0

       dic_ironPeriod                     = 0.0 _d 0
       dic_ironRepCycle                   = DIC_forcingPeriod
       dic_ironStartTime                  = UNSET_RL
       dic_ironStartDate1                 = 0
       dic_ironStartDate2                 = 0
       dic_iron_exfremo_intercept         = 0.0 _d 0
       dic_iron_exfremo_slope             = 0.0 _d 0
       dic_ironMask                       = 'c'
       dic_inscal_iron                    = 1. _d 0

       dic_atmospCO2Period                = 0.0 _d 0
       dic_atmospCO2RepCycle              = DIC_forcingPeriod
       dic_atmospCO2StartTime             = UNSET_RL
       dic_atmospCO2StartDate1            = 0
       dic_atmospCO2StartDate2            = 0
       dic_atmospCO2_exfremo_intercept    = 0.0 _d 0
       dic_atmospCO2_exfremo_slope        = 0.0 _d 0
       dic_atmospCO2Mask                  = 'c'
       dic_inscal_atmospCO2                 = 1. _d 0

       dic_icePeriod                      = 0.0 _d 0
       dic_iceRepCycle                    = DIC_forcingPeriod
       dic_iceStartTime                   = UNSET_RL
       dic_iceStartDate1                  = 0
       dic_iceStartDate2                  = 0
       dic_ice_exfremo_intercept          = 0.0 _d 0
       dic_ice_exfremo_slope              = 0.0 _d 0
       dic_iceMask                        = 'c'
       dic_inscal_ice                     = 1. _d 0

       dic_windPeriod                     = 0.0 _d 0
       dic_windRepCycle                   = DIC_forcingPeriod
       dic_windStartTime                  = UNSET_RL
       dic_windStartDate1                 = 0
       dic_windStartDate2                 = 0
       dic_wind_exfremo_intercept         = 0.0 _d 0
       dic_wind_exfremo_slope             = 0.0 _d 0
       dic_windMask                       = 'c'
       dic_inscal_wind                    = 1. _d 0

       dic_atmospPeriod                   = 0.0 _d 0
       dic_atmospRepCycle                 = DIC_forcingPeriod
       dic_atmospStartTime                = UNSET_RL
       dic_atmospStartDate1               = 0
       dic_atmospStartDate2               = 0
       dic_atmosp_exfremo_intercept       = 0.0 _d 0
       dic_atmosp_exfremo_slope           = 0.0 _d 0
       dic_atmospMask                     = 'c'
C convert atmospheric pressure from Pa or mbar to Atm for use in dic_surfforcing
       dic_inscal_atmosp                  = 1.01325 _d 5

       dic_chlPeriod                      = 0.0 _d 0
       dic_chlRepCycle                    = DIC_forcingPeriod
       dic_chlStartTime                   = UNSET_RL
       dic_chlStartDate1                  = 0
       dic_chlStartDate2                  = 0
       dic_chl_exfremo_intercept          = 0.0 _d 0
       dic_chl_exfremo_slope              = 0.0 _d 0
       dic_chlMask                        = 'c'
       dic_inscal_chl                     = 1. _d 0

       dic_SSTPeriod                      = 0.0 _d 0
       dic_SSTRepCycle                    = DIC_forcingPeriod
       dic_SSTStartTime                   = UNSET_RL
       dic_SSTStartDate1                  = 0
       dic_SSTStartDate2                  = 0
       dic_SST_exfremo_intercept          = 0.0 _d 0
       dic_SST_exfremo_slope              = 0.0 _d 0
       dic_SSTMask                        = 'c'
       dic_inscal_SST                     = 1. _d 0

       dic_SSSPeriod                      = 0.0 _d 0
       dic_SSSRepCycle                    = DIC_forcingPeriod
       dic_SSSStartTime                   = UNSET_RL
       dic_SSSStartDate1                  = 0
       dic_SSSStartDate2                  = 0
       dic_SSS_exfremo_intercept          = 0.0 _d 0
       dic_SSS_exfremo_slope              = 0.0 _d 0
       dic_SSSMask                        = 'c'
       dic_inscal_SSS                     = 1. _d 0

C     If useEXF=F, s/r exf_readparms is not called and exf-parameters
C     are not initialiased properly. The following few exf-related
C     parameters need to have a defined value to be used in s/r
C     exf_set_fld.
       dic_exf_iprec           = 32
       dic_useExfYearlyFields  = .FALSE.
       dic_twoDigitYear        = .FALSE.
       dic_exf_debugLev        = debugLevel

       IF ( .NOT. useEXF ) THEN
        repeatPeriod       = DIC_forcingPeriod
        exf_iprec          = dic_exf_iprec
        useExfYearlyFields = dic_useExfYearlyFields
        twoDigitYear       = dic_twoDigitYear
        exf_debugLev       = dic_exf_debugLev
       ENDIF

#  ifdef USE_EXF_INTERPOLATION

       IF ( .NOT. useEXF ) THEN
C     This is done in s/r exf_init_interp only if useEXF=T
# ifdef ALLOW_EXCH2
        inp_gNx = exch2_mydNx(1)
        inp_gNy = exch2_mydNy(1)
# else /* ALLOW_EXCH2 */
        inp_gNx = Nx
        inp_gNy = Ny
# endif /* ALLOW_EXCH2 */

        inp_lon0 = xgOrigin + delX(1)*halfRL
        inp_lat0 = ygOrigin + delY(1)*halfRL

        inp_dLon = delX(1)
        DO j=1,MAX_LAT_INC
         IF (j.LT.inp_gNy) THEN
          inp_dLat(j) = (delY(j) + delY(j+1))*halfRL
         ELSE
          inp_dLat(j) = 0.
         ENDIF
        ENDDO
       ENDIF

       Silicate_lon0              = inp_lon0
       Silicate_lat0              = inp_lat0
       Silicate_nlon              = inp_gNx
       Silicate_nlat              = inp_gNy
       Silicate_lon_inc           = inp_dLon
       Silicate_interpMethod      = 1

       SilicateDeep_lon0          = inp_lon0
       SilicateDeep_lat0          = inp_lat0
       SilicateDeep_nlon          = inp_gNx
       SilicateDeep_nlat          = inp_gNy
       SilicateDeep_lon_inc       = inp_dLon
       SilicateDeep_interpMethod  = 1
       SilicateDeep_nzin          = UNSET_I
       DO j = 1, MAX_LEV_IN
         SilicateDeep_dzin(j)     = UNSET_RL
       ENDDO

       par_lon0                   = inp_lon0
       par_lat0                   = inp_lat0
       par_nlon                   = inp_gNx
       par_nlat                   = inp_gNy
       par_lon_inc                = inp_dLon
       par_interpMethod           = 1

       iron_lon0                  = inp_lon0
       iron_lat0                  = inp_lat0
       iron_nlon                  = inp_gNx
       iron_nlat                  = inp_gNy
       iron_lon_inc               = inp_dLon
       iron_interpMethod          = 1

       atmospCO2_lat0               = inp_lat0
       atmospCO2_lon0               = inp_lon0
       atmospCO2_nlon               = inp_gNx
       atmospCO2_nlat               = inp_gNy
       atmospCO2_lon_inc            = inp_dLon
       atmospCO2_interpMethod       = 1

       wind_lon0                  = inp_lon0
       wind_lat0                  = inp_lat0
       wind_nlon                  = inp_gNx
       wind_nlat                  = inp_gNy
       wind_lon_inc               = inp_dLon
       wind_interpMethod          = 1

       ice_lon0                   = inp_lon0
       ice_lat0                   = inp_lat0
       ice_nlon                   = inp_gNx
       ice_nlat                   = inp_gNy
       ice_lon_inc                = inp_dLon
       ice_interpMethod           = 1

       atmosp_lat0                 = inp_lat0
       atmosp_nlon                 = inp_gNx
       atmosp_nlat                 = inp_gNy
       atmosp_lon_inc              = inp_dLon
       atmosp_interpMethod         = 1

       chl_lat0                    = inp_lat0
       chl_nlon                    = inp_gNx
       chl_nlat                    = inp_gNy
       chl_lon_inc                 = inp_dLon
       chl_interpMethod            = 1

       SST_lat0                    = inp_lat0
       SST_nlon                    = inp_gNx
       SST_nlat                    = inp_gNy
       SST_lon_inc                 = inp_dLon
       SST_interpMethod            = 1

       SSS_lat0                    = inp_lat0
       SSS_nlon                    = inp_gNx
       SSS_nlat                    = inp_gNy
       SSS_lon_inc                 = inp_dLon
       SSS_interpMethod            = 1

       DO j=1,MAX_LAT_INC
        Silicate_lat_inc(j)        = inp_dLat(j)
        SilicateDeep_lat_inc(j)    = inp_dLat(j)
        par_lat_inc(j)             = inp_dLat(j)
        iron_lat_inc(j)            = inp_dLat(j)
        atmospCO2_lat_inc(j)         = inp_dLat(j)
        wind_lat_inc(j)            = inp_dLat(j)
        ice_lat_inc(j)             = inp_dLat(j)
        atmosp_lat_inc(j)          = inp_dLat(j)
        chl_lat_inc(j)             = inp_dLat(j)
        SST_lat_inc(j)             = inp_dLat(j)
        SSS_lat_inc(j)             = inp_dLat(j)
       ENDDO

       dic_exf_output_interp = .FALSE.

       IF ( .NOT. useEXF ) THEN
        exf_output_interp   = dic_exf_output_interp
       ENDIF
#  endif /* USE_EXF_INTERPOLATION */
C     dic_useEXF
      ENDIF
# endif /* ALLOW_EXF */

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
       CALL WRITE_0D_C( DIC_silicaDeepFile, -1,INDEX_NONE,
     & 'DIC_silicaDeepFile=','  /* File name of 3d silica field */')
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
#ifdef ALLOW_EXF
      IF ( dic_useEXF ) THEN
C--   Print general parameters:
        WRITE(msgBuf,'(A)') ' dic_useEXF general parameters:'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(A)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
#ifdef USE_EXF_INTERPOLATION
        WRITE(msgBuf,'(A)')
     &  '// USE_EXF_INTERPOLATION:              defined'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
#else
          WRITE(msgBuf,'(A)')
     &  '// USE_EXF_INTERPOLATION:          NOT defined'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
#endif
C--   For each data set used the summary prints the calendar data
C     and the corresponding file from which the data will be read.
      prtBlkLn = .FALSE.
      addBlkLn = .TRUE.

C--   dic wind speed
        IF ( dic_WindFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'wind speed',
     I         dic_windfile, 
     I         dic_windRepCycle, 
     I         dic_windperiod,
     I         dic_windStartTime, 
     I         useExfYearlyFields,
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicWind',
     &     wind_lon0, 
     I         wind_lon_inc, 
     I         wind_lat0, 
     I         wind_lat_inc,
     &     wind_nlon, 
     I         wind_nlat, 
     I         wind_interpMethod, 
     I         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C--   dic atmospheric pressure
        IF ( dic_atmospFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'atmospheric pressure',
     I         dic_atmospfile, 
     I         dic_atmospRepCycle, 
     I         dic_atmospperiod,
     I         dic_atmospStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicAtmosP',
     &         atmosp_lon0, 
     &         atmosp_lon_inc, 
     &         atmosp_lat0, 
     &         atmosp_lat_inc,
     &         atmosp_nlon, 
     &         atmosp_nlat, 
     &         atmosp_interpMethod, 
     &         myThid )
#endif  
         prtBlkLn = .TRUE.
        ENDIF 

C--   dic surface silica
        IF ( dic_SilicaFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'surface silica',
     I         dic_SilicaFile, 
     I         dic_SilicaSurfRepCycle, 
     I         dic_SilicaSurfperiod,
     I         dic_SilicaSurfStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicSilicaSurf',
     I         silicaSurf_lon0, 
     I         silicaSurf_lon_inc, 
     I         silicaSurf_lat0, 
     I         silicaSurf_lat_inc,
     I         silicaSurf_nlon, 
     I         silicaSurf_nlat, 
     I         silicaSurf_interpMethod, 
     I         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C--  dic deep silica
        IF ( dic_SilicaDeepFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'deep silica',
     I         dic_SilicaDeepFile, 
     I         dic_SilicaDeepRepCycle, 
     I         dic_SilicaDeepperiod,
     I         dic_SilicaDeepStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicSilicaDeep',
     &         silicaDeep_lon0, 
     &         silicaDeep_lon_inc, 
     &         silicaDeep_lat0, 
     &         silicaDeep_lat_inc,
     &         silicaDeep_nlon, 
     &         silicaDeep_nlat, 
     &         silicaDeep_interpMethod, 
     &         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C--   dic par
        IF ( dic_parFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 
     I        'photosynthetically active radiation',
     I         dic_parFile, 
     I         dic_parRepCycle, 
     I         dic_parperiod,
     I         dic_parStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicPAR',
     &         par_lon0, 
     &         par_lon_inc, 
     &         par_lat0, 
     &         par_lat_inc,
     &         par_nlon, 
     &         par_nlat, 
     &         par_interpMethod, 
     &         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C--   dic iron forcing
        IF ( dic_ironFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'aeolian iron flux',
     I         dic_ironFile, 
     I         dic_ironRepCycle, 
     I         dic_ironperiod,
     I         dic_ironStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'InputFe',
     &         iron_lon0, 
     &         iron_lon_inc, 
     &         iron_lat0, 
     &         iron_lat_inc,
     &         iron_nlon, 
     &         iron_nlat, 
     &         iron_interpMethod, 
     &         myThid )
#endif   
          prtBlkLn = .TRUE.
      ENDIF

C--   dic atmospheric CO2 forcing
        IF ( dic_atmospCO2File .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'atmospheric CO2',
     I         dic_atmospCO2File, 
     I         dic_atmospCO2RepCycle, 
     I         dic_atmospCO2period,
     I         dic_atmospCO2StartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid)
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicAtmosCO2',
     &         atmospCO2_lon0, 
     &         atmospCO2_lon_inc, 
     &         atmospCO2_lat0, 
     &         atmospCO2_lat_inc,
     &         atmospCO2_nlon, 
     &         atmospCO2_nlat, 
     &         atmospCO2_interpMethod, 
     &         myThid )
#endif  
         prtBlkLn = .TRUE.
        ENDIF

C--  dic sea ice forcing
        IF ( dic_iceFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'sea ice fraction',
     I         dic_iceFile, 
     I         dic_iceRepCycle, 
     I         dic_iceperiod,
     I         dic_iceStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid ) 
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicIce',
     &         ice_lon0, 
     &         ice_lon_inc, 
     &         ice_lat0, 
     &         ice_lat_inc,
     &         ice_nlon, 
     &         ice_nlat, 
     &         ice_interpMethod, 
     &         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C-- dic chlorophyll forcing
        IF ( dic_chlaFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY( 'chlorophyll',
     I         dic_chlaFile, 
     I         dic_chlRepCycle, 
     I         dic_chlperiod,
     I         dic_chlStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicChl',
     &         chl_lon0, 
     &         chl_lon_inc, 
     &         chl_lat0, 
     &         chl_lat_inc,
     &         chl_nlon, 
     &         chl_nlat, 
     &         chl_interpMethod, 
     &         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C-- dic SST forcing
        IF ( dic_SSTFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY(
     I        'carbon chem sea surface temperature',
     I         dic_SSTFile, 
     I         dic_SSTRepCycle, 
     I         dic_SSTperiod,
     I         dic_SSTStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicSST',
     &         SST_lon0, 
     &         SST_lon_inc, 
     &         SST_lat0, 
     &         SST_lat_inc,
     &         SST_nlon, 
     &         SST_nlat, 
     &         SST_interpMethod, 
     &         myThid )
#endif   
          prtBlkLn = .TRUE.
        ENDIF

C-- dic SSS forcing
        IF ( dic_SSSFile .NE. ' ' ) THEN
          CALL EXF_FLD_SUMMARY(
     I        'carbon chem sea surface salinity',
     I         dic_SSSFile, 
     I         dic_SSSRepCycle, 
     I         dic_SSSperiod,
     I         dic_SSSStartTime, 
     I         useExfYearlyFields, 
     I         addBlkLn, 
     I         myThid )
#ifdef USE_EXF_INTERPOLATION
          CALL EXF_PRINT_INTERP( 'dicSSS',
     &         SSS_lon0, 
     &         SSS_lon_inc, 
     &         SSS_lat0, 
     &         SSS_lat_inc,
     &         SSS_nlon, 
     &         SSS_nlat, 
     &         SSS_interpMethod, 
     &         myThid )
#endif   
           prtBlkLn = .TRUE.
        ENDIF
      ELSE
        IF ( dic_SSSFile .NE. ' ' ) THEN
         WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &    'dic_SSSFile WITHOUT dic_useEXF',
     &    'is not implemented'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
        IF ( dic_SSTFile .NE. ' ' ) THEN
         WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &    'dic_SSTFile WITHOUT dic_useEXF',
     &    'is not implemented'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
C dic_useEXF
      ENDIF

C     dic_useEXF and useExfYearlyFields
      IF ( dic_useEXF .AND. useExfYearlyFields ) THEN
       IF ( dic_parRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &   'useExfYearlyFields AND dic_parRepCycle',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_ironRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &   'useExfYearlyFields AND dic_ironRepCycle',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_SilicaSurfRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &   'useExfYearlyFields AND dic_SilicaSurfRepCycle',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_SilicaDeepRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &    'useExfYearlyFields AND dic_SilicaDeepRepCycle',
     &    'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_atmospCO2RepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &   'useExfYearlyFields AND dic_atmospCO2RepCycle',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_windRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &   'useExfYearlyFields AND dic_windRepCycle',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_iceRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &   'useExfYearlyFields AND dic_iceRepCycle',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_atmospRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &   'useExfYearlyFields AND dic_atmospRepCycle',
     &   'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_chlRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &    'useExfYearlyFields AND dic_chlRepCycle',
     &    'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_SSTRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &    'useExfYearlyFields AND dic_SSTRepCycle',
     &    'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( dic_SSSRepCycle.NE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: The use of ',
     &    'useExfYearlyFields AND dic_SSSRepCycle',
     &    'is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
C     useEXF and useExfYearlyFields
#endif /* ALLOW_EXF */

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
C Issue a soft message if a silicaDeepFile is supplied
C    but will not be used because useCalciteSaturation is FALSE
      IF ( DIC_silicaDeepFile .NE. ' ' .AND.
     &                        .NOT. useCalciteSaturation ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: DIC_silicaDeepFile (3d silicate input)'
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

C Issue an error and stop if silicaDeepFile is supplied but
C    DIC_CALCITE_SATURATION code is not compiled
      IF ( DIC_silicaDeepFile .NE. ' '  ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: DIC_silicaDeepFile (3d silicate input)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define DIC_CALCITE_SAT" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* DIC_CALCITE_SAT */

#ifndef ALLOW_FE
      IF ( dic_ironFile .NE. ' ' ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: dic_ironFile (aeolian iron flux input)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define ALLOW_FE" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_FE */

#ifndef READ_PAR
      IF ( dic_parfile .NE. ' ' ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: dic_parFile (PAR input)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define READ_PAR" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* READ_PAR */

#ifndef LIGHT_CHL
      IF ( dic_chlaFile .NE. ' ' ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: dic_chlaFile (chlorophyll input)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#define LIGHT_CHL" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* LIGHT_CHL */

C USE_PLOAD invokes atmospheric presure from ATMOSPHERIC model
C  If USE_PLOAD is not defined, then dic_atmospFile may be used
#ifdef USE_PLOAD
      IF ( dic_atmospFile .NE. ' ' ) THEN
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'to use: dic_atmospFile (atmospheric pressure input)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'DIC_READPARMS: ',
     &    'needs: "#undef USE_PLOAD" in "DIC_OPTIONS.h"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* USE_PLOAD */

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
