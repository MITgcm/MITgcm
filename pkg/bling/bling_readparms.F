#include "BLING_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
      SUBROUTINE BLING_READPARMS( myThid )

C     *========================================================*
C     | subroutine bling_readparms
C     | o Initialise and read parameters for BLING model
C     *========================================================*

      implicit none

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef USE_EXFCO2
# ifdef USE_EXF_INTERPOLATION
#  ifdef ALLOW_EXCH2
#   include "W2_EXCH2_SIZE.h"
#   include "W2_EXCH2_TOPOLOGY.h"
#  endif /* ALLOW_EXCH2 */
#  include "SET_GRID.h"
# endif /* USE_EXF_INTERPOLATION */
# include "EXF_PARAM.h"
# include "EXF_CONSTANTS.h"
#endif /* USE_EXFCO2 */
#include "BLING_VARS.h"

C     === Routine arguments ===
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING

C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     errCount  :: error counter
C     iUnit     :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
      INTEGER iUnit

C ==========================================================
C   Abiotic parameters
C ==========================================================

      NAMELIST /ABIOTIC_PARMS/
     &                        permil, Pa2Atm

C ==========================================================
C   BLING parameters
C ==========================================================

      NAMELIST /BIOTIC_PARMS/
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
     &                     epsln

C ==========================================================
C   BLING forcing
C ==========================================================

      NAMELIST /BLING_FORCING/
     &          bling_windFile, bling_atmospFile, bling_iceFile,
     &          bling_ironFile, bling_silicaFile,
     &          bling_psmFile, bling_plgFile, bling_PdiazFile,
     &          bling_forcingPeriod, bling_forcingCycle,
     &          bling_pCO2, river_conc_trac,
     &          bling_Pc_2dFile, bling_Pc_2d_diazFile,
     &          bling_alpha_photo2dFile,bling_phi_DOM2dFile,
     &          bling_k_Fe2dFile, bling_k_Fe_diaz2dFile,
     &          bling_gamma_POM2dFile, bling_wsink0_2dFile,
     &          bling_phi_sm2dFile,bling_phi_lg2dFile
#ifdef USE_EXFCO2
     &        , apco2file, apco2startdate1, apco2startdate2,
     &          apco2RepCycle, apco2period, apco2StartTime,
     &          exf_inscal_apco2, exf_outscal_apco2, apco2const,
     &          apco2_exfremo_intercept, apco2_exfremo_slope
#ifdef USE_EXF_INTERPOLATION
     &        , apco2_lon0, apco2_lon_inc, apco2_lat0, apco2_lat_inc,
     &          apco2_nlon, apco2_nlat, apco2_interpMethod
#endif /* USE_EXF_INTERPOLATION */
#endif

C ==========================================================
C   secperday        :: seconds in a day = 24*60*60
C   permil           :: set carbon mol/m3 <---> mol/kg conversion factor
C                       default permil = 1024.5 kg/m3
C   Pa2Atm           :: Conversion factor for atmospheric pressure pLoad 
C                       (when coupled to atmospheric model) into Atm.  
C                       Default assumes pLoad in Pascal
C                       1 Atm = 1.01325e5 Pa = 1013.25 mb
C   pivotal          :: Pivotal phytoplankton biomass
C   Pc_0             :: Maximum phytoplankton carbon-specific growth rate at 0C
C   Pc_0_diaz        :: Maximum diazotroph carbon-specific growth rate at 0C
C   lambda_0         :: Carbon-specific phytoplankton mortality rate
C   chl_min          :: minimum chlorophyll concentration [ug kg-1]
C   CtoN             :: Carbon to nitrogen ratio in organic matter 
C   NO3toN           ::
C   HtoC             ::
C   O2toN            :: Oxygen to nitrogen for biological activity 
C   CatoN            :: Calcium to nitrogen uptake by small phyto
C   masstoN          :: 
C   alpha_photo      :: [g C g Chl-1 m2 W-1 s-1]
C   theta_Fe_max_hi  :: Maximum Chl:c ratio, abundant iron 
C   theta_Fe_max_lo  :: Maximum Chl:c ratio, extreme iron limitation 
C   gamma_irr_mem    :: Photoadaptation time scale 
C   gamma_DON        :: Decay timescale of DON 
C   gamma_DOP        :: Decay timescale of DOP
C   gamma_POM        :: 
C   k_Fe             :: Dissolved Fe uptake half-saturation constant 
C   k_Fe_diaz        :: Dissolved Fe uptake half-saturation constant for diazotrophs
C   k_O2             :: Half-saturation constant for aerobic respiration
C   k_NO3            :: Nitrate uptake half-saturation constant
C   k_PO4            :: Phosphate uptake half-saturation constant
C   k_PtoN           :: Half-saturation cellular P:N
C   k_FetoN          :: Half-saturation cellular Fe:N
C   kFe_eq_lig_max   :: Maximum Fe-ligand stability constant
C   kFe_eq_lig_min   :: Minimum Fe-ligand stability constant
C   kFe_eq_lig_Femin :: Constant having to do with photodissociation
C   kFe_eq_lig_irr   :: Iron ligand stability constant  
C   kFe_org          :: Organic-matter dependent scavenging rate 
C   kFe_inorg        :: Inorganic scavenging rate 
C   PtoN_min         :: Minimum P:N uptake ratio 
C   PtoN_max         :: Maximum P:N uptake ratio 
C   FetoN_min        :: Minimum Fe:N uptake ratio 
C   FetoN_max        :: Maximum Fe:N uptake ratio 
C   FetoC_sed        :: Fe:P in sediments
C   remin_min        :: Minimum anaerobic respiration rate 
C   oxic_min         :: Minimum O2 concentration for aerobic respiration 
C   ligand           :: Ligand concentration 
C   kappa_eppley     :: Temperature dependence of growth 
C   kappa_eppley_diaz:: Temperature dependence of growth for diazotrophs
C   kappa_remin      :: Temperature dependence of remineralization
C   ca_remin_depth   :: CaCO3 remineralization lengthscale
C   phi_DOM          :: Fraction of non-sinking production to DOM 
C   phi_sm           :: Fraction of small phytoplankton biomass converted to detritus
C   phi_lg           :: Fraction of large phytoplankton biomass converted to detritus
C   phi_dvm          ::
C   sigma_dvm        ::
C   wsink0z          :: Depth at which sinking rate starts increasing   
C   wsink0           :: Initial sinking rate 
C   wsinkacc         :: Acceleration rate of sinking with depth 
C   parfrac          :: fraction of Qsw avail for photosynthesis
C   alpfe            :: solubility of aeolian iron
C   k0               :: Light attentuation coefficient 
C   epsln            :: a very small number
     
      _RL secperday
      integer k
#ifdef USE_EXFCO2
# ifdef USE_EXF_INTERPOLATION
      INTEGER gridNx, gridNy
      INTEGER j
      _RL inp_lon0, inp_lat0, inp_dLon, inp_dLat
# ifdef ALLOW_EXCH2
      gridNx = exch2_mydNx(1)
      gridNy = exch2_mydNy(1)
# else /* ALLOW_EXCH2 */
      gridNx = Nx
      gridNy = Ny
# endif /* ALLOW_EXCH2 */
# endif /* USE_EXF_INTERPOLATION */
#endif /* USE_EXFCO2 */

      _BEGIN_MASTER(myThid)
      errCount = 0

C ==========================================================
C     Default values

      secperday            = 86400. _d 0
      permil               = 1. _d 0 / 1024.5 _d 0
      Pa2Atm               = 1.01325 _d 5
      CtoN                 = 6.75 _d 0
      HtoC                 = 48. _d 0 / 106. _d 0
      O2toN                = CtoN * (1. _d 0 + 0.25 _d 0 * HtoC)
     &                        + 2. _d 0
      NO3toN               = CtoN * (1. _d 0 + 0.25 _d 0 * HtoC)
     &                        * 0.8 _d 0 + 0.6 _d 0
      CatoN                = CtoN * 0.015 _d 0
      masstoN              = CtoN * 12.001 _d 0
      pivotal              = 1.9 _d -3 / 1028. _d 0 / CtoN / permil
      Pc_0                 = 1.7 _d -5
      Pc_0_diaz            = 0.01 _d -5
      lambda_0             = 0.19 _d 0 / secperday
      chl_min              = 1. _d -5
      alpha_photo          = 0.7 _d -5 * 2.77 _d 18 / 6.022 _d 17
      theta_Fe_max_hi      = 0.04 _d 0
      theta_Fe_max_lo      = 0.01 _d 0
      gamma_irr_mem        = 1. _d 0 / secperday
      gamma_DON            = 0.25 _d 0 / (365.25 _d 0 * secperday)
      gamma_DOP            = 0.5 _d 0 / (365.25 _d 0 * secperday)
      gamma_POM            = 0.12 _d 0 / secperday
      k_Fe                 = 1.6 _d -10 / permil
      k_Fe_diaz            = 7. _d -10 / permil
      k_O2                 = 20. _d -6 / permil
      k_NO3                = 2. _d -6 / permil
      k_PO4                = 1. _d -8 / permil
      k_PtoN               = 1.5 _d -6 / permil
      k_FetoN              = 8. _d -10 / permil
      PtoN_min             = 1. / 28.
      PtoN_max             = 1. / 9.
      FetoN_min            = 2. _d -6 * 6.75
      FetoN_max            = 25. _d -6 * 6.75
      FetoC_sed            = 1. _d -4
      kFe_eq_lig_max       = 8.0 _d 10 * permil
      kFe_eq_lig_min       = 8.0 _d 9 * permil
      kFe_eq_lig_Femin     = 0.05 _d -9 / permil
      kFe_eq_lig_irr       = 0.1 _d 0
      kFe_org              = 0.5 _d 0 / secperday * permil**(0.58)
      kFe_inorg            = 1. _d 3 / secperday * permil**(0.5)
      remin_min            = 0.15 _d 0
      oxic_min             = 1. _d -6 / permil
      Ligand               = 1. _d -9 / permil
      kappa_eppley         = 0.063 _d 0
      kappa_eppley_diaz    = 0.18 _d 0
      kappa_remin          = -0.032 _d 0
      ca_remin_depth       = 1343. _d 0
      phi_DOM              = 0.1 _d 0
      phi_sm               = 0.18 _d 0
      phi_lg               = 1. _d 0
      phi_dvm              = 0.2 _d 0
      sigma_dvm            = 40.0 _d 0
      wsink0z              = 80. _d 0
      wsink0               = 16. _d 0 / secperday
      wsinkacc             = 0.05 _d 0 / secperday
      parfrac              = 0.4 _d 0
      alpfe                = 0.01 _d 0
      k0                   = 0.04 _d 0
      epsln                = 1. _d -30

      bling_windFile  = ' '
      bling_atmospFile= ' '
      bling_iceFile   = ' '
      bling_ironFile  = ' '
      bling_silicaFile= ' '
      bling_psmFile   = ' '
      bling_plgFile   = ' '
      bling_pdiazFile = ' '
      bling_pCO2      = 278. _d -6
      DO k=1,8
       river_conc_trac(k) = 0. _d 0
      ENDDO
      bling_Pc_2dFile        = ' '
      bling_Pc_2d_diazFile   = ' '
      bling_alpha_photo2dFile= ' '
      bling_k_Fe2dFile       = ' '
      bling_k_Fe_diaz2dFile  = ' '
      bling_gamma_POM2dFile  = ' '
      bling_wsink0_2dFile    = ' '
      bling_phi_DOM2dFile    = ' '
      bling_phi_sm2dFile     = ' '
      bling_phi_lg2dFile     = ' '

#ifdef USE_EXFCO2
      apco2startdate1   = 0
      apco2startdate2   = 0
      apco2StartTime    = UNSET_RL
      apco2period       = 0.0 _d 0
      apco2RepCycle     = repeatPeriod
      apco2const        = 0.0 _d 0
      apco2_exfremo_intercept = 0.0 _d 0
      apco2_exfremo_slope = 0.0 _d 0
      apco2file         = ' '
      exf_inscal_apco2  =  1. _d 0
      exf_outscal_apco2 =  1. _d 0
#ifdef USE_EXF_INTERPOLATION
C--   set default input location to match (in case of simple Lat-Long grid)
C     model grid cell-center position (leading to trivial interpolation)
      inp_lon0 = xgOrigin + delX(1)*exf_half
      inp_lat0 = ygOrigin + delY(1)*exf_half
      inp_dLon = delX(1)
      inp_dLat = delY(1)
      apco2_lon0     = inp_lon0
      apco2_lat0     = inp_lat0
      apco2_nlon     = gridNx
      apco2_nlat     = gridNy
      apco2_lon_inc  = inp_dLon
      DO j=1,MAX_LAT_INC
        IF (j.LT.gridNy) THEN
          inp_dLat = (delY(j) + delY(j+1))*exf_half
        ELSE
          inp_dLat = 0.
        ENDIF
        apco2_lat_inc(j) = inp_dLat
      ENDDO
      apco2_interpMethod = 1
#endif /* USE_EXF_INTERPOLATION */
#endif

C     default periodic forcing to same as for physics
       bling_forcingPeriod = externForcingPeriod
       bling_forcingCycle  = externForcingCycle

      WRITE(msgBuf,'(A)') ' BLING_READPARMS: opening data.bling'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data.bling', 'BLING_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Abiotic parameters
      READ(UNIT=iUnit,NML=ABIOTIC_PARMS)

C-    BLING parameters
      READ(UNIT=iUnit,NML=BIOTIC_PARMS)

C-    forcing filenames and parameters
      READ(UNIT=iUnit,NML=BLING_FORCING)

      WRITE(msgBuf,'(A)')
     &   ' BLING_READPARMS: finished reading data.BLING'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C-    derive other parameters:

       QSW_underice = .FALSE.
#ifdef USE_QSW_UNDERICE
       QSW_underice = .TRUE.
#elif (defined (USE_QSW))
C if using Qsw and seaice, then ice fraction is already
C taken into account
       IF ( useSEAICE ) QSW_underice = .TRUE.
       IF ( useThSIce ) QSW_underice = .TRUE.
#endif

      IF ( errCount.GE.1 ) THEN
       WRITE(msgBuf,'(A,I3,A)')
     &     'BLING_READPARMS: detected', errCount,' fatal error(s)'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R BLING_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_BLING */

      RETURN
      END
