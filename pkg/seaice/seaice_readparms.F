C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_readparms.F,v 1.100 2011/10/05 18:33:49 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_READPARMS
C     !INTERFACE:
      SUBROUTINE SEAICE_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_READPARMS
C     | o Routine to read in file data.seaice
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_COST
# include "SEAICE_COST.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_CONSTANTS.h"
#endif /* ALLOW_EXF */
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid     :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL chkFlag
      INTEGER iUnit
#if (defined SEAICE_AGE)||(defined ALLOW_SITRACER)
      INTEGER iTracer
#endif

C     retired parameters
      INTEGER nRetired
      _RL SEAICE_sensHeat, SEAICE_latentWater, SEAICE_latentIce
      _RL SEAICE_salinity
      _RL SEAICE_lhSublim
      _RL areaMin, areaMax, A22, hiceMin
      CHARACTER*(MAX_LEN_MBUF) IceAgeFile

C--   SEAICE parameters
      NAMELIST /SEAICE_PARM01/
     & SEAICEwriteState, SEAICEuseDYNAMICS, SEAICEuseTEM,
     & SEAICEuseEVPpickup, SEAICEuseFluxForm,
     & SEAICEuseMetricTerms,
     & useHB87stressCoupling, SEAICEuseFlooding, usePW79thermodynamics,
     & SEAICErestoreUnderIce,
     & SEAICEadvHeff, SEAICEadvArea, SEAICEadvSnow,
     & SEAICEadvSalt, SEAICEadvAge,
     & SEAICE_clipVelocities, SEAICE_maskRHS, SEAICE_no_slip,
     & LAD, IMAX_TICE, SEAICEadvScheme, SEAICEadvSchArea,
     & SEAICEadvSchHeff, SEAICEadvSchSnow,
     & SEAICEadvSchSalt, SEAICEadvSchAge,
     & SEAICEdiffKhHeff, SEAICEdiffKhSnow, SEAICEdiffKhArea,
     & SEAICEdiffKhSalt, SEAICEdiffKhAge,
     & SEAICE_deltaTtherm, SEAICE_deltaTdyn,
     & SEAICE_deltaTevp, SEAICE_elasticParm, SEAICE_evpTauRelax,
     & SEAICE_evpDampC, SEAICE_zetaMin, SEAICE_zetaMaxFac,
     & SEAICE_monFreq, SEAICE_dumpFreq, SEAICE_taveFreq,
     & SEAICE_initialHEFF, SEAICEareaFormula,
     & SEAICE_rhoAir, SEAICE_rhoIce, SEAICE_rhoSnow, ICE2WATR,
     & SEAICE_cpAir,
     & SEAICE_drag, SEAICE_waterDrag, SEAICE_dryIceAlb,
     & SEAICE_wetIceAlb, SEAICE_drySnowAlb, SEAICE_wetSnowAlb, HO,
     & SEAICE_drag_south, SEAICE_waterDrag_south,
     & SEAICE_dryIceAlb_south, SEAICE_wetIceAlb_south,
     & SEAICE_drySnowAlb_south, SEAICE_wetSnowAlb_south, HO_south,
     & SEAICE_waterAlbedo, SEAICE_strength, SEAICE_eccen,
     & SEAICE_lhFusion, SEAICE_lhEvap, SEAICE_dalton,
     & SEAICE_sensHeat, SEAICE_latentWater, SEAICE_latentIce,
     & SEAICE_salinity,
     & areaMin, areaMax, A22, hiceMin,
     & SEAICE_iceConduct, SEAICE_snowConduct, SEAICE_emissivity,
     & SEAICE_snowThick, SEAICE_shortwave, SEAICE_freeze, OCEAN_drag,
     & SIsalFRAC, SIsal0, SEAICEstressFactor,
     & SEAICE_gamma_t, SEAICE_gamma_t_frz,
     & SEAICE_availHeatFrac, SEAICE_availHeatFracFrz,
     & AreaFile, HeffFile, uIceFile, vIceFile, HsnowFile, HsaltFile,
     & SOLV_MAX_ITERS, SOLV_NCHECK, NPSEUDOTIMESTEPS,
     & LSR_ERROR, DIFF1, SEAICEuseFREEDRIFT,
     & SEAICE_area_reg, SEAICE_hice_reg,
     & SEAICE_area_floor, SEAICE_area_max,
     & SEAICE_airTurnAngle, SEAICE_waterTurnAngle,
     & MAX_HEFF, MIN_ATEMP, MIN_LWDOWN, MAX_TICE, MIN_TICE,
     & SEAICE_EPS, SEAICE_EPS_SQ,
     & SEAICE_tave_mnc, SEAICE_dump_mnc, SEAICE_mon_mnc,
     & SEAICE_debugPointI, SEAICE_debugPointJ

#ifdef ALLOW_COST
      NAMELIST /SEAICE_PARM02/
     &          mult_ice_export, mult_ice, cost_ice_flag,
     &          costIceStart1, costIceStart2,
     &          costIceEnd1, costIceEnd2,
     &          cost_ice_flag,
     &          SEAICE_clamp_salt, SEAICE_clamp_theta,
     &          mult_smrsst, smrsstbarfile,
     &          mult_smrsss, smrsssbarfile,
     &          mult_smrarea, smrareabarfile, smrareadatfile,
     &          wsmrarea0, wmean_smrarea, smrarea_errfile,
     &          smrareastartdate1, smrareastartdate2, smrareaperiod
#endif

#if (defined SEAICE_AGE)||(defined ALLOW_SITRACER)
      NAMELIST /SEAICE_PARM03/
     &  IceAgeTrFile, SItrName, SItrNameLong, SItrUnit, SItrMate
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)')
     &' '
      CALL PRINT_MESSAGE( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)') ' SEAICE_READPARMS: opening data.seaice'
      CALL PRINT_MESSAGE( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.seaice', 'SEAICE_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   set default sea ice parameters
      SEAICEwriteState   = .FALSE.
#ifdef SEAICE_ALLOW_DYNAMICS
      SEAICEuseDYNAMICS  = .TRUE.
#else
      SEAICEuseDYNAMICS  = .FALSE.
#endif
      SEAICEadjMODE      = 0
      SEAICEuseFREEDRIFT = .FALSE.
      SEAICEuseTEM       = .FALSE.
      SEAICEuseMetricTerms = .TRUE.
      SEAICEuseEVPpickup = .TRUE.
      SEAICEuseFluxForm  = .TRUE.
C--   old default:
c     SEAICEuseFluxForm  = .FALSE.
      SEAICErestoreUnderIce = .FALSE.
      useHB87stressCoupling = .FALSE.
      usePW79thermodynamics = .TRUE.
      SEAICEadvHeff      = .TRUE.
      SEAICEadvArea      = .TRUE.
      SEAICEadvSnow      = .TRUE.
#ifdef SEAICE_VARIABLE_SALINITY
      SEAICEadvSalt      = .TRUE.
#else
      SEAICEadvSalt      = .FALSE.
#endif
#ifdef SEAICE_AGE
      SEAICEadvAge       = .TRUE.
#else
      SEAICEadvAge       = .FALSE.
#endif
#ifdef ALLOW_SEAICE_FLOODING
      SEAICEuseFlooding  = .TRUE.
#else
      SEAICEuseFlooding  = .FALSE.
#endif /* ALLOW_SEAICE_FLOODING */
      SEAICE_no_slip     = .FALSE.
      SEAICE_clipVelocities = .FALSE.
      SEAICE_maskRHS     = .FALSE.
      SEAICEadvScheme    = 2
      SEAICEadvSchArea   = UNSET_I
      SEAICEadvSchHeff   = UNSET_I
      SEAICEadvSchSnow   = UNSET_I
      SEAICEadvSchSalt   = UNSET_I
      SEAICEadvSchAge    = UNSET_I
      SEAICEdiffKhArea   = UNSET_RL
      SEAICEdiffKhHeff   = UNSET_RL
      SEAICEdiffKhSnow   = UNSET_RL
      SEAICEdiffKhSalt   = UNSET_RL
      SEAICEdiffKhAge    = UNSET_RL
      SEAICE_deltaTtherm = dTtracerLev(1)
      SEAICE_deltaTdyn   = dTtracerLev(1)
      SEAICE_deltaTevp   = UNSET_RL
C     Hunke, JCP, 2001 use 615 kg/m^2 for this, but does not recommend using it
      SEAICE_evpDampC    = -1. _d 0
      SEAICE_zetaMin     = 0. _d 0
      SEAICE_zetaMaxFac  = 2.5 _d 8
      SEAICE_monFreq     = monitorFreq
      SEAICE_dumpFreq    = dumpFreq
      SEAICE_taveFreq    = taveFreq
      SEAICE_elasticParm = 0.33333333333333333333333333 _d 0
      SEAICE_evpTauRelax = -1. _d 0
#ifdef ALLOW_MNC
      SEAICE_tave_mnc = timeave_mnc
      SEAICE_dump_mnc = snapshot_mnc
      SEAICE_mon_mnc  = monitor_mnc
#else
      SEAICE_tave_mnc = .FALSE.
      SEAICE_dump_mnc = .FALSE.
      SEAICE_mon_mnc  = .FALSE.
#endif
      SEAICE_initialHEFF = ZERO
#ifdef SEAICE_GROWTH_LEGACY
      SEAICEareaFormula = 1
#else
      SEAICEareaFormula = 2
#endif
      SEAICE_rhoIce      = 0.91   _d +03
      SEAICE_rhoSnow     = 330.   _d 0
      ICE2WATR           = UNSET_RL
      SEAICE_drag        = 0.002  _d 0
      OCEAN_drag         = 0.001  _d 0
      SEAICE_waterDrag   = 5.5    _d 0
      SEAICE_dryIceAlb   = 0.75   _d 0
      SEAICE_wetIceAlb   = 0.66   _d 0
      SEAICE_drySnowAlb  = 0.84   _d 0
      SEAICE_wetSnowAlb  = 0.7    _d 0
      HO                 = 0.5    _d 0
      SEAICE_drag_south       = UNSET_RL
      SEAICE_waterDrag_south  = UNSET_RL
      SEAICE_dryIceAlb_south  = UNSET_RL
      SEAICE_wetIceAlb_south  = UNSET_RL
      SEAICE_drySnowAlb_south = UNSET_RL
      SEAICE_wetSnowAlb_south = UNSET_RL
      HO_south                = UNSET_RL
      SEAICE_waterAlbedo = 0.1    _d +00
      SEAICE_strength    = 2.75   _d +04
      SEAICE_eccen       = 2.     _d 0
C     coefficients for flux computations/bulk formulae
      SEAICE_dalton      = 1.75   _d -03
#ifdef ALLOW_EXF
      IF ( useEXF ) THEN
C     Use parameters that have already been set in data.exf
C     to be consistent
       SEAICE_rhoAir     = atmrho
       SEAICE_cpAir      = atmcp
       SEAICE_lhEvap     = flamb
       SEAICE_lhFusion   = flami
      ELSE
#else
      IF ( .TRUE. ) THEN
#endif /* ALLOW_EXF */
       SEAICE_rhoAir     = 1.3    _d 0
       SEAICE_cpAir      = 1004.  _d 0
       SEAICE_lhEvap     = 2.50   _d 6
       SEAICE_lhFusion   = 3.34   _d 5
      ENDIF
      SEAICE_iceConduct  = 2.1656 _d +00
      SEAICE_snowConduct = 3.1    _d -01
      SEAICE_emissivity  = 5.5    _d -08
      SEAICE_snowThick   = 0.15   _d 0
      SEAICE_shortwave   = 0.30   _d 0
      SEAICE_freeze      = -1.96  _d 0
      SIsalFRAC          = 0.0    _d 0
      SIsal0             = 0.0    _d 0
      SEAICE_gamma_t     = UNSET_RL
      SEAICE_gamma_t_frz = UNSET_RL
      SEAICE_availHeatFrac = UNSET_RL
      SEAICE_availHeatFracFrz = UNSET_RL
      SEAICEstressFactor = 1.     _d 0
      AreaFile   = ' '
      HsnowFile  = ' '
      HsaltFile  = ' '
      HeffFile   = ' '
      uIceFile   = ' '
      vIceFile   = ' '
      LAD        = 2
      IMAX_TICE  = 10
      SOLV_MAX_ITERS = 1500
      SOLV_NCHECK= 2
C     two pseudo time steps correspond to the original modified
C     Euler time stepping scheme of Zhang+Hibler (1997)
      NPSEUDOTIMESTEPS = 2
      LSR_ERROR  = 0.0001    _d 0
      DIFF1      = UNSET_RL
C--   old DIFF1 default:
c     DIFF1      = .004 _d 0

#ifdef SEAICE_GROWTH_LEGACY
      SEAICE_area_reg   = 0.15 _d 0
      SEAICE_area_floor = 0.15 _d 0
#else
      SEAICE_area_floor = siEPS
      SEAICE_area_reg   = siEPS
#endif
      SEAICE_hice_reg   = 0.05 _d 0
      SEAICE_area_max    = 1.00 _d 0

      SEAICE_airTurnAngle   = 0.0 _d 0
      SEAICE_waterTurnAngle = 0.0 _d 0
      MAX_HEFF          = 10.     _d 0
      MIN_ATEMP         = -50.    _d 0
      MIN_LWDOWN        = 60.     _d 0
      MAX_TICE          = 30.     _d 0
      MIN_TICE          = -50.    _d 0
      SEAICE_EPS        = 1.      _d -10
      SEAICE_EPS_SQ     = -99999.
C     retired parameters
C     SEAICE_sensHeat    = 1.75 _d -03 * 1004 * 1.3
C     SEAICE_sensHeat    = 2.284  _d +00
      SEAICE_sensHeat    = UNSET_RL
C     SEAICE_latentWater = 1.75 _d -03 * 2.500 _d 06 * 1.3
C     SEAICE_latentWater = 5.6875 _d +03
      SEAICE_latentWater = UNSET_RL
C     SEAICE_latentIce   = 1.75 _d -03 * 2.834 _d 06 * 1.3
C     SEAICE_latentIce   = 6.4474 _d +03
      SEAICE_latentIce   = UNSET_RL
      SEAICE_salinity    = UNSET_RL
      IceAgeFile         = ' '
      areaMin            = UNSET_RL
      hiceMin            = UNSET_RL
      A22                = UNSET_RL
      areaMax            = UNSET_RL
      SEAICE_lhSublim    = UNSET_RL
C     end retired parameters
      SEAICE_debugPointI = UNSET_I
      SEAICE_debugPointJ = UNSET_I

#ifdef ALLOW_COST
      mult_ice_export   =  0. _d 0
      mult_ice          =  0. _d 0
      costIceStart1     =  0
      costIceStart2     =  0
      costIceEnd1       =  0
      costIceEnd2       =  0
      cost_ice_flag     =  1
      SEAICE_clamp_salt  = 27.5   _d 0
      SEAICE_clamp_theta = 0.0001 _d 0
c
      mult_smrsst       =  0. _d 0
      mult_smrsss       =  0. _d 0
      mult_smrarea      =  0. _d 0
      wsmrarea0         =  0.5 _d 0
      wmean_smrarea     =  0.5 _d 0
      smrsstbarfile     =  'smrsstbar'
      smrsssbarfile     =  'smrsssbar'
      smrareabarfile    =  'smrareabar'
      smrareadatfile    =  ' '
      smrarea_errfile   =  ' '
# ifdef ALLOW_CAL
      smrareastartdate1 = startDate_1
      smrareastartdate2 = startDate_2
# endif
#endif

C--   Read settings from model parameter file "data.seaice".
      READ(UNIT=iUnit,NML=SEAICE_PARM01)

#ifdef ALLOW_COST
      READ(UNIT=iUnit,NML=SEAICE_PARM02)
#endif

#if (defined SEAICE_AGE)||(defined ALLOW_SITRACER)
      DO iTracer = 1, SEAICE_num
       IceAgeTrFile(iTracer) = ' '
      ENDDO
      DO iTracer = 1, SItrMaxNum
       SItrName(iTracer) = ' '
       SItrNameLong(iTracer) = ' '
       SItrUnit(iTracer) = ' '
       SItrMate(iTracer) = 'HEFF'
      ENDDO
C--
      READ(UNIT=iUnit,NML=SEAICE_PARM03)
#endif

      CLOSE(iUnit)

      WRITE(msgBuf,'(A)')
     &     ' SEAICE_READPARMS: finished reading data.seaice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)

      IF (ICE2WATR.EQ.UNSET_RL) ICE2WATR = SEAICE_rhoIce*recip_rhoConst
      IF (SEAICE_drag_south       .EQ. UNSET_RL)
     &    SEAICE_drag_south       = SEAICE_drag
      IF (SEAICE_waterDrag_south  .EQ. UNSET_RL)
     &    SEAICE_waterDrag_south  = SEAICE_waterDrag
      IF (SEAICE_dryIceAlb_south  .EQ. UNSET_RL)
     &    SEAICE_dryIceAlb_south  = SEAICE_dryIceAlb
      IF (SEAICE_wetIceAlb_south  .EQ. UNSET_RL)
     &    SEAICE_wetIceAlb_south  = SEAICE_wetIceAlb
      IF (SEAICE_drySnowAlb_south .EQ. UNSET_RL)
     &    SEAICE_drySnowAlb_south = SEAICE_drySnowAlb
      IF (SEAICE_wetSnowAlb_south .EQ. UNSET_RL)
     &    SEAICE_wetSnowAlb_south = SEAICE_wetSnowAlb
      IF (HO_south                .EQ. UNSET_RL)
     &    HO_south                = HO

C     Check that requested time step size is supported.  The combination
C     below is the only one that is supported at this time.  Does not
C     mean that something fancier will not work, just that it has not
C     yet been tried nor thought through.
      IF ( SEAICE_deltaTtherm .NE. dTtracerLev(1)     .OR.
     &     SEAICE_deltaTdyn   .LT. SEAICE_deltaTtherm .OR.
     &     (SEAICE_deltaTdyn/SEAICE_deltaTtherm) .NE.
     &     INT(SEAICE_deltaTdyn/SEAICE_deltaTtherm) ) THEN
         WRITE(msgBuf,'(A)')
     &        'Unsupported combination of SEAICE_deltaTtherm,'
         CALL PRINT_ERROR( msgBuf , myThid)
         WRITE(msgBuf,'(A)')
     &        ' SEAICE_deltaTdyn, and dTtracerLev(1)'
         CALL PRINT_ERROR( msgBuf , myThid)
         STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF
#ifdef SEAICE_ALLOW_EVP
      SEAICEuseEVP = .FALSE.
      IF ( SEAICE_deltaTevp .NE. UNSET_RL ) SEAICEuseEVP = .TRUE.
      IF ( SEAICEuseEVP ) THEN
       IF (    (SEAICE_deltaTdyn/SEAICE_deltaTevp) .NE.
     &      INT(SEAICE_deltaTdyn/SEAICE_deltaTevp) ) THEN
        WRITE(msgBuf,'(A)')
     &       'SEAICE_deltaTevp must be a factor of SEAICE_deltaTdyn.'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
       ENDIF
       IF ( SEAICE_elasticParm .LE. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A)')
     &       'SEAICE_elasticParm must greater than 0.'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
       ENDIF
       IF ( SEAICE_evpTauRelax .LE. 0. _d 0 )
     &      SEAICE_evpTauRelax = SEAICE_deltaTdyn*SEAICE_elasticParm
      ENDIF
#endif /* SEAICE_ALLOW_EVP */
C
#ifdef SEAICE_ALLOW_FREEDRIFT
#ifdef SEAICE_ALLOW_EVP
      IF ( SEAICEuseFREEDRIFT ) SEAICEuseEVP = .FALSE.
#endif
      IF ( SEAICEuseFREEDRIFT ) THEN
        WRITE(msgBuf,'(A,A)')
     &       'WARNING FROM S/R SEAICE_READPARMS:',
     &       ' switch seaice from LSR or EVP to "free drift"'
      CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      ENDIF
#endif /* SEAICE_ALLOW_FREEDRIFT */

C     Check the consitency of a few parameters
      IF ( SEAICE_availHeatFrac .NE. UNSET_RL .AND.
     &     SEAICE_gamma_t       .NE. UNSET_RL ) THEN
         WRITE(msgBuf,'(A)')
     &        'SEAICE_availHeatFrac and SEAICE_gamma_t '//
     &        'must not be set at the same time.'
         CALL PRINT_ERROR( msgBuf , myThid)
         STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF
      IF ( SEAICE_availHeatFracFrz .NE. UNSET_RL .AND.
     &     SEAICE_gamma_t_frz      .NE. UNSET_RL ) THEN
         WRITE(msgBuf,'(A)')
     &        'SEAICE_availHeatFracFrz and SEAICE_gamma_t_frz '//
     &        'must not be set at the same time.'
         CALL PRINT_ERROR( msgBuf , myThid)
         STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF


C     Make sure that we have least two pseudo time steps
      NPSEUDOTIMESTEPS = MAX(NPSEUDOTIMESTEPS,2)
C     Set default values
      IF ( SEAICE_availHeatFrac .EQ. UNSET_RL ) THEN
         SEAICE_availHeatFrac = 1. _d 0
      ENDIF
      IF ( SEAICE_gamma_t .NE. UNSET_RL .AND.
     &     SEAICE_gamma_t .NE. 0. _d 0 ) THEN
         SEAICE_availHeatFrac = SEAICE_deltaTtherm/SEAICE_gamma_t
      ENDIF
      IF ( SEAICE_availHeatFracFrz .EQ. UNSET_RL ) THEN
         SEAICE_availHeatFracFrz = SEAICE_availHeatFrac
      ENDIF
      IF ( SEAICE_gamma_t_frz .EQ. 0. _d 0 ) THEN
         SEAICE_availHeatFracFrz = 1. _d 0
      ELSEIF ( SEAICE_gamma_t_frz .NE. UNSET_RL ) THEN
         SEAICE_availHeatFracFrz = SEAICE_deltaTtherm/SEAICE_gamma_t_frz
      ENDIF

      IF ( useThSice ) THEN
C     If the thsice package with the Winton thermodynamics is used
C     is does not make sense to have the following parameters defined,
C     so we reset them here
       usePW79thermodynamics = .FALSE.
       SEAICEadvHeff         = .FALSE.
       SEAICEadvArea         = .FALSE.
       SEAICEadvSnow         = .FALSE.
       SEAICEadvSalt         = .FALSE.
       SEAICEadvAge          = .FALSE.
      ENDIF
C     Set advection schemes to some sensible values if not done
C     in data.seaice
      IF ( SEAICEadvSchArea .EQ. UNSET_I )
     &     SEAICEadvSchArea = SEAICEadvSchHeff
      IF ( SEAICEadvSchArea .EQ. UNSET_I )
     &     SEAICEadvSchArea = SEAICEadvScheme
      IF ( SEAICEadvScheme .NE. SEAICEadvSchArea )
     &     SEAICEadvScheme  = SEAICEadvSchArea
      IF ( SEAICEadvSchHeff .EQ. UNSET_I )
     &     SEAICEadvSchHeff = SEAICEadvSchArea
      IF ( SEAICEadvSchSnow .EQ. UNSET_I )
     &     SEAICEadvSchSnow = SEAICEadvSchHeff
      IF ( SEAICEadvSchSalt .EQ. UNSET_I )
     &     SEAICEadvSchSalt = SEAICEadvSchHeff
      IF ( SEAICEadvSchAge  .EQ. UNSET_I )
     &     SEAICEadvSchAge  = SEAICEadvSchHeff
C     Set diffusivity to some sensible values if not done in data.seaice
      IF ( SEAICEdiffKhArea .EQ. UNSET_RL )
     &     SEAICEdiffKhArea = SEAICEdiffKhHeff
      IF ( SEAICEdiffKhArea .EQ. UNSET_RL )
     &     SEAICEdiffKhArea = 0. _d 0
      IF ( SEAICEdiffKhHeff .EQ. UNSET_RL )
     &     SEAICEdiffKhHeff = SEAICEdiffKhArea
      IF ( SEAICEdiffKhSnow .EQ. UNSET_RL )
     &     SEAICEdiffKhSnow = SEAICEdiffKhHeff
      IF ( SEAICEdiffKhSalt .EQ. UNSET_RL )
     &     SEAICEdiffKhSalt = SEAICEdiffKhHeff
      IF ( SEAICEdiffKhAge  .EQ. UNSET_RL )
     &     SEAICEdiffKhAge  = SEAICEdiffKhHeff

      IF ( SEAICE_EPS_SQ .EQ. -99999. )
     &     SEAICE_EPS_SQ = SEAICE_EPS * SEAICE_EPS

C     retired parameter
      nRetired = 0
      IF ( SEAICE_sensHeat    .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_sensHeat" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: set "SEAICE_cpAir", ',
     &  '"SEAICE_dalton", and "SEAICE_rhoAir" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_latentWater .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_latentWater" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: set "SEAICE_lhEvap", ',
     &  '"SEAICE_dalton", and "SEAICE_rhoAir" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_latentIce   .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_latentIce" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: set "SEAICE_lhFusion", ',
     &  '"SEAICE_dalton", and "SEAICE_rhoAir" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( SEAICE_salinity   .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_salinity" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: set "SIsalFRAC instead"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( IceAgeFile .NE. ' ' ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "IceAgeFile" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"IceAgeTrFile(SEAICE_num)" array '
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( areaMax .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "areaMax" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"SEAICE_area_max"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( areaMin .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "areaMin" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"SEAICE_area_reg" for regularization and ',
     &  '"SEAICE_area_floor" setting a lower bound'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF (SEAICE_lhSublim .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "SEAICE_lhSublim" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: specify ',
     &  '"SEAICE_lhFusion" and "SEAICE_lhEvap" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( A22 .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "A22" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"SEAICE_area_reg" for regularization'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( hiceMin .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: "hiceMin" ',
     &  'is no longer allowed in file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R SEAICE_READPARMS: replaced by ',
     &  '"SEAICE_hice_reg" for regularization'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C-    Set Output type flags :
      SEAICE_tave_mdsio = .TRUE.
      SEAICE_dump_mdsio = .TRUE.
      SEAICE_mon_stdio  = .TRUE.
#ifdef ALLOW_MNC
      IF (useMNC) THEN
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_tave_mnc ) SEAICE_tave_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_dump_mnc ) SEAICE_dump_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_mon_mnc  ) SEAICE_mon_stdio  = .FALSE.
      ENDIF
#endif

      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'Error reading parameter file "data.seaice"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'some out of date parameters were found in the namelist'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF

      IF ( DIFF1 .EQ. UNSET_RL ) THEN
        DIFF1 = 0. _d 0
        chkFlag = .FALSE.
        IF ( SEAICEadvScheme.EQ.2 ) THEN
C--   Since DIFF1 default value has been changed (2011/05/29), issue a warning
C     in case using centered avection scheme without any diffusion:
         IF ( SEAICEadvHeff .AND. SEAICEdiffKhHeff .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for HEFF  without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( SEAICEadvArea .AND. SEAICEdiffKhArea .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for AREA  without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( SEAICEadvSnow .AND. SEAICEdiffKhSnow .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for HSNOW without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( SEAICEadvSalt .AND. SEAICEdiffKhSalt .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for HSALT without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( SEAICEadvAge  .AND. SEAICEdiffKhAge  .EQ. 0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &    'will use AdvScheme = 2 for IceAgeTr without any diffusion'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          chkFlag = .TRUE.
         ENDIF
         IF ( chkFlag ) THEN
          WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_READPARMS: ',
     &      'since DIFF1 is set to 0 (= new DIFF1 default value)'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDIF
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C--   Summarise pkg/seaice configuration
      CALL SEAICE_SUMMARY( myThid )

      RETURN
      END
