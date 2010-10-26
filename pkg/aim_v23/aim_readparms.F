C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_readparms.F,v 1.19 2010/10/26 20:59:53 dfer Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_READPARMS
C     !INTERFACE:
      SUBROUTINE AIM_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_READPARMS
C     | o Read AIM physics package parameters
C     *==========================================================*
C     | Initialized parameter in common blocks:
C     |  FORCON, SFLCON, CNVCON, LSCCON, RADCON, VDICON
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "AIM_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AIM_PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_AIM_CO2
#include "AIM_CO2.h"
#endif


C-   Physical constants + functions of sigma and latitude
c #include "com_physcon.h"
C-   Constants for sub-grid-scale physics
#include "com_forcon.h"
#include "com_sflcon.h"
#include "com_cnvcon.h"
#include "com_lsccon.h"
#include "com_radcon.h"
#include "com_vdicon.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM

C     == Local Variables ==
C     msgBuf      :: Informational/error meesage buffer
C     iUnit       :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   AIM interface parameter (common AIM_PARM_?):
C     aim_useFMsurfBC :: select surface B.C. from Franco Molteni
C     aim_useMMsurfFc :: select Monthly Mean surface forcing (e.g., NCEP)
C     aim_surfForc_TimePeriod :: Length of forcing time period (e.g. 1 month)
C     aim_surfForc_NppCycle   :: Number of time period per Cycle (e.g. 12)
C     aim_surfForc_TransRatio :: transition ratio from one month to the next
C     aim_surfPotTemp :: surf.Temp input file is in Pot.Temp (aim_useMMsurfFc)
C     aim_energPrecip :: account for energy of precipitation (snow & rain temp)
C     aim_splitSIOsFx :: compute separately Sea-Ice & Ocean surf. Flux
C                 (also land SW & LW) ; default=F as in original version
C     aim_MMsufx   :: sufix for all Monthly Mean surface forcing files
C     aim_MMsufxLength :: Length of sufix (Monthly Mean surf. forc. files)
C     aim_LandFile :: file name for Land fraction       (F.M. surfBC)
C     aim_albFile  :: file name for Albedo input file   (F.M. surfBC)
C     aim_vegFile  :: file name for vegetation fraction (F.M. surfBC)
C     aim_sstFile  :: file name for  Sea.Surf.Temp      (F.M. surfBC)
C     aim_lstFile  :: file name for Land.Surf.Temp      (F.M. surfBC)
C     aim_oiceFile :: file name for Sea Ice fraction    (F.M. surfBC)
C     aim_snowFile :: file name for Snow depth          (F.M. surfBC)
C     aim_swcFile  :: file name for Soil Water content  (F.M. surfBC)
C     aim_dragStrato :: stratospheric-drag damping time scale (s)
C     aim_clrSkyDiag :: compute clear-sky radiation for diagnostics
C     aim_taveFreq :: Frequency^-1 for time-average output (s)
C     aim_diagFreq :: Frequency^-1 for diagnostic output (s)
C     aim_tendFreq :: Frequency^-1 for tendencies output (s)
C     aim_selectOceAlbedo :: Ocean albedo(latitude) scheme (integer)
C                              0 = constant (default)
C                              1 = Briegleb et al. 1986
      NAMELIST /AIM_PARAMS/
     &    aim_useFMsurfBC, aim_useMMsurfFc,
     &    aim_surfForc_TimePeriod, aim_surfForc_NppCycle,
     &    aim_surfForc_TransRatio, aim_surfPotTemp,
     &    aim_energPrecip, aim_splitSIOsFx,
     &    aim_MMsufx, aim_MMsufxLength,
     &    aim_LandFile, aim_albFile, aim_vegFile,
     &    aim_sstFile, aim_lstFile, aim_oiceFile, aim_snowFile,
     &    aim_swcFile, aim_qfxFile,
     &    aim_dragStrato,
     &    aim_clrSkyDiag, aim_taveFreq, aim_diagFreq, aim_tendFreq,
     &    aim_timeave_mnc, aim_snapshot_mnc,
     &    aim_pickup_write_mnc, aim_pickup_read_mnc,
     &    aim_selectOceAlbedo

C--   Physical constants (common PHYCON) :
C       P0    = reference pressure                 [Pa=N/m2]
C       GG    = gravity accel.                     [m/s2]
C       RD    = gas constant for dry air           [J/kg/K]
C       CP    = specific heat at constant pressure [J/kg/K]
C       ALHC  = latent heat of condensation        [J/g]
C       ALHF  = latent heat of freezing            [J/g]
C       SBC   = Stefan-Boltzmann constant
C     rainCP  = heat capacity of liquid water      [J/g/K]
C     tFreeze = freezing temperature of pure water [K]

C--   Constants for forcing fields (common FORCON) :
C      SOLC   = Solar constant (area averaged) in W/m^2
C      OBLIQ  = Obliquity (in degree) only used if #define ALLOW_INSOLATION
C      ALBSEA = Albedo over sea
C      ALBICE = Albedo over sea ice (for ice fraction = 1)
C      ALBSN  = Albedo over snow (for snow depth > SDALB)
C      SDALB  = Snow depth (mm water) corresponding to maximum albedo
C      SWCAP  = Soil wetness at field capacity (volume fraction)
C      SWWIL  = Soil wetness at wilting point  (volume fraction)
C      hSnowWetness :: snow depth (m) corresponding to maximum wetness
      NAMELIST /AIM_PAR_FOR/
     &    SOLC, ALBSEA, ALBICE, ALBSN,
     &    SDALB, SWCAP, SWWIL, hSnowWetness, OBLIQ

C--   Constants for surface fluxes (common SFLCON) :
C      FWIND0 = ratio of near-sfc wind to lowest-level wind
C      FTEMP0 = weight for near-sfc temperature extrapolation (0-1) :
C               1 : linear extrapolation from two lowest levels
C               0 : constant potential temperature ( = lowest level)
C      FHUM0  = weight for near-sfc specific humidity extrapolation (0-1) :
C               1 : extrap. with constant relative hum. ( = lowest level)
C               0 : constant specific hum. ( = lowest level)
C      CDL    = drag coefficient for momentum over land
C      CDS    = drag coefficient for momentum over sea
C      CHL    = heat exchange coefficient over land
C      CHS    = heat exchange coefficient over sea
C      VGUST  = wind speed for sub-grid-scale gusts
C      CTDAY  = daily-cycle correction (dTskin/dSSRad)
C      DTHETA = Potential temp. gradient for stability correction
C      dTstab = potential temp. increment for stability function derivative
C      FSTAB  = Amplitude of stability correction (fraction)
C      HDRAG  = Height scale for orographic correction
C      FHDRAG = Amplitude of orographic correction (fraction)
      NAMELIST /AIM_PAR_SFL/
     &    FWIND0, FTEMP0, FHUM0,
     &    CDL, CDS, CHL, CHS, VGUST, CTDAY,
     &    DTHETA, dTstab, FSTAB, HDRAG, FHDRAG

C--   Convection constants (common CNVCON) :
C      PSMIN  = minimum (norm.) sfc. pressure for the occurrence of convection
C      TRCNV  = time of relaxation (in hours) towards reference state
C      QBL    = specific hum. threshold in the boundary layer
C      RHBL   = relative hum. threshold in the boundary layer
C      RHIL   = rel. hum. threshold in intermed. layers for secondary mass flux
C      ENTMAX = max. entrainment as a fraction of cloud-base mass flux
C      SMF    = ratio between secondary and primary mass flux at cloud-base
      NAMELIST /AIM_PAR_CNV/
     &    PSMIN, TRCNV, QBL, RHBL, RHIL, ENTMAX, SMF


C--   Constants for large-scale condendation (common LSCCON) :
C      TRLSC  = Relaxation time (in hours) for specific humidity
C      RHLSC  = Maximum relative humidity threshold (at sigma=1)
C      DRHLSC = Vertical range of relative humidity threshold
C      QSMAX  = used to define the maximum latent heat release
      NAMELIST /AIM_PAR_LSC/
     &    TRLSC, RHLSC, DRHLSC, QSMAX

C--    Radiation constants (common RADCON) :
C      RHCL1  = relative hum. corresponding to cloud cover = 0
C      RHCL2  = relative hum. corresponding to cloud cover = 1
C      QACL1  = specific hum. threshold for cloud cover in the upper troposphere
C      QACL2  = specific hum. threshold for cloud cover in the upper troposphere
C      ALBCL  = cloud albedo (for cloud cover = 1)
C      EPSSW  = fraction of incoming solar radiation absorbed by ozone
C      EPSLW  = fraction of surface LW radiation emitted directly to space
C      EMISFC = longwave surface emissivity
C--:            shortwave absorptivities (for dp = 10^5 Pa) :
C      ABSDRY = abs. of dry air      (visible band)
C      ABSAER = abs. of aerosols     (visible band)
C      ABSWV1 = abs. of water vapour (visible band, for dq = 1 g/kg)
C      ABSWV2 = abs. of water vapour (near IR band, for dq = 1 g/kg)
C      ABSCL1 = abs. of clouds       (visible band, constant term)
C      ABSCL2 = abs. of clouds       (visible band, for dw = 1 g/kg)
C--:            longwave absorptivities (per dp = 10^5 Pa) :
C      ABLWIN = abs. of air in "window" band
C      ABLCO2 = abs. of air in CO2 band
C      ABLWV1 = abs. of water vapour in H2O band 1 (weak),   for dq = 1 g/kg
C      ABLWV2 = abs. of water vapour in H2O band 2 (strong), for dq = 1 g/kg
C      ABLCL1 = abs. of clouds       in "window" band,       constant term
C      ABLCL2 = abs. of clouds       in "window" band,       for dw = 1 g/kg
      NAMELIST /AIM_PAR_RAD/
     &    RHCL1,  RHCL2,  QACL1,  QACL2,  ALBCL,
     &    EPSSW,  EPSLW,  EMISFC,
     &    ABSDRY, ABSAER, ABSWV1, ABSWV2, ABSCL1, ABSCL2,
     &    ABLWIN, ABLCO2, ABLWV1, ABLWV2, ABLCL1, ABLCL2

C--   Constants for vertical dif. and sh. conv. (common VDICON) :
C      TRSHC  = relaxation time (in hours) for shallow convection
C      TRVDI  = relaxation time (in hours) for moisture diffusion
C      TRVDS  = relaxation time (in hours) for super-adiab. conditions
C      RHGRAD = maximum gradient of relative humidity (d_RH/d_sigma)
C      SEGRAD = minimum gradient of dry static energy (d_DSE/d_phi)
      NAMELIST /AIM_PAR_VDI/
     &    TRSHC, TRVDI, TRVDS, RHGRAD, SEGRAD

#ifdef ALLOW_AIM_CO2
      NAMELIST /AIM_PAR_CO2/ atmpCO2init, Aim_CO2_Flag
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C--  Set default value for AIM interface code (AIM_PARAMS.h):
      aim_useFMsurfBC = .TRUE.
      aim_useMMsurfFc = .FALSE.
      aim_surfPotTemp = .FALSE.
      aim_energPrecip = .FALSE.
      aim_splitSIOsFx = .FALSE.
      aim_clrSkyDiag  = .FALSE.
#ifdef ALLOW_MNC
      aim_timeave_mnc       = timeave_mnc .AND. useMNC
      aim_snapshot_mnc      = snapshot_mnc .AND. useMNC
      aim_pickup_write_mnc  = pickup_write_mnc .AND. useMNC
      aim_pickup_read_mnc   = pickup_read_mnc .AND. useMNC
#else
      aim_timeave_mnc       = .FALSE.
      aim_snapshot_mnc      = .FALSE.
      aim_pickup_write_mnc  = .FALSE.
      aim_pickup_read_mnc   = .FALSE.
#endif
      aim_MMsufx = '.bin'
      aim_MMsufxLength = 4
C-    Length (s) of one time period:
      aim_surfForc_TimePeriod = 30.*86400.
C-    Number of time period per Cycle:
      aim_surfForc_NppCycle   = 12
      aim_selectOceAlbedo = 0
C-    define how fast the (linear) transition is from one month to the next
C      = 1                 -> linear between 2 midle month
C      > TimePeriod/deltaT -> jump from one month to the next one
      aim_surfForc_TransRatio = 1.
      aim_LandFile = ' '
      aim_albFile  = ' '
      aim_vegFile  = ' '
      aim_sstFile  = ' '
      aim_lstFile  = ' '
      aim_oiceFile = ' '
      aim_snowFile = ' '
      aim_swcFile  = ' '
      aim_qfxFile  = ' '
      aim_dragStrato = 0.
      aim_taveFreq = taveFreq
      aim_diagFreq = dumpFreq
      aim_tendFreq = 0.
      OBLIQ = 23.45 _d 0
#ifdef ALLOW_AIM_CO2
      atmpCO2init  = 326. _d -6
      Aim_CO2_Flag = 1
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      WRITE(msgBuf,'(A)') ' AIM_READ_PHYSPARMS: opening data.aimphys'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE( 'data.aimphys', 'AIM_READ_PHYSPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Parameters for AIM interface code:
      READ(UNIT=iUnit,NML=AIM_PARAMS)

C-    Constants for boundary forcing
      READ(UNIT=iUnit,NML=AIM_PAR_FOR)

C-    Constants for surface fluxes
      READ(UNIT=iUnit,NML=AIM_PAR_SFL)

C-    Constants for convection
      READ(UNIT=iUnit,NML=AIM_PAR_CNV)

C-    Constants for large-scale condensation
      READ(UNIT=iUnit,NML=AIM_PAR_LSC)

C-    Constants for radiation
      READ(UNIT=iUnit,NML=AIM_PAR_RAD)

C-    Constants for vertical diffusion and sh. conv.
      READ(UNIT=iUnit,NML=AIM_PAR_VDI)

#ifdef ALLOW_AIM_CO2
C-    Constants for CO2
      READ(UNIT=iUnit,NML=AIM_PAR_CO2)
#endif

      WRITE(msgBuf,'(A)')
     &   ' AIM_READ_PHYSPARMS: finished reading data.aimphys'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check parameters and model configuration

#ifndef ALLOW_CLR_SKY_DIAG
C-    If aim_clrSkyDiag is set, then must compile the Clear-Sky Diagnostic code
      IF ( aim_clrSkyDiag ) THEN
        WRITE(msgBuf,'(A)')
     &  'AIM_READPARMS: aim_clrSkyDiag=T but ALLOW_CLR_SKY_DIAG undef'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)')
     &  'Re-compile with: #define ALLOW_CLR_SKY_DIAG (AIM_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R AIM_READPARMS'
      ENDIF
#endif

C-    If aim_taveFreq is positive, then must compile the aim-diagnostics code
#ifndef ALLOW_AIM_TAVE
      IF (aim_taveFreq.GT.0.) THEN
        WRITE(msgBuf,'(A)')
     &  'AIM_READPARMS: aim_taveFreq > 0 but ALLOW_AIM_TAVE undefined'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)')
     &  'Re-compile with: #define ALLOW_AIM_TAVE (AIM_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R AIM_READPARMS'
      ENDIF
#endif /* ALLOW_AIM_TAVE */

#ifdef ALLOW_MNC
      aim_timeave_mnc        = useMNC .AND. aim_timeave_mnc
      aim_snapshot_mnc       = useMNC .AND. aim_snapshot_mnc
      aim_pickup_write_mnc   = useMNC .AND. aim_pickup_write_mnc
      aim_pickup_read_mnc    = useMNC .AND. aim_pickup_read_mnc
#else /* ALLOW_MNC */
      aim_timeave_mnc        = .FALSE.
      aim_snapshot_mnc       = .FALSE.
      aim_pickup_write_mnc   = .FALSE.
      aim_pickup_read_mnc    = .FALSE.
#endif /* ALLOW_MNC */
      aim_timeave_mdsio      = (.NOT. aim_timeave_mnc)
     &                     .OR. outputTypesInclusive
      aim_snapshot_mdsio     = (.NOT. aim_snapshot_mnc)
     &                     .OR. outputTypesInclusive
      aim_pickup_write_mdsio = (.NOT. aim_pickup_write_mnc)
     &                     .OR. outputTypesInclusive
      aim_pickup_read_mdsio  = (.NOT. aim_pickup_read_mnc)
     &                     .OR. outputTypesInclusive


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print out parameter values :

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// AIM physics parameters :'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

       CALL WRITE_0D_L( useLand, INDEX_NONE,
     &                 'useLand =',
     &'   /* use Land package for surf.BC over land  */')

C- namelist AIM_PARAMS:
       CALL WRITE_0D_L( aim_useFMsurfBC, INDEX_NONE,
     &                 'aim_useFMsurfBC =',
     &'   /* F.Molteni surf. BC scheme on/off flag */')
       CALL WRITE_0D_L( aim_useMMsurfFc, INDEX_NONE,
     &                 'aim_useMMsurfFc =',
     &'   /* Monthly Mean surf. Forcing on/off flag */')
       CALL WRITE_0D_RL(aim_surfForc_TimePeriod, INDEX_NONE,
     &                 'aim_surfForc_TimePeriod =',
     &  ' /* Length of forcing time period (s) */')
       CALL WRITE_0D_I( aim_surfForc_NppCycle, INDEX_NONE,
     &                 'aim_surfForc_NppCycle   =',
     &  ' /* Number of time period per Cycle */')
       CALL WRITE_0D_RL(aim_surfForc_TransRatio, INDEX_NONE,
     &                 'aim_surfForc_TransRatio =',
     &  ' /* transition ratio from one month to the next */')
       CALL WRITE_0D_L( aim_surfPotTemp, INDEX_NONE,
     &                 'aim_surfPotTemp =',
     &'   /* Surf Temp file in Pot.Temp on/off flag */')
       CALL WRITE_0D_L( aim_energPrecip, INDEX_NONE,
     &                 'aim_energPrecip =',
     &  ' /* account for energy of precip. on/off flag */')
       CALL WRITE_0D_L( aim_splitSIOsFx, INDEX_NONE,
     &                 'aim_splitSIOsFx =',
     &  ' /* separate Sea-Ice & Ocean Flux on/off flag */')
       CALL WRITE_0D_RL(aim_dragStrato,INDEX_NONE,'aim_dragStrato=',
     &    ' /* stratospheric-drag damping time scale (s) */')
       CALL WRITE_0D_L( aim_clrSkyDiag, INDEX_NONE,
     &                 'aim_clrSkyDiag =',
     &  ' /* do clear-sky radiation diagnostics */')
       CALL WRITE_0D_RL( aim_taveFreq, INDEX_NONE,'aim_taveFreq =',
     &  '   /* Frequency^-1 for time-Aver. output (s) */')
       CALL WRITE_0D_RL( aim_diagFreq, INDEX_NONE,'aim_diagFreq =',
     &  '   /* Frequency^-1 for diagnostic output (s) */')
       CALL WRITE_0D_RL( aim_tendFreq, INDEX_NONE,'aim_tendFreq =',
     &  '   /* Frequency^-1 for tendencies output (s) */')
       CALL WRITE_0D_I( aim_selectOceAlbedo, INDEX_NONE,
     &                 'aim_selectOceAlbedo =',
     &  '   /* Ocean albedo(latitude) flag */')

C- namelist AIM_PAR_FOR:
       CALL WRITE_0D_RL( SOLC,  INDEX_NONE,'AIM_FOR:   SOLC =',
     &  '   /* Solar constant (area averaged) in W/m2 */')
       CALL WRITE_0D_RL( OBLIQ, INDEX_NONE,'AIM_FOR:  OBLIQ =',
     &  '   /* Obliquity (degree) used with ALLOW_INSOLATION */')
       CALL WRITE_0D_RL( ALBSEA,INDEX_NONE,'AIM_FOR: ALBSEA =',
     &  '   /* Albedo over sea [0-1]      */')
       CALL WRITE_0D_RL( ALBICE,INDEX_NONE,'AIM_FOR: ALBICE =',
     &  '   /* Albedo over sea ice (for ice fraction =1) */')
       CALL WRITE_0D_RL( ALBSN, INDEX_NONE,'AIM_FOR:  ALBSN =',
     &  '   /* Albedo over snow (for snow depth > SDALB) */')
       CALL WRITE_0D_RL( SDALB, INDEX_NONE,'AIM_FOR:  SDALB =',
     &  '   /* Snow depth (mm H2O) corresp. maximum albedo */')
       CALL WRITE_0D_RL( SWCAP, INDEX_NONE,'AIM_FOR:  SWCAP =',
     &  '   /* Soil wetness at field capacity (Vol.fract.) */')
       CALL WRITE_0D_RL( SWWIL, INDEX_NONE,'AIM_FOR:  SWWIL =',
     &  '   /* Soil wetness at wilting point  (Vol.fract.) */')
       CALL WRITE_0D_RL( hSnowWetness, INDEX_NONE,
     &  'AIM_FOR: hSnowWetness=',
     &  '   /* snow depth corresp. maximum wetness (m) */')

C- namelist AIM_PAR_SFL:
       CALL WRITE_0D_RL( FWIND0,INDEX_NONE,'AIM_SFL: FWIND0 =',
     &  '   /* ratio of near-sfc wind to lowest-level wind */')
       CALL WRITE_0D_RL( FTEMP0,INDEX_NONE,'AIM_SFL: FTEMP0 =',
     &  '  /* weight for near-sfc temp. extrapolation (0-1)*/')
       CALL WRITE_0D_RL( FHUM0, INDEX_NONE,'AIM_SFL:  FHUM0 =',
     &  '  /* weight for near-sfc spec.humid. extrap. (0-1)*/')
       CALL WRITE_0D_RL( CDL,   INDEX_NONE,'AIM_SFL:    CDL =',
     &  '   /* drag coefficient for momentum over land */')
       CALL WRITE_0D_RL( CDS,   INDEX_NONE,'AIM_SFL:    CDS =',
     &  '   /* drag coefficient for momentum over sea  */')
       CALL WRITE_0D_RL( CHL,   INDEX_NONE,'AIM_SFL:    CHL =',
     &  '   /* heat exchange coefficient over land */')
       CALL WRITE_0D_RL( CHS,   INDEX_NONE,'AIM_SFL:    CHS =',
     &  '   /* heat exchange coefficient over sea  */')
       CALL WRITE_0D_RL( VGUST, INDEX_NONE,'AIM_SFL:  VGUST =',
     &  '   /* wind speed [m/s] for sub-grid-scale gusts */')
       CALL WRITE_0D_RL( CTDAY, INDEX_NONE,'AIM_SFL:  CTDAY =',
     &  '   /* daily-cycle correction (dTskin/dSSRad) */')
       CALL WRITE_0D_RL( DTHETA,INDEX_NONE,'AIM_SFL: DTHETA =',
     &  '   /* Pot.Temp. gradient for stability correction */')
       CALL WRITE_0D_RL( dTstab,INDEX_NONE,'AIM_SFL: dTstab =',
     &  '   /* Pot.Temp. increment for stab.funct. derivative */')
       CALL WRITE_0D_RL( FSTAB, INDEX_NONE,'AIM_SFL:  FSTAB =',
     &  '   /* Amplitude of stability correction (fract.) */')
       CALL WRITE_0D_RL( HDRAG, INDEX_NONE,'AIM_SFL:  HDRAG =',
     &  '   /* Height scale for orographic correction */')
       CALL WRITE_0D_RL( FHDRAG,INDEX_NONE,'AIM_SFL: FHDRAG =',
     &  '   /* Amplitude of orographic correction (fract.)*/')

C- namelist AIM_PAR_CNV:
       CALL WRITE_0D_RL( PSMIN, INDEX_NONE,'AIM_CNV:  PSMIN =',
     &  '   /* min norm.sfc.P. for occurrence of convect. */')
       CALL WRITE_0D_RL( TRCNV, INDEX_NONE,'AIM_CNV:  TRCNV =',
     &  '   /* time of relaxation [h] towards ref state */')
       CALL WRITE_0D_RL( QBL,   INDEX_NONE,'AIM_CNV:    QBL =',
     &  '   /* specific hum. threshold in the Bound.layer */')
       CALL WRITE_0D_RL( RHBL,  INDEX_NONE,'AIM_CNV:   RHBL =',
     &  '   /* relative hum. threshold in the Bound.layer */')
       CALL WRITE_0D_RL( RHIL,  INDEX_NONE,'AIM_CNV:   RHIL =',
     &  '   /* rel.hum. threshold (intern) for 2nd mass.Flx*/')
       CALL WRITE_0D_RL( ENTMAX,INDEX_NONE,'AIM_CNV: ENTMAX =',
     &  '   /* time of relaxation [h] towards neutral eq. */')
       CALL WRITE_0D_RL( SMF,   INDEX_NONE,'AIM_CNV:    SMF =',
     &  '   /* ratio 2ndary/primary mass.Flx at cloud-base*/')

C- namelist AIM_PAR_LSC:
       CALL WRITE_0D_RL( TRLSC, INDEX_NONE,'AIM_LSC:  TRLSC =',
     &  '   /* relaxation time [h] for supersat. spec.hum. */')
       CALL WRITE_0D_RL( RHLSC, INDEX_NONE,'AIM_LSC:  RHLSC =',
     &  '   /* Max rel.humidity threshold (at sigma=1) */')
       CALL WRITE_0D_RL( DRHLSC,INDEX_NONE,'AIM_LSC: DRHLSC =',
     &  '   /* Vertical range of rel.humidity threshold */')
       CALL WRITE_0D_RL( QSMAX, INDEX_NONE,'AIM_LSC:  QSMAX =',
     &  '   /* Define the maximum latent heat release */')

C- namelist AIM_PAR_RAD:
       CALL WRITE_0D_RL( RHCL1, INDEX_NONE,'AIM_RAD:  RHCL1 =',
     &  '   /*  rel.hum. corresponding to cloud cover = 0 */')
       CALL WRITE_0D_RL( RHCL2, INDEX_NONE,'AIM_RAD:  RHCL2 =',
     &  '   /*  rel.hum. corresponding to cloud cover = 1 */')
       CALL WRITE_0D_RL( QACL1, INDEX_NONE,'AIM_RAD:  QACL1 =',
     &  '   /* spec.hum. threshold for cloud, upper trop. */')
       CALL WRITE_0D_RL( QACL2, INDEX_NONE,'AIM_RAD:  QACL2 =',
     &  '   /* spec.hum. threshold for cloud, upper trop. */')
       CALL WRITE_0D_RL( ALBCL, INDEX_NONE,'AIM_RAD:  ALBCL =',
     &  '   /*  cloud albedo (for cloud cover = 1) */')
       CALL WRITE_0D_RL( EPSSW, INDEX_NONE,'AIM_RAD:  EPSSW =',
     &  '   /*  fract. of inc.solar rad. absorbed by ozone */')
       CALL WRITE_0D_RL( EPSLW, INDEX_NONE,'AIM_RAD:  EPSLW =',
     &  '   /*  fract. of sfc LW emitted directly to space */')
       CALL WRITE_0D_RL( EMISFC,INDEX_NONE,'AIM_RAD: EMISFC =',
     &  '   /*  longwave surface emissivity */')

       WRITE(msgBuf,'(A)')
     &  ' AIM_RAD : ShortWave absorptivities (for dp = 10^5 Pa) :'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
       CALL WRITE_0D_RL( ABSDRY,INDEX_NONE,'AIM_RAD: ABSDRY =',
     &  '   /* abs. of dry air (visible band) */')
       CALL WRITE_0D_RL( ABSAER,INDEX_NONE,'AIM_RAD: ABSAER =',
     &  '   /* abs. of aerosols (visible band) */')
       CALL WRITE_0D_RL( ABSWV1,INDEX_NONE,'AIM_RAD: ABSWV1 =',
     &  '   /* abs. of water vap. (vis. band)(/dq, 1g/kg) */')
       CALL WRITE_0D_RL( ABSWV2,INDEX_NONE,'AIM_RAD: ABSWV2 =',
     &  '   /* abs. of water vap.(near IR bd)(/dq, 1g/kg) */')
       CALL WRITE_0D_RL( ABSCL1,INDEX_NONE,'AIM_RAD: ABSCL1 =',
     &  '   /* abs. of clouds (vis.band)(constant term) */')
       CALL WRITE_0D_RL( ABSCL2,INDEX_NONE,'AIM_RAD: ABSCL2 =',
     &  '   /* abs. of clouds (vis.band) (/dw, 1g/kg) */')

       WRITE(msgBuf,'(A)')
     &  ' AIM_RAD : LongWave absorptivities (per dp = 10^5 Pa) :'
       CALL WRITE_0D_RL( ABLWIN,INDEX_NONE,'AIM_RAD: ABLWIN =',
     &  '   /* abs. of air in "window" band */')
       CALL WRITE_0D_RL( ABLCO2,INDEX_NONE,'AIM_RAD: ABLCO2 =',
     &  '   /* abs. of air in CO2 band */')
       CALL WRITE_0D_RL( ABLWV1,INDEX_NONE,'AIM_RAD: ABLWV1 =',
     &  '   /* abs. of Water vap. H2O bd-1(weak)(/dq,1g/kg)*/')
       CALL WRITE_0D_RL( ABLWV2,INDEX_NONE,'AIM_RAD: ABLWV2 =',
     &  '   /* abs. of W. vap., H2O bd-2(strong)(/dq,1g/kg)*/')
       CALL WRITE_0D_RL( ABLCL1,INDEX_NONE,'AIM_RAD: ABLCL1 =',
     &  '   /* abs. of clouds in window band (const term) */')
       CALL WRITE_0D_RL( ABLCL2,INDEX_NONE,'AIM_RAD: ABLCL2 =',
     &  '   /* abs. of clouds in window band (/dw, 1g/kg) */')

C- namelist AIM_PAR_VDI:
       CALL WRITE_0D_RL( TRSHC, INDEX_NONE,'AIM_VDI:  TRSHC =',
     &  '   /* relaxation time [h] for shallow convection */')
       CALL WRITE_0D_RL( TRVDI, INDEX_NONE,'AIM_VDI:  TRVDI =',
     &  '   /* relaxation time [h] for moisture diffusion */')
       CALL WRITE_0D_RL( TRVDS, INDEX_NONE,'AIM_VDI:  TRVDS =',
     &  '   /* relaxation time [h] for super-adiab. cond. */')
       CALL WRITE_0D_RL( RHGRAD,INDEX_NONE,'AIM_VDI: RHGRAD =',
     &  '   /* max gradient of rel.humidity (d_RH/d_sigma)*/')
       CALL WRITE_0D_RL( SEGRAD,INDEX_NONE,'AIM_VDI: SEGRAD =',
     &  '   /* max grad. of dry static Energy(d_DSE/d_phi)*/')

#ifdef ALLOW_AIM_CO2
C- namelist AIM_PAR_CO2:
       CALL WRITE_0D_I( Aim_CO2_Flag, INDEX_NONE,
     &                 'AIM_PAR_CO2: Aim_CO2_Flag =',
     &  ' /* CO2 flag, 1: prescribed, 2: interactive  */')
       CALL WRITE_0D_RL( atmpCO2init,    INDEX_NONE,
     &                 'AIM_PAR_CO2: atmpCO2init =',
     &  ' /* pCO2 when prescribed or IC when interactive */')
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_AIM */

      RETURN
      END
