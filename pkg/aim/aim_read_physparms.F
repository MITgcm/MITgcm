C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_read_physparms.F,v 1.1 2002/10/09 01:01:24 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_READ_PHYSPARMS
C     !INTERFACE:
      SUBROUTINE AIM_READ_PHYSPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_READ_PHYSPARMS
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
c #include "PARAMS.h"

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
C     myThid -  Number of this instance
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM

C     == Local Variables == 
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Physical constants (common PHYCON) :
C      P0   = reference pressure
C      GG   = gravity accel.
C      RD   = gas constant for dry air
C      CP   = specific heat at constant pressure
C      ALHC = latent heat of condensation
C      SBC  = Stefan-Boltzmann constant            

C--   Constants for forcing fields (common FORCON) :
C      SOLC   = Solar constant (area averaged) in W/m^2
C      ALBSEA = Albedo over sea
C      ALBICE = Albedo over sea ice (for ice fraction = 1)
C      ALBSN  = Albedo over snow (for snow depth > SDALB)
C      SDALB  = Snow depth corresponding to maximum albedo
C      SWCAP  = Soil wetness capacity
C      SWWIL  = Soil wetness at wilting point 
      NAMELIST /AIM_PAR_FOR/
     &    SOLC, ALBSEA, ALBICE, ALBSN,
     &    SDALB, SWCAP, SWWIL

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
C      SWMAX  = Soil wetness (in mm) corresp. to potential evapotranspiration  
      NAMELIST /AIM_PAR_SFL/
     &    FWIND0, FTEMP0, FHUM0,
     &    CDL, CDS, CHL, CHS, VGUST, 
     &    SWMAX

C--   Convection constants (common CNVCON) :
C      RHBL   = relative hum. threshold in the boundary (lowest) layer
C      TRCNV  = time of relaxation (in hours) towards neutral equilibrium
C      ENTMAX = max. entrainment as a fraction of cloud-base mass flux
      NAMELIST /AIM_PAR_CNV/
     &    RHBL, TRCNV, ENTMAX

C--   Constants for large-scale condendation (common LSCCON) : 
C      RHLSC  = Relative humidity threshold
C      TRLSC  = Relaxation time (in hours) for supersat. specific humidity 
      NAMELIST /AIM_PAR_LSC/
     &    RHLSC, TRLSC

C--    Radiation constants (common RADCON) :
C      ABSSW  = shortwave absorptivity for dry air (per dp = 10^5 Pa)
C      ABSLW  =  longwave absorptivity for dry air (per dp = 10^5 Pa)
C      ABWSW  = shortwave absorptivity for water vapour (per dq = 1 g/kg)
C      ABWLW  =  longwave absorptivity for water vapour (per dq = 1 g/kg)
C      ABCSW  = shortwave absorptivity for cloud fraction
C      ABCLW  =  longwave absorptivity for cloud fraction
C      EPSSW  = fraction of incoming solar radiation absorbed by ozone
C      EPSLW  = fraction of surface LW radiation emitted directly to space
C      ALBCL  = cloud albedo (for cloud cover = 1)
C      RHCL1  = relative hum. corresponding to cloud cover = 0
C      RHCL2  = relative hum. corresponding to cloud cover = 1
C      QACL   = specific hum. threshold for cloud cover
      NAMELIST /AIM_PAR_RAD/
     &    ABSSW, ABSLW, ABWSW, ABWLW, ABCSW, ABCLW,
     &    EPSSW, EPSLW,
     &    ALBCL, RHCL1, RHCL2, QACL

C--   Constants for vertical dif. and sh. conv. (common VDICON) : 
C      TRVDI  = relaxation time (in hours) for moisture diffusion
C      TRSHC  = relaxation time (in hours) for shallow convection
      NAMELIST /AIM_PAR_VDI/
     &    TRVDI, TRSHC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)
      
      WRITE(msgBuf,'(A)') ' AIM_READ_PHYSPARMS: opening data.aimphys'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE( 'data.aimphys', 'AIM_READ_PHYSPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

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

      WRITE(msgBuf,'(A)') 
     &   ' AIM_READ_PHYSPARMS: finished reading data.aimphys'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
 
C--   Close the open data file
      CLOSE(iUnit)

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

C- namelist AIM_PAR_FOR:
       CALL WRITE_0D_R8( SOLC,  INDEX_NONE,'AIM_FOR:   SOLC =',
     &  '   /* Solar constant (area averaged) in W/m2 */')

C- namelist AIM_PAR_SFL:
       CALL WRITE_0D_R8( FWIND0,INDEX_NONE,'AIM_SFL: FWIND0 =',
     &  '   /* ratio of near-sfc wind to lowest-level wind */')
       CALL WRITE_0D_R8( FTEMP0,INDEX_NONE,'AIM_SFL: FTEMP0 =',
     &  '  /* weight for near-sfc temp. extrapolation (0-1)*/')
       CALL WRITE_0D_R8( FHUM0, INDEX_NONE,'AIM_SFL:  FHUM0 =',
     &  '  /* weight for near-sfc spec.humid. extrap. (0-1)*/')
       CALL WRITE_0D_R8( CDL,   INDEX_NONE,'AIM_SFL:    CDL =',
     &  '   /* drag coefficient for momentum over land */')
       CALL WRITE_0D_R8( CDS,   INDEX_NONE,'AIM_SFL:    CDS =',
     &  '   /* drag coefficient for momentum over sea  */')
       CALL WRITE_0D_R8( CHL,   INDEX_NONE,'AIM_SFL:    CHL =',
     &  '   /* heat exchange coefficient over land */')
       CALL WRITE_0D_R8( CHS,   INDEX_NONE,'AIM_SFL:    CHS =',
     &  '   /* heat exchange coefficient over sea  */')
       CALL WRITE_0D_R8( VGUST, INDEX_NONE,'AIM_SFL:  VGUST =',
     &  '   /* wind speed [m/s] for sub-grid-scale gusts */')

C- namelist AIM_PAR_CNV:
       CALL WRITE_0D_R8( RHBL,  INDEX_NONE,'AIM_CNV:   RHBL =',
     &  '   /* rel. hum. threshold in the B.(lowest) layer */')
       CALL WRITE_0D_R8( TRCNV, INDEX_NONE,'AIM_CNV:  TRCNV =',
     &  '   /* time of relaxation [h] towards neutral eq. */')
       CALL WRITE_0D_R8( ENTMAX,INDEX_NONE,'AIM_CNV: ENTMAX =',
     &  '   /* time of relaxation [h] towards neutral eq. */')

C- namelist AIM_PAR_LSC:
       CALL WRITE_0D_R8( RHLSC, INDEX_NONE,'AIM_LSC:  RHLSC =',
     &  '   /* Relative humidity threshold */')
       CALL WRITE_0D_R8( TRLSC, INDEX_NONE,'AIM_LSC:  TRLSC =',
     &  '   /* relaxation time [h] for supersat. spec.hum. */')

C- namelist AIM_PAR_RAD:
       CALL WRITE_0D_R8( ABSSW, INDEX_NONE,'AIM_RAD:  ABSSW =',
     &  '   /* ShortW. absorptivity for dry air (/dp, 1e5 Pa)*/')
       CALL WRITE_0D_R8( ABSLW, INDEX_NONE,'AIM_RAD:  ABSLW =',
     &  '   /*  LongW. absorptivity for dry air (/dp, 1e5 Pa)*/')
       CALL WRITE_0D_R8( ABWSW, INDEX_NONE,'AIM_RAD:  ABWSW =',
     &  '   /* ShortW. absorptivity for WaterVap.(/dq, 1g/kg)*/')
       CALL WRITE_0D_R8( ABWLW, INDEX_NONE,'AIM_RAD:  ABWLW =',
     &  '   /*  LongW. absorptivity for WaterVap.(/dq, 1g/kg)*/')
       CALL WRITE_0D_R8( ABCSW, INDEX_NONE,'AIM_RAD:  ABCSW =',
     &  '   /* ShortW. absorptivity for cloud fraction */')
       CALL WRITE_0D_R8( ABCLW, INDEX_NONE,'AIM_RAD:  ABCLW =',
     &  '   /*  LongW. absorptivity for cloud fraction */')
       CALL WRITE_0D_R8( EPSSW, INDEX_NONE,'AIM_RAD:  EPSSW =',
     &  '   /*  fraction of inc.solar rad. absorbed by ozone */')   
       CALL WRITE_0D_R8( EPSLW, INDEX_NONE,'AIM_RAD:  EPSLW =',
     &  '   /*  fraction of sfc LW emitted directly to space */')   
       CALL WRITE_0D_R8( ALBCL, INDEX_NONE,'AIM_RAD:  ALBCL =',
     &  '   /*  cloud albedo (for cloud cover = 1) */')
       CALL WRITE_0D_R8( RHCL1, INDEX_NONE,'AIM_RAD:  RHCL1 =',
     &  '   /*  rel.hum. corresponding to cloud cover = 0 */')
       CALL WRITE_0D_R8( RHCL2, INDEX_NONE,'AIM_RAD:  RHCL2 =',
     &  '   /*  rel.hum. corresponding to cloud cover = 1 */')
       CALL WRITE_0D_R8( QACL,  INDEX_NONE,'AIM_RAD:   QACL =',
     &  '   /*  specific hum. threshold for cloud cover */')

C- namelist AIM_PAR_VDI:
       CALL WRITE_0D_R8( TRVDI, INDEX_NONE,'AIM_VDI:  TRVDI =',
     &  '   /* relaxation time [h] for moisture diffusion */')
       CALL WRITE_0D_R8( TRSHC, INDEX_NONE,'AIM_VDI:  TRSHC =',
     &  '   /* relaxation time [h] for shallow convection */')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)
 
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_AIM */

      RETURN
      END
