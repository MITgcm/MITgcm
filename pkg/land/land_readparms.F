C $Header: /u/gcmpack/MITgcm/pkg/land/land_readparms.F,v 1.3 2004/01/18 18:14:20 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_READPARMS
C     !INTERFACE:
      SUBROUTINE LAND_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_READPARMS
C     | o Read Land package parameters
C     *==========================================================*
C     \ev
 
C     !USES:
      IMPLICIT NONE

C     == Global variables ===

C-- size for MITgcm & Land package :
#include "LAND_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==       
C     myThid -  Number of this instance
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND

C Functions
      INTEGER ILNBLNK

C     == Local Variables == 
C     msgBuf     :: Informational/error meesage buffer
C     iUnit      :: Work variable for IO unit number
C     k          :: loop counter
C     iL         :: Work variable for length of file-name
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, k, iL
      _RL tmpvar

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Land model parameters:
C     land_calc_grT :: step forward ground Temperature
C     land_calc_grW :: step forward soil moiture
C     land_grT_iniFile :: File containing initial ground Temp.
C     land_grW_iniFile :: File containing initial ground Water.
C     land_deltaT   :: land model time-step
C     land_taveFreq :: Frequency^-1 for time-Aver. output (s)
C     land_diagFreq :: Frequency^-1 for diagnostic output (s)
C     land_dzF      :: layer thickness
      NAMELIST /LAND_MODEL_PAR/
     &    land_calc_grT, land_calc_grW, 
     &    land_grT_iniFile, land_grW_iniFile,
     &    land_deltaT, land_taveFreq, land_diagFreq,
     &    land_dzF

C--   Physical constants :
C     land_grdLambda  :: Thermal conductivity of the ground
C     land_heatCs     :: Heat capacity of dry soil
C     land_heatCw     :: Heat capacity of water
C     land_wTauDiff   :: soil moisture diffusion time scale
C     land_waterCap   :: field capacity per meter of soil
C     land_fractRunOff:: fraction of water in excess which run-off

      NAMELIST /LAND_PHYS_PAR/
     &    land_grdLambda, land_heatCs, land_heatCw,
     &    land_wTauDiff, land_waterCap, land_fractRunOff

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Set default value:
      land_calc_grT = .TRUE. 
      land_calc_grW = .TRUE.
      land_grT_iniFile = ' '
      land_grW_iniFile = ' '
      land_deltaT   = deltaTclock
      land_taveFreq = taveFreq
      land_diagFreq = dumpFreq
      land_grdLambda= 0.42 _d 0
      land_heatCs   = 1.13 _d 6
      land_heatCw   =  4.2 _d 6
      land_wTauDiff =  48. _d 0*3600. _d 0
      land_waterCap = 0.24 _d 0
      land_fractRunOff = 0.5 _d 0
      DO k=1,land_nLev
       land_dzF(k) = -1.
       land_rec_dzC(k) = -1.
      ENDDO
      
      _BEGIN_MASTER(myThid)
      
      WRITE(msgBuf,'(A)') ' LAND_READPARMS: opening data.land'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE( 'data.land', 'LAND_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Parameters for Land model:
      READ(UNIT=iUnit,NML=LAND_MODEL_PAR)

C-    Physical Constants for Land package
      READ(UNIT=iUnit,NML=LAND_PHYS_PAR)

      WRITE(msgBuf,'(A)') 
     &   ' LAND_READPARMS: finished reading data.land'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
 
C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check parameters and model configuration

C-    If land_taveFreq is positive, then must compile the land-diagnostics code
#ifndef ALLOW_LAND_TAVE
      IF (land_taveFreq.GT.0.) THEN
        WRITE(msgBuf,'(2A)') 'LAND_READPARMS:',
     &  ' land_taveFreq > 0 but ALLOW_LAND_TAVE undefined'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(2A)')
     &  'Re-compile with: #define ALLOW_LAND_TAVE',
     &                    '  or -DALLOW_LAND_TAVE'
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R LAND_READPARMS'
      ENDIF
#endif /* ALLOW_LAND_TAVE */

C-    derive other parameters:
      tmpvar = 0. _d 0
      DO k=1,land_nLev
       tmpvar = tmpvar+land_dzF(k)
       IF (tmpvar.GT.0. _d 0) land_rec_dzC(k) = 2. _d 0 / tmpvar
       tmpvar = land_dzF(k)
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print out parameter values :

      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// Land package parameters :'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)

C- namelist LAND_MODEL_PAR:
       CALL WRITE_0D_L( land_calc_grT, INDEX_NONE,
     &                 'land_calc_grT =',
     &   ' /* step forward ground Temp. on/off flag */')       
       CALL WRITE_0D_L( land_calc_grW, INDEX_NONE,
     &                 'land_calc_grW =',
     &   ' /* step forward soil moiture on/off flag */')       
      iL = ILNBLNK( land_grT_iniFile )
c     IF ( iL.EQ.LEN(land_grT_iniFile) ) iL=0
      IF ( iL.GE.1 ) THEN 
       WRITE(msgBuf,'(A,A)') 'land_grT_iniFile = ', 
     &                       '/* Initial ground-Temp Input-File */'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
       WRITE(msgBuf,'(16X,A)') land_grT_iniFile(1:iL)
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
       msgBuf='    ;'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      ENDIF
      iL = ILNBLNK( land_grW_iniFile )
c     IF ( iL.EQ.LEN(land_grW_iniFile) ) iL=0
      IF ( iL.GE.1 ) THEN 
       WRITE(msgBuf,'(A,A)') 'land_grW_iniFile = ', 
     &                       '/* Initial soil-Water Input-File */'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
       WRITE(msgBuf,'(16X,A)') land_grW_iniFile(1:iL)
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
       msgBuf='    ;'
       CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      ENDIF
       CALL WRITE_0D_R8( land_deltaT, INDEX_NONE,'land_deltaT =',
     &  ' /* land model Time-Step (s) */')
       CALL WRITE_0D_R8( land_taveFreq, INDEX_NONE,'land_taveFreq =',
     &   ' /* Frequency^-1 for time-Aver. output (s) */')
       CALL WRITE_0D_R8( land_diagFreq, INDEX_NONE,'land_diagFreq =',
     &   ' /* Frequency^-1 for diagnostic output (s) */')
       CALL WRITE_1D_R8( land_dzF,land_nLev, INDEX_K,'land_dzF = ',
     &   ' /* layer thickness ( m ) */')
       CALL WRITE_1D_R8(land_rec_dzC,land_nLev,INDEX_K,'land_rec_dzC= '
     &  ,' /* recip. vertical spacing (m-1) */')

C- namelist LAND_PHYS_PAR:
       CALL WRITE_0D_R8(land_grdLambda,INDEX_NONE,'land_grdLambda =',
     &   ' /* Thermal conductivity of the ground (W/m/K)*/')
       CALL WRITE_0D_R8( land_heatCs,INDEX_NONE,'land_heatCs =',
     &   ' /* Heat capacity of dry soil (J.m-3.K-1) */')
       CALL WRITE_0D_R8( land_heatCw,INDEX_NONE,'land_heatCw =',
     &   ' /* Heat capacity of water    (J.m-3.K-1) */')
       CALL WRITE_0D_R8( land_wTauDiff,INDEX_NONE,'land_wTauDiff =',
     &   ' /* soil moisture diffusion time scale (s) */')
       CALL WRITE_0D_R8( land_waterCap,INDEX_NONE,'land_waterCap =',
     &   ' /* field capacity per meter of soil (1) */')
       CALL WRITE_0D_R8(land_fractRunOff,INDEX_NONE,'land_fractRunOff='
     &  ,' /* fraction of water in excess which run-off */')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)
 
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_LAND */

      RETURN
      END
