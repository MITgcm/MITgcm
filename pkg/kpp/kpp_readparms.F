#include "KPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: KPP_READPARMS

C     !INTERFACE:
      SUBROUTINE KPP_READPARMS( myThid )

C     !DESCRIPTION:
C     Routine to read in file data.kpp

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP_PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
#ifdef ALLOW_KPP
C     msgBuf     :: Informational/error message buffer
C     errIO      :: IO error flag
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit
C-    retired parameters:
C     nRetired   :: Count number of "retired" parameters found in namelist.
      INTEGER nRetired
      LOGICAL KPPmixingMaps
      INTEGER num_v_smooth_BV, num_z_smooth_sh, num_m_smooth_sh
      _RL kpp_taveFreq

C--   KPP vertical mixing parameters
      NAMELIST /KPP_PARM01/
     & kpp_freq, kpp_dumpFreq, kpp_taveFreq,
     & KPPwriteState, KPP_ghatUseTotalDiffus,
     & KPPuseDoubleDiff, LimitHblStable, KPPuseSWfrac3D,
     & minKPPhbl,
     & epsln, phepsi, epsilon, vonk, dB_dz,
     & conc1, conam, concm, conc2, zetam,
     & conas, concs, conc3, zetas,
     & Ricr, cekman, cmonob, concv, hbf,
     & zmin, zmax, umin, umax,
     & num_v_smooth_Ri,
     & Riinfty, BVSQcon, difm0, difs0, dift0,
     & difmcon, difscon, diftcon,
     & Rrho0, dsfmax,
     & cstar,
     & KPPmixingMaps,
     & num_v_smooth_BV, num_z_smooth_sh, num_m_smooth_sh

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useKPP ) THEN
C-    pkg KPP is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.kpp is found
         CALL PACKAGES_UNUSED_MSG( 'useKPP', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' KPP_READPARMS: opening data.kpp'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      errIO = 0
      CALL OPEN_COPY_DATA_FILE(
     I                          'data.kpp', 'KPP_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   set default KPP vertical mixing parameters
      kpp_freq               = deltaTClock
      kpp_dumpFreq           = dumpFreq
      KPPwriteState          = .FALSE.
      KPPuseDoubleDiff       = .FALSE.
      LimitHblStable         = .TRUE.
      KPP_ghatUseTotalDiffus = .FALSE.
      KPPuseSWfrac3D         = .FALSE.
      minKPPhbl = UNSET_RL

C-----------------------------------------------------------------------
C define some non-dimensional constants and
C the vertical mixing coefficients in m-k-s units
C-----------------------------------------------------------------------

      epsln   =   1. _d -20
      phepsi  =   1. _d -10
      epsilon =   1. _d -1
      vonk    =   0.4 _d 0
      dB_dz   =   5.2 _d -5
      conc1   =   5. _d 0
      conam   =   1.257 _d 0
      concm   =   8.380 _d 0
      conc2   =  16. _d 0
      zetam   =  -0.2 _d 0
      conas   = -28.86 _d 0
      concs   =  98.96 _d 0
      conc3   =  16. _d 0
      zetas   =  -1. _d 0

C     parameters for subroutine "bldepth"

      Ricr    = 0.3 _d 0
      cekman  = 0.7 _d 0
      cmonob  = 1.  _d 0
      concv   = 1.8 _d 0
      hbf     = 1.  _d 0

C     parameters and common arrays for subroutines
C     "kmixinit" and "wscale"

      zmin    = -4. _d -7
      zmax    =  0. _d 0
      umin    =  0. _d 0
      umax    =  4. _d -2

C     parameters for subroutine "Ri_iwmix"

      num_v_smooth_Ri = 0
      Riinfty =  0.7 _d 0
      BVSQcon = -0.2 _d -4

      difm0   = 5. _d -3
      difs0   = 5. _d -3
      dift0   = 5. _d -3

      difmcon = 0.1 _d 0
      difscon = 0.1 _d 0
      diftcon = 0.1 _d 0

C     parameters for double diffusion routine "KPP_DOUBLEDIFF"

      Rrho0   = 1.9 _d 0
      dsfmax  = 10. _d -3

C     parameters for subroutine "blmix"

      cstar   = 10.

C-    Retired parameters: initialised to unlikely value:
      nRetired = 0
      KPPmixingMaps   = .FALSE.
      num_v_smooth_BV = UNSET_I
      num_z_smooth_sh = UNSET_I
      num_m_smooth_sh = UNSET_I
      kpp_taveFreq    = UNSET_RL

C-----------------------------------------------------------------------

C--   Read settings from model parameter file "data.kpp".
      WRITE(msgBuf,'(A)') ' KPP_READPARMS ; starts to read KPP_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ( UNIT=iUnit, NML=KPP_PARM01 )
c     READ( UNIT=iUnit, NML=KPP_PARM01, IOSTAT=errIO )
c     WRITE(msgBuf,'(A,I6,A)')
c    & ' KPP_READPARMS ; read KPP_PARM01 (', errIO, ' )'
c     CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c    &                    SQUEEZE_RIGHT, myThid )
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R KPP_READPARMS: Error reading parameter file "data.kpp"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R KPP_READPARMS: Problem in namelist KPP_PARM01'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R KPP_READPARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' KPP_READPARMS ; read KPP_PARM01 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      WRITE(msgBuf,'(A)') ' KPP_READPARMS: finished reading data.kpp'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Check for retired parameters:
C     print message and stop when retired parameters were found in namelist
      IF ( KPPmixingMaps ) THEN
        nRetired = nRetired + 1
        WRITE(msgBuf,'(2A)') 'S/R KPP_READPARMS: "KPPmixingMaps"',
     &                       ' no longer allowed in file "data.kpp"'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( num_v_smooth_BV .NE. UNSET_I ) THEN
        nRetired = nRetired + 1
        WRITE(msgBuf,'(2A)') 'S/R KPP_READPARMS: "num_v_smooth_BV"',
     &                       ' no longer allowed in file "data.kpp"'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( num_z_smooth_sh .NE. UNSET_I ) THEN
        nRetired = nRetired + 1
        WRITE(msgBuf,'(2A)') 'S/R KPP_READPARMS: "num_z_smooth_sh"',
     &                       ' no longer allowed in file "data.kpp"'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( num_m_smooth_sh .NE. UNSET_I ) THEN
        nRetired = nRetired + 1
        WRITE(msgBuf,'(2A)') 'S/R KPP_READPARMS: "num_m_smooth_sh"',
     &                       ' no longer allowed in file "data.kpp"'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( kpp_taveFreq .NE. UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'KPP_READPARMS: "kpp_taveFreq"',
     &    ' is no longer allowed in file "data.kpp"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'KPP_READPARMS: ',
     &    ' since "pkg/timeave" has been removed.'
        CALL PRINT_ERROR( msgBuf, myThid )
        nRetired = nRetired + 1
      ENDIF

      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'S/R KPP_READPARMS: ',
     &  'Error reading file "data.kpp":'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(I4,A)') nRetired,
     &      ' out-of-date parameters were found in the namelist(s)'
       CALL PRINT_ERROR( msgBuf, myThid )
c      errCount = errCount + 1
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R KPP_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_KPP */

      RETURN
      END
