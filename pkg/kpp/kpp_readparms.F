C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_readparms.F,v 1.22 2014/05/27 23:41:29 jmc Exp $
C $Name:  $
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
C     msgBuf      - Informational/error message buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit

C--   KPP vertical mixing parameters
      NAMELIST /KPP_PARM01/
     & kpp_freq, kpp_dumpFreq, kpp_taveFreq,
     & KPPmixingMaps, KPPwriteState, KPP_ghatUseTotalDiffus,
     & KPPuseDoubleDiff, LimitHblStable,
     & minKPPhbl,
     & epsln, phepsi, epsilon, vonk, dB_dz,
     & conc1, conam, concm, conc2, zetam,
     & conas, concs, conc3, zetas,
     & Ricr, cekman, cmonob, concv, hbf,
     & zmin, zmax, umin, umax,
     & num_v_smooth_Ri, num_v_smooth_BV,
     & num_z_smooth_sh, num_m_smooth_sh,
     & Riinfty, BVSQcon, difm0, difs0, dift0,
     & difmcon, difscon, diftcon,
     & Rrho0, dsfmax,
     & cstar

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

      WRITE(msgBuf,'(A)') ' KPP_INIT: opening data.kpp'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.kpp', 'KPP_INIT',
     O                          iUnit,
     I                          myThid )

C--   set default KPP vertical mixing parameters
      kpp_freq               = deltaTClock
      kpp_dumpFreq           = dumpFreq
      kpp_taveFreq           = taveFreq
      KPPmixingMaps          = .FALSE.
      KPPwriteState          = .FALSE.
      KPPuseDoubleDiff       = .FALSE.
      LimitHblStable         = .TRUE.
      KPP_ghatUseTotalDiffus = .FALSE.
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
      num_v_smooth_BV = 0
      num_z_smooth_sh = 0
      num_m_smooth_sh = 0
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

C-----------------------------------------------------------------------

C--   Read settings from model parameter file "data.kpp".
      READ(UNIT=iUnit,NML=KPP_PARM01,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.kpp"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist KPP_PARM01'
       CALL PRINT_ERROR( msgBuf , 1)
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R KPP_INIT'
      ENDIF

      CLOSE(iUnit)

      WRITE(msgBuf,'(A)') ' KPP_INIT: finished reading data.kpp'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_KPP */

      RETURN
      END
