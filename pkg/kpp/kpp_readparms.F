#include "KPP_OPTIONS.h"

      SUBROUTINE KPP_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE KPP_READPARMS                                 |
C     | o Routine to read in file data.kpp                       |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "KPP_PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of KPP_INIT
      INTEGER myThid

#ifdef ALLOW_KPP

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number
C     k           - Loop counter

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit, k

C--   KPP vertical mixing parameters
      NAMELIST /KPP_PARM01/
     & kpp_freq, kpp_dumpFreq, kpp_taveFreq,
     & KPPmixingMaps, KPPwriteState,
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
     & cstar

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' KPP_INIT: opening data.kpp'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


      CALL OPEN_COPY_DATA_FILE(
     I                          'data.kpp', 'KPP_INIT',
     O                          iUnit,
     I                          myThid )

C--   set default KPP vertical mixing parameters
      kpp_freq      = deltaTClock
      kpp_dumpFreq  = 0.
      kpp_taveFreq  = 0.
      KPPmixingMaps = .FALSE.
      KPPwriteState = .FALSE.
      minKPPhbl     = delZ(1)*0.5

C-----------------------------------------------------------------------
C define some non-dimensional constants and
C the vertical mixing coefficients in m-k-s units
C-----------------------------------------------------------------------
 
      epsln   = 1.e-20
      phepsi  = 1.e-10
      epsilon = 0.1
      vonk    = 0.40
      dB_dz   = 5.2e-5
      conc1   = 5.0
      conam   = 1.257
      concm   = 8.380
      conc2   = 16.0
      zetam   = -0.2
      conas   = -28.86
      concs   = 98.96
      conc3   = 16.0
      zetas   = -1.0

c     parameters for subroutine "bldepth"

      Ricr    = 0.30
      cekman  = 0.7
      cmonob  = 1.0
      concv   = 1.8
      hbf     = 1.0

c     parameters and common arrays for subroutines
c     "kmixinit" and "wscale"

      zmin    = -4.e-7
      zmax    = 0.0
      umin    = 0.0
      umax    = .04

c     parameters for subroutine "Ri_iwmix"

      num_v_smooth_Ri = 0
      num_v_smooth_BV = 0
      num_z_smooth_sh = 0
      num_m_smooth_sh = 0
      Riinfty = 0.7
      BVSQcon = -0.2e-4

      difm0   = 0.005
      difs0   = 0.005
      dift0   = 0.005

      difmcon = 0.1
      difscon = 0.1
      diftcon = 0.1

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

      return
      end
