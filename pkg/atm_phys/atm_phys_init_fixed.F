#include "ATM_PHYS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_INIT_FIXED

C !INTERFACE:
      SUBROUTINE ATM_PHYS_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize ATM_PHYS variables that are kept fixed during the run.

C     !USES:
      use gcm_params_mod,          only: gcm_params_init
      use constants_mod,           only: constants_init
      use radiation_mod,           only: radiation_init
      use lscale_cond_mod,         only: lscale_cond_init
      use dargan_bettsmiller_mod,  only: dargan_bettsmiller_init
      use vert_turb_driver_mod,    only: vert_turb_driver_init
      use vert_diff_mod,           only: gcm_vert_diff_init
      use mixed_layer_mod,         only: mixed_layer_init

      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER axes(4)
      INTEGER i,j,k,bi,bj
      _RL cst_albedo, cst_mxlDepth

      namelist / atmosphere_nml /
     &   turb, ldry_convection, lwet_convection, do_virtual,
     &   two_stream, mixed_layer_bc,
     &   roughness_heat, roughness_moist, roughness_mom

      cst_albedo   = 0.
      cst_mxlDepth = 1.

      _BARRIER
      _BEGIN_MASTER(myThid)
      module_is_initialized =.false.
      turb = .false.
      ldry_convection = .false.
      lwet_convection = .false.
      do_virtual = .false.      ! whether virtual temp used in gcm_vert_diff
      two_stream = .true.
      mixed_layer_bc = .false.
      roughness_heat = 0.05
      roughness_moist = 0.05
      roughness_mom = 0.05
      WRITE(msgBuf,'(A)') 'ATM_PHYS_INIT_FIXED: opening data.atm_gray'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.atm_gray', 'ATM_PHYS_INIT_FIXED',
     O                     iUnit,
     I                     myThid )

C      Read parameters from open data file
      READ(UNIT=iUnit,NML=atmosphere_nml)
      WRITE(msgBuf,'(A)')
     &     'ATM_PHYS_INIT_FIXED: finished reading data.atm_gray'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      CALL GCM_PARAMS_INIT(
     I         UNSET_RS, UNSET_RL, UNSET_I,
!    I         MAX_LEN_MBUF, MAX_LEN_FNAM, MAX_LEN_PREC,
     I         SQUEEZE_RIGHT, SQUEEZE_LEFT, SQUEEZE_BOTH,
     I         precFloat32, precFloat64,
     I         debLevZero, debLevA, debLevB,
     I         debLevC, debLevD, debLevE,
     I         standardMessageUnit, errorMessageUnit,
     I         debugLevel, debugMode,
     I         myThid )

      CALL CONSTANTS_INIT
      _END_MASTER(myThid)
      _BARRIER

      CALL MIXED_LAYER_INIT ( 1, sNx, 1, sNy, Nr, axes,
c    I                        lat2d, ocean_qflux,
     &                        startTime, cst_mxlDepth, myThid )
      CALL GCM_VERT_DIFF_INIT ( sNx, sNy, Nr,
     I                         .true., myThid, do_virtual )

      CALL LSCALE_COND_INIT( myThid )

      IF (lwet_convection) THEN
        CALL DARGAN_BETTSMILLER_INIT( myThid )
      ENDIF

      IF( two_stream) THEN
        CALL RADIATION_INIT( 1, sNx, 1, sNy, Nr, nSx,nSy, axes,
     &                       startTime, cst_albedo, myThid )
      ENDIF

      IF (turb) THEN
        CALL VERT_TURB_DRIVER_INIT( sNx,sNy,Nr,axes,
     &                              startTime, myThid )
      ENDIF

      module_is_initialized = .true.

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            atmPhys_Qflx(i,j,bi,bj) = 0. _d 0
            atmPhys_MxLD(i,j,bi,bj) = cst_mxlDepth
            atmPhys_Albedo(i,j,bi,bj) = cst_albedo
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            atmPhys_Ozone(i,j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
       ENDDO
      ENDDO
      IF ( atmPhys_qFlxFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( atmPhys_qFlxFile,' ',
     &                       atmPhys_Qflx, 0, myThid )
        CALL EXCH_XY_RL( atmPhys_Qflx, myThid )
      ENDIF
      IF ( atmPhys_mxldFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( atmPhys_mxldFile,' ',
     &                       atmPhys_MxLD, 0, myThid )
        CALL EXCH_XY_RL( atmPhys_MxLD, myThid )
      ENDIF
      IF ( atmPhys_albedoFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( atmPhys_albedoFile,' ',
     &                       atmPhys_Albedo, 0, myThid )
        CALL EXCH_XY_RL( atmPhys_Albedo, myThid )
      ENDIF
      IF ( atmPhys_ozoneFile .NE. ' ' ) THEN
        CALL READ_FLD_XYZ_RL( atmPhys_ozoneFile,' ',
     &                        atmPhys_Ozone, 0, myThid )
        CALL EXCH_XYZ_RL( atmPhys_Ozone, myThid )
      ENDIF

#ifdef ALLOW_MNC
c     IF (useMNC) THEN
c       CALL ATM_PHYS_MNC_INIT( myThid )
c     ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL ATM_PHYS_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
