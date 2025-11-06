#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_READPARMS( myThid )

C     Read LAYERS parameters from data file.

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C     INPUT PARAMETERS:
      INTEGER myThid

#ifdef ALLOW_LAYERS
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
C     k          :: index
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, k, iLa
      INTEGER errCount

C--   old pkg/layers parameter setting (only single tracer layers diagnostics):
C      layers_G :: boundaries of tracer layers
      INTEGER LAYER_nb, layers_kref
      LOGICAL useBOLUS
      _RL layers_G(Nlayers+1)

C-    retired parameters:
      _RL layers_taveFreq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      NAMELIST /LAYERS_PARM01/
     &       layers_G, layers_taveFreq, layers_diagFreq,
     &       LAYER_nb, layers_kref, useBOLUS, layers_bolus,
     &       layers_name, layers_bounds, layers_krho

      IF ( .NOT.useLayers ) THEN
C-    pkg LAYERS is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.layers is found
         CALL PACKAGES_UNUSED_MSG( 'useLayers', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)
      errCount = 0
      layers_taveFreq = UNSET_RL

C--   Default values for LAYERS

      layers_diagFreq = dumpFreq
C     The MNC stuff is not working yet
      layers_MNC = .FALSE.
      layers_MDSIO = .TRUE.

      DO iLa=1,layers_maxNum
        layers_name(iLa) = ' '
        layers_krho(iLa)= 1
        layers_bolus(iLa) = useGMRedi
        DO k=1,Nlayers+1
          layers_bounds(k,iLa) = UNSET_RL
        ENDDO
      ENDDO

C--   old params default:
      LAYER_nb = 0
      layers_kref = 1
      useBOLUS = useGMRedi
      DO k=1,Nlayers+1
        layers_G(k) = UNSET_RL
      ENDDO

      WRITE(msgBuf,'(A)') 'LAYERS_READPARMS: opening data.layers'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.layers', 'LAYERS_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=LAYERS_PARM01)
      WRITE(msgBuf,'(A)')
     &    'LAYERS_READPARMS: finished reading data.layers'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C--   Process old params setting (single averaging tracer)
      IF ( LAYER_nb.LT.0 .OR. LAYER_nb.GT.3 ) THEN
        WRITE(msgBuf,'(2A,I2,A,I9)') 'LAYERS_READPARMS: ',
     &    'Invalid LAYER_nb=', LAYER_nb
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( LAYER_nb.EQ.0 ) THEN
        IF ( layers_kref.NE.1 )         errCount = errCount + 1
        DO k=1,Nlayers+1
         IF ( layers_G(k).NE.UNSET_RL ) errCount = errCount + 1
        ENDDO
      ELSE
        DO iLa=1,layers_maxNum
         IF ( layers_name(iLa).NE.' ' ) errCount = errCount + 1
         IF ( layers_krho(iLa).NE.1 )   errCount = errCount + 1
         DO k=1,Nlayers+1
          IF ( layers_bounds(k,iLa).NE.UNSET_RL ) errCount = errCount+1
         ENDDO
        ENDDO
C-    Transfert to new params setting:
        IF ( LAYER_nb.EQ.1 ) layers_name(1) = 'TH '
        IF ( LAYER_nb.EQ.2 ) layers_name(1) = 'SLT'
        IF ( LAYER_nb.EQ.3 ) layers_name(1) = 'RHO'
        layers_krho(1) = layers_kref
        layers_bolus(1) = useBOLUS
        DO k=1,Nlayers+1
          layers_bounds(k,1) = layers_G(k)
        ENDDO
      ENDIF
      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(2A)') 'LAYERS_READPARMS: ',
     &    'Cannot mix old params setting (LAYER_nb > 0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'LAYERS_READPARMS: ',
     &    ' with new params setting (layer_name(#)= ...)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I4,A)') 'LAYERS_READPARMS: ',
     &    'Detected', errCount,' fatal error/conflict(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R LAYERS_READPARMS'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Check for retired parameters:
      IF ( layers_taveFreq .NE. UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'LAYERS_READPARMS: "layers_taveFreq"',
     &    ' is no longer allowed in file "data.layers"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'LAYERS_READPARMS: ',
     &    ' since "pkg/timeave" has been removed.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--   Set layers_num according to layers_name:
      DO iLa=1,layers_maxNum
        layers_num(iLa) = 0
        IF ( layers_name(iLa).EQ.'TH ' ) layers_num(iLa) = 1
        IF ( layers_name(iLa).EQ.'SLT' ) layers_num(iLa) = 2
        IF ( layers_name(iLa).EQ.'RHO' ) layers_num(iLa) = 3
        IF ( layers_name(iLa).EQ.'MSE' ) layers_num(iLa) = 4
        IF ( layers_name(iLa).NE.' ' .AND.
     &       layers_num(iLa).EQ.0 ) THEN
          WRITE(msgBuf,'(2A,I2,3A)') 'LAYERS_READPARMS: ',
     &    'invalid layers_name(',iLa,')= "',layers_name(iLa),'"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
C--   bolus contribution only available if using GMRedi
        layers_bolus(iLa) = layers_bolus(iLa) .AND. useGMRedi
      ENDDO

C--   Make sure the layers_bounds we just read is big enough
      DO iLa=1,layers_maxNum
       IF ( layers_num(iLa).NE.0 ) THEN
        DO k=1,Nlayers+1
         IF ( layers_bounds(k,iLa).EQ.UNSET_RL ) THEN
          WRITE(msgBuf,'(2A,I4,A,I3,A)') 'LAYERS_READPARMS: ',
     &        'No value for layers_bounds(k=',k,', iLa=', iLa, ')'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
         ENDIF
        ENDDO
       ENDIF
#ifndef LAYERS_PRHO_REF
       IF ( layers_num(iLa).EQ.3 ) THEN
        WRITE(msgBuf,'(2A,I4)')
     &      'S/R LAYERS_READPARMS: ',
     &      'Layering in density requires to define LAYERS_PRHO_REF'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
       ENDIF
#endif
#ifndef LAYERS_MSE
       IF ( layers_num(iLa).EQ.4 ) THEN
        WRITE(msgBuf,'(2A,I4)')
     &      'S/R LAYERS_READPARMS: ',
     &      'Layering in MSE requires to define LAYERS_MSE'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
       ENDIF
#endif
C- Check MSE is not used with grey atmosphere nor with fizhi
       IF ( layers_num(iLa).EQ.4 ) THEN
        IF ( useAtm_Phys .OR. useFizhi ) THEN
         WRITE(msgBuf,'(2A,I4)')
     &       'S/R LAYERS_READPARMS: ',
     &       'Layering in MSE only work with Aim_v23'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
       ENDIF
      ENDDO

C--   Make sure that we locally honor the global MNC on/off flag
      layers_MNC = layers_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      layers_MNC = .FALSE.
#endif
      layers_MDSIO = (.NOT. layers_MNC) .OR. outputTypesInclusive

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &    'LAYERS_READPARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R LAYERS_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_MYPACKAGE */

      RETURN
      END
