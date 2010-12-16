C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_readparms.F,v 1.4 2010/12/16 00:56:48 dfer Exp $
C $Name:  $

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

      NAMELIST /LAYERS_PARM01/
     &       layers_G, layers_taveFreq, layers_diagFreq,
     &       LAYER_nb, useBOLUS

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
C     k           - index
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, k

      _BEGIN_MASTER(myThid)

C--   Default values for LAYERS

C     The MNC stuff is not working yet
      layers_MNC = .FALSE.
      layers_MDSIO = .TRUE.

      DO k=1,Nlayers+1
        layers_G(k) = UNSET_RL
      ENDDO
      layers_taveFreq = taveFreq
      layers_diagFreq = dumpFreq
      LAYER_nb = 1
      useBOLUS = .TRUE.

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
      CLOSE(iUnit)

C--   Make sure the layers_G we just read is big enough
      DO k=1,Nlayers+1
        IF ( layers_G(k) .EQ. UNSET_RL ) THEN
          WRITE(msgBuf,'(A,I4)')
     &        'S/R LAYERS_READPARMS: No value for layers_G at k =', k
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_READPARMS'
        ELSE IF ( k .EQ. 1 ) THEN
C         Do nothing
        ELSE IF ( layers_G(k) .LE. layers_G(k-1) ) THEN
C         Check to make sure layers_G is increasing
          WRITE(msgBuf,'(A,I4)')
     &      'S/R LAYERS_READPARMS: layers_G is not increasing at k =', k
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_READPARMS'
        ENDIF
      ENDDO

C--   Make sure that we locally honor the global MNC on/off flag
      layers_MNC = layers_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      layers_MNC = .FALSE.
#endif
      layers_MDSIO = (.NOT. layers_MNC) .OR. outputTypesInclusive

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_MYPACKAGE */

      RETURN
      END
