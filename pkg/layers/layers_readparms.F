C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_readparms.F,v 1.9 2014/05/27 23:41:31 jmc Exp $
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
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
C     k          :: index
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, k, iLa

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

C--   Default values for LAYERS

C     The MNC stuff is not working yet
      layers_MNC = .FALSE.
      layers_MDSIO = .TRUE.

      DO iLa=1,layers_maxNum
        layers_name(iLa) = ' '
        layers_num(iLa) = 0
        layers_krho(iLa)= 1
        layers_bolus(iLa) = useGMRedi
        DO k=1,Nlayers+1
          layers_bounds(k,iLa) = UNSET_RL
        ENDDO
      ENDDO

      DO k=1,Nlayers+1
        layers_G(k) = UNSET_RL
      ENDDO
      layers_taveFreq = taveFreq
      layers_diagFreq = dumpFreq
      LAYER_nb = 0
      layers_kref = 1
      useBOLUS = useGMRedi

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

c revert to old approach
c (unless I simply retore the old ways, I need a
c  print statement or two as I override the new way)
      IF ( LAYER_nb.NE.0 ) THEN
        layers_num(1) = LAYER_nb
        layers_krho(1) = layers_kref
        layers_bolus(1) = useBOLUS
        DO k=1,Nlayers+1
          layers_bounds(k,1) = layers_G(k)
        ENDDO
        DO iLa=2,layers_maxNum
          layers_num(iLa) = 0
          layers_name(iLa) = ' '
          layers_krho(iLa) = 1
          layers_bolus(iLa) = useGMRedi
          DO k=1,Nlayers+1
            layers_bounds(k,iLa) = UNSET_RL
          ENDDO
        ENDDO
      ENDIF

C--   ensure layers_name/layers_num setup consistency
      DO iLa=1,layers_maxNum
        IF ( ( layers_name(iLa).EQ.'TH' ).OR.
     &       ( layers_num(iLa).EQ.1 ) ) THEN
          layers_name(iLa)='TH'
          layers_num(iLa)=1
        ELSEIF ( ( layers_name(iLa).EQ.'SLT' ).OR.
     &       ( layers_num(iLa).EQ.2 ) ) THEN
          layers_name(iLa)='SLT'
          layers_num(iLa)=2
        ELSEIF ( ( layers_name(iLa).EQ.'RHO' ).OR.
     &       ( layers_num(iLa).EQ.3 ) ) THEN
          layers_name(iLa)='RHO'
          layers_num(iLa)=3
        ELSE
          layers_name(iLa)=' '
          layers_num(iLa)=0
        ENDIF
C--   bolus contribution only available if using GMRedi
        layers_bolus(iLa) = layers_bolus(iLa) .AND. useGMRedi
      ENDDO

C--   Make sure the layers_bounds we just read is big enough
      DO iLa=1,layers_maxNum
       IF ( layers_num(iLa).NE.0 ) THEN
        DO k=1,Nlayers+1
         IF ( layers_bounds(k,iLa) .EQ. UNSET_RL ) THEN
          WRITE(msgBuf,'(2A,I4)')
     &        'S/R LAYERS_READPARMS: ',
     &        'No value for layers_bounds at k =', k
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_READPARMS'
         ELSEIF ( k .EQ. 1 ) THEN
C         Do nothing
         ELSEIF ( layers_bounds(k,iLa) .LE.
     &            layers_bounds(k-1,iLa) ) THEN
C         Check to make sure layers_bounds is increasing
          WRITE(msgBuf,'(2A,I4)')
     &      'S/R LAYERS_READPARMS: ',
     &      'layers_bounds is not increasing at k =', k
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_READPARMS'
         ENDIF
        ENDDO
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
