#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_CHECK( myThid )

C     Check dependances with other packages

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS_SWITCH.h"
#include "LAYERS.h"

C     myThid   :: my Thread Id number
      INTEGER myThid

C     LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(40) tmpName
      CHARACTER*(1) sfx
      INTEGER iLa, k, errCount
      _RL tmpVar

#ifdef ALLOW_LAYERS
      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'LAYERS_CHECK: #define LAYERS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

C--   Print out some key parameters :
       CALL WRITE_0D_I( NZZ, INDEX_NONE, 'NZZ =',
     &  ' /* number of levels in the fine vertical grid */')
       CALL WRITE_1D_RL( dZZf, NZZ, INDEX_K, 'dZZf =',
     &  ' /* fine vertical grid spacing for isopycnal interp */')

      DO iLa=1,layers_maxNum
       IF ( layers_num(iLa).NE.0 ) THEN
        sfx = '#'
        IF ( iLa.LE.9 ) WRITE(sfx,'(I1)') iLa

        WRITE(tmpName,'(3A)') 'layers_num(', sfx, ') ='
        CALL WRITE_0D_I( layers_num(iLa), INDEX_NONE, tmpName(1:15),
     &   ' /* averaging field: 1= theta, 2= salt, 3= prho */' )
        WRITE(tmpName,'(3A)') 'layers_name(', sfx, ') ='
        CALL WRITE_0D_C( layers_name(iLa),-1,INDEX_NONE, tmpName(1:16),
     &   ' /* averaging field: TH = theta, SLT= salt, RHO= prho */' )
        WRITE(tmpName,'(3A)') 'layers_bolus(', sfx, ') ='
        IF ( useGMRedi )
     &  CALL WRITE_0D_L ( layers_bolus(iLa), INDEX_NONE, tmpName(1:17),
     &   ' /* include potential GM bolus velocity */')
        WRITE(tmpName,'(3A)') 'layers_krho(', sfx, ') ='
        IF ( layers_num(iLa).EQ.3 )
     &  CALL WRITE_0D_I( layers_krho(iLa), INDEX_NONE, tmpName(1:16),
     &   ' /* model level to reference potential density to */' )
        WRITE(tmpName,'(3A)') 'layers_bounds(*,', sfx, ') ='
        CALL WRITE_1D_RL( layers_bounds(1,iLa), Nlayers+1, INDEX_K,
     &   tmpName(1:20), ' /* boundaries of tracer-averaging bins */')

       ENDIF !IF ( layers_num(iLa).NE.0 ) THEN
      ENDDO !DO iLa=1,layers_maxNum

C--   Check parameters:
      errCount = 0
      DO iLa=1,layers_maxNum

       IF ( layers_num(iLa).NE.0 ) THEN
C-    Check for inconsistent density layers_bounds specification
C     a) make sure layers_bounds is increasing:
        DO k=1,Nlayers
         IF ( layers_bounds(k,iLa).GE.layers_bounds(k+1,iLa) ) THEN
          WRITE(msgBuf,'(A,I2,A,I4)') 'LAYERS_CHECK(iLa=', iLa,
     &      '): layers_bounds k -> k+1 not increasing at k=', k
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
         ENDIF
        ENDDO
       ENDIF

       IF ( layers_num(iLa).EQ.3 ) THEN
C     Pot.Density is now expressed as rho-1000 (previously just rho):
C     b) check for realistic layers_bounds values:
        tmpVar = layers_bounds(Nlayers+1,iLa) - layers_bounds(1,iLa)
        IF ( tmpVar.LE.50. .AND. layers_bounds(1,iLa).GE.950. ) THEN
          WRITE(msgBuf,'(A,I2,A)') 'LAYERS_CHECK(iLa=', iLa,
     &      '): layers_bounds seems to be expressed as "rho"'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I2,A)') 'LAYERS_CHECK(iLa=', iLa,
     &      '): while it should be expressed as "rho - 1000"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
C-     Check for valid density reference level:
        IF ( layers_krho(iLa).LT.1 .OR. layers_krho(iLa).GT.Nr ) THEN
          WRITE(msgBuf,'(2A,I3,A,I9)') 'LAYERS_CHECK: ',
     &        'Invalid layer_krho(iLa=', iLa,') =', layers_krho(iLa)
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
       ENDIF

      ENDDO

      IF ( layers_useThermo .AND. .NOT. useDiagnostics ) THEN
        WRITE(msgBuf,'(2A)') 'LAYERS_CHECK: ',
     &    'layers-thermodynamics part requires useDiagnostics=T'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'LAYERS_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R LAYERS_CHECK'
      ELSE
       WRITE(msgBuf,'(A)') 'LAYERS_CHECK: done'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

      _END_MASTER(myThid)
#endif /* ALLOW_LAYERS */

      RETURN
      END
