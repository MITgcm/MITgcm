C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_check.F,v 1.8 2015/06/11 21:40:40 jmc Exp $
C $Name:  $

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
#include "LAYERS.h"

C     myThid   :: my Thread Id number
      INTEGER myThid

C     LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iLa, k
      _RL tmpVar

#ifdef ALLOW_LAYERS
      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'LAYERS_CHECK: #define LAYERS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

C--   Print out some key parameters :
       CALL WRITE_0D_I( NZZ, INDEX_NONE, 'NZZ =',
     &  ' /* number of levels in the fine vertical grid */')
       CALL WRITE_1D_RL( dZZf, NZZ, INDEX_K, 'dZZf =',
     &  ' /* fine vertical grid spacing for isopycnal interp */')

      DO iLa=1,layers_maxNum
       IF ( layers_num(iLa).NE.0 ) THEN

        CALL WRITE_0D_I( layers_num(iLa), INDEX_NONE, 'layers_num =',
     &  '/* (1) theta; (2) salt; (3) prho; as averaging field */' )
        CALL WRITE_0D_C( layers_name(iLa),-1,INDEX_NONE,'layers_name =',
     &  '/* (TH) theta; (SLT) salt; (RHO) prho; as averaging field */')
        CALL WRITE_0D_L ( layers_bolus(iLa), INDEX_NONE,
     &  'layers_bolus =',' /* include potential GM bolus velocity */')
        IF ( layers_num(iLa).EQ.3 )
     &  CALL WRITE_0D_I( layers_krho(iLa), INDEX_NONE, 'layers_krho =',
     &  ' /* model level to reference potential density to */' )
        CALL WRITE_1D_RL( layers_bounds(1,iLa), Nlayers+1, INDEX_K,
     &  'layers_bounds =', ' /* boundaries of tracer-averaging bins */')

       ENDIF !IF ( layers_num(iLa).NE.0 ) THEN
      ENDDO !DO iLa=1,layers_maxNum

C--   Check parameters:
      DO iLa=1,layers_maxNum

       IF ( layers_num(iLa).NE.0 ) THEN
C-    check for inconsistent density layers_bounds specification
C     make sure layers_bounds is increasing:
        DO k=1,Nlayers
         IF ( layers_bounds(k,iLa).GE.layers_bounds(k+1,iLa) ) THEN
          WRITE(msgBuf,'(A,I2,A,I4)') 'LAYERS_CHECK(iLa=', iLa,
     &      '): layers_bounds k -> k+1 not increasing at k=', k
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_CHECK'
         ENDIF
        ENDDO
       ENDIF

       IF ( layers_num(iLa).EQ.3 ) THEN
C     Pot.Density is now expressed as rho-1000 (previously just rho):
C     check for realistic layers_bounds values:
        tmpVar = layers_bounds(Nlayers+1,iLa) - layers_bounds(1,iLa)
        IF ( tmpVar.LE.50. .AND. layers_bounds(1,iLa).GE.950. ) THEN
          WRITE(msgBuf,'(A,I2,A)') 'LAYERS_CHECK(iLa=', iLa,
     &      '): layers_bounds seems to be expressed as "rho"'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I2,A)') 'LAYERS_CHECK(iLa=', iLa,
     &      '): while it should be expressed as "rho - 1000"'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_CHECK'
        ENDIF
       ENDIF

      ENDDO

      _END_MASTER(myThid)
#endif /* ALLOW_LAYERS */

      RETURN
      END
