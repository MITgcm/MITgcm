C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_check.F,v 1.7 2012/09/19 18:48:18 gforget Exp $
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
      INTEGER iLa

#ifdef ALLOW_LAYERS

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'LAYERS_CHECK: #define LAYERS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

C--  Print out some key parameters :
       CALL WRITE_0D_I( NZZ, INDEX_NONE, 'NZZ =',
     &  ' /* number of levels in the fine vertical grid */')

       CALL WRITE_1D_RL( dZZf, NZZ, INDEX_K, 'dZZf =',
     &  ' /* fine vertical grid spacing for isopycnal interp */')

      DO iLa=1,layers_maxNum
      IF ( layers_num(iLa).NE.0 ) THEN

       CALL WRITE_0D_I( layers_num(iLa), INDEX_NONE, 'layers_num =',
     &  '/* (1) theta; (2) salt; (3) prho; as averaging field */' )

       CALL WRITE_0D_C( layers_name(iLa),-1,INDEX_NONE,'layers_name =',
     &  '/* (TH) theta; (SLT) salt; (RHO) prho; as averaging field */' )

      CALL WRITE_0D_L ( layers_bolus(iLa), INDEX_NONE,
     & 'layers_bolus =', ' /* include potential GM bolus velocity */')
     
       IF ( layers_num(iLa).EQ.3)
     &  CALL WRITE_0D_I( layers_krho(iLa), INDEX_NONE, 'layers_krho =',
     &  ' /* model level to reference potential density to */' )

       CALL WRITE_1D_RL(layers_bounds(1,iLa),
     &  Nlayers+1, INDEX_K,'layers_bounds =',
     &  ' /* boundaries of tracer-averaging bins */')
     
      ENDIF !IF ( layers_num(iLa).NE.0 ) THEN
      ENDDO !DO iLa=1,layers_maxNum

C--  Check parameters:


C     For now the package will only work if density ~ temperature
c     IF ( (eosType .EQ. 'LINEAR')
c    &  .AND. (sBeta .EQ. 0.0 _d 0) ) THEN
C        we are good
c     ELSE
c       WRITE(msgBuf,'(2A)') 'eosType must be eosType=''LINEAR''',
c    &   ' and sBeta must = 0.0'
c       CALL PRINT_ERROR( msgBuf , 1)
c       STOP 'ABNORMAL END: S/R LAYERS_CHECK'
c     ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_LAYERS */

      RETURN
      END
