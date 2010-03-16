C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_check.F,v 1.4 2010/03/16 00:14:47 jmc Exp $
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

       CALL WRITE_1D_RL(layers_G,Nlayers+1, INDEX_K,'layers_G =',
     &  ' /* boundaries of isopycnal-averaging bins */')

C--  Check parameters:


C     For now the package will only work if density ~ temperature
      IF ( (eosType .EQ. 'LINEAR')
     &  .AND. (sBeta .EQ. 0.0 _d 0) ) THEN
C        we are good
      ELSE
        WRITE(msgBuf,'(2A)') 'eosType must be eosType=''LINEAR''',
     &   ' and sBeta must = 0.0'
        CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R LAYERS_CHECK'
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_LAYERS */

      RETURN
      END
