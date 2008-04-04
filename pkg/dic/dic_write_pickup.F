C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_write_pickup.F,v 1.2 2008/04/04 21:37:06 dfer Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"
#include "DIC_OPTIONS.h"

      SUBROUTINE DIC_WRITE_PICKUP( permPickup, suff,
     I                               myTime, myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_VARS.h"

C     == Routine arguments ==
C     permPickup :: write a permanent pickup
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC
#ifdef DIC_BIOTIC

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      prec = precFloat64
      WRITE(fn,'(A,A)') 'pickup_dic.',suff

      CALL WRITE_REC_3D_RL( fn, prec, 1, pH, 1, myIter, myThid )

#endif /*  DIC_BIOTIC  */
#endif /*  ALLOW_DIC  */

      RETURN
      END
