C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_read_pickup.F,v 1.1 2008/03/11 20:56:14 dfer Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"
#include "DIC_OPTIONS.h"

      SUBROUTINE DIC_READ_PICKUP( myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_ABIOTIC.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of DIC_READ_CHECKPOINT
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC
#ifdef DIC_BIOTIC

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec
CEOP

C--   Suffix for pickup files
      IF (pickupSuff.EQ.' ') THEN
         WRITE(suff,'(I10.10)') myIter
      ELSE
         WRITE(suff,'(A10)') pickupSuff
      ENDIF

      _BARRIER

      prec = precFloat64

      WRITE(fn,'(A,A10)') 'pickup_dic.',suff
      CALL READ_REC_3D_RL( fn, prec, 1, pH, 1, myIter, myThid )

      _EXCH_XY_R8( pH, myThid )

#endif /*  DIC_BIOTIC  */
#endif /*  ALLOW_DIC  */

      RETURN
      END

