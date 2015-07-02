C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_read_pickup.F,v 1.1 2015/07/02 04:42:43 heimbach Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE GGL90_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE GGL90_READ_PICKUP
C     | o read ggl90 pickups
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of GGL90_READ_CHECKPOINT
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90

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

      WRITE(fn,'(A,A10)') 'pickup_ggl90.',suff
      CALL READ_REC_3D_RL( fn, prec, Nr, GGL90TKE, 1, myIter, myThid )

c     _BARRIER

      _EXCH_XYZ_RL( GGL90TKE, myThid )

#ifdef ALLOW_GGL90_IDEMIX
      IF (useIDEMIX) THEN
       CALL READ_REC_3D_RL( fn, prec, Nr, IDEMIX_E, 2, myIter, myThid )
       _EXCH_XYZ_RL( IDEMIX_E, myThid )
      ENDIF
#endif

#endif /*  ALLOW_GGL90  */

      RETURN
      END

