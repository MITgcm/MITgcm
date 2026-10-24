#include "WETDRY_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_WRITE_PICKUP

C !INTERFACE:
      SUBROUTINE WETDRY_WRITE_PICKUP( permPickup,
     &                    suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Write the pkg/wetdry pickup file "pickup_wetdry.{suff}": the global
C     volume-budget scalars wdVolAdj, wdCumIn and wdVol0. The face
C     masks are recomputed from etaH at restart (S/R WETDRY_UPDATE_MASKS).

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "WETDRY.h"

C !INPUT PARAMETERS:
C     permPickup :: write a permanent pickup
C     suff       :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime     :: model time
C     myIter     :: time-step number
C     myThid     :: thread number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C !LOCAL VARIABLES:
      _RL budget(3)
      CHARACTER*(MAX_LEN_FNAM) fn

      budget(1) = wdVolAdj
      budget(2) = wdCumIn
      budget(3) = wdVol0
      CALL WRITE_GLVEC_RL( 'pickup_wetdry.', suff, budget, 3,
     &                     myIter, myThid )
C-    upwind side of each face (wdUpwindFace), for an exact restart
      WRITE(fn,'(A,A)') 'pickup_wetdryface.', suff
      CALL WRITE_REC_3D_RL( fn, precFloat64, 1, wdUpW, 1,
     &                      myIter, myThid )
      CALL WRITE_REC_3D_RL( fn, precFloat64, 1, wdUpS, 2,
     &                      myIter, myThid )
#endif /* ALLOW_WETDRY */

      RETURN
      END
