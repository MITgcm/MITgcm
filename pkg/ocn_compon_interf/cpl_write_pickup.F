#include "OCN_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE CPL_WRITE_PICKUP(
     I                             suff, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_WRITE_PICKUP                              |
C     | o Store coupling state for restart.                      |
C     | - Oceanic version -                                      |
C     *==========================================================*
C     | Presently, the atmospheric compon. is in charge of the   |
C     |  writing of coupling fields ; nothing done here for now. |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     permPickup :: write a permanent pickup
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
c     LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     fn          :: Workspace for building file name
C     rec_ind     :: index of the record in pickup file
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec, rec_ind

      WRITE(fn,'(A,A)') 'pickup_cpl.',suff
      prec = precFloat64
      rec_ind=0

c     IF ( .NOT. cpl_earlyExpImpCall ) THEN
c       CALL WRITE_REC_3D_RL( fn,prec,1, atmSLPr   , 1, myIter, myThid )
c       CALL WRITE_REC_3D_RL( fn,prec,1, HeatFlux  , 2, myIter, myThid )
c       CALL WRITE_REC_3D_RL( fn,prec,1, qShortWave, 3, myIter, myThid )
c       CALL WRITE_REC_3D_RL( fn,prec,1, tauX      , 4, myIter, myThid )
c       CALL WRITE_REC_3D_RL( fn,prec,1, tauY      , 5, myIter, myThid )
c       CALL WRITE_REC_3D_RL( fn,prec,1, FWFlux    , 6, myIter, myThid )
c       CALL WRITE_REC_3D_RL( fn,prec,1, iceSaltFlx, 7, myIter, myThid )
c       CALL WRITE_REC_3D_RL( fn,prec,1, seaIceMass, 8, myIter, myThid )
c       rec_ind=8
c     ENDIF

#ifdef ALLOW_DIC
      IF ( ocn_cplExch_DIC ) THEN
        rec_ind = rec_ind + 1
        CALL WRITE_REC_3D_RL( fn, prec, 1,
     &                        fluxCO2cpl, rec_ind, myIter, myThid )
      ENDIF
#endif /* ALLOW_DIC */

#endif /* COMPONENT_MODULE */

      RETURN
      END
