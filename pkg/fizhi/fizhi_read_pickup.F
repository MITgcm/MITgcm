CBOP
C     !ROUTINE: FIZHI_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE FIZHI_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FIZHI_READ_PICKUP
C     | o Reads current fizhi state from a pickup file
C     *==========================================================*
C     \ev

C     !USES:
CEOP
      IMPLICIT NONE

C     == Global variables ===
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "land_SIZE.h"
#include "fizhi_coms.h"
#include "land_coms.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine Arguments ==
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     lgf     :: flag to write "global" files
C     k       :: loop index
      INTEGER prec, iChar, lChar, k
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf

      WRITE(fn,'(A,I10.10)') 'pickup_fizhi.',myIter
      prec = precFloat64

C-    Read fields as consecutive records
      CALL MDSREADFIELD(fn,prec,'RL',Nrphys,uphy,1,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',Nrphys,vphy,2,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',Nrphys,thphy,3,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',Nrphys,sphy,4,myThid)

      RETURN
      END
