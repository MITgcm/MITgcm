CBOP
C     !ROUTINE: FIZHI_WRITE_PICKUP 
C     !INTERFACE:
      SUBROUTINE FIZHI_WRITE_PICKUP( suff, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FIZHI_WRITE_PICKUP
C     | o Writes current state of fizhi package to a pickup file
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     lgf     :: flag to write "global" files
      INTEGER prec, iChar, lChar
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      lChar = 0
      DO iChar = 1,len(suff)
       IF ( suff(iChar:iChar) .NE. ' ') lChar=iChar
      ENDDO
      WRITE(fn,'(A,A)') 'pickup_fizhi.',suff(1:lChar)
      prec = precFloat64
      lgf = globalFiles

C--   Write fields as consecutive records
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nrphys,uphy,1,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nrphys,vphy,2,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nrphys,thphy,3,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nrphys,sphy,4,myIter,myThid)

      RETURN
      END
