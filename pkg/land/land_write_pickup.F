C $Header: /u/gcmpack/MITgcm/pkg/land/land_write_pickup.F,v 1.2 2004/03/11 14:42:00 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_WRITE_PICKUP 
C     !INTERFACE:
      SUBROUTINE LAND_WRITE_PICKUP( suff, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_WRITE_PICKUP
C     | o Writes current state of land package to a pickup file
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "LAND_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"

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

#ifdef ALLOW_LAND

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     lgf     :: flag to write "global" files
c     INTEGER prec, iChar, lChar, k
      INTEGER prec, lChar, k
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf

      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      lChar = ILNBLNK(suff)
      WRITE(fn,'(A,A)') 'pickup_land.',suff(1:lChar)
      prec = precFloat64
      lgf = globalFiles

C--   Write fields as consecutive records
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',land_nLev,
     &                   land_enthalp,1,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',land_nLev,
     &                   land_groundW,2,myIter,myThid)
      k=2*land_nLev
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,
     &                   land_skinT,  k+1,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,
     &                   land_hSnow,  k+2,myIter,myThid)
      CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,
     &                   land_snowAge,k+3,myIter,myThid)

#endif /* ALLOW_LAND */

      RETURN
      END
