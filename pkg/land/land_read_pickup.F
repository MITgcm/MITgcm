C $Header: /u/gcmpack/MITgcm/pkg/land/land_read_pickup.F,v 1.1 2003/06/12 17:54:22 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE LAND_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_READ_PICKUP
C     | o Reads current state of land model from a pickup file
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

C     == Routine Arguments ==
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_LAND

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     lgf     :: flag to write "global" files
C     k       :: loop index
      INTEGER prec, iChar, lChar, k
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      WRITE(fn,'(A,I10.10)') 'pickup_land.',myIter
      prec = precFloat64

C-    Read fields as consecutive records
      CALL MDSREADFIELD(fn,prec,'RL',land_nLev,land_groundT,1,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',land_nLev,land_groundW,2,myThid)

C- jmc: exchange is not really necessary for land model
C-      and presently exchange S/R cannot work for 3d array where 3rd dim <> Nr

#endif /* ALLOW_LAND */

      RETURN
      END
