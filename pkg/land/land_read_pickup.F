C $Header: /u/gcmpack/MITgcm/pkg/land/land_read_pickup.F,v 1.2 2004/03/11 14:42:00 jmc Exp $
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
C     k       :: loop index
      INTEGER prec, k
      CHARACTER*(MAX_LEN_FNAM) fn
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      WRITE(fn,'(A,I10.10)') 'pickup_land.',myIter
      prec = precFloat64

      IF ( land_oldPickup ) THEN
C-    Read fields as consecutive records
      CALL MDSREADFIELD(fn,prec,'RL',land_nLev,land_groundT,1,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',land_nLev,land_groundW,2,myThid)

      ELSE

C-    Read fields as consecutive records
      CALL MDSREADFIELD(fn,prec,'RL',land_nLev,land_enthalp,1,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',land_nLev,land_groundW,2,myThid)
      k=2*land_nLev
      CALL MDSREADFIELD(fn,prec,'RL', 1, land_skinT,  k+1, myThid)
      CALL MDSREADFIELD(fn,prec,'RL', 1, land_hSnow,  k+2, myThid)
      CALL MDSREADFIELD(fn,prec,'RL', 1, land_snowAge,k+3, myThid)

      ENDIF

C- jmc: exchange is not really necessary for land model
C-      and presently exchange S/R cannot work for 3d array where 3rd dim <> Nr

#endif /* ALLOW_LAND */

      RETURN
      END
