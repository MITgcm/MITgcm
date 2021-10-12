#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C--  File ecco_toolbox.F: Routines to handle basic operations common in ecco.
C--   Contents
C--   o ECCO_ZERO
C--   o ECCO_CP
C--   o ECCO_CPRSRL
C--   o ECCO_DIFFMSK
C--   o ECCO_ADDCOST
C--   o ECCO_ADD         (currently not used)
C--   o ECCO_SUBTRACT
C--   o ECCO_ADDMASK
C--   o ECCO_DIV
C--   o ECCO_DIVFIELD
C--   o ECCO_MULT
C--   o ECCO_MULTFIELD   (currently not used)
C--   o ECCO_MASKMINDEPTH
C--   o ECCO_OFFSET
C--   o ECCO_READBAR
C--   o ECCO_READWEI
C--   o ECCO_ERROR

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_ZERO
C     !INTERFACE:
      SUBROUTINE ECCO_ZERO( fld, nzIn, zeroLoc, myThid )
C     !DESCRIPTION: \bv
C     fill a field with zeroLoc
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn
      INTEGER myThid
      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL zeroLoc

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nzIn
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           fld(i,j,k,bi,bj) = zeroLoc
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_DIFFMSK
C     !INTERFACE:
      SUBROUTINE ECCO_DIFFMSK(
     I                   localBar, localObs, localMask,
     I                   nzIn, nLev, spMinLoc, spMaxLoc, spzeroLoc,
     O                   localDif, difMask,
     I                   myThid )

C     !DESCRIPTION: \bv
C     compute masked difference between model and observations
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL localBar (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL localObs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL localMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL spMinLoc, spMaxLoc, spzeroLoc
C     !OUTPUT PARAMETERS:
      _RL localDif (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL difMask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

c--   Determine the model-data difference mask
      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_DIFFMSK',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
#ifdef ECCO_CTRL_DEPRECATED
           difMask(i,j,k,bi,bj) = cosphi(i,j,bi,bj)
     &                          * localMask(i,j,k,bi,bj)
#else
           difMask(i,j,k,bi,bj) = localMask(i,j,k,bi,bj)
#endif
           IF ( localObs(i,j,k,bi,bj) .LT. spMinLoc .OR.
     &          localObs(i,j,k,bi,bj) .GT. spMaxLoc .OR.
     &          localObs(i,j,k,bi,bj) .EQ. spzeroLoc ) THEN
            difMask(i,j,k,bi,bj) = 0. _d 0
           ENDIF
           localDif(i,j,k,bi,bj) = difMask(i,j,k,bi,bj)*
     &          (localBar(i,j,k,bi,bj)-localObs(i,j,k,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_CP
C     !INTERFACE:
      SUBROUTINE ECCO_CP(
     I                   fldIn,
     U                   fldOut,
     I                   nzIn, nlev, myThid )

C     !DESCRIPTION: \bv
C     copy a field to another array
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_CP',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fldOut(i,j,k,bi,bj) = fldIn(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_CPRSRL
C     !INTERFACE:
      SUBROUTINE ECCO_CPRSRL(
     I                   fldIn,
     U                   fldOut,
     I                   nzIn, nlev, myThid )

C     !DESCRIPTION: \bv
C     copy a field to another array, switching from _RS to _RL
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RS fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_CPRSRL',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fldOut(i,j,k,bi,bj) = fldIn(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_ADDCOST
C     !INTERFACE:
      SUBROUTINE ECCO_ADDCOST(
     I     localDif, localWeight, difMask, nzIn, nLev, doSumSq,
     U     objf_local, num_local,
     I     myThid )

C     !DESCRIPTION: \bv
C     adds to a cost function term
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT PARAMETERS:
      INTEGER nzIn, nLev
      LOGICAL doSumSq
      INTEGER myThid
      _RL localDif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL localWeight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL difMask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
C     !OUTPUT PARAMETERS:
      _RL objf_local (nSx,nSy)
      _RL num_local  (nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      _RL localwww
      _RL localcost
      _RL junk
CEOP

      localwww = 0. _d 0

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_ADDCOST',
     &     'nLev > nzIn not allowed.', myThid )
c--     Compute normalized model-obs cost function
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        localcost    = 0. _d 0
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           localwww  = localWeight(i,j,k,bi,bj) * difMask(i,j,k,bi,bj)
           junk      = localDif(i,j,k,bi,bj)
           IF ( doSumSq ) THEN
            localcost = localcost + junk*junk*localwww
           ELSE
            localcost = localcost + junk*localwww
           ENDIF
           IF ( localwww .NE. 0. )
     &               num_local(bi,bj) = num_local(bi,bj) + 1. _d 0
          ENDDO
         ENDDO
        ENDDO
        objf_local(bi,bj) = objf_local(bi,bj) + localcost
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_ADD
C     !INTERFACE:
      SUBROUTINE ECCO_ADD(
     U                    fldOut,
     I                    fldIn, nzIn, nLev, myThid )

C     !DESCRIPTION: \bv
C     add a field (fldIn) to another field (fldOut)
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_ADD',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                         + fldIn (i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_SUBTRACT
C     !INTERFACE:
      SUBROUTINE ECCO_SUBTRACT(
     U                         fldOut,
     I                         fldIn, nzIn, nLev, myThid )

C     !DESCRIPTION: \bv
C     subtract a field (fldIn) from another field (fldOut)
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_SUBTRACT',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                         - fldIn (i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_ADDMASK
C     !INTERFACE:
      SUBROUTINE ECCO_ADDMASK(
     I                   fldIn, fldInmask,
     U                   fldOut, fldOutnum,
     I                   nzIn, nLev, myThid )

C     !DESCRIPTION: \bv
C     add a field to another array only grids where the mask is non-zero.
C     Also increase the counter by one one those girds.
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL fldIn     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldInmask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOutnum (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_ADDMASK',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( fldInmask(i,j,k,bi,bj) .NE. 0. _d 0 ) THEN
            fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                          + fldIn (i,j,k,bi,bj)
            fldOutnum(i,j,k,bi,bj) = fldOutnum(i,j,k,bi,bj) + 1. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_DIV
C     !INTERFACE:
      SUBROUTINE ECCO_DIV(
     U                    fld,
     I                    numerLoc, nzIn, nLev, myThid )
C     !DESCRIPTION: \bv
C     divide a field with RL constant
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL numerLoc

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      _RL recip_num
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_DIV',
     &     'nLev > nzIn not allowed.', myThid )
      IF ( numerLoc .NE. 0. _d 0 ) THEN
       recip_num = 1. _d 0 / numerLoc
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO k = 1,nLev
          DO j = 1,sNy
           DO i = 1,sNx
            fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj) * recip_num
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_DIVFIELD
C     !INTERFACE:
      SUBROUTINE ECCO_DIVFIELD( fld, fldDenom, nzIn, nLev, myThid )
C     !DESCRIPTION: \bv
C     divide a field by another field
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL fld     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldDenom(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_DIVFIELD',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( fldDenom(i,j,k,bi,bj) .NE. 0. _d 0 ) THEN
            fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj)/fldDenom(i,j,k,bi,bj)
           ELSE
            fld(i,j,k,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_MULT
C     !INTERFACE:
      SUBROUTINE ECCO_MULT(
     U                     fld,
     I                     multLoc, nzIn, nLev, myThid )
C     !DESCRIPTION: \bv
C     multiply a field with RL constant
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL multLoc
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_MULT',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj)*multLoc
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_MULTFIELD
C     !INTERFACE:
      SUBROUTINE ECCO_MULTFIELD(
     U                          fldOut,
     I                          fldIn, nzIn, nLev, myThid )
C     !DESCRIPTION: \bv
C     multiply a field by another field, fldOut is updated on output
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_MULTFIELD',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)*fldIn(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_MASKMINDEPTH
C     !INTERFACE:
      SUBROUTINE ECCO_MASKMINDEPTH(
     U     difMask,
     I     nzIn, nLev, topoMin, myThid )
C     !DESCRIPTION: \bv
C     set difMask to zero where topography is shallower that topoMin
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nLev
      INTEGER myThid
      _RL difMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL topoMin

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_MASMINDEPTH',
     &     'nLev > nzIn not allowed.', myThid )
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( R_low(i,j,bi,bj) .GT. topoMin ) THEN
            DO k = 1,nLev
             difMask(i,j,k,bi,bj) = zeroRL
            ENDDO
           ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_OFFSET
C     !INTERFACE:
      SUBROUTINE ECCO_OFFSET(
     I                   fName,
     U                   fld,
     I                   difMask, nzIn, nLev,
     I                   myThid )

C     !DESCRIPTION: \bv
C     remove mean of masked field
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
      INTEGER nzIn, nLev
      CHARACTER*(*) fName
      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL difMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      _RL volTile(nSx,nSy), sumTile(nSx,nSy)
      _RL tmpVol, volGlob, sumGlob
      _RL theMean
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR( 'ECCO_OFFSET',
     &     'nLev > nzIn not allowed.', myThid )
c--   Compute the mean
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        volTile(bi,bj) = 0. _d 0
        sumTile(bi,bj) = 0. _d 0
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           tmpVol = difMask(i,j,k,bi,bj)
           volTile(bi,bj) = volTile(bi,bj) + tmpVol
           sumTile(bi,bj) = sumTile(bi,bj) + tmpVol*fld(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( volTile, volGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( sumTile, sumGlob, myThid )

      IF ( volGlob.GT.zeroRL ) THEN
       theMean = sumGlob/volGlob
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO k = 1,nLev
          DO j = 1,sNy
           DO i = 1,sNx
            IF (difMask(i,j,k,bi,bj) .NE. 0. _d 0) THEN
             fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj) - theMean
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       theMean = 0. _d 0
      ENDIF

C     Print the global mean to standard output
      _BEGIN_MASTER( myThid )
      WRITE(msgBuf,'(3A,1PE21.14)')
     &      'ecco_offset: # of nonzero constributions to mean of ',
     &      fname, ' = ', volGlob
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(3A,1PE21.14)')
     &      'ecco_offset:                         Global mean of ',
     &      fname, ' = ', theMean
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
      _END_MASTER( myThid )

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_READBAR
C     !INTERFACE:
      SUBROUTINE ECCO_READBAR(
     I                           active_var_file,
     O                           active_var,
     I                           iRec, nzIn, nLev,
     I                           dummy,
     I                           myThid )

C     !DESCRIPTION: \bv
C     reads one record from averaged time series ("bar file")
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file :: filename
C     active_var      :: array
C     iRec            :: record number
C     nzIn            :: size of active_var
C     nLev            :: number vertical levels used
      CHARACTER*(*) active_var_file
      INTEGER iRec, nzIn, nLev
      INTEGER myThid
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL     dummy

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      LOGICAL doGlobalRead
      LOGICAL lAdInit
      INTEGER i,j,bi,bj
      _RL     tmpFld2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      doGlobalRead = .FALSE.
      lAdInit      = .FALSE.

#ifdef ALLOW_AUTODIFF
      IF ( nLev .EQ. 1 ) THEN
       CALL ACTIVE_READ_XY( active_var_file, tmpFld2D,
     &                      iRec, doGlobalRead,
     &                      lAdInit, eccoiter, myThid,
     &                      dummy )
      ELSEIF ( nLev .EQ. Nr .AND. nzIn .EQ. Nr ) THEN
       CALL ACTIVE_READ_XYZ( active_var_file, active_var,
     &                       iRec, doGlobalRead,
     &                       lAdInit, eccoiter, myThid,
     &                       dummy )
      ELSE
       WRITE(msgBuf,'(A,I3,A)') 'nLev = ',nLev,' should not happen'
       CALL ECCO_ERROR( 'ECCO_READBAR', msgBuf, myThid )
      ENDIF
#else
      IF ( nLev .EQ. 1 ) THEN
       CALL READ_REC_XY_RL( active_var_file, tmpFld2D,
     &                      iRec, 1, myThid )
      ELSEIF ( nLev .EQ. Nr .AND. nzIn .EQ. Nr ) THEN
       CALL READ_REC_XYZ_RL( active_var_file, active_var,
     &                       iRec, 1, myThid )
      ELSE
       WRITE(msgBuf,'(A,I3,A)') 'nLev = ',nLev,' should not happen'
       CALL ECCO_ERROR( 'ECCO_READBAR', msgBuf, myThid )
      ENDIF
#endif

      IF ( nLev .EQ. 1 ) THEN
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           active_var(i,j,1,bi,bj) = tmpFld2D(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_READWEI
C     !INTERFACE:
      SUBROUTINE ECCO_READWEI(
     I                         localErr_file,
     O                         localWeight,
     I                         iRec, nzIn, nLev,
     I                         doSumSq,
     I                         myThid )

C     !DESCRIPTION: \bv
C     reads uncertainty field and compute weight as squared inverse
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     localErr_file :: filename
C     localWeight   :: array
C     iRec          :: record number
      CHARACTER*(*) localErr_file
      INTEGER iRec, nzIn, nLev
      LOGICAL doSumSq
      INTEGER myThid
      _RL     localWeight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
CEOP

      CALL READ_REC_LEV_RL( localErr_file, cost_iprec, nzIn, 1, nLev,
     &                      localWeight, iRec, 1, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
c--        Test for missing values.
           IF (localWeight(i,j,k,bi,bj) .LT. -9900.) THEN
             localWeight(i,j,k,bi,bj) = 0. _d 0
c--        Convert to weight
           ELSEIF ( localWeight(i,j,k,bi,bj).NE.0. .AND. doSumSq ) THEN
              localWeight(i,j,k,bi,bj) =
     &          oneRL/localWeight(i,j,k,bi,bj)/localWeight(i,j,k,bi,bj)
           ELSEIF ( localWeight(i,j,k,bi,bj).NE.0. ) THEN
              localWeight(i,j,k,bi,bj) =
     &           oneRL/localWeight(i,j,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ECCO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_ERROR
C     !INTERFACE:
      SUBROUTINE ECCO_ERROR( srName, errMsg, myThid )
C     !DESCRIPTION: \bv
C     prints error messages and stops
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     srName :: name of caller
C     errMsg :: error message to be printed
      CHARACTER*(*) srName, errMsg
      INTEGER myThid

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A,A,A)') srName, ': ', errMsg
      CALL PRINT_ERROR( msgBuf, myThid )
      CALL ALL_PROC_DIE( myThid )
      STOP 'ABNORMAL END: S/R ECCO_ERROR'

#endif /* ALLOW_ECCO */

      RETURN
      END
