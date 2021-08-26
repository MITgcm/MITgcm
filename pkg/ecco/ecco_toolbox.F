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
C--   o ECCO_DIFFANOMMSK (currently not used)
C--   o ECCO_OBSMSK      (currently not used)
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
      SUBROUTINE ECCO_ZERO( fld, nnzloc, zeroloc, myThid )
C     !DESCRIPTION: \bv
C     fill a field with zeroloc
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nnzloc
      _RL zeroloc
      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nnzloc
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           fld(i,j,k,bi,bj) = zeroloc
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
     I                   localbar, localobs, localmask,
     I                   nzIn, nLev,spminloc, spmaxloc, spzeroloc,
     O                   localdif, difmask,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     compute masked difference between model and observations
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nLev, nzIn

      _RL localbar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
C     output
      _RL localdif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL difmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

      _RL spminloc, spmaxloc, spzeroloc

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

c--   Determine the model-data difference mask
      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_DIFFMSK',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
#ifdef ECCO_CTRL_DEPRECATED
           difmask(i,j,k,bi,bj) = cosphi(i,j,bi,bj)*
     &                            localmask(i,j,k,bi,bj)
#else
           difmask(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)
#endif
           IF ( localobs(i,j,k,bi,bj) .LT. spminloc .OR.
     &          localobs(i,j,k,bi,bj) .GT. spmaxloc .OR.
     &          localobs(i,j,k,bi,bj) .EQ. spzeroloc ) THEN
            difmask(i,j,k,bi,bj) = 0. _d 0
           ENDIF
           localdif(i,j,k,bi,bj) = difmask(i,j,k,bi,bj)*
     &          (localbar(i,j,k,bi,bj)-localobs(i,j,k,bi,bj))
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
C     !ROUTINE: ECCO_DIFFANOMMSK
C     !INTERFACE:
      SUBROUTINE ECCO_DIFFANOMMSK(
     I                   localbar, localbarmean, nnzbar,
     I                   localobs, localobsmean, nnzobs,
     I                   localmask,
     I                   spminloc, spmaxloc, spzeroloc,
     O                   localdif, difmask,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     compute masked difference between time-anomaly model and observations
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nnzobs, nnzbar

      _RL localbar    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localbarmean(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localobs    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL localobsmean(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL localmask   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localdif    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL difmask     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)

      _RL spminloc, spmaxloc, spzeroloc

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nnzobs .GT. nnzbar ) CALL ECCO_ERROR('ECCO_DIFFANOMMSK',
     &     'nnzobs > nnzbar not allowed.', myThid)
c--   Determine the model-data difference mask
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nnzobs
         DO j = 1,sNy
          DO i = 1,sNx
#ifdef ECCO_CTRL_DEPRECATED
           difmask(i,j,k,bi,bj) = cosphi(i,j,bi,bj)*
     &                            localmask(i,j,k,bi,bj)
#else
           difmask(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)
#endif
           IF ( localobs(i,j,k,bi,bj) .LT. spminloc .OR.
     &          localobs(i,j,k,bi,bj) .GT. spmaxloc .OR.
     &          localobs(i,j,k,bi,bj) .EQ. spzeroloc ) THEN
            difmask(i,j,k,bi,bj) = 0. _d 0
           ENDIF
           localdif(i,j,k,bi,bj) = difmask(i,j,k,bi,bj)*
     &          ( (localbar(i,j,k,bi,bj)-localbarmean(i,j,k,bi,bj))
     &           -(localobs(i,j,k,bi,bj)-localobsmean(i,j,k,bi,bj)) )
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
C     !ROUTINE: ECCO_OBSMSK
C     !INTERFACE:
      SUBROUTINE ECCO_OBSMSK(
     I                   localbar, nnzbar, localobs, nnzobs, localmask,
     I                   spminloc, spmaxloc, spzeroloc,
     O                   localout, obsmask,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     mask (model) fieds if observation is out-of-bound or missing.
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nnzobs, nnzbar

      _RL localbar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL localout   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL obsmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)

      _RL spminloc, spmaxloc, spzeroloc

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nnzobs .GT. nnzbar ) CALL ECCO_ERROR('ECCO_OBSMSK',
     &     'nnzobs > nnzbar not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nnzobs
         DO j = 1,sNy
          DO i = 1,sNx
#ifdef ECCO_CTRL_DEPRECATED
           obsmask(i,j,k,bi,bj) = cosphi(i,j,bi,bj)*
     &                            localmask(i,j,k,bi,bj)
#else
           obsmask(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)
#endif
           IF ( localobs(i,j,k,bi,bj) .LT. spminloc .OR.
     &          localobs(i,j,k,bi,bj) .GT. spmaxloc .OR.
     &          localobs(i,j,k,bi,bj) .EQ. spzeroloc ) THEN
            obsmask(i,j,k,bi,bj) = 0. _d 0
           ENDIF
           localout(i,j,k,bi,bj) = obsmask(i,j,k,bi,bj)*
     &          localbar(i,j,k,bi,bj)
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
     I                   fldIn, fldOut, nzIn, nlev,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     copy a field to another array
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nLev, nzIn

      _RL fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

c--
      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_CP',
     &     'nLev > nzIn not allowed.', myThid)
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
     I                   fldIn, fldOut, nzIn, nLev,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     copy a field to another array, switching from _RS to _RL
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nLev, nzIn

      _RS fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

c--
      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_CPRSRL',
     &     'nLev > nzIn not allowed.', myThid)
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
     I     localdif, localweight, difmask, nzIn, nLev, dosumsq,
     U     objf_local, num_local,
     I     myThid
     &     )

C     !DESCRIPTION: \bv
C     adds to a cost function term
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nzIn
      INTEGER nLev

      _RL localdif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL localweight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL difmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

      _RL objf_local (nSx,nSy)
      _RL num_local  (nSx,nSy)

      LOGICAL dosumsq

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k

      _RL localwww
      _RL localcost
      _RL junk
CEOP

      localwww = 0. _d 0

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_ADDCOST',
     &     'nLev > nzIn not allowed.', myThid)
c--     Compute normalized model-obs cost function
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        localcost    = 0. _d 0
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           localwww  = localweight(i,j,k,bi,bj) * difmask(i,j,k,bi,bj)
           junk      = localdif(i,j,k,bi,bj)
           IF ( dosumsq ) THEN
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
     I                   fldIn, fldOut, nzIn, nLev,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     add a field (fldIn) to another field (fldOut)
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nLev, nzIn

      _RL fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn ,nSx,nSy)
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_ADD',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                          + fldIn(i,j,k,bi,bj)
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
     I                   fldIn, fldOut, nzIn, nLev,
     I                   myThid
     &                   )

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
# include "ecco.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nLev, nzIn

      _RL fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_SUBTRACT',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                          - fldIn(i,j,k,bi,bj)
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
     I                   fldIn, fldInmask, fldOut, fldOutnum, nzIn,
     I                   nLev, myThid
     &                   )

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
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nLev, nzIn

      _RL fldIn     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldInmask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOutnum (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_ADDMASK',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( fldInmask(i,j,k,bi,bj) .NE. 0. _d 0 ) THEN
            fldOut(i,j,k,bi,bj) = fldOut(i,j,k,bi,bj)
     &                           + fldIn(i,j,k,bi,bj)
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
      SUBROUTINE ECCO_DIV( fld, nzIn, nLev, numerloc, myThid )
C     !DESCRIPTION: \bv
C     divide a field with RL constant
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nzIn, nLev
      _RL numerloc
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
      _RL recip_num
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_DIV',
     &     'nLev > nzIn not allowed.', myThid)
      IF ( numerloc .NE. 0. _d 0 ) THEN
       recip_num = 1. _d 0 / numerloc
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
      SUBROUTINE ECCO_DIVFIELD( fld, flddenom, nzIn, nLev, myThid )
C     !DESCRIPTION: \bv
C     divide a field by another field
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nzIn, nLev
      _RL fld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL flddenom (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_DIVFIELD',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( flddenom(i,j,k,bi,bj) .NE. 0. _d 0 ) THEN
            fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj)/flddenom(i,j,k,bi,bj)
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
      SUBROUTINE ECCO_MULT( fld, nzIn, nLev, multloc, myThid )
C     !DESCRIPTION: \bv
C     multiply a field with RL constant
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nzIn, nLev
      _RL multloc
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_MULT',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj)*multloc
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
      SUBROUTINE ECCO_MULTFIELD( fld, fld2, nzIn, nLev, myThid )
C     !DESCRIPTION: \bv
C     multiply a field by another field, fld2 is updated on output
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nzIn, nLev
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fld2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_MULTFIELD',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           fld2(i,j,k,bi,bj) = fld(i,j,k,bi,bj) * fld2(i,j,k,bi,bj)
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
     U     difmask,
     I     nzIn, nLev, topomin, myThid )
C     !DESCRIPTION: \bv
C     set difmask to zero where topography is shallower that topomin
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nzIn, nLev
      _RL difmask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL topomin

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_MASMINDEPTH',
     &     'nLev > nzIn not allowed.', myThid)
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( R_low(i,j,bi,bj) .GT. topomin ) THEN
            DO k = 1,nLev
             difmask(i,j,k,bi,bj) = zeroRL
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
     I                   fname,
     U                   fld,
     I                   difmask, nzIn, nLev,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     remove mean of masked field
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
      INTEGER nzIn, nLev

      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL difmask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      CHARACTER*(*) fname

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,i,j,k
      _RL volTile(nSx,nSy), sumTile(nSx,nSy)
      _RL tmpVol, volGlob, sumGlob
      _RL theMean
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( nLev .GT. nzIn ) CALL ECCO_ERROR('ECCO_OFFSET',
     &     'nLev > nzIn not allowed.', myThid)
c--   Compute the mean
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        volTile(bi,bj) = 0. _d 0
        sumTile(bi,bj) = 0. _d 0
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
           tmpVol = difmask(i,j,k,bi,bj)
           volTile(bi,bj) = volTile(bi,bj) + tmpVol
           sumTile(bi,bj) = sumTile(bi,bj) + tmpVol*fld(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( volTile, volGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( sumTile, sumGlob, myThid )

      if ( volGlob.GT.zeroRL ) THEN
       theMean = sumGlob/volGlob
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO k = 1,nLev
          DO j = 1,sNy
           DO i = 1,sNx
            IF (difmask(i,j,k,bi,bj) .NE. 0. _d 0) THEN
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

C--- +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_READBAR
C     !INTERFACE:
      SUBROUTINE ECCO_READBAR(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           nz,
     I                           nnzbar,
     I                           dummy,
     I                           myThid
     &                         )

C     !DESCRIPTION: \bv
C     reads one record from averaged time series ("bar file")
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     nz:              size of active_var
C     nnzbar:          number vertical levels used
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nz,nSx,nSy)
      INTEGER iRec
      INTEGER myThid
      INTEGER nz
      INTEGER nnzbar
      _RL     dummy

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     tmpFld2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER i,j,bi,bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_AUTODIFF
      IF ( nnzbar .EQ. 1 ) THEN
       CALL ACTIVE_READ_XY( active_var_file, tmpFld2D,
     &                      irec, doglobalread,
     &                      ladinit, eccoiter, myThid,
     &                      dummy )
      ELSEIF ( nnzbar .EQ. Nr ) THEN
       CALL ACTIVE_READ_XYZ( active_var_file, active_var,
     &                       irec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       dummy )
      ELSE
       WRITE(msgBuf,'(A,I3,A)') 'nnzbar = ',nnzbar,' should not happen'
       CALL ECCO_ERROR('ECCO_READBAR',
     &      'this should not happen.', myThid)
      ENDIF
#else
      IF ( nnzbar .EQ. 1 ) THEN
       CALL READ_REC_XY_RL( active_var_file, tmpFld2D,
     &                      iRec, 1, myThid )
      ELSEIF ( nnzbar .EQ. Nr ) THEN
       CALL READ_REC_XYZ_RL( active_var_file, active_var,
     &                        iRec, 1, myThid )
      ELSE
       WRITE(msgBuf,'(A,I3,A)') 'nnzbar = ',nnzbar,' should not happen'
       CALL ECCO_ERROR('ECCO_READBAR',
     &      'this should not happen.', myThid)
      ENDIF
#endif

      IF ( nnzbar .EQ. 1 ) THEN
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
     I                           localerr_file,
     O                           localweight,
     I                           iRec,
     I                           nzIn,
     I                           nLev,
     I                           dosumsq,
     I                           myThid
     &                         )

C     !DESCRIPTION: \bv
C     reads uncertainty field and compute weight as squared inverse
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     localerr_file: filename
C     localweight:      array
C     iRec:            record number
      CHARACTER*(*) localerr_file
      _RL     localweight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      INTEGER iRec
      INTEGER myThid
      INTEGER nzIn
      INTEGER nLev
      LOGICAL dosumsq

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j,k
CEOP

      CALL READ_REC_LEV_RL( localerr_file, cost_iprec, nzIn, 1, nLev,
     &                      localweight, iRec, 1, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k = 1,nLev
         DO j = 1,sNy
          DO i = 1,sNx
c--        Test for missing values.
           IF (localweight(i,j,k,bi,bj) .LT. -9900.) THEN
             localweight(i,j,k,bi,bj) = 0. _d 0
c--        Convert to weight
           ELSEIF ( localweight(i,j,k,bi,bj).NE.0. .AND. dosumsq ) THEN
              localweight(i,j,k,bi,bj) =
     &          oneRL/localweight(i,j,k,bi,bj)/localweight(i,j,k,bi,bj)
           ELSEIF ( localweight(i,j,k,bi,bj).ne.0. ) THEN
              localweight(i,j,k,bi,bj) =
     &           oneRL/localweight(i,j,k,bi,bj)
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
      SUBROUTINE ECCO_ERROR( srname, errmsg, myThid )
C     !DESCRIPTION: \bv
C     prints error messages and stops
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     srname :: name of caller
C     errmsg :: error message to be printed
      CHARACTER*(*) srname, errmsg
      INTEGER myThid

#ifdef ALLOW_ECCO

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A,A,A)') srname, ': ', errmsg
      CALL PRINT_ERROR( msgBuf, myThid )
      CALL ALL_PROC_DIE( myThid )
      STOP 'ABNORMAL END: S/R ECCO_ERROR'

#endif /* ALLOW_ECCO */

      RETURN
      END
