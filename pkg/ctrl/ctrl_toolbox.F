#include "CTRL_OPTIONS.h"

C--  File ctrl_toolbox.F: Routines to handle basic operations common in pkg/ctrl
C--  Note: basically same as ecco_toolbox routines, but copied here for package
C--  independence (pkg/ecco doesn't work with openad)
C--   Contents
C--   o CTRL_ASSIGN
C--   o CTRL_CPRLRL
C--   o CTRL_CPRSRL
C--   o CTRL_CPRSRS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_ASSIGN
C     !INTERFACE:
      SUBROUTINE CTRL_ASSIGN( fld, nnzloc, fill_value, myThid )
C     !DESCRIPTION: \bv
C     fill a field with fill_value
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nnzloc
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzloc,nSx,nSy)
      _RL fill_value
      INTEGER myThid

#ifdef ALLOW_CTRL
C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1,nnzloc
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            fld(i,j,k,bi,bj) = fill_value
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_CTRL */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_CPRLRL
C     !INTERFACE:
      SUBROUTINE CTRL_CPRLRL(
     I                   fldIn, nzIn, fldOut, nzOut,
     I                   myThid )

C     !DESCRIPTION: \bv
C     copy a field (RL) to another array (RL)
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nzOut
      _RL fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_CTRL
C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1,nzOut
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            fldOut(i,j,k,bi,bj) = fldIn(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_CTRL */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_CPRSRL
C     !INTERFACE:
      SUBROUTINE CTRL_CPRSRL(
     I                   fldIn, nzIn, fldOut, nzOut,
     I                   myThid )

C     !DESCRIPTION: \bv
C     copy a field to another array, switching from _RS to _RL
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nzOut
      _RS fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RL fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_CTRL
C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1,nzOut
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            fldOut(i,j,k,bi,bj) = fldIn(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_CTRL */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_CPRSRS
C     !INTERFACE:
      SUBROUTINE CTRL_CPRSRS(
     I                   fldIn, nzIn, fldOut, nzOut,
     I                   myThid )

C     !DESCRIPTION: \bv
C     copy a field (RS) to another array (RS)
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER nzIn, nzOut
      _RS fldIn (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzIn,nSx,nSy)
      _RS fldOut(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nzOut,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_CTRL
C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1,nzOut
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            fldOut(i,j,k,bi,bj) = fldIn(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_CTRL */

      RETURN
      END
