#include "CTRL_OPTIONS.h"
C- note: although we just need CPP_EEOPTIONS.h here, prefer to
C        include the same option-file above as in the AD version.

C--  File ctrl_bound.F:
C--   Contents
C--   o CTRL_BOUND_3D
C--   o CTRL_BOUND_2D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_BOUND_3D
C     !INTERFACE:
      SUBROUTINE CTRL_BOUND_3D(
     U                fieldCur,
     I                maskFld3d, boundsVec, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | started: Gael Forget gforget@mit.edu 20-Aug-2007
C     |
C     | o in forward mode: impose bounds on ctrl vector values
C     | o in adjoint mode: do nothing ... or emulate local minimum
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL fieldCur (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS maskFld3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL boundsVec(5)
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
CEOP

      IF (boundsVec(1).LT.boundsVec(4)) THEN

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)

         DO k = 1,Nr
          DO j = 1,sNy
           DO i = 1,sNx
             IF (maskFld3d(i,j,k,bi,bj).NE.0.) THEN
              IF (fieldCur(i,j,k,bi,bj).GT.boundsVec(4)) THEN
               fieldCur(i,j,k,bi,bj)=boundsVec(4)
              ENDIF
              IF (fieldCur(i,j,k,bi,bj).LT.boundsVec(1)) THEN
               fieldCur(i,j,k,bi,bj)=boundsVec(1)
              ENDIF
             ENDIF
           ENDDO
          ENDDO
         ENDDO

        ENDDO
       ENDDO

      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C
C     !ROUTINE: CTRL_BOUND_2D
C     !INTERFACE:
      SUBROUTINE CTRL_BOUND_2D(
     U                fieldCur,
     I                mask2D, boundsVec, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | started: Gael Forget gforget@mit.edu 20-Aug-2007
C     |
C     | o in forward mode: impose bounds on ctrl vector values
C     | o in adjoint mode: do nothing ... or emulate local minimum
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL fieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RS mask2D  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL boundsVec(5)
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j
CEOP

      IF (boundsVec(1).LT.boundsVec(4)) THEN

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)

          DO j = 1-OLy,sNy+OLy
           DO i = 1-OLx,sNx+OLx
             IF (mask2D(i,j,bi,bj).NE.0.) THEN
              IF (fieldCur(i,j,bi,bj).GT.boundsVec(4)) THEN
               fieldCur(i,j,bi,bj)=boundsVec(4)
              ENDIF
              IF (fieldCur(i,j,bi,bj).LT.boundsVec(1)) THEN
               fieldCur(i,j,bi,bj)=boundsVec(1)
              ENDIF
             ENDIF
           ENDDO
          ENDDO

        ENDDO
       ENDDO

      ENDIF

      RETURN
      END
