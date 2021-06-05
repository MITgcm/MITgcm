#include "CTRL_OPTIONS.h"

C--  File ctrl_bound.F:
C--   Contents
C--   o ADCTRL_BOUND_3D
C--   o ADCTRL_BOUND_2D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADCTRL_BOUND_3D
C     !INTERFACE:
      SUBROUTINE ADCTRL_BOUND_3D(
     U                fieldCur, adjFieldCur,
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
      _RL fieldCur   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL adjFieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS maskFld3d  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL boundsVec(5)
      INTEGER myThid

#ifdef ALLOW_ADCTRLBOUND
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      _RL x0,x0p5,l0p5,x1,x2,x2p5,l2p5,x3
      _RL tmpCur,xCur,adxCur
CEOP

      x0=boundsVec(1)
      x1=boundsVec(2)
        x0p5=(x0+x1)/2.0
        l0p5=(x1-x0)/2.0
      x2=boundsVec(3)
      x3=boundsVec(4)
        x2p5=(x2+x3)/2.0
        l2p5=(x3-x2)/2.0

C  x0<x1<x2<x3  => ctrl_bound and adctrl_bound   act on xx/adxx
C  x0=x3        => ctrl_bound and adctrl_bound   DO nothing
C  otherwise    => error

      IF ( x0.LT.x3 ) THEN
       IF ( (x0.LT.x1).AND.(x1.LT.x2).AND.(x2.LT.x3) ) THEN

        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)

          DO k = 1,Nr
           DO j = 1,sNy
            DO i = 1,sNx
              IF (maskFld3d(i,j,k,bi,bj).NE.0.) THEN
                xCur=fieldCur(i,j,k,bi,bj)
                adxCur=adjFieldCur(i,j,k,bi,bj)
                IF ( (xCur.GT.x2).AND.(adxCur.LT.0.) ) THEN
                  tmpCur=1.0
                  adjFieldCur(i,j,k,bi,bj)=abs(adxCur)*
     &            min((xCur-x2p5)/l2p5,tmpCur)
                ENDIF
                IF ( (xCur.LT.x1).AND.(adxCur.GT.0.) ) THEN
                  tmpCur=-1.0
                  adjFieldCur(i,j,k,bi,bj)=abs(adxCur)*
     &            max((xCur-x0p5)/l0p5,tmpCur)
                ENDIF
              ENDIF
            ENDDO
           ENDDO
          ENDDO

         ENDDO
        ENDDO

       ELSE
          PRINT*,"boundsVec is not self-consistent"
          STOP 'ABNORMAL END: S/R ADCTRL_BOUND_3D'
       ENDIF
      ENDIF

#endif /* ALLOW_ADCTRLBOUND */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADCTRL_BOUND_2D
C     !INTERFACE:
      SUBROUTINE ADCTRL_BOUND_2D(
     U                fieldCur, adjFieldCur,
     I                mask2D, boundsVec, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | started: Gael Forget gforget@mit.edu 20-Aug-2007
C     |
C     | o in forward mode: impose bounds on ctrl vector values
C     | o in adjoint mode: DO nothing ... or emulate local minimum
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL fieldCur   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL adjFieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RS mask2D     (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL boundsVec(5)
      INTEGER myThid

#ifdef ALLOW_ADCTRLBOUND
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j
      _RL x0,x0p5,l0p5,x1,x2,x2p5,l2p5,x3
      _RL tmpCur,xCur,adxCur
CEOP

      x0=boundsVec(1)
      x1=boundsVec(2)
        x0p5=(x0+x1)/2.0
        l0p5=(x1-x0)/2.0
      x2=boundsVec(3)
      x3=boundsVec(4)
        x2p5=(x2+x3)/2.0
        l2p5=(x3-x2)/2.0

C  x0<x1<x2<x3  => ctrl_bound and adctrl_bound   act on xx/adxx
C  x0=x3        => ctrl_bound and adctrl_bound   DO nothing
C  otherwise    => error

      IF ( x0.LT.x3 ) THEN
       IF ( (x0.LT.x1).AND.(x1.LT.x2).AND.(x2.LT.x3) ) THEN

        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)

           DO j = 1,sNy
            DO i = 1,sNx
              IF (mask2D(i,j,bi,bj).NE.0.) THEN
                xCur=fieldCur(i,j,bi,bj)
                adxCur=adjFieldCur(i,j,bi,bj)
                IF ( (xCur.GT.x2).AND.(adxCur.LT.0.) ) THEN
                  tmpCur=1.0
                  adjFieldCur(i,j,bi,bj)=abs(adxCur)*
     &            min((xCur-x2p5)/l2p5,tmpCur)
                ENDIF
                IF ( (xCur.LT.x1).AND.(adxCur.GT.0.) ) THEN
                  tmpCur=-1.0
                  adjFieldCur(i,j,bi,bj)=abs(adxCur)*
     &            max((xCur-x0p5)/l0p5,tmpCur)
                ENDIF
              ENDIF
            ENDDO
           ENDDO

         ENDDO
        ENDDO

       ELSE
          PRINT*,"boundsVec is not self-consistent"
          STOP 'ABNORMAL END: S/R ADCTRL_BOUND_2D'
       ENDIF
      ENDIF

#endif /* ALLOW_ADCTRLBOUND */

      RETURN
      END
