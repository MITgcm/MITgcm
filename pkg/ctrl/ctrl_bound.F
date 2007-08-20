
#include "CPP_OPTIONS.h"

C     !ROUTINE: CTRL_BOUND_3D
C     !INTERFACE:
      SUBROUTINE CTRL_BOUND_3D(
     I             bi, bj, fieldCur,
     I             minCur,maxCur,
     I             myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o bound ctrl vector values in forward mode
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL fieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL minCur,maxCur


          do k = 1,nr
            do j = 1-oly,sny+oly
              do i = 1-olx,snx+olx
          IF (fieldCur(i,j,k).gt.maxCur) then
             fieldCur(i,j,k)=maxCur
          ENDIF
          IF (fieldCur(i,j,k).lt.minCur) then
             fieldCur(i,j,k)=minCur
          ENDIF
              enddo
            enddo
          enddo


      end

C     !ROUTINE: CTRL_BOUND_2D
C     !INTERFACE:
      SUBROUTINE CTRL_BOUND_2D(
     I             bi, bj, fieldCur,
     I             minCur,maxCur,
     I             myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o bound ctrl vector values in forward mode
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL fieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL minCur,maxCur


            do j = 1-oly,sny+oly
              do i = 1-olx,snx+olx
          IF (fieldCur(i,j).gt.maxCur) then
             fieldCur(i,j)=maxCur
          ENDIF
          IF (fieldCur(i,j).lt.minCur) then
             fieldCur(i,j)=minCur
          ENDIF
              enddo
            enddo


      end

