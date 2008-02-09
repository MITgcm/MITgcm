C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_bound.F,v 1.3 2008/02/09 23:28:59 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     !ROUTINE: CTRL_BOUND_3D
C     !INTERFACE:
      SUBROUTINE CTRL_BOUND_3D(
     I             fieldCur,maskFld3d,
     I             boundsVec,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | started: Gael Forget gforget@mit.edu 20-Aug-2007
C     |
C     | o in forward mode: impose bounds on ctrl vector values
C     | o in adjoint mode: do nothing ... or emulate local minimum
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      integer myThid,bi,bj,i,j,k
      integer itlo,ithi,jtlo,jthi
      _RL fieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nsx,nsy)
      _RL maskFld3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nsx,nsy)
      _RL boundsVec(5)

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      if (boundsVec(1).LT.boundsVec(4)) then
 
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = 1,sny
              do i = 1,snx
          IF (maskFld3d(i,j,k,bi,bj).NE.0.) then
            IF (fieldCur(i,j,k,bi,bj).gt.boundsVec(4)) then
             fieldCur(i,j,k,bi,bj)=boundsVec(4)
            ENDIF
            IF (fieldCur(i,j,k,bi,bj).lt.boundsVec(1)) then
             fieldCur(i,j,k,bi,bj)=boundsVec(1)
            ENDIF
          ENDIF
              enddo
            enddo
          enddo
        enddo
      enddo

      endif

      end

C     !ROUTINE: CTRL_BOUND_2D
C     !INTERFACE:
      SUBROUTINE CTRL_BOUND_2D(
     I             fieldCur,maskFld3d,
     I             boundsVec,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | started: Gael Forget gforget@mit.edu 20-Aug-2007
C     |
C     | o in forward mode: impose bounds on ctrl vector values
C     | o in adjoint mode: do nothing ... or emulate local minimum
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"


      integer myThid,bi,bj,i,j,k
      integer itlo,ithi,jtlo,jthi
      _RL fieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nsx,nsy)
      _RL maskFld3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nsx,nsy)
      _RL boundsVec(5)

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      if (boundsVec(1).LT.boundsVec(4)) then
 
      do bj = jtlo,jthi
        do bi = itlo,ithi
            do j = 1-oly,sny+oly
              do i = 1-olx,snx+olx
          IF (maskFld3d(i,j,1,bi,bj).NE.0.) then
            IF (fieldCur(i,j,bi,bj).gt.boundsVec(4)) then
             fieldCur(i,j,bi,bj)=boundsVec(4)
            ENDIF
            IF (fieldCur(i,j,bi,bj).lt.boundsVec(1)) then
             fieldCur(i,j,bi,bj)=boundsVec(1)
            ENDIF
          ENDIF
              enddo
            enddo
        enddo
      enddo

      endif


      end

