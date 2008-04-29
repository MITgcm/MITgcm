C $Header: /u/gcmpack/MITgcm/pkg/ctrl/adctrl_bound.F,v 1.5 2008/04/29 21:43:22 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     !ROUTINE: ADCTRL_BOUND_3D
C     !INTERFACE:
      SUBROUTINE ADCTRL_BOUND_3D(
     I             fieldCur,maskFld3d,
     I             boundsVec,myThid,
     I             adjFieldCur)
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
      _RL adjFieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nsx,nsy)
      _RL boundsVec(5)
      _RL x0,x0p5,l0p5,x1,x2,x2p5,l2p5,x3
      _RL tmpCur,xCur,adxCur 

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)


#ifdef ALLOW_ADCTRLBOUND

      x0=boundsVec(1)
      x1=boundsVec(2)
        x0p5=(x0+x1)/2.0
        l0p5=(x1-x0)/2.0
      x2=boundsVec(3)
      x3=boundsVec(4)
        x2p5=(x2+x3)/2.0
        l2p5=(x3-x2)/2.0

C  x0<x1<x2<x3 	=> ctrl_bound and adctrl_bound   act on xx/adxx
C  x0=x3 	=> ctrl_bound and adctrl_bound   do nothing
C  otherwise	=> error

      if ( x0.LT.x3 ) then
        if ( (x0.LT.x1).AND.(x1.LT.x2).AND.(x2.LT.x3) ) then

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = 1,sny
              do i = 1,snx
      IF (maskFld3d(i,j,k,bi,bj).NE.0.) then
        xCur=fieldCur(i,j,k,bi,bj)
        adxCur=adjFieldCur(i,j,k,bi,bj)
        IF ( (xCur.gt.x2).AND.(adxCur.LT.0) ) then
          tmpCur=1.0
          adjFieldCur(i,j,k,bi,bj)=abs(adxCur)*
     &    min((xCur-x2p5)/l2p5,tmpCur)
        ENDIF
        IF ( (xCur.lt.x1).AND.(adxCur.GT.0) ) then
          tmpCur=-1.0
          adjFieldCur(i,j,k,bi,bj)=abs(adxCur)*
     &    max((xCur-x0p5)/l0p5,tmpCur)
        ENDIF
      ENDIF
              enddo
            enddo
          enddo
        enddo
      enddo

        else
          print*,"boundsVec is not self-consistent"
          stop
        endif
      endif

#endif

      end

C     !ROUTINE: ADCTRL_BOUND_2D
C     !INTERFACE:
      SUBROUTINE ADCTRL_BOUND_2D(
     I             fieldCur,maskFld3d,
     I             boundsVec,myThid,
     I             adjFieldCur)
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
      _RL adjFieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nsx,nsy)
      _RL boundsVec(5)
      _RL x0,x0p5,l0p5,x1,x2,x2p5,l2p5,x3
      _RL tmpCur,xCur,adxCur

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)


#ifdef ALLOW_ADCTRLBOUND

      x0=boundsVec(1)
      x1=boundsVec(2)
        x0p5=(x0+x1)/2.0
        l0p5=(x1-x0)/2.0
      x2=boundsVec(3)
      x3=boundsVec(4)
        x2p5=(x2+x3)/2.0
        l2p5=(x3-x2)/2.0

C  x0<x1<x2<x3 	=> ctrl_bound and adctrl_bound   act on xx/adxx
C  x0=x3 	=> ctrl_bound and adctrl_bound   do nothing
C  otherwise	=> error

      if ( x0.LT.x3 ) then
        if ( (x0.LT.x1).AND.(x1.LT.x2).AND.(x2.LT.x3) ) then

      do bj = jtlo,jthi
        do bi = itlo,ithi
            do j = 1,sny
              do i = 1,snx
      IF (maskFld3d(i,j,1,bi,bj).NE.0.) then
        xCur=fieldCur(i,j,bi,bj)
        adxCur=adjFieldCur(i,j,bi,bj)
        IF ( (xCur.gt.x2).AND.(adxCur.LT.0) ) then
          tmpCur=1.0
          adjFieldCur(i,j,bi,bj)=abs(adxCur)*
     &    min((xCur-x2p5)/l2p5,tmpCur)
        ENDIF
        IF ( (xCur.lt.x1).AND.(adxCur.GT.0) ) then
          tmpCur=-1.0
          adjFieldCur(i,j,bi,bj)=abs(adxCur)*
     &    max((xCur-x0p5)/l0p5,tmpCur)
        ENDIF
      ENDIF
              enddo
            enddo
        enddo
      enddo

        else
          print*,"boundsVec is not self-consistent"
          stop
        endif
      endif

#endif

      end

