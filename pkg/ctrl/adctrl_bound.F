C $Header: /u/gcmpack/MITgcm/pkg/ctrl/adctrl_bound.F,v 1.3 2008/01/23 17:32:16 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     !ROUTINE: ADCTRL_BOUND_3D
C     !INTERFACE:
      SUBROUTINE ADCTRL_BOUND_3D(
     I             bi, bj,
     I             minCur,maxCur,
     I             myThid,fieldCur)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o bound ctrl vector values in forward mode
C     | ... nothing happens in adjoint mode
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL fieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL minCur,maxCur

      end

C     !ROUTINE: ADCTRL_BOUND_2D
C     !INTERFACE:
      SUBROUTINE ADCTRL_BOUND_2D(
     I             bi, bj,
     I             minCur,maxCur,
     I             myThid,fieldCur)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o bound ctrl vector values in forward mode
C     | ... nothing happens in adjoint mode
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL fieldCur(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL minCur,maxCur

      end

