C $Header: /u/gcmpack/MITgcm/verification/internal_wave/code/Attic/set_obcs.F,v 1.1 2000/06/29 19:29:04 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE SET_OBCS( K, bi, bj, myCurrentTime, myThid )
C     /==========================================================\
C     | SUBROUTINE SET_OBCS                                      |
C     | o Set boundary conditions at open boundaries             |
C     |==========================================================|
C     |                                                          |
C     | Specific OBCs for internal wave problem.                 |
C     |                                         slegg@whoi.edu   |
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER K, bi, bj
      _RL myCurrentTime
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
C     xG, yG - Global coordinate location.
C     zG
C     zUpper - Work arrays for upper and lower
C     zLower   cell-face heights.
C     phi    - Temporary scalar
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     zUpper - Temporary arrays holding z coordinates of
C     zLower   upper and lower faces.
C     I,i,K
      INTEGER iG, jG
      INTEGER  I,  J
      _RL obTimeScale,Uinflow,rampTime2
      _RL vertStructWst(Nr)
      _RL mz,strat,kx
      _RL tmpsum
      _RL CVEL
      _RL ab05, ab15
 
C Vertical mode number
      mz=1.0
C Stratification
      strat = 1.0 _d -6 / (gravity*tAlpha)
 
C Create a vertical structure function with zero mean
      tmpsum=0.
      do J=1,Nr
       vertStructWst(J)=cos(mz*PI* (rC(J)/rF(Nr+1)) )
       tmpsum=tmpsum+vertStructWst(J)*drF(J)
      enddo
      tmpsum=tmpsum/rF(Nr+1)
      do J=1,Nr
       vertStructWst(J)=vertStructWst(J)-tmpsum
      enddo
c
      obTimeScale = 44567.0
       kx=mz*2.*pi/400.0*sqrt((2.0*pi*2.0*pi/(obTimeScale*obTimeScale)
     & - f0*f0)/(1.0 _d -6
     & - 2.0*pi*2.0*pi/(obTimeScale*obTimeScale)))
      Uinflow = 0.024
      rampTime2 = 4*44567.0

C     Eastern boundary
      DO J=1-Oly,sNy+Oly
       IF (OB_Ie(J,bi,bj).NE.0) THEN
          OBEu(J,K,bi,bj)=0.
          OBEv(J,K,bi,bj)=0.
          OBEt(J,K,bi,bj)=tRef(K)
#ifdef ALLOW_NONHYDROSTATIC
          OBEw(J,K,bi,bj)=0.
#endif
       ENDIF
      ENDDO


C     Western boundary
      DO J=1-Oly,sNy+Oly
       IF (OB_Iw(J,bi,bj).NE.0) THEN
          OBWu(J,K,bi,bj)=0.
     &       +Uinflow
     &       *vertStructWst(K)
     &       *sin(2.*PI*myCurrentTime/obTimeScale)
     &       *(exp(myCurrentTime/rampTime2)
     &   - exp(-myCurrentTime/rampTime2))
     &   /(exp(myCurrentTime/rampTime2)
     &  + exp(-myCurrentTime/rampTime2))
     &   *cos(kx*(3-2-0.5)*delX(1))
          OBWv(J,K,bi,bj)=0.
     &       +Uinflow
     &       *f0/(2.0*PI/obTimeScale)
     &       *vertStructWst(K)
     &       *cos(2.*PI*myCurrentTime/obTimeScale )
     & * (exp(myCurrentTime/rampTime2)
     &   - exp(-myCurrentTime/rampTime2))
     &   /(exp(myCurrentTime/rampTime2)
     &  + exp(-myCurrentTime/rampTime2))
          OBWt(J,K,bi,bj)=tRef(K)
     & + Uinflow*sin(mz*PI*(float(k)-0.5)/float(Nr))
     & * sin(2.0*PI*myCurrentTime/obTimeScale)
     & *sqrt(strat/(tAlpha*gravity))
     & *sqrt(2.0*PI/obTimeScale*2.0*PI/obTimeScale - f0*f0)
     & /(2.0*PI/obTimeScale)
     & * (exp(myCurrentTime/rampTime2)
     &   - exp(-myCurrentTime/rampTime2))
     &   /(exp(myCurrentTime/rampTime2)
     &  + exp(-myCurrentTime/rampTime2))
#ifdef ALLOW_NONHYDROSTATIC
          OBWw(J,K,bi,bj)=-Uinflow
     & *sqrt(2.0*PI/obTimeScale*2.0*PI/obTimeScale - f0*f0)
     & /sqrt(strat*strat - 2.0*PI/obTimeScale*2.0*PI/obTimeScale)
     & *sin(mz*PI*(float(k)-0.5)/float(Nr))
     &       *cos(2.*PI*myCurrentTime/obTimeScale)
     &       *(exp(myCurrentTime/rampTime2)
     &   - exp(-myCurrentTime/rampTime2))
     &   /(exp(myCurrentTime/rampTime2)
     &  + exp(-myCurrentTime/rampTime2))

#endif
       ENDIF
      ENDDO

C     Northern boundary
      DO I=1-Olx,sNx+Olx
       IF (OB_Jn(I,bi,bj).NE.0) THEN
         OBNu(I,K,bi,bj)=0.
         OBNv(I,K,bi,bj)=0.
         OBNt(I,K,bi,bj)=tRef(K)
#ifdef ALLOW_NONHYDROSTATIC
          OBNw(I,K,bi,bj)=0.
#endif
       ENDIF
      ENDDO

C     Southern boundary
      DO I=1-Olx,sNx+Olx
       IF (OB_Js(I,bi,bj).NE.0) THEN
         OBSu(I,K,bi,bj)=0.
         OBSv(I,K,bi,bj)=0.
         OBSt(I,K,bi,bj)=tRef(K)
#ifdef ALLOW_NONHYDROSTATIC
          OBSw(I,K,bi,bj)=0.
#endif
       ENDIF
      ENDDO

#endif
      RETURN
      END
