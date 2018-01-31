#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CALC( futureTime, futureIter,
     &                      uVel, vVel, wVel, theta, salt,
     &                      myThid )
C     *==========================================================*
C     | SUBROUTINE OBCS_CALC
C     | o Calculate future boundary data at open boundaries
C     |   at time = futureTime
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "EOS.h"

C     == Routine arguments ==
      INTEGER futureIter
      _RL futureTime
      _RL uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER bi, bj
      INTEGER I, J ,K
      _RL rampTime2
      _RL rexpt
      _RL hinit, delh
      _RL z(nr)
      _RL Dmax,Dinf,dTemp,gp_inflow,Lrho
      _RL Width,x,Xcenter,Fz,zt,Rit

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_CALC',myThid)
#endif

c     Total depth
      Dmax = 600. _d 0
c     width of flow transition
      delh=5. _d 0
c     total depth of active layer
      Dinf=300. _d 0
c     g_prime of inflow
      dTemp=(2. _d 0/rhonil)/(2. _d -4)
      gp_inflow=tAlpha*gravity*dTemp
c     Deformation radius
      Lrho=sqrt(gp_inflow*Dinf)/f0
c     dimensional width
      Width=100. _d 3
c     nondimensional width
      Width=Width/Lrho
c      print *,'Width=',Width
c     coordinate of center of embayment
      Xcenter = 1700. _d 3
c     Critical Richardson number
      Rit = 1. _d 0/3. _d 0
c      Ttarget=-5.0e6
c      areaCh=Width*Lrho*Dmax
c      relaxtime=180000.0
c      relaxConst=1.0/(areaCh*relaxtime)


      z(1) = -delR(1)/2. _d 0
      do j=2,nr
Caja NOTE: This gives the depths of W points, not U,V,T,S,p points
C          USe instead drC but requires different indexing. See documentation
C          in AJAs head
      z(j) = z(j-1) - delR(j)
      enddo

c     ramptime for velocity
      rampTime2 = 4. _d 0*44567. _d 0
      rexpt=1. _d 0/exp(futureTime/rampTime2)

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

C     Eastern OB
      IF (useOrlanskiEast) THEN
        CALL ORLANSKI_EAST(
     &          bi, bj, futureTime,
     &          uVel, vVel, wVel, theta, salt,
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            OBEu(J,K,bi,bj)=0.
c     &       +Uinflow
c     & *0.5*(1.0 - (exp((z(k)-hinit)/delh)
c     &   - exp(-(z(k)-hinit)/delh))
c     &   /(exp((z(k)-hinit)/delh)
c     &  + exp(-(z(k)-hinit)/delh)))
c     &*(1.0 - rexpt*rexpt)/(1.0 + rexpt*rexpt)
            OBEv(J,K,bi,bj)=0.
            OBEt(J,K,bi,bj)=tRef(K)
c     &  +Tinflow
c     & *0.5*(1.0 - (exp((z(k)-hinit)/delh)
c     &   - exp(-(z(k)-hinit)/delh))
c     &   /(exp((z(k)-hinit)/delh)
c     &  + exp(-(z(k)-hinit)/delh)))
c     &*(1.0 - rexpt*rexpt)/(1.0 + rexpt*rexpt)
            OBEs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBEw(J,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C     Western OB
      IF (useOrlanskiWest) THEN
        CALL ORLANSKI_WEST(
     &          bi, bj, futureTime,
     &          uVel, vVel, wVel, theta, salt,
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
          OBWu(J,K,bi,bj)=0.
          OBWv(J,K,bi,bj)=0.
          OBWt(J,K,bi,bj)= Tref(k)
#ifdef ALLOW_NONHYDROSTATIC
          OBWw(J,K,bi,bj)=0.0
#endif
          ENDDO
        ENDDO
      ENDIF

C         Northern OB, template for forcing
      IF (useOrlanskiNorth) THEN
        CALL ORLANSKI_NORTH(
     &          bi, bj, futureTime,
     &          uVel, vVel, wVel, theta, salt,
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
c Make center of embayment x=0
           x=(xC(I,1,bi,bj)-Xcenter)/Lrho +Width/2. _d 0
           IF ((x.GE.0.).AND.(x.LE.Width)) THEN
            hinit=Dinf*(exp(-x)) - Dmax
c      if (k.eq.5) then
c      print *,'x=',x,'  xC',xC(I,1,bi,bj),'  I=',I
c      print *,'hinit=',hinit
c      endif
            zt=(z(k) + Dmax - (hinit + Dmax))/(hinit+Dmax)
            IF (zt.GE.(Rit/(2. _d 0 -Rit))) THEN
             Fz=1.
            ELSE
             IF (zt.GE.(-Rit/(2.+Rit))) THEN
              Fz=1. _d 0/Rit*zt/(zt+1. _d 0) + 0.5 _d 0
             ELSE
              Fz=0.
             ENDIF
            ENDIF
           ELSE
            Fz=1.
           ENDIF
c       if ((x.ge.0).and.(x.le.Width)) then
c        print *,'z(k)',z(k),'zt',zt,'Fz',Fz
c       endif
            OBNv(I,K,bi,bj)=0.
     &     -  sqrt(gp_inflow*Dinf)*exp(-x)
     &                            *(1. _d 0 - Fz)
c           if ((x.ge.0).and.(x.le.Width)) then
c           if (k.eq.5) then
c       print *,'V=',OBNv(I,K,bi,bj)
c           endif
c           endif
            OBNu(I,K,bi,bj)=0.
           IF (tRef(K).LE. (-dTemp*(1. _d 0 - Fz))) THEN
            OBNt(I,K,bi,bj) = tRef(K)
           ELSE
            OBNt(I,K,bi,bj) = -dTemp*(1. _d 0 - Fz)
           ENDIF
c           if ((x.ge.0).and.(x.le.Width)) then
c       print *,'T=',OBNt(I,K,bi,bj)
c           endif
c            OBNt(I,K,bi,bj)=tRef(K)
c     & - dTemp
c     & *(1.0 - Fz)
            OBNs(I,K,bi,bj)=sRef(K) + 1. _d 0*(1. _d 0 - Fz)
#ifdef ALLOW_NONHYDROSTATIC
            OBNw(I,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C         Southern OB, template for forcing
      IF (useOrlanskiSouth) THEN
        CALL ORLANSKI_SOUTH(
     &          bi, bj, futureTime,
     &          uVel, vVel, wVel, theta, salt,
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            OBSu(I,K,bi,bj)=0.
            OBSv(I,K,bi,bj)=0.
            OBSt(I,K,bi,bj)=tRef(K)
            OBSs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBSw(I,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C--   end bi,bj loops.
      ENDDO
      ENDDO

#ifdef ALLOW_OBCS_BALANCE
      IF ( useOBCSbalance ) THEN
        CALL OBCS_BALANCE_FLOW( futureTime, futureIter, myThid )
      ENDIF
#endif /* ALLOW_OBCS_BALANCE */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_CALC',myThid)
#endif
#endif /* ALLOW_OBCS */

      RETURN
      END
