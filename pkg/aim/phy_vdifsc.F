C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_vdifsc.F,v 1.6 2002/09/27 20:05:11 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

cch      SUBROUTINE VDIFSC (UA,VA,SE,RH,QA,QSAT,
      SUBROUTINE VDIFSC (UA,VA,Ta,RH,QA,QSAT,
     &                   UTENVD,VTENVD,TTENVD,QTENVD,
     &                   myThid)
C-
C--   SUBROUTINE VDIFSC (UA,VA,SE,RH,QA,QSAT,
C--  &                   UTENVD,VTENVD,TTENVD,QTENVD)
C-
C--   Purpose: Compute tendencies of momentum, energy and moisture
C--            due to vertical diffusion and shallow convection
C--   Input:   UA     = u-wind                           (3-dim)
C--            VA     = v-wind                           (3-dim)
C--            SE     = dry static energy                (3-dim)
C--            RH     = relative humidity [0-1]          (3-dim)
C--            QA     = specific humidity [g/kg]         (3-dim)
C--            QSAT   = saturation sp. humidity [g/kg]   (3-dim)
C--   Output:  UTENVD = u-wind tendency                  (3-dim)
C--            VTENVD = v-wind tendency                  (3-dim)
C--            TTENVD = temperature tendency             (3-dim)
C--            QTENVD = sp. humidity tendency [g/(kg s)] (3-dim)
C-

      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"

#include "AIM_GRID.h"

C     Physical constants + functions of sigma and latitude
C
#include "com_physcon.h"
C
C     Vertical diffusion constants
C
#include "com_vdicon.h"

C-- Routine arguments:
      INTEGER  myThid
c     REAL UA(NGP,NLEV), VA(NGP,NLEV), SE(NGP,NLEV),
      _RL UA(NGP,NLEV), VA(NGP,NLEV), Ta(NGP,NLEV),
     &     RH(NGP,NLEV), QA(NGP,NLEV), QSAT(NGP,NLEV)
C
      _RL UTENVD(NGP,NLEV), VTENVD(NGP,NLEV),
     &     TTENVD(NGP,NLEV), QTENVD(NGP,NLEV)

#ifdef ALLOW_AIM

C-- Local variables: 
      INTEGER NL1(NGP)
      _RL RTST(NGP)
      _RL RNL1(NGP)
C
      _RL Th(NGP,NLEV)
      _RL dThdp
      _RL stab(NGP)
c     REAL AUX(NGP)
      _RL Prefw(NLEV), Prefs(NLEV)
      DATA Prefs / 75., 250., 500., 775., 950./
      DATA Prefw / 0., 150., 350., 650., 900./
      _RL Pground
      DATA pground /1000./
Cchdbg
c     REAL xindconv1
c     SAVE xindconv1
c     REAL xindconv
c     SAVE xindconv
c     INTEGER npas
c     SAVE npas
c     LOGICAL ifirst
c     DATA ifirst /.TRUE./       
c     SAVE ifirst
      INTEGER J,K

C- jmc: declare all local variables: 
      _RL RTVD, RTSQ, DMSE, QEQL 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Initalization
C
      DO K=1,NLEV
        DO J=1,NGP
          UTENVD(J,K) = 0.
          VTENVD(J,K) = 0.
          TTENVD(J,K) = 0.
          QTENVD(J,K) = 0.
        ENDDO
      ENDDO

c
C
C *****************************************
C *****************************************
Cchdbg
C     if(ifirst) then
C       xindconv=0.
C       xindconv1=0.
C       npas=0
C       ifirst=.FALSE.
C     endif
C     npas = npas +1
Cchdbg
C ******************************************
C *****************************************
C
C--   2. Vertical diffusion and shallow convection
C
      DO J=1,NGP
        NL1(J)=NLEVxy(J,myThid)-1
      ENDDO
C
      RTVD = -1./(3600.*TRVDI)
      RTSQ = -1./(3600.*TRSHC)
C
      DO J=1,NGP
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        RTST(J) = RTSQ*DSIG(NL1(J))
     &          /((DSIG(NLEVxy(J,myThid))+DSIG(NL1(J)))*CP)
        RNL1(J) = -DSIG(NLEVxy(J,myThid))/DSIG(NL1(J))
       ENDIF
      ENDDO

C
C
C New writing of the Conditional stability
C ----------------------------------------
      DO J=1,NGP
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        DO k=NL1(J),NLEVxy(J,myThid)
         Th(J,K)=Ta(J,K)*(Pground/Prefs(k))**(RD/CP)
        ENDDO
       ENDIF
      ENDDO
C
      DO J=1,NGP
       stab(J)=0.
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        dThdp=(Th(J,NL1(J))-Th(J,NLEVxy(J,myThid)))
     &              *((Prefw(NLEVxy(J,myThid))/Pground)**(RD/CP))*CP
        stab(J)=dThdp+ALHC*(QSAT(J,NL1(J))-QSAT(J,NLEVxy(J,myThid)))
       ENDIF
      ENDDO
 121  continue
C
      DO J=1,NGP
C
cch        DMSE = (SE(J,NLEVxy(J,myThid))-SE(J,NL1(J)))+
cch     &                ALHC*(QA(J,NLEVxy(J,myThid))-QSAT(J,NL1(J)))
       DMSE = - stab(J)
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        QEQL = MIN( QA(J,NLEVxy(J,myThid)),
     &              RH(J,NL1(J))*QSAT(J,NLEVxy(J,myThid)) )
cchdbg        QEQL = MIN(QA(J,NLEVxy(J,myThid)),QA(J,NL1(J)))
       ENDIF
C
        IF (DMSE.GE.0.0) THEN
C
C ***************************************************
C ***************************************************
C chdbg
C         if(J.ge.6336 .and. J.eq.6348) then
C            xindconv=xindconv+1./13.
C         endif
C         if(J.ge.4160 .and. J.eq.4172) then
C            xindconv1=xindconv1+1./13.
C         endif
C         if(npas.eq.960 .and. J.eq.1) then
C           write(0,*) 'xindconv=',xindconv
C           write(0,*) 'xindconv1=',xindconv1
C         endif
Cchdbg
C ****************************************************
C ****************************************************
C
C         2.1 Shallow convection
C
          IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
           TTENVD(J,NLEVxy(J,myThid)) = RTST(J)*DMSE 
           TTENVD(J,NL1(J))  = RNL1(J)*TTENVD(J,NLEVxy(J,myThid))
           QTENVD(J,NLEVxy(J,myThid)) =
     &                         RTSQ*(QA(J,NLEVxy(J,myThid))-QEQL)
           QTENVD(J,NL1(J))  = RNL1(J)*QTENVD(J,NLEVxy(J,myThid))
          ENDIF
C
        ELSE
C
C         2.2 Vertical diffusion of moisture

          QTENVD(J,NLEVxy(J,myThid)) = 
     &                        RTVD*(QA(J,NLEVxy(J,myThid))-QEQL)
          QTENVD(J,NL1(J))  = RNL1(J)*QTENVD(J,NLEVxy(J,myThid))
C
        ENDIF
C
      ENDDO
C
#endif /* ALLOW_AIM */ 

      RETURN
      END
