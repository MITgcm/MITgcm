C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_vdifsc.F,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

cch      SUBROUTINE VDIFSC (UA,VA,SE,RH,QA,QSAT,
      SUBROUTINE VDIFSC (UA,VA,Ta,RH,QA,QSAT,
     &                   UTENVD,VTENVD,TTENVD,QTENVD)
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


      IMPLICIT rEAL*8 (A-H,O-Z)


C     Resolution parameters

#include "atparam.h"
#include "atparam1.h"
#include "Lev_def.h"
C
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
C
C     Physical constants + functions of sigma and latitude
C
#include "com_physcon.h"
C
C     Vertical diffusion constants
C
#include "com_vdicon.h"
C
      REAL UA(NGP,NLEV), VA(NGP,NLEV), SE(NGP,NLEV),
     &     RH(NGP,NLEV), QA(NGP,NLEV), QSAT(NGP,NLEV)
C
      REAL UTENVD(NGP,NLEV), VTENVD(NGP,NLEV),
     &     TTENVD(NGP,NLEV), QTENVD(NGP,NLEV)
C
      INTEGER NL1(NGP)
      REAL RTST(NGP)
      REAL RNL1(NGP)
C
      REAL Th(NGP,NLEV), Ta(NGP,NLEV)
      REAL dThdp
      REAL stab(NGP)
      REAL AUX(NGP)
      REAL Prefw(NLEV), Prefs(NLEV)
      DATA Prefs / 75., 250., 500., 775., 950./
      DATA Prefw / 0., 150., 350., 650., 900./
      REAL Pground
      DATA pground /1000./
Cchdbg
      REAL xindconv1
      SAVE xindconv1
      REAL xindconv
      SAVE xindconv
      INTEGER npas
      SAVE npas
      LOGICAL ifirst
      DATA ifirst /.TRUE./       
      SAVE ifirst
C
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
      if(ifirst) then
        xindconv=0.
        xindconv1=0.
        npas=0
        ifirst=.FALSE.
      endif
      npas = npas +1
Cchdbg
C ******************************************
C *****************************************
C
C--   2. Vertical diffusion and shallow convection
C
      DO J=1,NGP
        NL1(J)=NLEVxy(J)-1
      ENDDO
C
      RTVD = -1./(3600.*TRVDI)
      RTSQ = -1./(3600.*TRSHC)
C
      DO J=1,NGP
       IF ( NLEVxy(J) .GT. 0 ) THEN
        RTST(J) = RTSQ*DSIG(NL1(J))/((DSIG(NLEVxy(J))+DSIG(NL1(J)))*CP)
        RNL1(J) = -DSIG(NLEVxy(J))/DSIG(NL1(J))
       ENDIF
      ENDDO
C
C
C New writing of the Conditional stability
C ----------------------------------------
      DO J=1,NGP
       IF ( NLEVxy(J) .GT. 0 ) THEN
        DO k=NL1(J),NLEVxy(J)
         Th(J,K)=Ta(J,K)*(Pground/Prefs(k))**(RD/CP)
        ENDDO
       ENDIF
      ENDDO
C
      DO J=1,NGP
       IF ( NLEVxy(J) .GT. 0 ) THEN
        dThdp=(Th(J,NL1(J))-Th(J,NLEVxy(J)))
     &              *((Prefw(NLEVxy(J))/Pground)**(RD/CP))*CP
        stab(J)=dThdp+ALHC*(QSAT(J,NL1(J))-QSAT(J,NLEVxy(J)))
       ENDIF
      ENDDO
 121  continue
C
      DO J=1,NGP
C
cch        DMSE = (SE(J,NLEVxy(J))-SE(J,NL1(J)))+
cch     &                ALHC*(QA(J,NLEVxy(J))-QSAT(J,NL1(J)))
       DMSE = - stab(J)
       IF ( NLEVxy(J) .GT. 0 ) THEN
        QEQL = MIN(QA(J,NLEVxy(J)),RH(J,NL1(J))*QSAT(J,NLEVxy(J)))
cchdbg        QEQL = MIN(QA(J,NLEVxy(J)),QA(J,NL1(J)))
       ENDIF
C
        IF (DMSE.GE.0.0) THEN
C
C ***************************************************
C ***************************************************
C chdbg
          if(J.ge.6336 .and. J.eq.6348) then
             xindconv=xindconv+1./13.
          endif
          if(J.ge.4160 .and. J.eq.4172) then
             xindconv1=xindconv1+1./13.
          endif
          if(npas.eq.960 .and. J.eq.1) then
            write(0,*) 'xindconv=',xindconv
            write(0,*) 'xindconv1=',xindconv1
          endif
Cchdbg
C ****************************************************
C ****************************************************
C
C         2.1 Shallow convection
C
          IF ( NLEVxy(J) .GT. 0 ) THEN
           TTENVD(J,NLEVxy(J)) = RTST(J)*DMSE 
           TTENVD(J,NL1(J))  = RNL1(J)*TTENVD(J,NLEVxy(J))
           QTENVD(J,NLEVxy(J)) = RTSQ*(QA(J,NLEVxy(J))-QEQL)
           QTENVD(J,NL1(J))  = RNL1(J)*QTENVD(J,NLEVxy(J))
          ENDIF
C
        ELSE
C
C         2.2 Vertical diffusion of moisture

          QTENVD(J,NLEVxy(J)) = RTVD*(QA(J,NLEVxy(J))-QEQL)
          QTENVD(J,NL1(J))  = RNL1(J)*QTENVD(J,NLEVxy(J))
C
        ENDIF
C
      ENDDO
C
      RETURN
      END
