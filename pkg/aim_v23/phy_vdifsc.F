C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_vdifsc.F,v 1.1 2002/11/22 17:17:03 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE VDIFSC (dpFac,SE,RH,QA,QSAT,
     O                   TTENVD,QTENVD,
     I                   kGrd,bi,bj,myThid)
C--
C--   SUBROUTINE VDIFSC (UA,VA,SE,RH,QA,QSAT,PHI,
C--  &                   UTENVD,VTENVD,TTENVD,QTENVD)
C--
C--   Purpose: Compute tendencies of momentum, energy and moisture
C--            due to vertical diffusion and shallow convection
C--   Input:   UA     = u-wind                           (3-dim)
C--            VA     = v-wind                           (3-dim)
C              dpFac  = cell delta_P fraction            (3-dim)
C--            SE     = dry static energy                (3-dim)
C--            RH     = relative humidity [0-1]          (3-dim)
C--            QA     = specific humidity [g/kg]         (3-dim)
C--            QSAT   = saturation sp. humidity [g/kg]   (3-dim)
C--            PHI    = geopotential                     (3-dim)
C--   Output:  UTENVD = u-wind tendency                  (3-dim)
C--            VTENVD = v-wind tendency                  (3-dim)
C--            TTENVD = temperature tendency             (3-dim)
C--            QTENVD = sp. humidity tendency [g/(kg s)] (3-dim)
C    Input:    kGrd   = Ground level index               (2-dim)
C              bi,bj  = tile index
C           myThid    = Thread number for this instance of the routine
C-------
C  Note: a) dry static energy (SE,input) has been replaced by Pot.Temp.
C        b) UA,VA & U,V_TENVD not used => removed 
C-------
C In contrast to other Physics S/R, VDIFSC return a real tendency (dQ/dt,dT/dt)
C  nevertheless /dpFac is not applied here but later in AIM_AIM2DYN
C-------

      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"

C     Physical constants + functions of sigma and latitude
#include "com_physcon.h"

C     Vertical diffusion constants
#include "com_vdicon.h"

C-- Routine arguments:
c     _RL  UA(NGP,NLEV), VA(NGP,NLEV)
      _RL  dpFac(NGP,NLEV)
      _RL  SE(NGP,NLEV), RH(NGP,NLEV), QA(NGP,NLEV), QSAT(NGP,NLEV)
c     _RL  PHI(NGP,NLEV)

c     _RL  UTENVD(NGP,NLEV), VTENVD(NGP,NLEV)
      _RL  TTENVD(NGP,NLEV), QTENVD(NGP,NLEV)

      INTEGER  kGrd(NGP)
      INTEGER  bi,bj,myThid

#ifdef ALLOW_AIM

C-- Local variables:
      INTEGER J, K, Ktmp, NL1
      _RL  RSIG(NLEV)
      _RL  dSEdp(NGP,NLEV-1), DeltaPI(NLEV-1), factP

C- jmc: declare all local variables:
      _RL  CVDI(NGP), FSHCQ
      _RL  DRH0, DRH, DMSE, FLUXSE, FLUXQ
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Initalization

C     N.B. In this routine, fluxes of dry static energy and humidity
C          are scaled in such a way that:
C          d_T/dt = d_F'(SE)/d_sigma,  d_Q/dt = d_F'(Q)/d_sigma

c_FM  NL1  = NLEV-1
c_FM  CSHC = DSIG(NLEV)/3600.
c_FM  CVDI = (SIGH(NL1)-SIGH(1))/((NL1-1)*3600.)

c_FM  FSHCQ  = CSHC/TRSHC
c_FM  FSHCSE = CSHC/(TRSHC*CP)

c_FM  FVDIQ  = CVDI/TRVDI
c_FM  FVDISE = CVDI/(TRVDS*CP)

      DO J=1,NGP
        NL1 = kGrd(J)-1
        CVDI(J) = 0.
        IF (NL1.GE.2) THEN
          CVDI(J) = (SIGH(NL1)-SIGH(1))/((NL1-1)*3600. _d 0)
        ENDIF
      ENDDO

      DO K=1,NLEV
        RSIG(K)=1./DSIG(K)
      ENDDO

      DO K=1,NLEV
        DO J=1,NGP
c         UTENVD(J,K) = 0.
c         VTENVD(J,K) = 0.
          TTENVD(J,K) = 0.
          QTENVD(J,K) = 0.
        ENDDO
      ENDDO

C ---------------------------------------------
C    Write Conditional stability based on Pot.Temp :
C    dSEdp(K) = Delta[Static-Energy] between 2 Plevels(k,k+1)
C    and corresponds to SE(K+1)-SE(K) in the original code
C -------
      DO K=1,NLEV-1
       factP = CP*SIGH(K)**(RD/CP)
       DO J=1,NGP
         dSEdp(J,K)=(SE(J,K+1)-SE(J,K))*factP
       ENDDO
       DeltaPI(K) = SIG(K+1)**(RD/CP) - SIG(K)**(RD/CP)
      ENDDO

C--   2. Shallow convection

      DO J=1,NGP
        Ktmp = kGrd(J)
        NL1 = Ktmp - 1
       IF (Ktmp.GE.2) THEN

        DRH0=RHGRAD*(SIG(Ktmp)-SIG(NL1))
        FSHCQ = DSIG(Ktmp)*dpFac(J,Ktmp)/(TRSHC*3600. _d 0)

c_FM    DMSE = (SE(J,NLEV)-SE(J,NL1))+ALHC*(QA(J,NLEV)-QSAT(J,NL1))
        DMSE = dSEdp(J,NL1)         + ALHC*(QA(J,Ktmp)-QSAT(J,NL1))
        DRH  = RH(J,Ktmp)-RH(J,NL1)

        IF (DMSE.GE.0.0) THEN

c_FM      FLUXSE         = FSHCSE*DMSE
          FLUXSE         = FSHCQ *DMSE/CP
          TTENVD(J,NL1)  = FLUXSE*RSIG(NL1)
          TTENVD(J,Ktmp) =-FLUXSE*RSIG(Ktmp)

          IF (DRH.GE.0.0) THEN
            FLUXQ          = FSHCQ*QSAT(J,Ktmp)*DRH
            QTENVD(J,NL1)  = FLUXQ*RSIG(NL1)
            QTENVD(J,Ktmp) =-FLUXQ*RSIG(Ktmp)
          ENDIF

        ELSE IF (DRH.GE.DRH0) THEN

c_FM      FLUXQ          = FVDIQ*QSAT(J,NL1)*DRH
          FLUXQ          = QSAT(J,NL1)*DRH*CVDI(J)/TRVDI
          QTENVD(J,NL1)  = FLUXQ*RSIG(NL1)
          QTENVD(J,Ktmp) =-FLUXQ*RSIG(Ktmp)

        ENDIF

       ENDIF
      ENDDO

C--   3. Vertical diffusion of moisture above the PBL

      DO J=1,NGP

        DO K=3,kGrd(J)-2

          DRH0=RHGRAD*(SIG(K+1)-SIG(K))

          DRH=RH(J,K+1)-RH(J,K)

          IF (DRH.GE.DRH0) THEN
c_FM        FLUXQ        = FVDIQ*QSAT(J,K)*DRH
            FLUXQ        = QSAT(J,K)*DRH*CVDI(J)/TRVDI
            QTENVD(J,K)  = QTENVD(J,K)  +FLUXQ*RSIG(K)
            QTENVD(J,K+1)= QTENVD(J,K+1)-FLUXQ*RSIG(K+1)
          ENDIF

        ENDDO

      ENDDO

C--   4. Damping of super-adiabatic lapse rate

      DO J=1,NGP
       DO K=1,kGrd(J)-1

c_FM     SE0 = SE(J,K+1)+SEGRAD*(PHI(J,K)-PHI(J,K+1))
         DMSE = dSEdp(J,K)
     &        +SEGRAD*CP*DeltaPI(K)*(SE(J,K+1)+SE(J,K))*0.5 _d 0

c_FM     IF (SE(J,K).LT.SE0) THEN
c_FM       FLUXSE        = FVDISE*(SE0-SE(J,K))
         IF (DMSE.GT.0.) THEN
           FLUXSE        = DMSE*CVDI(J)/(TRVDS*CP)
           TTENVD(J,K  ) = TTENVD(J,K  )+FLUXSE*RSIG(K)
           TTENVD(J,K+1) = TTENVD(J,K+1)-FLUXSE*RSIG(K+1)
         ENDIF

       ENDDO
      ENDDO
C--
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */

      RETURN
      END
