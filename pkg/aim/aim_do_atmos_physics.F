C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_do_atmos_physics.F,v 1.11 2002/10/09 01:01:24 jmc Exp $
C     $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE AIM_DO_ATMOS_PHYSICS( bi, bj, currentTime, myThid ) 
                                      
C     /==================================================================\
C     | S/R AIM_DO_ATMOS_PHYSICS                                         |
C     |==================================================================|
C     | Interface interface between atmospheric physics package and the  |
C     | dynamical model.                                                 |
C     | Routine calls physics pacakge after mapping model variables to   |
C     | the package grid. Package should derive and set tendency terms   |
C     | which can be included as external forcing terms in the dynamical |
C     | tendency routines. Packages should communicate this information  |
C     | through common blocks.                                           |
C     \==================================================================/
      IMPLICIT NONE

C     -------------- Global variables ------------------------------------
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

C-- MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"

C-- Physics package
#include "AIM_FFIELDS.h"
#include "AIM_GRID.h"
#include "com_physvar.h"
#include "com_forcing1.h"

C     -------------- Routine arguments -----------------------------------
c     _RL phi_hyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL currentTime
      INTEGER myThid
      INTEGER bi, bj

#ifdef ALLOW_AIM
C     -------------- Local variables -------------------------------------
C     I,J,K,I2      - Loop counters
C     tYear         - Fraction into year
C     hInital       - Initial height of pressure surfaces (m)
C     pSurfs        - Pressure surfaces (Pa)
C     Katm          - Atmospheric K index
      INTEGER I
      INTEGER I2
      INTEGER J
      INTEGER K
      _RL     tYear
      _RL     hInitial(Nr)
      _RL     hInitialW(Nr)
      DATA    hInitial / 17338.0,10090.02,5296.88,2038.54,418.038/
      SAVE    hInitial
      DATA    hInitialW / 15090.4, 8050.96, 4087.75, 1657.54, 0. /
      _RL     pSurfs(Nr)
      DATA    pSurfs   / 75.D2, 250.D2, 500.D2, 775.D2, 950.D2 /
      SAVE    pSurfs
      _RL     pSurfw(Nr)
      DATA    pSurfw / 150.D2, 350.D2, 650.D2, 900.D2, 1000.D2 /
      SAVE    pSurfw
      _RL     RD
      _RL     CPAIR
      _RL     pGround
      _RL     RhoG1(sNx*sNy,Nr)
c     _RL  phiTotal(sNx,sNy,Nr)
c     _RL  phiTCount
c     _RL  phiTSum
c     _RL  ans
c     _RL  pvoltotNiv5
c     SAVE pvoltotNiv5
c     _RL  ptotalNiv5
      INTEGER Katm

      pGround = 1.D5
      CPAIR   = 1004
      RD      =  287

      CALL AIM_DYN2AIM( bi, bj, currentTime, myThid )

C     Physics package works with sub-domains 1:sNx,1:sNy,1:Nr.
C     Internal index mapping is linear in X and Y with a second
C     dimension for the vertical.

C     Adjustment for heave due to mean heating/cooling
C     ( I don't think the old formula was strictly "correct" for orography
C       but I have implemented it as was for now. As implemented
C       the mean heave of the bottom (K=Nr) level is calculated rather than
C       the mean heave of the base of the atmosphere. )
c     phiTCount = 0.
c     phiTSum   = 0.
c     DO K=1,Nr
c     DO J=1,sNy
c      DO I=1,sNx
c       phiTotal(I,J,K)  = etaN(i,j,bi,bj)
c       phiTCount        = phiTCount + hFacC(i,j,Nr,bi,bj)
c      ENDDO
c     ENDDO
c     ENDDO
c     DO K=1,Nr
c      DO J=1,sNy
c       DO I=1,sNx
c        phiTotal(I,J,K) = phiTotal(I,J,K) +
c    &   recip_rhoConst*(phi_hyd(i,j,k))
c       ENDDO
c      ENDDO
c     ENDDO
c     DO J=1,sNy
c      DO I=1,sNx
c       phiTSum = phiTSum + phiTotal(I,J,Nr)
c      ENDDO
c     ENDDO
c     ans = phiTCount
C     _GLOBAL_SUM_R8( phiTCount, myThid )
c     phiTcount = ans
c     ans = phiTSum
C     _GLOBAL_SUM_R8( phiTSum, myThid )
c     phiTSum = ans
C     ptotalniv5=phiTSum/phiTCount
c     ptotalniv5=0.

#ifndef OLD_AIM_INTERFACE
C_jmc: Because AIM physics LSC is not applied in the stratosphere (top level),
C      ==> move water wapor from the stratos to the surface level.
      DO J = 1-Oly, sNy+Oly
       DO I = 1-Olx, sNx+Olx
        k = ksurfC(i,j,bi,bj)
        IF (k.LE.Nr)
     &    salt(I,J,k,bi,bj) = salt(I,J,k,bi,bj)
     &                      + salt(I,J,Nr,bi,bj)*drF(Nr)*recip_drF(k)
        salt(I,J,Nr,bi,bj) = 0.
       ENDDO
      ENDDO
#endif /* OLD_AIM_INTERFACE */

C     Note the mapping here is only valid for one tile per proc.
      DO K = 1, Nr
       DO J = 1, sNy
        DO I = 1, sNx
         I2 = (sNx)*(J-1)+I
         Katm = _KD2KA( K )
C - to reproduce old results (coupled run, summer 2000) :
         UG1(I2,Katm,myThid)   = uVel(I,J,K,bi,bj)
         VG1(I2,Katm,myThid)   = vVel(I,J,K,bi,bj)
C        Physics works with temperature - not potential temp.
         TG1(I2,Katm,myThid)   = theta(I,J,K,bi,bj)
     &                  / ((pGround/pSurfs(Katm))**(RD/CPAIR))
#ifdef OLD_AIM_INTERFACE
         QG1(I2,Katm,myThid)   = salt(I,J,K,bi,bj)
#else
         QG1(I2,Katm,myThid)   = MAX(salt(I,J,K,bi,bj), 0. _d 0)
#endif
         PHIG1(I2,Katm,myThid) = gravity*Hinitial(Katm)
c    &                         +(phiTotal(I,J,K)- ptotalniv5 )
         IF (maskC(i,j,k,bi,bj).EQ.1.) THEN
           RHOG1(I2,Katm) = pSurfs(Katm)/RD/TG1(I2,Katm,myThid)
         ELSE
           RHOG1(I2,Katm)=0.
         ENDIF
        ENDDO
       ENDDO
      ENDDO

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C_jmc: add square of surface wind speed (center of C grid) = 2 * KE_surf
      DO J = 1, sNy
       DO I = 1, sNx
        I2 = I+(J-1)*sNx
#ifdef OLD_AIM_INTERFACE
C - to reproduce old results (coupled run, summer 2000) :
         Vsurfsq(I2,myThid) = 0.
        IF (NLEVxyU(I2,myThid).GT.0) 
     &   Vsurfsq(I2,myThid) = Vsurfsq(I2,myThid)
     &    +UG1(I2,NLEVxyU(I2,myThid),myThid)
     &    *UG1(I2,NLEVxyU(I2,myThid),myThid)
        IF (NLEVxyV(I2,myThid).GT.0) 
     &   Vsurfsq(I2,myThid) = Vsurfsq(I2,myThid)
     &    +VG1(I2,NLEVxyV(I2,myThid),myThid)
     &    *VG1(I2,NLEVxyV(I2,myThid),myThid)
#else /* OLD_AIM_INTERFACE */
        K = ksurfC(i,j,bi,bj)
        IF (K.LE.Nr) THEN
         Vsurfsq(I2,myThid) = 0.5 * (
     &                 uVel(I,J,K,bi,bj)*uVel(I,J,K,bi,bj)
     &               + uVel(I+1,J,K,bi,bj)*uVel(I+1,J,K,bi,bj)
     &               + vVel(I,J,K,bi,bj)*vVel(I,J,K,bi,bj)
     &               + vVel(I,J+1,K,bi,bj)*vVel(I,J+1,K,bi,bj)
     &                        )
        ELSE
         Vsurfsq(I2,myThid) = 0.
        ENDIF
#endif /* OLD_AIM_INTERFACE */
       ENDDO
      ENDDO
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C      
C     Set geopotential surfaces
C     -------------------------
      DO J=1,sNy
       DO I=1,sNx
        I2 = (sNx)*(J-1)+I
        IF ( Nlevxy(I2,myThid) .NE. 0 ) THEN
         PHI0(I2,myThid) = gravity*Hinitialw(Nlevxy(I2,myThid))
        ELSE
         PHI0(I2,myThid) = 0.
        ENDIF
       ENDDO
      ENDDO

C
C     Physics package works with log of surface pressure
C     Get surface pressure from pbot-dpref/dz*Z'
      DO J=1,sNy
       DO I=1,sNx
        I2 = (sNx)*(J-1)+I
        IF ( Nlevxy(I2,myThid) .NE. 0 ) THEN
         PNLEVW(I2,myThid) = PsurfW(Nlevxy(I2,myThid))/pGround
        ELSE
C        Dummy value for land
         PNLEVW(I2,myThid) = PsurfW(Nr)/pGround
        ENDIF
        PSLG1(I2,myThid) = 0.
       ENDDO
      ENDDO
C
C
C     Physics package needs to know time of year as a fraction
      tYear = currentTime/(86400.*360.) -
     &        FLOAT(INT(currentTime/(86400.*360.)))

C
C     Load external data needed by physics package
C     1. Albedo                (between 0-1)
C     2. Soil moisture         (between 0-1)
C     3. Surface temperatures  (in situ Temp. [K])
C     4. Snow depth            - assume no snow for now
C     5. Sea ice               - assume no sea ice for now
C     6. Land sea mask         - infer from exact zeros in soil moisture dataset
C     7. Surface geopotential  - to be done when orography is in
C                                dynamical kernel. Assume 0. for now.

C      Load in surface albedo data (in [0,1]) from aim_albedo to alb0 :
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         alb0(I2,myThid) = 0.
         alb0(I2,myThid) = aim_albedo(I,J,bi,bj)
        ENDDO
       ENDDO
C      Load in surface temperature data from aim_surfTemp to stl1 & sst1 :
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         sst1(I2,myThid) = 300.
         stl1(I2,myThid) = 300.
         sst1(I2,myThid) = aim_surfTemp(I,J,bi,bj)
         stl1(I2,myThid) = aim_surfTemp(I,J,bi,bj)
        ENDDO
       ENDDO
C
C      Load in soil moisture data (in [0,1]) from aim_soilMoisture to soilq1 :
C??? NOT CLEAR  scale for bucket depth of 75mm which is what Franco uses.
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         soilq1(I2,myThid) = 0.
         soilq1(I2,myThid) = aim_soilMoisture(I,J,bi,bj)
        ENDDO
       ENDDO
C      Set snow depth, sea ice to zero for now
C      Land-sea mask ( figure this out from where 
C                      soil moisture is exactly zero ).
       DO J=1,sNy
        DO I=1,sNx
         I2 = (sNx)*(J-1)+I
         fMask1(I2,myThid) = 1.
         IF ( soilq1(I2,myThid) .EQ. 0. ) fMask1(I2,myThid) = 0.
         oice1(I2,myThid) = 0.
         snow1(I2,myThid) = 0.
        ENDDO
       ENDDO
C      open(77,file='lsmask',form='unformatted')
C      write(77) fmask1
C      close(77)

C Addition may 15 . Reset humidity to 0. if negative
C ---------------------------------------------------
#ifdef OLD_AIM_INTERFACE
      DO K=1,Nr
       DO J=1-OLy,sNy+OLy
        DO I=1-Olx,sNx+OLx
         IF ( salt(i,j,k,bi,bj) .LT. 0. .OR. K .EQ. Nr ) THEN
          salt(i,j,k,bi,bj) = 0.
         ENDIF
        ENDDO
       ENDDO
      ENDDO
#endif /* OLD_AIM_INTERFACE */

      CALL PDRIVER( tYear, myThid )

#ifdef ALLOW_TIMEAVE
C     Calculate diagnostics for AIM
      CALL AIM_CALC_DIAGS( bi, bj, currentTime, myThid )
#endif /* ALLOW_TIMEAVE */

      CALL AIM_AIM2DYN( bi, bj, currentTime, myThid )

#endif /* ALLOW_AIM */

      RETURN
      END
