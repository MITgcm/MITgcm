C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_dyn2aim.F,v 1.5 2006/08/04 22:27:46 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_DYN2AIM(
     O           TA, QA, ThA, Vsurf2, PSA, dpFac,
     O           kGrd,
     I           bi,bj, myTime, myIter, myThid)
C     *==========================================================*
C     | S/R AIM_DYN2AIM
C     | o Map dynamics conforming arrays to AIM internal arrays.
C     *==========================================================*
C     | this routine transfers grid information
C     | and all dynamical variables (T,Q, ...) to AIM physics
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"

#include "AIM_GRID.h"
#include "com_physcon.h"

C     == Routine arguments ==
C--   Input:
C     bi,bj  - Tile index
C     myTime - Current time of simulation ( s )
C     myIter - Current iteration number in simulation
C     myThid - Number of this instance of the routine
C--   Output:  TA     = temperature  [K}                        (3-dim) 
C              QA     = specific humidity [g/kg]                (3-dim)
C              ThA    = Pot.Temp. [K] (replace dry stat. energy)(3-dim)
C              Vsurf2 = square of surface wind speed            (2-dim)
C              PSA    = norm. surface pressure [p/p0]           (2-dim)
C              dpFac  = cell delta_P fraction                   (3-dim) 
C              kGrd   = Ground level index                      (2-dim)
C--  Updated common blocks: AIM_GRID_R 
C             WVSurf  : weights for near surf interpolation     (2-dim)
C             fOrogr  : orographic factor (for surface drag)    (2-dim)
C         snLat,csLat : sin(Lat) & cos(Lat)                     (2-dim)

      INTEGER bi, bj, myIter, myThid
      _RL myTime

      _RL TA(NGP,NLEV), QA(NGP,NLEV), ThA(NGP,NLEV)
      _RL Vsurf2(NGP), PSA(NGP), dpFac(NGP,NLEV)
      INTEGER kGrd(NGP)

CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER I, J, I2, K, Katm
      _RL conv_theta2T, temp1, temp2

c     _RL hInitC(5), hInitF(5)
c     DATA    hInitC / 17338.0,10090.02,5296.88,2038.54,418.038/ 
c     DATA    hInitF / 15090.4, 8050.96, 4087.75, 1657.54, 0. / 

C-    Compute Sin & Cos (Latitude) :
      DO J = 1,sNy
       DO I = 1,sNx
        I2 = I+(J-1)*sNx
        snLat(I2,myThid) = SIN(yC(I,J,bi,bj)*deg2rad)
        csLat(I2,myThid) = COS(yC(I,J,bi,bj)*deg2rad)
       ENDDO
      ENDDO

C-    Set surface level index :
      DO J = 1,sNy
       DO I = 1,sNx
        I2 = I+(J-1)*sNx
        kGrd(I2) = (Nr+1) - ksurfC(I,J,bi,bj)
       ENDDO
      ENDDO

C-    Set (normalized) surface pressure :
      DO J=1,sNy
       DO I=1,sNx
        I2 = I+(J-1)*sNx
        K = ksurfC(i,j,bi,bj)
        IF ( K.LE.Nr ) THEN
c        PSA(I2) = rF(K)/atm_po
         PSA(I2) = Ro_surf(i,j,bi,bj)/atm_po
        ELSE
         PSA(I2) = 1.
        ENDIF
       ENDDO
      ENDDO

C-    Set cell delta_P fraction (of the full delta.P = drF_k):
#ifdef NONLIN_FRSURF
      IF ( staggerTimeStep .AND. nonlinFreeSurf.GT.0 ) THEN
       IF ( select_rStar.GT.0 ) THEN
        DO k = 1,Nr
         Katm = _KD2KA( k )
         DO j = 1,sNy
          DO i = 1,sNx
           I2 = i+(j-1)*sNx
           dpFac(I2,Katm) = h0FacC(i,j,k,bi,bj)*rStarFacC(i,j,bi,bj)
c          dpFac(I2,Katm) = 1. _d 0
          ENDDO
         ENDDO
        ENDDO
       ELSE
        DO k = 1,Nr
         Katm = _KD2KA( k )
         DO j = 1,sNy
          DO i = 1,sNx
           I2 = i+(j-1)*sNx
           IF ( k.EQ.ksurfC(i,j,bi,bj) ) THEN
            dpFac(I2,Katm) = hFac_surfC(i,j,bi,bj)
           ELSE
            dpFac(I2,Katm) = hFacC(i,j,k,bi,bj)
           ENDIF
c          dpFac(I2,Katm) = 1. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDIF
      ELSE
#else /* ndef NONLIN_FRSURF */
      IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */
        DO k = 1,Nr
         Katm = _KD2KA( k )
         DO j = 1,sNy
          DO i = 1,sNx
           I2 = i+(j-1)*sNx
           dpFac(I2,Katm) = hFacC(i,j,k,bi,bj)
c          dpFac(I2,Katm) = 1. _d 0
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Physics package works with sub-domains 1:sNx,1:sNy,1:Nr.
C     Internal index mapping is linear in X and Y with a second
C     dimension for the vertical.

C-    Dynamical var --> AIM var :
C       note: UA & VA are not used  => removed
      temp1 = lwTemp1
      temp2 = lwTemp2
      DO K = 1,Nr
       conv_theta2T = (rC(K)/atm_po)**atm_kappa 
       Katm = _KD2KA( K )
       DO J = 1,sNy
        DO I = 1,sNx
         I2 = I+(J-1)*sNx
         IF (maskC(i,j,k,bi,bj).EQ.1. _d 0) THEN
c         UA(I2,Katm)  = uVel(I,J,K,bi,bj)
c         VA(I2,Katm)  = vVel(I,J,K,bi,bj)
C     Physics works with temperature - not potential temp.
          TA(I2,Katm)  = theta(I,J,K,bi,bj)*conv_theta2T
c         TA(I2,Katm)  = max(temp1,min(temp2,
c    &                       theta(I,J,K,bi,bj)*conv_theta2T ))
C     In atm.Phys, water vapor must be > 0 : 
          QA(I2,Katm)  = MAX(salt(I,J,K,bi,bj), 0. _d 0)
C     Dry static energy replaced by Pot.Temp:
          ThA(I2,Katm) = theta(I,J,K,bi,bj)
         ELSE
          TA(I2,Katm)  = 300. _d 0
          QA(I2,Katm)  =   0. _d 0
          ThA(I2,Katm) = 300. _d 0
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C_jmc: add square of surface wind speed (center of C grid) = 2 * KE_surf
      DO J = 1,sNy
       DO I = 1,sNx
        I2 = I+(J-1)*sNx
        K = ksurfC(i,j,bi,bj)
        IF (K.LE.Nr) THEN
         Vsurf2(I2) = 0.5 * (
     &                uVel(I,J,K,bi,bj)*uVel(I,J,K,bi,bj)
     &              + uVel(I+1,J,K,bi,bj)*uVel(I+1,J,K,bi,bj)
     &              + vVel(I,J,K,bi,bj)*vVel(I,J,K,bi,bj)
     &              + vVel(I,J+1,K,bi,bj)*vVel(I,J+1,K,bi,bj)
     &                        )
        ELSE
         Vsurf2(I2) = 0.
        ENDIF
       ENDDO
      ENDDO

C-    Check that Temp is OK for LW Radiation scheme :
      DO K = 1,Nr
       Katm = _KD2KA( K )
       DO I2=1,NGP
        IF (  TA(I2,Katm).LT.lwTemp1 .OR.
     &        TA(I2,Katm).GT.lwTemp2 ) THEN
         i = 1 + mod((I2-1),sNx)
         j = 1 + int((I2-1)/sNx)
         WRITE(msgBuf,'(A,1PE20.13,A,2I4)')
     &    'AIM_DYN2AIM: Temp=', TA(I2,Katm),
     &    ' out of range ',lwTemp1,lwTemp2
         CALL PRINT_ERROR( msgBuf , myThid)
         WRITE(msgBuf,'(A,3I4,3I3,I6,2F9.3)')
     &    'AIM_DYN2AIM: Pb in i,j,k,bi,bj,myThid,I2,X,Y=',
     &        i,j,k,bi,bj,myThid,I2,xC(i,j,bi,bj),yC(i,j,bi,bj)
         CALL PRINT_ERROR( msgBuf , myThid)
         STOP 'ABNORMAL END: S/R AIM_DYN2AIM' 
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Set geopotential surfaces
c     DO Katm=1,NLEV
c      DO I2=1,NGP
c       PHIG1(I2,Katm) = gravity*HinitC(Katm)
c      ENDDO
c     ENDDO

C-    Weights for vertical interpolation down to the surface
C     Fsurf = Ffull(nlev)+WVS*(Ffull(nlev)-Ffull(nlev-1))
      DO J = 1,sNy
       DO I = 1,sNx
        I2 = I+(J-1)*sNx
        WVSurf(I2,myThid) = 0.
        K = kGrd(I2)
        IF (K.GT.1) THEN
C- full cell version of Franco Molteni formula:
c         WVSurf(I2,myThid) = (LOG(SIGH(K))-SIGL(K))*WVI(K-1,2)
C- partial cell version using true log-P extrapolation:
          WVSurf(I2,myThid) = (LOG(PSA(I2))-SIGL(K))*WVI(K-1,1)
C- like in the old code:
c         WVSurf(I2,myThid) = WVI(K,2)
        ENDIF
       ENDDO
      ENDDO
      IF (myIter.EQ.nIter0) 
     &  CALL AIM_WRITE_PHYS( 'aim_WeightSurf', '', 1, WVSurf,
     &                       1, bi, bj, 1, myIter, myThid )

#endif /* ALLOW_AIM */

      RETURN
      END
