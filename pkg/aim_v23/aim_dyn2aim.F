C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_dyn2aim.F,v 1.6 2014/05/12 01:32:41 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_DYN2AIM
C     !INTERFACE:
      SUBROUTINE AIM_DYN2AIM(
     O           TA, QA, ThA, Vsurf2, PSA, dpFac,
     O           kGrd,
     I           bi,bj, myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_DYN2AIM
C     | o Map dynamics conforming arrays to AIM internal arrays.
C     *==========================================================*
C     | this routine transfers grid information
C     | and all dynamical variables (T,Q, ...) to AIM physics
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"

#include "AIM_GRID.h"
#include "com_physcon.h"

C     !INPUT/OUTPUT PARAMETERS:
C--   Input:
C     bi,bj  :: Tile index
C     myTime :: Current time of simulation ( s )
C     myIter :: Current iteration number in simulation
C     myThid :: Number of this instance of the routine
C--   Output:  TA     :: temperature  [K}                        (3-dim)
C              QA     :: specific humidity [g/kg]                (3-dim)
C              ThA    :: Pot.Temp. [K] (replace dry stat. energy)(3-dim)
C              Vsurf2 :: square of surface wind speed            (2-dim)
C              PSA    :: norm. surface pressure [p/p0]           (2-dim)
C              dpFac  :: cell delta_P fraction                   (3-dim)
C              kGrd   :: Ground level index                      (2-dim)
C--  Updated common blocks: AIM_GRID_R
C             WVSurf  :: weights for near surf interpolation     (2-dim)
C             fOrogr  :: orographic factor (for surface drag)    (2-dim)
C         snLat,csLat :: sin(Lat) & cos(Lat)                     (2-dim)
      _RL TA(NGP,NLEV), QA(NGP,NLEV), ThA(NGP,NLEV)
      _RL Vsurf2(NGP), PSA(NGP), dpFac(NGP,NLEV)
      INTEGER kGrd(NGP)
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_AIM
C     !LOCAL VARIABLES:
C     i, j, I2 :: Loop counters
C     k, Katm  :: Loop counters
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i, j, I2, k, Katm
      _RL conv_theta2T, temp1, temp2

c     _RL hInitC(5), hInitF(5)
c     DATA    hInitC / 17338.0,10090.02,5296.88,2038.54,418.038/
c     DATA    hInitF / 15090.4, 8050.96, 4087.75, 1657.54, 0. /

C-    Compute Sin & Cos (Latitude) :
      DO j = 1,sNy
       DO i = 1,sNx
        I2 = i+(j-1)*sNx
        snLat(I2,myThid) = SIN(yC(i,j,bi,bj)*deg2rad)
        csLat(I2,myThid) = COS(yC(i,j,bi,bj)*deg2rad)
       ENDDO
      ENDDO

C-    Set surface level index :
      DO j = 1,sNy
       DO i = 1,sNx
        I2 = i+(j-1)*sNx
        kGrd(I2) = (Nr+1) - kSurfC(i,j,bi,bj)
       ENDDO
      ENDDO

C-    Set (normalized) surface pressure :
      DO j=1,sNy
       DO i=1,sNx
        I2 = i+(j-1)*sNx
        k = kSurfC(i,j,bi,bj)
        IF ( k.LE.Nr ) THEN
c        PSA(I2) = rF(k)/atm_Po
         PSA(I2) = Ro_surf(i,j,bi,bj)/atm_Po
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
           IF ( k.EQ.kSurfC(i,j,bi,bj) ) THEN
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
      DO k = 1,Nr
       conv_theta2T = (rC(k)/atm_Po)**atm_kappa
       Katm = _KD2KA( k )
       DO j = 1,sNy
        DO i = 1,sNx
         I2 = i+(j-1)*sNx
         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN
c         UA(I2,Katm)  = uVel(i,j,k,bi,bj)
c         VA(I2,Katm)  = vVel(i,j,k,bi,bj)
C     Physics works with temperature - not potential temp.
          TA(I2,Katm)  = theta(i,j,k,bi,bj)*conv_theta2T
c         TA(I2,Katm)  = max(temp1,min(temp2,
c    &                       theta(i,j,k,bi,bj)*conv_theta2T ))
C     In atm.Phys, water vapor must be > 0 :
          QA(I2,Katm)  = MAX( salt(i,j,k,bi,bj), zeroRL )
C     Dry static energy replaced by Pot.Temp:
          ThA(I2,Katm) = theta(i,j,k,bi,bj)
         ELSE
          TA(I2,Katm)  = 300. _d 0
          QA(I2,Katm)  =   0. _d 0
          ThA(I2,Katm) = 300. _d 0
         ENDIF
        ENDDO
       ENDDO
#ifdef NONLIN_FRSURF
       IF ( select_rStar.GE.1 ) THEN
        DO j = 1,sNy
         DO i = 1,sNx
          I2 = i+(j-1)*sNx
          TA(I2,Katm)  = TA(I2,Katm)*pStarFacK(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDIF
#endif /* NONLIN_FRSURF */
      ENDDO

C_jmc: add square of surface wind speed (center of C grid) = 2 * KE_surf
      DO j = 1,sNy
       DO i = 1,sNx
        I2 = i+(j-1)*sNx
        k = kSurfC(i,j,bi,bj)
        IF (k.LE.Nr) THEN
         Vsurf2(I2) = 0.5 * (
     &                uVel(i,j,k,bi,bj)*uVel(i,j,k,bi,bj)
     &              + uVel(i+1,j,k,bi,bj)*uVel(i+1,j,k,bi,bj)
     &              + vVel(i,j,k,bi,bj)*vVel(i,j,k,bi,bj)
     &              + vVel(i,j+1,k,bi,bj)*vVel(i,j+1,k,bi,bj)
     &                        )
        ELSE
         Vsurf2(I2) = 0.
        ENDIF
       ENDDO
      ENDDO

C-    Check that Temp is OK for LW Radiation scheme :
      DO k = 1,Nr
       Katm = _KD2KA( k )
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
      DO j = 1,sNy
       DO i = 1,sNx
        I2 = i+(j-1)*sNx
        WVSurf(I2,myThid) = 0.
        k = kGrd(I2)
        IF (k.GT.1) THEN
C- full cell version of Franco Molteni formula:
c         WVSurf(I2,myThid) = (LOG(SIGH(k))-SIGL(k))*WVI(k-1,2)
C- partial cell version using true log-P extrapolation:
          WVSurf(I2,myThid) = (LOG(PSA(I2))-SIGL(k))*WVI(k-1,1)
C- like in the old code:
c         WVSurf(I2,myThid) = WVI(k,2)
        ENDIF
       ENDDO
      ENDDO
      IF (myIter.EQ.nIter0)
     &  CALL AIM_WRITE_PHYS( 'aim_WeightSurf', '', 1, WVSurf,
     &                       1, bi, bj, 1, myIter, myThid )

#endif /* ALLOW_AIM */

      RETURN
      END
