C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_advection.F,v 1.2 2001/09/10 13:09:04 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_ADVECTION(bi,bj,advectionScheme,tracerIdentity,
     U                         Tracer,Gtracer,
     I                         myTime,myIter,myThid)
C     /==========================================================\
C     | SUBROUTINE GAD_ADVECTION                                 |
C     | o Solves the pure advection tracer equation.             |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "GAD.h"

C     == Routine arguments ==
      INTEGER bi,bj
      INTEGER advectionScheme
      INTEGER tracerIdentity
      _RL Tracer(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL Gtracer(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i,j,k,kup,kDown,kp1
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL localTij(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localTijk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kp1Msk

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        xA(i,j)      = 0. _d 0
        yA(i,j)      = 0. _d 0
        uTrans(i,j)  = 0. _d 0
        vTrans(i,j)  = 0. _d 0
        rTrans(i,j)  = 0. _d 0
        fVerT(i,j,1) = 0. _d 0
        fVerT(i,j,2) = 0. _d 0
       ENDDO
      ENDDO

      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

C--   Start of k loop for horizontal fluxes
      DO k=1,Nr

C--   Get temporary terms used by tendency routines
      CALL CALC_COMMON_FACTORS (
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     O         xA,yA,uTrans,vTrans,rTrans,maskUp,
     I         myThid)

C--   Make local copy of tracer array
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        localTij(i,j)=tracer(i,j,k,bi,bj)
       ENDDO
      ENDDO

C-    Advective flux in X
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        af(i,j) = 0.
       ENDDO
      ENDDO
      IF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_X(
     &      bi,bj,k,deltaTtracer,uTrans,uVel,localTij,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
       CALL GAD_DST3_ADV_X(
     &       bi,bj,k,deltaTtracer,uTrans,uVel,localTij,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
       CALL GAD_DST3FL_ADV_X(
     &       bi,bj,k,deltaTtracer,uTrans,uVel,localTij,af,myThid)
      ELSE
       STOP 'GAD_ADVECTION: adv. scheme incompatibale with mutli-dim'
      ENDIF
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
        localTij(i,j)=localTij(i,j)-deltaTtracer*
     &    _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rA(i,j,bi,bj)
     &    *( af(i+1,j)-af(i,j)
     &      -tracer(i,j,k,bi,bj)*(uTrans(i+1,j)-uTrans(i,j))
     &     )
       ENDDO
      ENDDO

#ifdef ALLOW_OBCS
C--   Apply open boundary conditions
      IF (useOBCS) THEN
       IF (tracerIdentity.EQ.GAD_TEMPERATURE) THEN
        CALL OBCS_APPLY_TLOC( bi, bj, k, localTij, myThid )
       ELSEIF (tracerIdentity.EQ.GAD_SALINITY) THEN
        CALL OBCS_APPLY_SLOC( bi, bj, k, localTij, myThid )
       END IF
      END IF
#endif /* ALLOW_OBCS */

C-    Advective flux in Y
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        af(i,j) = 0.
       ENDDO
      ENDDO
      IF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localTij,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
       CALL GAD_DST3_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localTij,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
       CALL GAD_DST3FL_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localTij,af,myThid)
      ELSE
       STOP 'GAD_ADVECTION: adv. scheme incompatibale with mutli-dim'
      ENDIF
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        localTij(i,j)=localTij(i,j)-deltaTtracer*
     &    _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rA(i,j,bi,bj)
     &    *( af(i,j+1)-af(i,j)
     &      -tracer(i,j,k,bi,bj)*(vTrans(i,j+1)-vTrans(i,j))
     &     )
       ENDDO
      ENDDO
#ifdef ALLOW_OBCS
C--   Apply open boundary conditions
      IF (useOBCS) THEN
       IF (tracerIdentity.EQ.GAD_TEMPERATURE) THEN
        CALL OBCS_APPLY_TLOC( bi, bj, k, localTij, myThid )
       ELSEIF (tracerIdentity.EQ.GAD_SALINITY) THEN
        CALL OBCS_APPLY_SLOC( bi, bj, k, localTij, myThid )
       END IF
      END IF
#endif /* ALLOW_OBCS */
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        localTijk(i,j,k)=localTij(i,j)
       ENDDO
      ENDDO


C--   End of K loop for horizontal fluxes
      ENDDO

C--   Start of k loop for vertical flux
      DO k=Nr,1,-1

C--   kup    Cycles through 1,2 to point to w-layer above
C--   kDown  Cycles through 2,1 to point to w-layer below
      kup  = 1+MOD(k+1,2)
      kDown= 1+MOD(k,2)

C--   Get temporary terms used by tendency routines
      CALL CALC_COMMON_FACTORS (
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     O         xA,yA,uTrans,vTrans,rTrans,maskUp,
     I         myThid)

C-    Advective flux in R
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        af(i,j) = 0.
       ENDDO
      ENDDO

C     Note: wVel needs to be masked 
      IF (K.GE.2) THEN
C-    Compute vertical advective flux in the interior:
       IF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
        CALL GAD_FLUXLIMIT_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,localTijk,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
        CALL GAD_DST3_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,localTijk,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
        CALL GAD_DST3FL_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,localTijk,af,myThid)
       ELSE
        STOP 'GAD_ADVECTION: adv. scheme incompatibale with mutli-dim'
       ENDIF
C-    Surface "correction" term at k>1 :
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         af(i,j) = af(i,j)
     &           + (maskC(i,j,k,bi,bj)-maskC(i,j,k-1,bi,bj))*
     &             rTrans(i,j)*localTijk(i,j,k)
        ENDDO
       ENDDO 
      ELSE
C-    Surface "correction" term at k=1 :
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         af(i,j) = rTrans(i,j)*localTijk(i,j,k)
        ENDDO
       ENDDO 
      ENDIF
C-    add the advective flux to fVerT
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fVerT(i,j,kUp) = af(i,j)
       ENDDO
      ENDDO

C--   Divergence of fluxes
      kp1=min(Nr,k+1)
      kp1Msk=1.
      if (k.EQ.Nr) kp1Msk=0.
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        localTij(i,j)=localTijk(i,j,k)-deltaTtracer*
     &    _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rA(i,j,bi,bj)
     &    *( fVerT(i,j,kUp)-fVerT(i,j,kDown)
     &      -tracer(i,j,k,bi,bj)*rA(i,j,bi,bj)*
     &        (wVel(i,j,k,bi,bj)-kp1Msk*wVel(i,j,kp1,bi,bj))
     &     )*rkFac
        gTracer(i,j,k,bi,bj)=
     &   (localTij(i,j)-tracer(i,j,k,bi,bj))/deltaTtracer
       ENDDO
      ENDDO
 
C--   End of K loop for vertical flux
      ENDDO

      RETURN
      END
