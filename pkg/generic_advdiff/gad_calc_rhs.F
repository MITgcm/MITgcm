C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_calc_rhs.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_CALC_RHS( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           diffKh, diffK4, KappaRT, Tracer,
     I           tracerIdentity,
     U           fVerT, gTracer,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_CALC_RHS                                  |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "GAD.h"

C     == Routine arguments ==
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffKh, diffK4
      _RL KappaRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER tracerIdentity
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
      LOGICAL TOP_LAYER
      _RL afFacT, dfFacT
      _RL df4   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverT is set in this subroutine
C--   the kDown is still required
      fVerT(1,1,kDown) = fVerT(1,1,kDown)
#endif
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fZon(i,j)      = 0.0
        fMer(i,j)      = 0.0
        fVerT(i,j,kUp) = 0.0
       ENDDO
      ENDDO

      afFacT = 1. _d 0
      dfFacT = 1. _d 0
      TOP_LAYER = K .EQ. 1

C--   Make local copy of tracer array
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        localT(i,j)=tracer(i,j,k,bi,bj)
       ENDDO
      ENDDO


C--   Pre-calculate del^2 T if bi-harmonic coefficient is non-zero
      IF (diffK4 .NE. 0.) THEN
       CALL GAD_GRAD_X(bi,bj,k,xA,localT,fZon,myThid)
       CALL GAD_GRAD_Y(bi,bj,k,yA,localT,fMer,myThid)
       CALL GAD_DEL2(bi,bj,k,fZon,fMer,df4,myThid)
      ENDIF

C--   Initialize net flux in X direction
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fZon(i,j) = 0.
       ENDDO
      ENDDO

C-    Advective flux in X
      IF (gad_advection_scheme.EQ.ENUM_CENTERED_2ND) THEN
       CALL GAD_C2_ADV_X(bi,bj,k,uTrans,localT,af,myThid)
      ELSEIF (gad_advection_scheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_X(
     &      bi,bj,k,deltaTtracer,uTrans,uVel,localT,af,myThid)
      ELSEIF (gad_advection_scheme.EQ.ENUM_CENTERED_4TH) THEN
       CALL GAD_C4_ADV_X(bi,bj,k,uTrans,localT,af,myThid)
      ELSE
       stop 'CALC_GTRACER: Bad gad_advection_scheme (X)'
      ENDIF
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = fZon(i,j) + af(i,j)
       ENDDO
      ENDDO

C-    Diffusive flux in X
      IF (diffKh.NE.0.) THEN
       CALL GAD_DIFF_X(bi,bj,k,xA,diffKh,localT,df,myThid)
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = 0.
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in X
      IF (useGMRedi) THEN
C *note* should update GMREDI_XTRANSPORT to use localT and set df  *aja*
        CALL GMREDI_XTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     xA,Tracer,
     U     df,
     I     myThid)
      ENDIF
#endif
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = fZon(i,j) + df(i,j)
       ENDDO
      ENDDO

C-    Bi-harmonic duffusive flux in X
      IF (diffK4 .NE. 0.) THEN
       CALL GAD_BIHARM_X(bi,bj,k,xA,df4,diffK4,df,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         fZon(i,j) = fZon(i,j) + df(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Initialize net flux in Y direction
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = 0.
       ENDDO
      ENDDO

C-    Advective flux in Y
      IF (gad_advection_scheme.EQ.ENUM_CENTERED_2ND) THEN
       CALL GAD_C2_ADV_Y(bi,bj,k,vTrans,localT,af,myThid)
      ELSEIF (gad_advection_scheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localT,af,myThid)
      ELSEIF (gad_advection_scheme.EQ.ENUM_CENTERED_4TH) THEN
       CALL GAD_C4_ADV_Y(bi,bj,k,vTrans,localT,af,myThid)
      ELSE
       stop 'CALC_GTRACER: Bad gad_advection_scheme (Y)'
      ENDIF
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = fMer(i,j) + af(i,j)
       ENDDO
      ENDDO

C-    Diffusive flux in Y
      IF (diffKh.NE.0.) THEN
       CALL GAD_DIFF_Y(bi,bj,k,yA,diffKh,localT,df,myThid)
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         df(i,j) = 0.
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in Y
      IF (useGMRedi) THEN
       CALL GMREDI_YTRANSPORT(
C *note* should update GMREDI_YTRANSPORT to use localT and set df  *aja*
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     yA,Tracer,
     U     df,
     I     myThid)
      ENDIF
#endif
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = fMer(i,j) + df(i,j)
       ENDDO
      ENDDO

C-    Bi-harmonic flux in Y
      IF (diffK4 .NE. 0.) THEN
       CALL GAD_BIHARM_Y(bi,bj,k,yA,df4,diffK4,df,myThid)
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         fMer(i,j) = fMer(i,j) + df(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Initialize net flux in R
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerT(i,j,kUp) = 0.
       ENDDO
      ENDDO

C-    Advective flux in R
      IF (gad_advection_scheme.EQ.ENUM_CENTERED_2ND) THEN
       CALL GAD_C2_ADV_R(bi,bj,k,rTrans,tracer,af,myThid)
      ELSEIF (gad_advection_scheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,tracer,af,myThid)
      ELSEIF (gad_advection_scheme.EQ.ENUM_CENTERED_4TH) THEN
       CALL GAD_C4_ADV_R(bi,bj,k,rTrans,tracer,af,myThid)
c      CALL GAD_C2_ADV_R(bi,bj,k,rTrans,tracer,af,myThid)
      ELSE
       stop 'CALC_GTRACER: Bad gad_advection_scheme (R)'
      ENDIF
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerT(i,j,kUp) = fVerT(i,j,kUp) + afFacT*af(i,j)
       ENDDO
      ENDDO

C-    Diffusive flux in R
C     Note: For K=1 then KM1=1 and this gives a dT/dr = 0 upper
C           boundary condition.
      IF (implicitDiffusion) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       CALL GAD_DIFF_R(bi,bj,k,KappaRT,tracer,df,myThid)
      ENDIF
c     DO j=jMin,jMax
c      DO i=iMin,iMax
c       fVerT(i,j,kUp) = fVerT(i,j,kUp) + dfFacT*df(i,j)*maskUp(i,j)
c      ENDDO
c     ENDDO

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in R
      IF (useGMRedi) THEN
C *note* should update GMREDI_RTRANSPORT to set df  *aja*
       CALL GMREDI_RTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     maskUp,Tracer,
     U     df,
     I     myThid)
c      DO j=jMin,jMax
c       DO i=iMin,iMax
c        fVerT(i,j,kUp) = fVerT(i,j,kUp) + dfFacT*df(i,j)*maskUp(i,j)
c       ENDDO
c      ENDDO
      ENDIF
#endif

      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerT(i,j,kUp) = fVerT(i,j,kUp) + dfFacT*df(i,j)*maskUp(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_KPP
C-    Add non local KPP transport term (ghat) to diffusive T flux.
      IF (useKPP) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = 0.
        ENDDO
       ENDDO
       IF (tracerIdentity.EQ.GAD_TEMPERATURE) THEN
C *note* should update KPP_TRANSPORT_T to set df  *aja*
        CALL KPP_TRANSPORT_T(
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     I     KappaRT,
     U     df )
       ELSEIF (tracerIdentity.EQ.GAD_SALINITY) THEN
        CALL KPP_TRANSPORT_S(
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     I     KappaRT,
     U     df )
       ELSE
        STOP 'GAD_CALC_RHS: Ooops'
       ENDIF
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerT(i,j,kUp) = fVerT(i,j,kUp) + dfFacT*df(i,j)*maskUp(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

C--   Divergence of fluxes
      DO j=jMin,jMax
       DO i=iMin,iMax
        gTracer(i,j,k,bi,bj)=
     &   -_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rA(i,j,bi,bj)
     &    *(
     &    +( fZon(i+1,j)-fZon(i,j) )
     &    +( fMer(i,j+1)-fMer(i,j) )
     &    +( fVerT(i,j,kUp)-fVerT(i,j,kDown) )*rkFac
     &    )
       ENDDO
      ENDDO

      RETURN
      END
