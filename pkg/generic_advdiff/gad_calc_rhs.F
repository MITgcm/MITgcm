C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_calc_rhs.F,v 1.12 2001/09/27 20:12:11 heimbach Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_CALC_RHS( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           diffKh, diffK4, KappaRT, Tracer,
     I           tracerIdentity, advectionScheme,
     U           fVerT, gTracer,
     I           myThid )

C !DESCRIPTION:
C Calculates the tendancy of a tracer due to advection and diffusion.
C It calculates the fluxes in each direction indepentently and then
C sets the tendancy to the divergence of these fluxes. The advective
C fluxes are only calculated here when using the linear advection schemes
C otherwise only the diffusive and parameterized fluxes are calculated.
C
C Contributions to the flux are calculated and added:
C \begin{equation*}
C {\bf F} = {\bf F}_{adv} + {\bf F}_{diff} +{\bf F}_{GM} + {\bf F}_{KPP}
C \end{equation*}
C
C The tendancy is the divergence of the fluxes:
C \begin{equation*}
C G_\theta = G_\theta + \nabla \cdot {\bf F}
C \end{equation*}
C
C The tendancy is assumed to contain data on entry.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  iMin,iMax,jMin,jMax  :: loop range for called routines
C  kup                  :: index into 2 1/2D array, toggles between 1 and 2
C  kdown                :: index into 2 1/2D array, toggles between 2 and 1
C  kp1                  :: =k+1 for k<Nr, =Nr for k=Nr
C  xA,yA                :: areas of X and Y face of tracer cells
C  uTrans,vTrans,rTrans :: 2-D arrays of volume transports at U,V and W points
C  maskUp               :: 2-D array for mask at W points
C  diffKh               :: horizontal diffusion coefficient
C  diffK4               :: bi-harmonic diffusion coefficient
C  KappaRT              :: 3-D array for vertical diffusion coefficient
C  Tracer               :: tracer field
C  tracerIdentity       :: identifier for the tracer (required only for KPP)
C  advectionScheme      :: advection scheme to use
C  myThid               :: thread number
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k,kUp,kDown,kM1
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
      INTEGER advectionScheme
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  gTracer              :: tendancy array
C  fVerT                :: 2 1/2D arrays for vertical advective flux
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  df4                  :: used for storing del^2 T for bi-harmonic term
C  fZon                 :: zonal flux
C  fmer                 :: meridional flux
C  af                   :: advective flux
C  df                   :: diffusive flux
C  localT               :: local copy of tracer field
      INTEGER i,j
      _RL df4   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverT is set in this subroutine
C--   the kDown is still required
      fVerT(1,1,kDown) = fVerT(1,1,kDown)
#endif
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fZon(i,j)      = 0. _d 0
        fMer(i,j)      = 0. _d 0
        fVerT(i,j,kUp) = 0. _d 0
       ENDDO
      ENDDO

C--   Make local copy of tracer array
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        localT(i,j)=tracer(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--   Unless we have already calculated the advection terms we initialize
C     the tendency to zero.
      IF (.NOT. multiDimAdvection .OR.
     &    advectionScheme.EQ.ENUM_CENTERED_2ND .OR.
     &    advectionScheme.EQ.ENUM_UPWIND_3RD .OR.
     &    advectionScheme.EQ.ENUM_CENTERED_4TH ) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         gTracer(i,j,k,bi,bj)=0. _d 0
        ENDDO
       ENDDO
      ENDIF

C--   Pre-calculate del^2 T if bi-harmonic coefficient is non-zero
      IF (diffK4 .NE. 0.) THEN
       CALL GAD_GRAD_X(bi,bj,k,xA,localT,fZon,myThid)
       CALL GAD_GRAD_Y(bi,bj,k,yA,localT,fMer,myThid)
       CALL GAD_DEL2(bi,bj,k,fZon,fMer,df4,myThid)
      ENDIF

C--   Initialize net flux in X direction
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fZon(i,j) = 0. _d 0
       ENDDO
      ENDDO

C-    Advective flux in X
      IF (.NOT. multiDimAdvection .OR.
     &    advectionScheme.EQ.ENUM_CENTERED_2ND .OR.
     &    advectionScheme.EQ.ENUM_UPWIND_3RD .OR.
     &    advectionScheme.EQ.ENUM_CENTERED_4TH ) THEN
      IF (advectionScheme.EQ.ENUM_CENTERED_2ND) THEN
       CALL GAD_C2_ADV_X(bi,bj,k,uTrans,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_X(
     &      bi,bj,k,deltaTtracer,uTrans,uVel,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_UPWIND_3RD ) THEN
       CALL GAD_U3_ADV_X(bi,bj,k,uTrans,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_CENTERED_4TH) THEN
       CALL GAD_C4_ADV_X(bi,bj,k,uTrans,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
       CALL GAD_DST3_ADV_X(
     &       bi,bj,k,deltaTtracer,uTrans,uVel,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
       CALL GAD_DST3FL_ADV_X(
     &       bi,bj,k,deltaTtracer,uTrans,uVel,localT,af,myThid)
      ELSE
       STOP 'GAD_CALC_RHS: Bad advectionScheme (X)'
      ENDIF
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fZon(i,j) = fZon(i,j) + af(i,j)
       ENDDO
      ENDDO
      ENDIF

C-    Diffusive flux in X
      IF (diffKh.NE.0.) THEN
       CALL GAD_DIFF_X(bi,bj,k,xA,diffKh,localT,df,myThid)
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         df(i,j) = 0. _d 0
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
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fZon(i,j) = fZon(i,j) + df(i,j)
       ENDDO
      ENDDO

C-    Bi-harmonic duffusive flux in X
      IF (diffK4 .NE. 0.) THEN
       CALL GAD_BIHARM_X(bi,bj,k,xA,df4,diffK4,df,myThid)
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         fZon(i,j) = fZon(i,j) + df(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Initialize net flux in Y direction
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = 0. _d 0
       ENDDO
      ENDDO

C-    Advective flux in Y
      IF (.NOT. multiDimAdvection .OR.
     &    advectionScheme.EQ.ENUM_CENTERED_2ND .OR.
     &    advectionScheme.EQ.ENUM_UPWIND_3RD .OR.
     &    advectionScheme.EQ.ENUM_CENTERED_4TH ) THEN
      IF (advectionScheme.EQ.ENUM_CENTERED_2ND) THEN
       CALL GAD_C2_ADV_Y(bi,bj,k,vTrans,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
       CALL GAD_FLUXLIMIT_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_UPWIND_3RD ) THEN
       CALL GAD_U3_ADV_Y(bi,bj,k,vTrans,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_CENTERED_4TH) THEN
       CALL GAD_C4_ADV_Y(bi,bj,k,vTrans,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
       CALL GAD_DST3_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localT,af,myThid)
      ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
       CALL GAD_DST3FL_ADV_Y(
     &       bi,bj,k,deltaTtracer,vTrans,vVel,localT,af,myThid)
      ELSE
       STOP 'GAD_CALC_RHS: Bad advectionScheme (Y)'
      ENDIF
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = fMer(i,j) + af(i,j)
       ENDDO
      ENDDO
      ENDIF

C-    Diffusive flux in Y
      IF (diffKh.NE.0.) THEN
       CALL GAD_DIFF_Y(bi,bj,k,yA,diffKh,localT,df,myThid)
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         df(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in Y
      IF (useGMRedi) THEN
C *note* should update GMREDI_YTRANSPORT to use localT and set df  *aja*
       CALL GMREDI_YTRANSPORT(
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

C-    Advective flux in R
      IF (.NOT. multiDimAdvection .OR.
     &    advectionScheme.EQ.ENUM_CENTERED_2ND .OR.
     &    advectionScheme.EQ.ENUM_UPWIND_3RD .OR.
     &    advectionScheme.EQ.ENUM_CENTERED_4TH ) THEN
C     Note: wVel needs to be masked 
      IF (K.GE.2) THEN
C-    Compute vertical advective flux in the interior:
       IF (advectionScheme.EQ.ENUM_CENTERED_2ND) THEN
        CALL GAD_C2_ADV_R(bi,bj,k,rTrans,tracer,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
        CALL GAD_FLUXLIMIT_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,tracer,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_UPWIND_3RD ) THEN
        CALL GAD_U3_ADV_R(bi,bj,k,rTrans,tracer,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_CENTERED_4TH) THEN
        CALL GAD_C4_ADV_R(bi,bj,k,rTrans,tracer,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
        CALL GAD_DST3_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,tracer,af,myThid)
       ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
        CALL GAD_DST3FL_ADV_R(
     &       bi,bj,k,deltaTtracer,rTrans,wVel,tracer,af,myThid)
       ELSE
        STOP 'GAD_CALC_RHS: Bad advectionScheme (R)'
       ENDIF
C-    Surface "correction" term at k>1 :
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         af(i,j) = af(i,j)
     &           + (maskC(i,j,k,bi,bj)-maskC(i,j,k-1,bi,bj))*
     &             rTrans(i,j)*Tracer(i,j,k,bi,bj)
        ENDDO
       ENDDO 
      ELSE
C-    Surface "correction" term at k=1 :
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         af(i,j) = rTrans(i,j)*Tracer(i,j,k,bi,bj)
        ENDDO
       ENDDO 
      ENDIF
C-    add the advective flux to fVerT
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fVerT(i,j,kUp) = fVerT(i,j,kUp) + af(i,j)
       ENDDO
      ENDDO
      ENDIF

C-    Diffusive flux in R
C     Note: For K=1 then KM1=1 and this gives a dT/dr = 0 upper
C           boundary condition.
      IF (implicitDiffusion) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         df(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ELSE
       CALL GAD_DIFF_R(bi,bj,k,KappaRT,tracer,df,myThid)
      ENDIF
c     DO j=1-Oly,sNy+Oly
c      DO i=1-Olx,sNx+Olx
c       fVerT(i,j,kUp) = fVerT(i,j,kUp) + df(i,j)*maskUp(i,j)
c      ENDDO
c     ENDDO

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in R
      IF (useGMRedi) THEN
C *note* should update GMREDI_RTRANSPORT to set df  *aja*
       CALL GMREDI_RTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     Tracer,
     U     df,
     I     myThid)
c      DO j=1-Oly,sNy+Oly
c       DO i=1-Olx,sNx+Olx
c        fVerT(i,j,kUp) = fVerT(i,j,kUp) + df(i,j)*maskUp(i,j)
c       ENDDO
c      ENDDO
      ENDIF
#endif

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fVerT(i,j,kUp) = fVerT(i,j,kUp) + df(i,j)*maskUp(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_KPP
C-    Add non local KPP transport term (ghat) to diffusive T flux.
      IF (useKPP) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         df(i,j) = 0. _d 0
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
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         fVerT(i,j,kUp) = fVerT(i,j,kUp) + df(i,j)*maskUp(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

C--   Divergence of fluxes
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        gTracer(i,j,k,bi,bj)=gTracer(i,j,k,bi,bj)
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
