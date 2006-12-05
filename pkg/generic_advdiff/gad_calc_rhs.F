C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_calc_rhs.F,v 1.43 2006/12/05 05:26:46 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA, yA, maskUp, uFld, vFld, wFld,
     I           uTrans, vTrans, rTrans, rTransKp1,
     I           diffKh, diffK4, KappaR, TracerN, TracAB,
     I           tracerIdentity, advectionScheme, vertAdvecScheme,
     I           calcAdvection, implicitAdvection, applyAB_onTracer,
     U           fVerT, gTracer,
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C Calculates the tendency of a tracer due to advection and diffusion.
C It calculates the fluxes in each direction indepentently and then
C sets the tendency to the divergence of these fluxes. The advective
C fluxes are only calculated here when using the linear advection schemes
C otherwise only the diffusive and parameterized fluxes are calculated.
C
C Contributions to the flux are calculated and added:
C \begin{equation*}
C {\bf F} = {\bf F}_{adv} + {\bf F}_{diff} +{\bf F}_{GM} + {\bf F}_{KPP}
C \end{equation*}
C
C The tendency is the divergence of the fluxes:
C \begin{equation*}
C G_\theta = G_\theta + \nabla \cdot {\bf F}
C \end{equation*}
C
C The tendency is assumed to contain data on entry.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "GAD.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C !INPUT PARAMETERS: ===================================================
C bi,bj            :: tile indices
C iMin,iMax        :: loop range for called routines
C jMin,jMax        :: loop range for called routines
C k                :: vertical index
C kM1              :: =k-1 for k>1, =1 for k=1
C kUp              :: index into 2 1/2D array, toggles between 1|2
C kDown            :: index into 2 1/2D array, toggles between 2|1
C xA,yA            :: areas of X and Y face of tracer cells
C maskUp           :: 2-D array for mask at W points
C uFld,vFld,wFld   :: Local copy of velocity field (3 components)
C uTrans,vTrans    :: 2-D arrays of volume transports at U,V points
C rTrans           :: 2-D arrays of volume transports at W points
C rTransKp1        :: 2-D array of volume trans at W pts, interf k+1
C diffKh           :: horizontal diffusion coefficient
C diffK4           :: bi-harmonic diffusion coefficient
C KappaR           :: 2-D array for vertical diffusion coefficient, interf k
C TracerN          :: tracer field @ time-step n (Note: only used
C                     if applying AB on tracer field rather than on tendency gTr)
C TracAB           :: current tracer field (@ time-step n if applying AB on gTr
C                     or extrapolated fwd in time to n+1/2 if applying AB on Tr)
C tracerIdentity   :: tracer identifier (required for KPP,GM)
C advectionScheme  :: advection scheme to use (Horizontal plane)
C vertAdvecScheme  :: advection scheme to use (Vertical direction)
C calcAdvection    :: =False if Advec computed with multiDim scheme
C implicitAdvection:: =True if vertical Advec computed implicitly
C applyAB_onTracer :: apply Adams-Bashforth on Tracer (rather than on gTr)
C myTime           :: current time
C myIter           :: iteration number
C myThid           :: thread number
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k,kUp,kDown,kM1
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffKh, diffK4
      _RL KappaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL TracerN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TracAB (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER tracerIdentity
      INTEGER advectionScheme, vertAdvecScheme
      LOGICAL calcAdvection
      LOGICAL implicitAdvection, applyAB_onTracer
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
C gTracer          :: tendency array
C fVerT            :: 2 1/2D arrays for vertical advective flux
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C df4              :: used for storing del^2 T for bi-harmonic term
C fZon             :: zonal flux
C fMer             :: meridional flux
C af               :: advective flux
C df               :: diffusive flux
C localT           :: local copy of tracer field
C locABT           :: local copy of (AB-extrapolated) tracer field
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 GAD_DIAG_SUFX, diagSufx
      EXTERNAL    GAD_DIAG_SUFX
#endif
      INTEGER i,j
      _RL df4   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL locABT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL advFac, rAdvFac
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverT is set in this subroutine
C--   the kDown is still required
      fVerT(1,1,kDown) = fVerT(1,1,kDown)
#endif

#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostic suffix for the current tracer
      IF ( useDiagnostics ) THEN
        diagSufx = GAD_DIAG_SUFX( tracerIdentity, myThid )
      ENDIF
#endif

      advFac  = 0. _d 0
      IF (calcAdvection) advFac = 1. _d 0
      rAdvFac = rkSign*advFac
      IF (implicitAdvection) rAdvFac = 0. _d 0

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fZon(i,j)      = 0. _d 0
        fMer(i,j)      = 0. _d 0
        fVerT(i,j,kUp) = 0. _d 0
        df(i,j)        = 0. _d 0
        df4(i,j)       = 0. _d 0
       ENDDO
      ENDDO

C--   Make local copy of tracer array
      IF ( applyAB_onTracer ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          localT(i,j)=TracerN(i,j,k,bi,bj)
          locABT(i,j)= TracAB(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ELSE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          localT(i,j)= TracAB(i,j,k,bi,bj)
          locABT(i,j)= TracAB(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

C--   Unless we have already calculated the advection terms we initialize
C     the tendency to zero.
C     <== now done earlier at the beginning of thermodynamics.
c     IF (calcAdvection) THEN
c      DO j=1-Oly,sNy+Oly
c       DO i=1-Olx,sNx+Olx
c        gTracer(i,j,k,bi,bj)=0. _d 0
c       ENDDO
c      ENDDO
c     ENDIF

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
      IF (calcAdvection) THEN
        IF (advectionScheme.EQ.ENUM_CENTERED_2ND) THEN
          CALL GAD_C2_ADV_X(bi,bj,k,uTrans,locABT,af,myThid)
        ELSEIF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &          .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
          CALL GAD_DST2U1_ADV_X( bi,bj,k, advectionScheme,
     I            dTtracerLev(k), uTrans, uFld, locABT,
     O            af, myThid )
        ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
          CALL GAD_FLUXLIMIT_ADV_X( bi,bj,k, dTtracerLev(k),
     I            uTrans, uFld, maskW(1-Olx,1-Oly,k,bi,bj), locABT,
     O            af, myThid )
        ELSEIF (advectionScheme.EQ.ENUM_UPWIND_3RD ) THEN
          CALL GAD_U3_ADV_X(bi,bj,k,uTrans,locABT,af,myThid)
        ELSEIF (advectionScheme.EQ.ENUM_CENTERED_4TH) THEN
          CALL GAD_C4_ADV_X(bi,bj,k,uTrans,locABT,af,myThid)
        ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
          CALL GAD_DST3_ADV_X( bi,bj,k, dTtracerLev(k),
     I            uTrans, uFld, maskW(1-Olx,1-Oly,k,bi,bj), locABT,
     O            af, myThid )
        ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
         IF ( inAdMode ) THEN
cph This block is to trick the adjoint:
cph IF inAdExact=.FALSE., we want to use DST3
cph with limiters in forward, but without limiters in reverse.
          CALL GAD_DST3_ADV_X( bi,bj,k, dTtracerLev(k),
     I           uTrans, uFld, maskW(1-Olx,1-Oly,k,bi,bj), locABT,
     O           af, myThid )
         ELSE
          CALL GAD_DST3FL_ADV_X( bi,bj,k, dTtracerLev(k),
     I           uTrans, uFld, maskW(1-Olx,1-Oly,k,bi,bj), locABT,
     O           af, myThid )
         ENDIF
        ELSE
         STOP 'GAD_CALC_RHS: Bad advectionScheme (X)'
        ENDIF
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fZon(i,j) = fZon(i,j) + af(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagName = 'ADVx'//diagSufx
          CALL DIAGNOSTICS_FILL(af,diagName, k,1, 2,bi,bj, myThid)
        ENDIF
#endif
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

C-    Add bi-harmonic diffusive flux in X
      IF (diffK4 .NE. 0.) THEN
       CALL GAD_BIHARM_X(bi,bj,k,xA,df4,diffK4,df,myThid)
      ENDIF

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in X
      IF (useGMRedi) THEN
C *note* should update GMREDI_XTRANSPORT to set df  *aja*
        IF ( applyAB_onTracer ) THEN
          CALL GMREDI_XTRANSPORT(
     I         iMin,iMax,jMin,jMax,bi,bj,k,
     I         xA,TracerN,tracerIdentity,
     U         df,
     I         myThid)
        ELSE
          CALL GMREDI_XTRANSPORT(
     I         iMin,iMax,jMin,jMax,bi,bj,k,
     I         xA,TracAB, tracerIdentity,
     U         df,
     I         myThid)
        ENDIF
      ENDIF
#endif
C     anelastic: advect.fluxes are scaled by rhoFac but hor.diff. flx are not
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fZon(i,j) = fZon(i,j) + df(i,j)*rhoFacC(k)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
C-    Diagnostics of Tracer flux in X dir (mainly Diffusive term),
C       excluding advective terms:
      IF ( useDiagnostics .AND.
     &    (diffKh.NE.0. .OR. diffK4 .NE.0. .OR. useGMRedi) ) THEN
          diagName = 'DFxE'//diagSufx
          CALL DIAGNOSTICS_FILL(df,diagName, k,1, 2,bi,bj, myThid)
      ENDIF
#endif

C--   Initialize net flux in Y direction
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = 0. _d 0
       ENDDO
      ENDDO

C-    Advective flux in Y
      IF (calcAdvection) THEN
        IF (advectionScheme.EQ.ENUM_CENTERED_2ND) THEN
          CALL GAD_C2_ADV_Y(bi,bj,k,vTrans,locABT,af,myThid)
        ELSEIF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &          .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
          CALL GAD_DST2U1_ADV_Y( bi,bj,k, advectionScheme,
     I            dTtracerLev(k), vTrans, vFld, locABT,
     O            af, myThid )
        ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
          CALL GAD_FLUXLIMIT_ADV_Y( bi,bj,k, dTtracerLev(k),
     I            vTrans, vFld, maskS(1-Olx,1-Oly,k,bi,bj), locABT,
     O            af, myThid )
        ELSEIF (advectionScheme.EQ.ENUM_UPWIND_3RD ) THEN
          CALL GAD_U3_ADV_Y(bi,bj,k,vTrans,locABT,af,myThid)
        ELSEIF (advectionScheme.EQ.ENUM_CENTERED_4TH) THEN
          CALL GAD_C4_ADV_Y(bi,bj,k,vTrans,locABT,af,myThid)
        ELSEIF (advectionScheme.EQ.ENUM_DST3 ) THEN
          CALL GAD_DST3_ADV_Y( bi,bj,k, dTtracerLev(k),
     I            vTrans, vFld, maskS(1-Olx,1-Oly,k,bi,bj), locABT,
     O            af, myThid )
        ELSEIF (advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
         IF ( inAdMode ) THEN
cph This block is to trick the adjoint:
cph IF inAdExact=.FALSE., we want to use DST3
cph with limiters in forward, but without limiters in reverse.
          CALL GAD_DST3_ADV_Y( bi,bj,k, dTtracerLev(k),
     I           vTrans, vFld, maskS(1-Olx,1-Oly,k,bi,bj), locABT,
     O           af, myThid )
         ELSE
          CALL GAD_DST3FL_ADV_Y( bi,bj,k, dTtracerLev(k),
     I           vTrans, vFld, maskS(1-Olx,1-Oly,k,bi,bj), locABT,
     O           af, myThid )
         ENDIF
        ELSE
          STOP 'GAD_CALC_RHS: Bad advectionScheme (Y)'
        ENDIF
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fMer(i,j) = fMer(i,j) + af(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagName = 'ADVy'//diagSufx
          CALL DIAGNOSTICS_FILL(af,diagName, k,1, 2,bi,bj, myThid)
        ENDIF
#endif
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

C-    Add bi-harmonic flux in Y
      IF (diffK4 .NE. 0.) THEN
       CALL GAD_BIHARM_Y(bi,bj,k,yA,df4,diffK4,df,myThid)
      ENDIF

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in Y
      IF (useGMRedi) THEN
C *note* should update GMREDI_YTRANSPORT to set df  *aja*
        IF ( applyAB_onTracer ) THEN
          CALL GMREDI_YTRANSPORT(
     I         iMin,iMax,jMin,jMax,bi,bj,k,
     I         yA,TracerN,tracerIdentity,
     U         df,
     I         myThid)
        ELSE
          CALL GMREDI_YTRANSPORT(
     I         iMin,iMax,jMin,jMax,bi,bj,k,
     I         yA,TracAB, tracerIdentity,
     U         df,
     I         myThid)
        ENDIF
      ENDIF
#endif
C     anelastic: advect.fluxes are scaled by rhoFac but hor.diff. flx are not
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = fMer(i,j) + df(i,j)*rhoFacC(k)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
C-    Diagnostics of Tracer flux in Y dir (mainly Diffusive terms),
C       excluding advective terms:
      IF ( useDiagnostics .AND.
     &    (diffKh.NE.0. .OR. diffK4 .NE.0. .OR. useGMRedi) ) THEN
          diagName = 'DFyE'//diagSufx
          CALL DIAGNOSTICS_FILL(df,diagName, k,1, 2,bi,bj, myThid)
      ENDIF
#endif

C--   Compute vertical flux fVerT(kUp) at interface k (between k-1 & k):
C-    Advective flux in R
#ifdef ALLOW_AIM
C- a hack to prevent Water-Vapor vert.transport into the stratospheric level Nr
      IF (calcAdvection .AND. .NOT.implicitAdvection .AND. k.GE.2 .AND.
     &     (.NOT.useAIM .OR.tracerIdentity.NE.GAD_SALINITY .OR.k.LT.Nr)
     &   ) THEN
#else
      IF (calcAdvection .AND. .NOT.implicitAdvection .AND. k.GE.2) THEN
#endif
C-    Compute vertical advective flux in the interior:
        IF (vertAdvecScheme.EQ.ENUM_CENTERED_2ND) THEN
          CALL GAD_C2_ADV_R(bi,bj,k,rTrans,TracAB,af,myThid)
        ELSEIF ( vertAdvecScheme.EQ.ENUM_UPWIND_1RST
     &          .OR. vertAdvecScheme.EQ.ENUM_DST2 ) THEN
          CALL GAD_DST2U1_ADV_R( bi,bj,k, vertAdvecScheme,
     I         dTtracerLev(k),rTrans,wFld,TracAB(1-Olx,1-Oly,1,bi,bj),
     O         af, myThid )
        ELSEIF (vertAdvecScheme.EQ.ENUM_FLUX_LIMIT) THEN
          CALL GAD_FLUXLIMIT_ADV_R( bi,bj,k,
     I         dTtracerLev(k),rTrans,wFld,TracAB(1-Olx,1-Oly,1,bi,bj),
     O         af, myThid )
        ELSEIF (vertAdvecScheme.EQ.ENUM_UPWIND_3RD ) THEN
          CALL GAD_U3_ADV_R(bi,bj,k,rTrans,TracAB,af,myThid)
        ELSEIF (vertAdvecScheme.EQ.ENUM_CENTERED_4TH) THEN
          CALL GAD_C4_ADV_R(bi,bj,k,rTrans,TracAB,af,myThid)
        ELSEIF (vertAdvecScheme.EQ.ENUM_DST3 ) THEN
          CALL GAD_DST3_ADV_R( bi,bj,k,
     I         dTtracerLev(k),rTrans,wFld,TracAB(1-Olx,1-Oly,1,bi,bj),
     O         af, myThid )
        ELSEIF (vertAdvecScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
cph This block is to trick the adjoint:
cph IF inAdExact=.FALSE., we want to use DST3
cph with limiters in forward, but without limiters in reverse.
          IF ( inAdMode ) THEN
           CALL GAD_DST3_ADV_R( bi,bj,k,
     I         dTtracerLev(k),rTrans,wFld,TracAB(1-Olx,1-Oly,1,bi,bj),
     O         af, myThid )
          ELSE
           CALL GAD_DST3FL_ADV_R( bi,bj,k,
     I         dTtracerLev(k),rTrans,wFld,TracAB(1-Olx,1-Oly,1,bi,bj),
     O         af, myThid )
          ENDIF
        ELSE
          STOP 'GAD_CALC_RHS: Bad vertAdvecScheme (R)'
        ENDIF
C-     add the advective flux to fVerT
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fVerT(i,j,kUp) = fVerT(i,j,kUp) + af(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagName = 'ADVr'//diagSufx
          CALL DIAGNOSTICS_FILL(af,diagName, k,1, 2,bi,bj, myThid)
C- note: needs to explicitly increment the counter since DIAGNOSTICS_FILL
C        does it only if k=1 (never the case here)
          IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT(diagName,bi,bj,myThid)
        ENDIF
#endif
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
       IF ( applyAB_onTracer ) THEN
         CALL GAD_DIFF_R(bi,bj,k,KappaR,TracerN,df,myThid)
       ELSE
         CALL GAD_DIFF_R(bi,bj,k,KappaR,TracAB, df,myThid)
       ENDIF
      ENDIF

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in R
      IF (useGMRedi) THEN
C *note* should update GMREDI_RTRANSPORT to set df  *aja*
        IF ( applyAB_onTracer ) THEN
          CALL GMREDI_RTRANSPORT(
     I         iMin,iMax,jMin,jMax,bi,bj,k,
     I         TracerN,tracerIdentity,
     U         df,
     I         myThid)
        ELSE
          CALL GMREDI_RTRANSPORT(
     I         iMin,iMax,jMin,jMax,bi,bj,k,
     I         TracAB, tracerIdentity,
     U         df,
     I         myThid)
        ENDIF
      ENDIF
#endif

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fVerT(i,j,kUp) = fVerT(i,j,kUp) + df(i,j)*maskUp(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
C-    Diagnostics of Tracer flux in R dir (mainly Diffusive terms),
C       Explicit terms only & excluding advective terms:
      IF ( useDiagnostics .AND.
     &    (.NOT.implicitDiffusion .OR. useGMRedi) ) THEN
          diagName = 'DFrE'//diagSufx
          CALL DIAGNOSTICS_FILL(df,diagName, k,1, 2,bi,bj, myThid)
      ENDIF
#endif

#ifdef ALLOW_KPP
C-    Set non local KPP transport term (ghat):
      IF ( useKPP .AND. k.GE.2 ) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         df(i,j) = 0. _d 0
        ENDDO
       ENDDO
       IF (tracerIdentity.EQ.GAD_TEMPERATURE) THEN
        CALL KPP_TRANSPORT_T(
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     O     df )
       ELSEIF (tracerIdentity.EQ.GAD_SALINITY) THEN
        CALL KPP_TRANSPORT_S(
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     O     df )
#ifdef ALLOW_PTRACERS
       ELSEIF (tracerIdentity .GE. GAD_TR1) THEN
        CALL KPP_TRANSPORT_PTR(
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     I     tracerIdentity-GAD_TR1+1,
     O     df )
#endif
       ELSE
        PRINT*,'invalid tracer indentity: ', tracerIdentity
        STOP 'GAD_CALC_RHS: Ooops'
       ENDIF
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         fVerT(i,j,kUp) = fVerT(i,j,kUp)
     &                  + df(i,j)*maskUp(i,j)*rhoFacF(k)
        ENDDO
       ENDDO
      ENDIF
#endif

C--   Divergence of fluxes
C     Anelastic: scale vertical fluxes by rhoFac and leave Horizontal fluxes unchanged
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        gTracer(i,j,k,bi,bj)=gTracer(i,j,k,bi,bj)
     &   -_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &   *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)*recip_rhoFacC(k)
     &   *( (fZon(i+1,j)-fZon(i,j))
     &     +(fMer(i,j+1)-fMer(i,j))
     &     +(fVerT(i,j,kDown)-fVerT(i,j,kUp))*rkSign
     &     -localT(i,j)*( (uTrans(i+1,j)-uTrans(i,j))
     &                   +(vTrans(i,j+1)-vTrans(i,j))
     &                   +(rTransKp1(i,j)-rTrans(i,j))*rAdvFac
     &                  )*advFac
     &    )
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB
     &   .AND. tracerIdentity.EQ.GAD_TEMPERATURE
     &   .AND. k.EQ.2 .AND. myIter.EQ.1+nIter0
     &   .AND. nPx.EQ.1 .AND. nPy.EQ.1
     &   .AND. useCubedSphereExchange ) THEN
        CALL DEBUG_CS_CORNER_UV( ' fZon,fMer from GAD_CALC_RHS',
     &             fZon,fMer, k, standardMessageUnit,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

      RETURN
      END
