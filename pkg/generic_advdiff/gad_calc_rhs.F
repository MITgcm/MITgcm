#include "GAD_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GAD_CALC_RHS

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA, yA, maskUp, uFld, vFld, wFld,
     I           uTrans, vTrans, rTrans, rTransKp1,
     I           diffKh, diffK4, KappaR, diffKr4, TracerN, TracAB,
     I           deltaTLev, trIdentity,
     I           advectionSchArg, vertAdvecSchArg,
     I           calcAdvection, implicitAdvection, applyAB_onTracer,
     I           trUseDiffKr4, trUseGMRedi, trUseKPP, trUseSmolHack,
     O           fZon, fMer,
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
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_PARAMS.h"
#endif /* ALLOW_AUTODIFF */

C !INPUT PARAMETERS: ===================================================
C bi, bj           :: tile indices
C iMin, iMax       :: for called routines, to get valid output "gTracer"
C jMin, jMax       ::                      over this range of indices
C k                :: vertical index
C kM1              :: =k-1 for k>1, =1 for k=1
C kUp              :: index into 2 1/2D array, toggles between 1|2
C kDown            :: index into 2 1/2D array, toggles between 2|1
C xA, yA           :: areas of X and Y face of tracer cells
C maskUp           :: 2-D array for mask at W points
C uFld, vFld, wFld :: Local copy of velocity field (3 components)
C uTrans, vTrans   :: 2-D arrays of volume transports at U,V points
C rTrans           :: 2-D arrays of volume transports at W points
C rTransKp1        :: 2-D array of volume trans at W pts, interf k+1
C diffKh           :: horizontal diffusion coefficient
C diffK4           :: horizontal bi-harmonic diffusion coefficient
C KappaR           :: 2-D array for vertical diffusion coefficient, interf k
C diffKr4          :: 1-D array for vertical bi-harmonic diffusion coefficient
C TracerN          :: tracer field @ time-step n (Note: only used
C                     if applying AB on tracer field rather than on tendency gTr)
C TracAB           :: current tracer field (@ time-step n if applying AB on gTr
C                     or extrapolated fwd in time to n+1/2 if applying AB on Tr)
C trIdentity       :: tracer identifier (required for KPP,GM)
C advectionSchArg  :: advection scheme to use (Horizontal plane)
C vertAdvecSchArg  :: advection scheme to use (Vertical direction)
C calcAdvection    :: =False if Advec computed with multiDim scheme
C implicitAdvection:: =True if vertical Advec computed implicitly
C applyAB_onTracer :: apply Adams-Bashforth on Tracer (rather than on gTr)
C trUseDiffKr4     :: true if this tracer uses vertical bi-harmonic diffusion
C trUseGMRedi      :: true if this tracer uses GM-Redi
C trUseKPP         :: true if this tracer uses KPP
C trUseSmolHack    :: true if this tracer uses Smolarkiewicz-Hack to remain > 0
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
      _RL diffKr4(Nr)
      _RL TracerN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL TracAB (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL deltaTLev(Nr)
      INTEGER trIdentity
      INTEGER advectionSchArg, vertAdvecSchArg
      LOGICAL calcAdvection
      LOGICAL implicitAdvection, applyAB_onTracer
      LOGICAL trUseDiffKr4, trUseGMRedi, trUseKPP, trUseSmolHack
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
C gTracer          :: tendency array
C fZon             :: zonal flux
C fMer             :: meridional flux
C fVerT            :: 2 1/2D arrays for vertical advective flux
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)

C !FUNCTIONS:       ====================================================
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif /* ALLOW_DIAGNOSTICS */

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C advectionScheme  :: local copy of routine argument advectionSchArg
C vertAdvecScheme  :: local copy of routine argument vertAdvecSchArg
C df4              :: used for storing del^2 T for bi-harmonic term
C af               :: advective flux
C df               :: diffusive flux
C localT           :: local copy of tracer field
C locABT           :: local copy of (AB-extrapolated) tracer field
      INTEGER i,j
      INTEGER advectionScheme, vertAdvecScheme
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df4   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL locABT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL advFac, rAdvFac
#ifdef GAD_SMOLARKIEWICZ_HACK
      _RL outFlux, trac, gTrFac
#endif
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
#endif
CEOP

C     make local copies to be tampered with if necessary
      advectionScheme = advectionSchArg
      vertAdvecScheme = vertAdvecSchArg
#ifdef ALLOW_AUTODIFF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT gad_local_tape = COMMON, 1
C     This store directive just suppresses a recomputation warning.
C     TAF generates an extra field with or without this directive.
CADJ STORE fvert = gad_local_tape
#endif
C--   only the kUp part of fverT is set in this subroutine
C--   the kDown is still required
      fVerT(1,1,kDown) = fVerT(1,1,kDown)
C
      IF ( inAdMode .AND. useApproxAdvectionInAdMode ) THEN
C     In AD-mode, we change non-linear, potentially unstable AD advection
C     schemes to linear schemes with more stability. So far only DST3 with
C     flux limiting is replaced by DST3 without flux limiting, but any
C     combination is possible.
       IF ( advectionSchArg.EQ.ENUM_DST3_FLUX_LIMIT )
     &      advectionScheme = ENUM_DST3
       IF ( vertAdvecSchArg.EQ.ENUM_DST3_FLUX_LIMIT )
     &      vertAdvecScheme = ENUM_DST3
C     here is room for more advection schemes as this becomes necessary
      ENDIF
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostic suffix for the current tracer
      IF ( useDiagnostics ) THEN
        diagSufx = GAD_DIAG_SUFX( trIdentity, myThid )
      ENDIF
#endif

      advFac  = 0. _d 0
      IF (calcAdvection) advFac = 1. _d 0
      rAdvFac = rkSign*advFac
      IF (implicitAdvection) rAdvFac = rkSign

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
          localT(i,j)=TracerN(i,j,k)
          locABT(i,j)= TracAB(i,j,k)
         ENDDO
        ENDDO
      ELSE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          localT(i,j)=TracerN(i,j,k)
          locABT(i,j)=TracerN(i,j,k)
         ENDDO
        ENDDO
      ENDIF

C--   Pre-calculate del^2 T if bi-harmonic coefficient is non-zero
      IF (diffK4 .NE. 0.) THEN
#ifdef ALLOW_AUTODIFF_TAMC
C     These store directives suppress two recomputation warnings and
C     avoid the corresponding recomputations of fZon, fMer at the cost
C     of only four extra local 2D-fields.
CADJ STORE localT = gad_local_tape
#endif
       CALL GAD_GRAD_X(bi,bj,k,xA,localT,fZon,myThid)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE localT = gad_local_tape
#endif
       CALL GAD_GRAD_Y(bi,bj,k,yA,localT,fMer,myThid)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fZon, fMer = gad_local_tape
#endif
       CALL GAD_DEL2(bi,bj,k,fZon,fMer,df4,myThid)
      ENDIF

C--   Initialize net flux in X direction
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fZon(i,j) = 0. _d 0
       ENDDO
      ENDDO

C-    Advective flux in X
      IF (calcAdvection) THEN
        IF ( advectionScheme.EQ.ENUM_CENTERED_2ND ) THEN
           CALL GAD_C2_ADV_X( bi,bj,k, uTrans, locABT, af, myThid )
        ELSEIF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &          .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
           CALL GAD_DST2U1_ADV_X( bi,bj,k, advectionScheme, .TRUE.,
     I              deltaTLev(k), uTrans, uFld, locABT,
     O              af, myThid )
        ELSE
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
#ifdef ALLOW_OBCS
           maskLocW(i,j) = _maskW(i,j,k,bi,bj)*maskInW(i,j,bi,bj)
#else /* ALLOW_OBCS */
           maskLocW(i,j) = _maskW(i,j,k,bi,bj)
#endif /* ALLOW_OBCS */
          ENDDO
         ENDDO
         IF ( advectionScheme.EQ.ENUM_FLUX_LIMIT ) THEN
           CALL GAD_FLUXLIMIT_ADV_X( bi,bj,k, .TRUE., deltaTLev(k),
     I              uTrans, uFld, maskLocW, locABT,
     O              af, myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_UPWIND_3RD ) THEN
           CALL GAD_U3_ADV_X( bi,bj,k, uTrans, maskLocW, locABT,
     O              af, myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_CENTERED_4TH ) THEN
           CALL GAD_C4_ADV_X( bi,bj,k, uTrans, maskLocW, locABT,
     O              af, myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_DST3 ) THEN
           CALL GAD_DST3_ADV_X( bi,bj,k, .TRUE., deltaTLev(k),
     I              uTrans, uFld, maskLocW, locABT,
     O              af, myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
           CALL GAD_DST3FL_ADV_X( bi,bj,k, .TRUE., deltaTLev(k),
     I              uTrans, uFld, maskLocW, locABT,
     O              af, myThid )
#ifndef ALLOW_AUTODIFF
         ELSEIF ( advectionScheme.EQ.ENUM_OS7MP ) THEN
           CALL GAD_OS7MP_ADV_X( bi,bj,k, .TRUE., deltaTLev(k),
     I              uTrans, uFld, maskLocW, locABT,
     O              af, myThid )
#endif
         ELSE
          STOP 'GAD_CALC_RHS: Bad advectionScheme (X)'
         ENDIF
        ENDIF
#ifdef ALLOW_OBCS
        IF ( useOBCS ) THEN
C-      replace advective flux with 1st order upwind scheme estimate
          CALL OBCS_U1_ADV_TRACER( .TRUE., trIdentity, bi, bj, k,
     I                             maskW(1-OLx,1-OLy,k,bi,bj),
     I                             uTrans, locABT,
     U                             af, myThid )
        ENDIF
#endif /* ALLOW_OBCS */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fZon(i,j) = fZon(i,j) + af(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagName = 'ADVx'//diagSufx
          CALL DIAGNOSTICS_FILL( af, diagName, k,1, 2,bi,bj, myThid )
        ENDIF
#ifdef ALLOW_LAYERS
        IF ( useLayers ) THEN
          CALL LAYERS_FILL( af, trIdentity, 'AFX',
     &                      k, 1, 2,bi,bj, myThid )
        ENDIF
#endif /* ENDIF */
#endif
      ENDIF

C-    Diffusive flux in X
      IF (diffKh.NE.0.) THEN
       CALL GAD_DIFF_X(bi,bj,k,xA,diffKh,localT,df,myThid)
      ELSE
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
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
      IF ( trUseGMRedi ) THEN
        CALL GMREDI_XTRANSPORT(
     I         trIdentity, bi, bj, k, iMin, iMax+1, jMin, jMax,
     I         xA, maskUp, TracerN,
     U         df,
     I         myThid )
      ENDIF
#endif
C     anelastic: advect.fluxes are scaled by rhoFac but hor.diff. flx are not
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fZon(i,j) = fZon(i,j) + df(i,j)*rhoFacC(k)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
C-    Diagnostics of Tracer flux in X dir (mainly Diffusive term),
C       excluding advective terms:
      IF ( useDiagnostics .AND.
     &    (diffKh.NE.0. .OR. diffK4 .NE.0. .OR. trUseGMRedi) ) THEN
          diagName = 'DFxE'//diagSufx
          CALL DIAGNOSTICS_FILL( df, diagName, k,1, 2,bi,bj, myThid )
#ifdef ALLOW_LAYERS
          IF ( useLayers ) THEN
           CALL LAYERS_FILL( df, trIdentity, 'DFX',
     &                       k, 1, 2,bi,bj, myThid )
          ENDIF
#endif /* ALLOW_LAYERS */
      ENDIF
#endif

C--   Initialize net flux in Y direction
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fMer(i,j) = 0. _d 0
       ENDDO
      ENDDO

C-    Advective flux in Y
      IF (calcAdvection) THEN
        IF ( advectionScheme.EQ.ENUM_CENTERED_2ND ) THEN
           CALL GAD_C2_ADV_Y( bi,bj,k, vTrans, locABT, af, myThid )
        ELSEIF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &          .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
           CALL GAD_DST2U1_ADV_Y( bi,bj,k, advectionScheme, .TRUE.,
     I              deltaTLev(k), vTrans, vFld, locABT,
     O              af, myThid )
        ELSE
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
#ifdef ALLOW_OBCS
           maskLocS(i,j) = _maskS(i,j,k,bi,bj)*maskInS(i,j,bi,bj)
#else /* ALLOW_OBCS */
           maskLocS(i,j) = _maskS(i,j,k,bi,bj)
#endif /* ALLOW_OBCS */
          ENDDO
         ENDDO
         IF ( advectionScheme.EQ.ENUM_FLUX_LIMIT ) THEN
           CALL GAD_FLUXLIMIT_ADV_Y( bi,bj,k, .TRUE., deltaTLev(k),
     I              vTrans, vFld, maskLocS, locABT,
     O              af, myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_UPWIND_3RD ) THEN
           CALL GAD_U3_ADV_Y( bi,bj,k, vTrans, maskLocS, locABT,
     O              af, myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_CENTERED_4TH ) THEN
           CALL GAD_C4_ADV_Y( bi,bj,k, vTrans, maskLocS, locABT,
     O              af, myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_DST3 ) THEN
           CALL GAD_DST3_ADV_Y( bi,bj,k, .TRUE., deltaTLev(k),
     I              vTrans, vFld, maskLocS, locABT,
     O              af, myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
           CALL GAD_DST3FL_ADV_Y( bi,bj,k, .TRUE., deltaTLev(k),
     I              vTrans, vFld, maskLocS, locABT,
     O              af, myThid )
#ifndef ALLOW_AUTODIFF
         ELSEIF ( advectionScheme.EQ.ENUM_OS7MP ) THEN
           CALL GAD_OS7MP_ADV_Y( bi,bj,k, .TRUE., deltaTLev(k),
     I              vTrans, vFld, maskLocS, locABT,
     O              af, myThid )
#endif
         ELSE
           STOP 'GAD_CALC_RHS: Bad advectionScheme (Y)'
         ENDIF
        ENDIF
#ifdef ALLOW_OBCS
        IF ( useOBCS ) THEN
C-      replace advective flux with 1st order upwind scheme estimate
          CALL OBCS_U1_ADV_TRACER( .FALSE., trIdentity, bi, bj, k,
     I                             maskS(1-OLx,1-OLy,k,bi,bj),
     I                             vTrans, locABT,
     U                             af, myThid )
        ENDIF
#endif /* ALLOW_OBCS */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fMer(i,j) = fMer(i,j) + af(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagName = 'ADVy'//diagSufx
          CALL DIAGNOSTICS_FILL( af, diagName, k,1, 2,bi,bj, myThid )
        ENDIF
#ifdef ALLOW_LAYERS
        IF ( useLayers ) THEN
          CALL LAYERS_FILL( af, trIdentity, 'AFY',
     &                          k, 1, 2,bi,bj, myThid )
        ENDIF
#endif /* ALLOW_LAYES */
#endif
      ENDIF

C-    Diffusive flux in Y
      IF (diffKh.NE.0.) THEN
       CALL GAD_DIFF_Y(bi,bj,k,yA,diffKh,localT,df,myThid)
      ELSE
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
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
      IF ( trUseGMRedi ) THEN
        CALL GMREDI_YTRANSPORT(
     I         trIdentity, bi, bj, k, iMin, iMax, jMin, jMax+1,
     I         yA, maskUp, TracerN,
     U         df,
     I         myThid )
      ENDIF
#endif
C     anelastic: advect.fluxes are scaled by rhoFac but hor.diff. flx are not
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fMer(i,j) = fMer(i,j) + df(i,j)*rhoFacC(k)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
C-    Diagnostics of Tracer flux in Y dir (mainly Diffusive terms),
C       excluding advective terms:
      IF ( useDiagnostics .AND.
     &    (diffKh.NE.0. .OR. diffK4 .NE.0. .OR. trUseGMRedi) ) THEN
          diagName = 'DFyE'//diagSufx
          CALL DIAGNOSTICS_FILL( df, diagName, k,1, 2,bi,bj, myThid )
#ifdef ALLOW_LAYERS
          IF ( useLayers ) THEN
           CALL LAYERS_FILL( df, trIdentity, 'DFY',
     &                           k, 1, 2,bi,bj, myThid )
          ENDIF
#endif /* ALLOW_LAYERS */
      ENDIF
#endif

C--   Compute vertical flux fVerT(kUp) at interface k (between k-1 & k):
C-    Advective flux in R
#ifdef ALLOW_AIM
C- a hack to prevent Water-Vapor vert.transport into the stratospheric level Nr
      IF (calcAdvection .AND. .NOT.implicitAdvection .AND. k.GE.2 .AND.
     &     (.NOT.useAIM .OR. trIdentity.NE.GAD_SALINITY .OR. k.LT.Nr)
     &   ) THEN
#else
      IF (calcAdvection .AND. .NOT.implicitAdvection .AND. k.GE.2) THEN
#endif
       IF ( applyAB_onTracer ) THEN
C-    Compute vertical advective flux in the interior using TracAB:
        IF ( vertAdvecScheme.EQ.ENUM_CENTERED_2ND ) THEN
           CALL GAD_C2_ADV_R( bi,bj,k, rTrans, TracAB, af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_UPWIND_1RST
     &          .OR. vertAdvecScheme.EQ.ENUM_DST2 ) THEN
           CALL GAD_DST2U1_ADV_R( bi,bj,k,vertAdvecScheme,deltaTLev(k),
     I              rTrans, wFld, TracAB,
     O              af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_FLUX_LIMIT ) THEN
           CALL GAD_FLUXLIMIT_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld, TracAB,
     O              af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_UPWIND_3RD ) THEN
           CALL GAD_U3_ADV_R( bi,bj,k, rTrans, TracAB, af, myThid )
        ELSEIF (vertAdvecScheme.EQ.ENUM_CENTERED_4TH) THEN
           CALL GAD_C4_ADV_R( bi,bj,k, rTrans, TracAB, af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_DST3 ) THEN
           CALL GAD_DST3_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld, TracAB,
     O              af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
           CALL GAD_DST3FL_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld, TracAB,
     O              af, myThid )
#ifndef ALLOW_AUTODIFF
        ELSEIF ( vertAdvecScheme.EQ.ENUM_OS7MP ) THEN
           CALL GAD_OS7MP_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld, TracAB,
     O              af, myThid )
#endif
        ELSE
          STOP 'GAD_CALC_RHS: Bad vertAdvecScheme (R)'
        ENDIF
       ELSE
C-    Compute vertical advective flux in the interior using TracerN:
        IF ( vertAdvecScheme.EQ.ENUM_CENTERED_2ND ) THEN
           CALL GAD_C2_ADV_R( bi,bj,k, rTrans, TracerN, af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_UPWIND_1RST
     &          .OR. vertAdvecScheme.EQ.ENUM_DST2 ) THEN
           CALL GAD_DST2U1_ADV_R( bi,bj,k,vertAdvecScheme,deltaTLev(k),
     I              rTrans, wFld, TracerN,
     O              af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_FLUX_LIMIT ) THEN
           CALL GAD_FLUXLIMIT_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld, TracerN,
     O              af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_UPWIND_3RD ) THEN
           CALL GAD_U3_ADV_R( bi,bj,k, rTrans, TracerN, af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_CENTERED_4TH ) THEN
           CALL GAD_C4_ADV_R( bi,bj,k, rTrans, TracerN, af, myThid )
        ELSEIF( vertAdvecScheme.EQ.ENUM_DST3 ) THEN
           CALL GAD_DST3_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld, TracerN,
     O              af, myThid )
        ELSEIF ( vertAdvecScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
           CALL GAD_DST3FL_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld, TracerN,
     O              af, myThid )
#ifndef ALLOW_AUTODIFF
        ELSEIF ( vertAdvecScheme.EQ.ENUM_OS7MP ) THEN
           CALL GAD_OS7MP_ADV_R( bi,bj,k, deltaTLev(k),
     I              rTrans, wFld, TracerN,
     O              af, myThid )
#endif
        ELSE
          STOP 'GAD_CALC_RHS: Bad vertAdvecScheme (R)'
        ENDIF
       ENDIF
C-     add the advective flux to fVerT
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          fVerT(i,j,kUp) = fVerT(i,j,kUp) + af(i,j)*maskInC(i,j,bi,bj)
        ENDDO
       ENDDO
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
          diagName = 'ADVr'//diagSufx
          CALL DIAGNOSTICS_FILL( af, diagName, k,1, 2,bi,bj, myThid )
C- note: needs to explicitly increment the counter since DIAGNOSTICS_FILL
C        does it only if k=1 (never the case here)
          IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT(diagName,bi,bj,myThid)
#ifdef ALLOW_LAYERS
          IF ( useLayers ) THEN
            CALL LAYERS_FILL(af,trIdentity,'AFR',k,1,2,bi,bj,myThid)
          ENDIF
#endif /* ALLOW_LAYERS */
       ENDIF
#endif
      ENDIF

C-    Diffusive flux in R
C     Note: For K=1 then KM1=1 and this gives a dT/dr = 0 upper
C           boundary condition.
      IF (implicitDiffusion) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         df(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ELSE
        CALL GAD_DIFF_R(
     I                    bi,bj,k, maskup, KappaR, TracerN,
     O                    df, myThid )
      ENDIF

      IF ( trUseDiffKr4 ) THEN
        CALL GAD_BIHARM_R(
     I                    bi,bj,k, maskUp, diffKr4, TracerN,
     U                    df, myThid )
      ENDIF

#ifdef ALLOW_GMREDI
C-    GM/Redi flux in R
      IF ( trUseGMRedi ) THEN
        CALL GMREDI_RTRANSPORT(
     I         trIdentity, bi, bj, k, iMin, iMax, jMin, jMax,
     I         maskUp, TracerN,
     U         df,
     I         myThid )
      ENDIF
#endif

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fVerT(i,j,kUp) = fVerT(i,j,kUp) + df(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
C-    Diagnostics of Tracer flux in R dir (mainly Diffusive terms),
C       Explicit terms only & excluding advective terms:
      IF ( useDiagnostics .AND.
     &    (.NOT.implicitDiffusion .OR. trUseDiffKr4 .OR. trUseGMRedi)
     &   ) THEN
          diagName = 'DFrE'//diagSufx
          CALL DIAGNOSTICS_FILL( df, diagName, k,1, 2,bi,bj, myThid )
#ifdef ALLOW_LAYERS
          IF ( useLayers ) THEN
           CALL LAYERS_FILL(df,trIdentity,'DFR',k,1,2,bi,bj,myThid)
          ENDIF
#endif /* ALLOW_LAYERS */
      ENDIF
#endif

#ifdef ALLOW_KPP
C-    Set non local KPP transport term (ghat):
      IF ( trUseKPP .AND. k.GE.2 ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         df(i,j) = 0. _d 0
        ENDDO
       ENDDO
       IF (trIdentity.EQ.GAD_TEMPERATURE) THEN
        CALL KPP_TRANSPORT_T(
     I           iMin,iMax,jMin,jMax,bi,bj,k,km1,
     O           df,
     I           myTime, myIter, myThid )
       ELSEIF (trIdentity.EQ.GAD_SALINITY) THEN
        CALL KPP_TRANSPORT_S(
     I           iMin,iMax,jMin,jMax,bi,bj,k,km1,
     O           df,
     I           myTime, myIter, myThid )
#ifdef ALLOW_PTRACERS
       ELSEIF (trIdentity .GE. GAD_TR1) THEN
        CALL KPP_TRANSPORT_PTR(
     I           iMin,iMax,jMin,jMax,bi,bj,k,km1,
     I           trIdentity-GAD_TR1+1,
     O           df,
     I           myTime, myIter, myThid )
#endif
       ELSE
        WRITE(errorMessageUnit,*)
     &    'tracer identity =', trIdentity, ' is not valid => STOP'
        STOP 'ABNORMAL END: S/R GAD_CALC_RHS: invalid tracer identity'
       ENDIF
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         fVerT(i,j,kUp) = fVerT(i,j,kUp)
     &                  + df(i,j)*maskUp(i,j)*rhoFacF(k)
        ENDDO
       ENDDO
#ifdef ALLOW_DIAGNOSTICS
C-    Diagnostics of Non-Local Tracer (vertical) flux
       IF ( useDiagnostics ) THEN
         diagName = 'KPPg'//diagSufx
         CALL DIAGNOSTICS_FILL( df, diagName, k,1, 2,bi,bj, myThid )
C- note: needs to explicitly increment the counter since DIAGNOSTICS_FILL
C        does it only if k=1 (never the case here)
         IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT(diagName,bi,bj,myThid)
#ifdef ALLOW_LAYERS
         IF ( useLayers ) THEN
          CALL LAYERS_FILL(df,trIdentity,'DFR',k,1,2,bi,bj,myThid)
         ENDIF
#endif /* ALLOW_LAYERS */
       ENDIF
#endif
      ENDIF
#endif /* ALLOW_KPP */

#ifdef GAD_SMOLARKIEWICZ_HACK
coj   Hack to make redi (and everything else in this s/r) positive
coj   (see Smolarkiewicz MWR 1989 and Bott MWR 1989).
coj   Only works if 'down' is k+1 and k loop in thermodynamics is k=Nr,1,-1
      IF ( trUseSmolHack ) THEN
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
coj   Add outgoing fluxes
         outFlux=deltaTLev(k)*
     &    _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &   *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)*recip_rhoFacC(k)
     &    *( MAX(0. _d 0,fZon(i+1,j)) + MAX(0. _d 0,-fZon(i,j))
     &      +MAX(0. _d 0,fMer(i,j+1)) + MAX(0. _d 0,-fMer(i,j))
     &      +MAX(0. _d 0,fVerT(i,j,kDown)*rkSign)
     &      +MAX(0. _d 0,-fVerT(i,j,kUp)*rkSign)
     &     )
         trac = localT(i,j)
coj   If they would reduce tracer by a fraction of more than
coj   SmolarkiewiczMaxFrac, scale them down
         IF (outFlux.GT.0. _d 0 .AND.
     &       outFlux.GT.SmolarkiewiczMaxFrac*trac) THEN
coj   If tracer is already negative, scale flux to zero
           gTrFac = MAX(0. _d 0,SmolarkiewiczMaxFrac*trac/outFlux)

           IF (fZon(i+1,j).GT.0. _d 0) fZon(i+1,j)=gTrFac*fZon(i+1,j)
           IF (-fZon(i,j) .GT.0. _d 0) fZon(i,j)  =gTrFac*fZon(i,j)
           IF (fMer(i,j+1).GT.0. _d 0) fMer(i,j+1)=gTrFac*fMer(i,j+1)
           IF (-fMer(i,j) .GT.0. _d 0) fMer(i,j)  =gTrFac*fMer(i,j)
           IF (-fVerT(i,j,kUp)*rkSign .GT.0. _d 0)
     &       fVerT(i,j,kUp)=gTrFac*fVerT(i,j,kUp)

           IF (k.LT.Nr .AND. fVerT(i,j,kDown)*rkSign.GT.0. _d 0) THEN
coj   Down flux is special: it has already been applied in lower layer,
coj   so we have to readjust this.
coj   undo down flux, ...
             gTracer(i,j,k+1) = gTracer(i,j,k+1)
     &        +_recip_hFacC(i,j,k+1,bi,bj)*recip_drF(k+1)
     &         *recip_rA(i,j,bi,bj)*recip_deepFac2C(k+1)
     &         *recip_rhoFacC(k+1)
     &         *( -fVerT(i,j,kDown)*rkSign )
coj   ... scale ...
             fVerT(i,j,kDown)=gTrFac*fVerT(i,j,kDown)
coj   ... and reapply
             gTracer(i,j,k+1) = gTracer(i,j,k+1)
     &        +_recip_hFacC(i,j,k+1,bi,bj)*recip_drF(k+1)
     &         *recip_rA(i,j,bi,bj)*recip_deepFac2C(k+1)
     &         *recip_rhoFacC(k+1)
     &         *( fVerT(i,j,kDown)*rkSign )
           ENDIF

         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif /* GAD_SMOLARKIEWICZ_HACK */

C--   Divergence of fluxes
C     Anelastic: scale vertical fluxes by rhoFac and leave Horizontal fluxes unchanged
C     for Stevens OBC: keep only vertical diffusive contribution on boundaries
      DO j=1-OLy,sNy+OLy-1
       DO i=1-OLx,sNx+OLx-1
        gTracer(i,j,k) = gTracer(i,j,k)
     &   -_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &   *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)*recip_rhoFacC(k)
     &   *( (fZon(i+1,j)-fZon(i,j))*maskInC(i,j,bi,bj)
     &     +(fMer(i,j+1)-fMer(i,j))*maskInC(i,j,bi,bj)
     &     +(fVerT(i,j,kDown)-fVerT(i,j,kUp))*rkSign
     &     -localT(i,j)*( (uTrans(i+1,j)-uTrans(i,j))*advFac
     &                   +(vTrans(i,j+1)-vTrans(i,j))*advFac
     &                   +(rTransKp1(i,j)-rTrans(i,j))*rAdvFac
     &                  )*maskInC(i,j,bi,bj)
     &    )
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevC
     &   .AND. trIdentity.EQ.GAD_TEMPERATURE
     &   .AND. k.EQ.2 .AND. myIter.EQ.1+nIter0
     &   .AND. nPx.EQ.1 .AND. nPy.EQ.1
     &   .AND. useCubedSphereExchange ) THEN
        CALL DEBUG_CS_CORNER_UV( ' fZon,fMer from GAD_CALC_RHS',
     &             fZon,fMer, k, standardMessageUnit,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

      RETURN
      END
