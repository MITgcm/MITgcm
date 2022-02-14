#include "PTRACERS_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: PTRACERS_INTEGRATE

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_INTEGRATE(
     I                    bi, bj, recip_hFac,
     I                    uFld, vFld, wFld,
     U                    KappaRk,
     I                    myTime, myIter, myThid )

C !DESCRIPTION:
C     Calculates tendency for passive tracers and integrates forward in
C     time. The tracer array is updated here while adjustments (filters,
C     conv.adjustment) are applied later, in S/R TRACERS_CORRECTION_STEP

C !USES: ===============================================================
#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP_PARAMS.h"
#endif
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"
#include "PTRACERS_FIELDS.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi, bj           :: tile indices
C  recip_hFac       :: reciprocal of cell open-depth factor (@ next iter)
C  uFld, vFld, wFld :: Local copy of velocity field (3 components)
C  KappaRk          :: vertical diffusion used for one passive tracer
C  myTime           :: model time
C  myIter           :: time-step number
C  myThid           :: thread number
      INTEGER bi, bj
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL uFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRk   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif /* ALLOW_DIAGNOSTICS */

C !LOCAL VARIABLES: ====================================================
C  iTracer          :: tracer index
C  iMin, iMax       :: 1rst index loop range
C  jMin, jMax       :: 2nd  index loop range
C  k                :: vertical level number
C  kUp,kDown        :: toggle indices for even/odd level fluxes
C  kM1              :: =min(1,k-1)
C  GAD_TR           :: passive tracer id (GAD_TR1+iTracer-1)
C  xA               :: face area at U points in level k
C  yA               :: face area at V points in level k
C  maskUp           :: mask for vertical transport
C  uTrans           :: zonal transport in level k
C  vTrans           :: meridional transport in level k
C  rTrans           :: vertical volume transport at interface k
C  rTransKp         :: vertical volume transport at interface k+1
C  fZon             :: passive tracer zonal flux
C  fMer             :: passive tracer meridional flux
C  fVer             :: passive tracer vertical flux
C  gTracer          :: passive tracer tendency
C  gTrForc          :: passive tracer forcing tendency
C  gTr_AB           :: Adams-Bashforth tracer tendency increment
C  gTr_trp          :: passive tracer total transport tendency before gchem
      INTEGER iTracer
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i, j, k
      INTEGER kUp, kDown, kM1
      INTEGER GAD_TR
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVer    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL gTracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gTrForc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gTr_AB  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL calcAdvection
      INTEGER iterNb
      _RL dummy(Nr)
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act0, act1, act2, act3, act4
      INTEGER max0, max1, max2, max3
      INTEGER  iptrkey, kkey
#endif
#ifdef ALLOW_DIAGNOSTICS
      _RL gTr_trp (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_dt
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL diagForcing, diagAB_tend, diagTrp_tend
#endif /* ALLOW_DIAGNOSTICS */
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Compute iter at beginning of ptracer time step
#ifdef ALLOW_LONGSTEP
      iterNb = myIter - LS_nIter + 1
      IF (LS_whenToSample.GE.2) iterNb = myIter - LS_nIter
#else
      iterNb = myIter
      IF (staggerTimeStep) iterNb = myIter - 1
#endif

C-    Loop ranges for daughter routines
c     iMin = 1
c     iMax = sNx
c     jMin = 1
c     jMax = sNy
C     Regarding model dynamics, only needs to get correct tracer tendency
C     (gTracer) in tile interior (1:sNx,1:sNy);
C     However, for some diagnostics, we may want to get valid tendency
C     extended over 1 point in tile halo region (0:sNx+1,0:sNy=1).
      iMin = 0
      iMax = sNx+1
      jMin = 0
      jMax = sNy+1

C--   Loop over tracers
      DO iTracer=1,PTRACERS_numInUse
       IF ( PTRACERS_StepFwd(iTracer) ) THEN
        GAD_TR = GAD_TR1 + iTracer - 1

C-    Initialise tracer tendency to zero
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gTracer(i,j,k) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        diagForcing = .FALSE.
        diagAB_tend = .FALSE.
        diagTrp_tend = .FALSE.
        IF ( useDiagnostics ) THEN
          diagSufx = GAD_DIAG_SUFX( GAD_TR, myThid )
          diagName = 'Forc'//diagSufx
          diagForcing = DIAGNOSTICS_IS_ON( diagName, myThid )
          diagName = 'AB_g'//diagSufx
          IF ( PTRACERS_AdamsBashGtr(iTracer) )
     &    diagAB_tend = DIAGNOSTICS_IS_ON( diagName, myThid )
          diagName = 'Tp_g'//diagSufx
          diagTrp_tend = DIAGNOSTICS_IS_ON( diagName, myThid )
        ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
          act0 = iTracer - 1
          max0 = PTRACERS_num
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          iptrkey = (act0 + 1)
     &                      + act1*max0
     &                      + act2*max0*max1
     &                      + act3*max0*max1*max2
     &                      + act4*max0*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C-    Apply AB on Tracer :
        IF ( PTRACERS_AdamsBash_Tr(iTracer) ) THEN
C     compute pTr^n+1/2 (stored in gpTrNm1) extrapolating pTr forward in time
          CALL ADAMS_BASHFORTH2(
     I                      bi, bj, 0, Nr,
     I                      pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     U                      gpTrNm1(1-OLx,1-OLy,1,bi,bj,iTracer),
     O                      gTr_AB,
     I                      PTRACERS_startAB(iTracer), iterNb, myThid )
        ENDIF

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           fVer(i,j,1) = 0. _d 0
           fVer(i,j,2) = 0. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           kappaRk(i,j,k) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF */

        CALL CALC_3D_DIFFUSIVITY(
     I         bi, bj, iMin,iMax,jMin,jMax,
     I         GAD_TR,
     I         PTRACERS_useGMRedi(iTracer), PTRACERS_useKPP(iTracer),
     O         kappaRk,
     I         myThid)

#ifndef DISABLE_MULTIDIM_ADVECTION
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE pTracer(:,:,:,bi,bj,iTracer)
CADJ &      = comlev1_bibj_ptracers, key=iptrkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef PTRACERS_ALLOW_DYN_STATE
        IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
# ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('GAD_SOM_ADVECT',myThid)
# endif
          CALL GAD_SOM_ADVECT(
     I                        PTRACERS_ImplVertAdv(iTracer),
     I                        PTRACERS_advScheme(iTracer),
     I                        PTRACERS_advScheme(iTracer),
     I                        GAD_TR,
     I                        PTRACERS_dTLev, uFld, vFld, wFld,
     I                        pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     U                        _Ptracers_som(:,:,:,:,:,:,iTracer),
     O                        gTracer,
     I                        bi, bj, myTime, myIter, myThid )
        ELSEIF ( PTRACERS_MultiDimAdv(iTracer) ) THEN
#else /* PTRACERS_ALLOW_DYN_STATE */
        IF ( PTRACERS_MultiDimAdv(iTracer) ) THEN
#endif /* PTRACERS_ALLOW_DYN_STATE */
# ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GAD_ADVECTION',myThid)
# endif
          CALL GAD_ADVECTION(
     I                        PTRACERS_ImplVertAdv(iTracer),
     I                        PTRACERS_advScheme(iTracer),
     I                        PTRACERS_advScheme(iTracer),
     I                        GAD_TR,
     I                        PTRACERS_dTLev, uFld, vFld, wFld,
     I                        pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     O                        gTracer,
     I                        bi, bj, myTime, myIter, myThid )
        ENDIF
#endif /* DISABLE_MULTIDIM_ADVECTION */

C-    Start vertical index (k) loop (Nr:1)
        calcAdvection = .NOT.PTRACERS_MultiDimAdv(iTracer)
     &                  .AND. (PTRACERS_advScheme(iTracer).NE.0)
        DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = (iptrkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

          kM1  = MAX(1,k-1)
          kUp  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fVer(:,:,:) = comlev1_bibj_k_ptracers,
CADJ &     key = kkey, byte = isbyte, kind = isbyte
CADJ STORE gTracer(:,:,k) = comlev1_bibj_k_ptracers,
CADJ &     key = kkey, byte = isbyte, kind = isbyte
CADJ STORE gpTrNm1(:,:,k,bi,bj,iTracer) = comlev1_bibj_k_ptracers,
CADJ &     key = kkey, byte = isbyte, kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL CALC_ADV_FLOW(
     I                uFld, vFld, wFld,
     U                rTrans,
     O                uTrans, vTrans, rTransKp,
     O                maskUp, xA, yA,
     I                k, bi, bj, myThid )

C--   Collect forcing term in local array gTrForc:
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gTrForc(i,j) = 0. _d 0
           ENDDO
          ENDDO
          CALL PTRACERS_APPLY_FORCING(
     U                    gTrForc,
     I                    surfaceForcingPTr(1-OLx,1-OLy,bi,bj,iTracer),
     I                    iMin,iMax,jMin,jMax, k, bi, bj,
     I                    iTracer, myTime, myIter, myThid )
#ifdef ALLOW_DIAGNOSTICS
          IF ( diagForcing ) THEN
            diagName = 'Forc'//diagSufx
            CALL DIAGNOSTICS_FILL(gTrForc,diagName,k,1,2,bi,bj,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-    Calculate active tracer tendencies (gTracer) due to internal processes
C      (advection, [explicit] diffusion, parameterizations,...)
          CALL GAD_CALC_RHS(
     I             bi,bj, iMin,iMax,jMin,jMax, k,kM1, kUp,kDown,
     I             xA, yA, maskUp, uFld(1-OLx,1-OLy,k),
     I             vFld(1-OLx,1-OLy,k), wFld(1-OLx,1-OLy,k),
     I             uTrans, vTrans, rTrans, rTransKp,
     I             PTRACERS_diffKh(iTracer),
     I             PTRACERS_diffK4(iTracer),
     I             KappaRk(1-OLx,1-OLy,k), dummy,
     I             pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     I             gpTrNm1(1-OLx,1-OLy,1,bi,bj,iTracer),
     I             PTRACERS_dTLev, GAD_TR,
     I             PTRACERS_advScheme(iTracer),
     I             PTRACERS_advScheme(iTracer),
     I             calcAdvection, PTRACERS_ImplVertAdv(iTracer),
     I             PTRACERS_AdamsBash_Tr(iTracer), .FALSE.,
     I             PTRACERS_useGMRedi(iTracer),
     I             PTRACERS_useKPP(iTracer),
     I             PTRACERS_stayPositive(iTracer),
     O             fZon, fMer,
     U             fVer, gTracer,
     I             myTime, myIter, myThid )

C-    External forcing term(s) inside Adams-Bashforth:
          IF ( tracForcingOutAB.NE.1 ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              gTracer(i,j,k) = gTracer(i,j,k) + gTrForc(i,j)
             ENDDO
            ENDDO
          ENDIF

C-    If using Adams-Bashforth II, then extrapolate tendencies
C     gTracer is now the tracer tendency for explicit advection/diffusion

C     If matrix is being computed, skip call to S/R ADAMS_BASHFORTH2 to
C     prevent gTracer from being replaced by the average of gTracer and gpTrNm1.
          IF ( .NOT.useMATRIX .AND.
     &         PTRACERS_AdamsBashGtr(iTracer) ) THEN
           CALL ADAMS_BASHFORTH2(
     I                      bi, bj, k, Nr,
     U                      gTracer,
     U                      gpTrNm1(1-OLx,1-OLy,1,bi,bj,iTracer),
     O                      gTr_AB,
     I                      PTRACERS_startAB(iTracer), iterNb, myThid )
#ifdef ALLOW_DIAGNOSTICS
           IF ( diagAB_tend ) THEN
             diagName = 'AB_g'//diagSufx
             CALL DIAGNOSTICS_FILL(gTr_AB,diagName,k,1,2,bi,bj,myThid)
           ENDIF
#endif /* ALLOW_DIAGNOSTICS */
          ENDIF

C-    External forcing term(s) outside Adams-Bashforth:
          IF ( tracForcingOutAB.EQ.1 ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              gTracer(i,j,k) = gTracer(i,j,k) + gTrForc(i,j)
             ENDDO
            ENDDO
          ENDIF

#ifdef NONLIN_FRSURF
C-    Account for change in level thickness
          IF (nonlinFreeSurf.GT.0) THEN
           CALL FREESURF_RESCALE_G(
     I                           bi, bj, k,
     U                           gTracer,
     I                           myThid )
           IF ( PTRACERS_AdamsBashGtr(iTracer) )
     &     CALL FREESURF_RESCALE_G(
     I                           bi, bj, k,
     U                           gpTrNm1(1-OLx,1-OLy,1,bi,bj,iTracer),
     I                           myThid )
          ENDIF
#endif /* NONLIN_FRSURF */

C-    end of vertical index (k) loop (Nr:1)
        ENDDO

#ifdef ALLOW_DOWN_SLOPE
        IF ( PTRACERS_useDWNSLP(iTracer) ) THEN
          IF ( usingPCoords ) THEN
            CALL DWNSLP_APPLY(
     I                  GAD_TR, bi, bj, kSurfC,
     I                  pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     U                  gTracer,
     I                  recip_hFac, recip_rA, recip_drF,
     I                  PTRACERS_dTLev, myTime, myIter, myThid )
          ELSE
            CALL DWNSLP_APPLY(
     I                  GAD_TR, bi, bj, kLowC,
     I                  pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     U                  gTracer,
     I                  recip_hFac, recip_rA, recip_drF,
     I                  PTRACERS_dTLev, myTime, myIter, myThid )
          ENDIF
        ENDIF
#endif /* ALLOW_DOWN_SLOPE */

C-    Integrate forward in time, storing in gTracer:  gTr <= pTr + dt*gTr
        CALL TIMESTEP_TRACER(
     I                bi, bj, PTRACERS_dTLev,
     I                pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     U                gTracer,
     I                myTime, myIter, myThid )

C     All explicit advection/diffusion/sources should now be
C     done. The updated tracer field is in gTracer.
#ifdef ALLOW_MATRIX
C       Accumalate explicit tendency and also reset gTracer to initial
C       tracer field for implicit matrix calculation
        IF ( useMATRIX ) THEN
          CALL MATRIX_STORE_TENDENCY_EXP(
     I                      iTracer, bi, bj,
     U                      gTracer,
     I                      myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_MATRIX */

C--   Vertical advection & diffusion (implicit) for passive tracers

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gTracer(:,:,:) = comlev1_bibj_ptracers,
CADJ &     key=iptrkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef INCLUDE_IMPLVERTADV_CODE
        IF ( PTRACERS_ImplVertAdv(iTracer) .OR. implicitDiffusion ) THEN
C      to recover older (prior to 2016-10-05) results:
c       IF ( PTRACERS_ImplVertAdv(iTracer) ) THEN
          CALL GAD_IMPLICIT_R(
     I         PTRACERS_ImplVertAdv(iTracer),
     I         PTRACERS_advScheme(iTracer), GAD_TR,
     I         PTRACERS_dTLev, kappaRk, recip_hFac, wFld,
     I         pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     U         gTracer,
     I         bi, bj, myTime, myIter, myThid )

        ELSEIF ( implicitDiffusion ) THEN
#else /* INCLUDE_IMPLVERTADV_CODE */
        IF ( implicitDiffusion ) THEN
#endif /* INCLUDE_IMPLVERTADV_CODE */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         GAD_TR, kappaRk, recip_hFac,
     U         gTracer,
     I         myThid )
        ENDIF

#ifdef ALLOW_DIAGNOSTICS
        IF ( diagTrp_tend ) THEN
         diagName = 'Tp_g'//diagSufx
         DO k=1,Nr
          IF ( PTRACERS_dTLev(k).NE.zeroRL ) THEN
            recip_dt = oneRL/PTRACERS_dTLev(k)
          ELSE
            recip_dt = 0. _d 0
          ENDIF
          DO j=1,sNy
           DO i=1,sNx
            gTr_trp(i,j) = ( gTracer(i,j,k)
     &                     - pTracer(i,j,k,bi,bj,iTracer) )*recip_dt
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL( gTr_trp,diagName,k,1,2,bi,bj,myThid )
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        IF ( PTRACERS_AdamsBash_Tr(iTracer) ) THEN
C--   Save current tracer field (for AB on tracer) and then update tracer
          CALL CYCLE_AB_TRACER(
     I               bi, bj, gTracer,
     U               pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     O               gpTrNm1(1-OLx,1-OLy,1,bi,bj,iTracer),
     I               myTime, myIter, myThid )
        ELSE
C--   Update tracer fields:  pTr(n) = pTr**
          CALL CYCLE_TRACER(
     I               bi, bj,
     O               pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     I               gTracer, myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_OBCS
C--   Apply open boundary conditions for each passive tracer
        IF ( useOBCS ) THEN
          CALL OBCS_APPLY_PTRACER(
     I         bi, bj, 0, iTracer,
     U         pTracer(1-OLx,1-OLy,1,bi,bj,iTracer),
     I         myThid )
        ENDIF
#endif /* ALLOW_OBCS */

C--   end of tracer loop
       ENDIF
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
