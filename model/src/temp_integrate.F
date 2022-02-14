#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: TEMP_INTEGRATE
C     !INTERFACE:
      SUBROUTINE TEMP_INTEGRATE(
     I           bi, bj, recip_hFac,
     I           uFld, vFld, wFld,
     U           KappaRk,
     I           myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE TEMP_INTEGRATE
C     | o Calculate tendency for temperature and integrates
C     |   forward in time. The temperature array is updated here
C     |   while adjustments (filters, conv.adjustment) are applied
C     |   later, in S/R TRACERS_CORRECTION_STEP.
C     *==========================================================*
C     | A procedure called APPLY_FORCING_T is called from
C     | here. These procedures can be used to add per problem
C     | heat flux source terms.
C     | Note: Although it is slightly counter-intuitive the
C     |       EXTERNAL_FORCING routine is not the place to put
C     |       file I/O. Instead files that are required to
C     |       calculate the external source terms are generally
C     |       read during the model main loop. This makes the
C     |       logistics of multi-processing simpler and also
C     |       makes the adjoint generation simpler. It also
C     |       allows for I/O to overlap computation where that
C     |       is supported by hardware.
C     | Aside from the problem specific term the code here
C     | forms the tendency terms due to advection and mixing
C     | The baseline implementation here uses a centered
C     | difference form for the advection term and a tensorial
C     | divergence of a flux form for the diffusive term. The
C     | diffusive term is formulated so that isopycnal mixing
C     | and GM-style subgrid-scale terms can be incorporated by
C     | simply setting the diffusion tensor terms appropriately.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "RESTART.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
# include "GAD_SOM_VARS.h"
#endif
#ifdef ALLOW_TIMEAVE
# include "TIMEAVE_STATV.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj,    :: tile indices
C     recip_hFac :: reciprocal of cell open-depth factor (@ next iter)
C     uFld,vFld  :: Local copy of horizontal velocity field
C     wFld       :: Local copy of vertical velocity field
C     KappaRk    :: Vertical diffusion for Tempertature
C     myTime     :: current time
C     myIter     :: current iteration number
C     myThid     :: my Thread Id. number
      INTEGER bi, bj
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL uFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRk   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GENERIC_ADVDIFF
#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !LOCAL VARIABLES:
C     iMin, iMax :: 1rst index loop range
C     jMin, jMax :: 2nd  index loop range
C     k          :: vertical index
C     kM1        :: =k-1 for k>1, =1 for k=1
C     kUp        :: index into 2 1/2D array, toggles between 1|2
C     kDown      :: index into 2 1/2D array, toggles between 2|1
C     xA         :: Tracer cell face area normal to X
C     yA         :: Tracer cell face area normal to X
C     maskUp     :: Land/water mask for Wvel points (interface k)
C     uTrans     :: Zonal volume transport through cell face
C     vTrans     :: Meridional volume transport through cell face
C     rTrans     ::   Vertical volume transport at interface k
C     rTransKp   :: Vertical volume transport at inteface k+1
C     fZon       :: Flux of temperature (T) in the zonal direction
C     fMer       :: Flux of temperature (T) in the meridional direction
C     fVer       :: Flux of temperature (T) in the vertical direction
C                   at the upper(U) and lower(D) faces of a cell.
C     gT_loc     :: Temperature tendency (local to this S/R)
C     gtForc     :: Temperature forcing tendency
C     gt_AB      :: Adams-Bashforth temperature tendency increment
C   useVariableK :: T when vertical diffusion is not constant
      INTEGER iMin, iMax, jMin, jMax
      INTEGER i, j, k
      INTEGER kUp, kDown, kM1
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
      _RL gT_loc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gtForc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gt_AB   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL diagForcing, diagAB_tend
#endif
      LOGICAL calcAdvection
      INTEGER iterNb
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER m2
#endif
#ifdef ALLOW_TIMEAVE
      LOGICAL useVariableK
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER itdkey, kkey
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      iterNb = myIter
      IF (staggerTimeStep) iterNb = myIter - 1

C-    Loop ranges for daughter routines:
c     iMin = 1
c     iMax = sNx
c     jMin = 1
c     jMax = sNy
C     Regarding model dynamics, only needs to get correct tracer tendency
C     (gT_loc) in tile interior (1:sNx,1:sNy);
C     However, for some diagnostics, we may want to get valid tendency
C     extended over 1 point in tile halo region (0:sNx+1,0:sNy=1).
      iMin = 0
      iMax = sNx+1
      jMin = 0
      jMax = sNy+1

#ifdef ALLOW_DIAGNOSTICS
      diagForcing = .FALSE.
      diagAB_tend = .FALSE.
      IF ( useDiagnostics .AND. tempForcing )
     &     diagForcing = DIAGNOSTICS_IS_ON( 'gT_Forc ', myThid )
      IF ( useDiagnostics .AND. AdamsBashforthGt )
     &     diagAB_tend = DIAGNOSTICS_IS_ON( 'AB_gT   ', myThid )
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      itdkey = (act1 + 1) + act2*max1
     &                    + act3*max1*max2
     &                    + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C-    Apply AB on T :
      IF ( AdamsBashforth_T ) THEN
C     compute T^n+1/2 (stored in gtNm) extrapolating T forward in time
#ifdef ALLOW_ADAMSBASHFORTH_3
c         m1 = 1 + MOD(iterNb+1,2)
c         m2 = 1 + MOD( iterNb ,2)
          CALL ADAMS_BASHFORTH3(
     I                           bi, bj, 0, Nr,
     I                           theta(1-OLx,1-OLy,1,bi,bj),
     U                           gtNm, gt_AB,
     I                           tempStartAB, iterNb, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
          CALL ADAMS_BASHFORTH2(
     I                           bi, bj, 0, Nr,
     I                           theta(1-OLx,1-OLy,1,bi,bj),
     U                           gtNm1(1-OLx,1-OLy,1,bi,bj), gt_AB,
     I                           tempStartAB, iterNb, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      ENDIF

C-    Tracer tendency needs to be set to zero (moved here from gad_calc_rhs):
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         gT_loc(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wFld(:,:,:)         = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE theta(:,:,:,bi,bj)  = comlev1_bibj , key=itdkey, byte=isbyte
# ifdef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gtNm(:,:,:,bi,bj,1) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE gtNm(:,:,:,bi,bj,2) = comlev1_bibj, key=itdkey, byte=isbyte
# else
CADJ STORE gtNm1(:,:,:,bi,bj)  = comlev1_bibj, key=itdkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef INCLUDE_CALC_DIFFUSIVITY_CALL
      CALL CALC_3D_DIFFUSIVITY(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         GAD_TEMPERATURE, useGMredi, useKPP,
     O         kappaRk,
     I         myThid )
#endif /* INCLUDE_CALC_DIFFUSIVITY_CALL */

#ifndef DISABLE_MULTIDIM_ADVECTION
C--     Some advection schemes are better calculated using a multi-dimensional
C       method in the absence of any other terms and, if used, is done here.
C
C The CPP flag DISABLE_MULTIDIM_ADVECTION is currently unset in GAD_OPTIONS.h
C The default is to use multi-dimensinal advection for non-linear advection
C schemes. However, for the sake of efficiency of the adjoint it is necessary
C to be able to exclude this scheme to avoid excessive storage and
C recomputation. It *is* differentiable, if you need it.
C Edit GAD_OPTIONS.h and #define DISABLE_MULTIDIM_ADVECTION to
C disable this section of code.
#ifdef GAD_ALLOW_TS_SOM_ADV
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE som_T = comlev1_bibj, key=itdkey, byte=isbyte
# endif
      IF ( tempSOM_Advection ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GAD_SOM_ADVECT',myThid)
# endif
        CALL GAD_SOM_ADVECT(
     I             tempImplVertAdv,
     I             tempAdvScheme, tempVertAdvScheme, GAD_TEMPERATURE,
     I             dTtracerLev, uFld, vFld, wFld, theta,
     U             som_T,
     O             gT_loc,
     I             bi, bj, myTime, myIter, myThid )
      ELSEIF (tempMultiDimAdvec) THEN
#else /* GAD_ALLOW_TS_SOM_ADV */
      IF (tempMultiDimAdvec) THEN
#endif /* GAD_ALLOW_TS_SOM_ADV */
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GAD_ADVECTION',myThid)
# endif
        CALL GAD_ADVECTION(
     I             tempImplVertAdv,
     I             tempAdvScheme, tempVertAdvScheme, GAD_TEMPERATURE,
     I             dTtracerLev, uFld, vFld, wFld, theta,
     O             gT_loc,
     I             bi, bj, myTime, myIter, myThid )
      ENDIF
#endif /* DISABLE_MULTIDIM_ADVECTION */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Start vertical index (k) loop (Nr:1)
      calcAdvection = tempAdvection .AND. .NOT.tempMultiDimAdvec
      DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC
        kkey = (itdkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */
        kM1  = MAX(1,k-1)
        kUp  = 1+MOD(k+1,2)
        kDown= 1+MOD(k,2)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fVer(:,:,:) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
CADJ STORE gT_loc(:,:,k) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
# ifdef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gtNm(:,:,k,bi,bj,1) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
CADJ STORE gtNm(:,:,k,bi,bj,2) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
# else
CADJ STORE gtNm1(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */
        CALL CALC_ADV_FLOW(
     I                uFld, vFld, wFld,
     U                rTrans,
     O                uTrans, vTrans, rTransKp,
     O                maskUp, xA, yA,
     I                k, bi, bj, myThid )

C--   Collect forcing term in local array gtForc:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          gtForc(i,j) = 0. _d 0
         ENDDO
        ENDDO
        IF ( tempForcing ) THEN
          CALL APPLY_FORCING_T(
     U                        gtForc,
     I                        iMin,iMax,jMin,jMax, k, bi,bj,
     I                        myTime, myIter, myThid )
#ifdef ALLOW_DIAGNOSTICS
          IF ( diagForcing ) THEN
            CALL DIAGNOSTICS_FILL(gtForc,'gT_Forc ',k,1,2,bi,bj,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF

#ifdef ALLOW_ADAMSBASHFORTH_3
c       m1 = 1 + MOD(iterNb+1,2)
        m2 = 1 + MOD( iterNb ,2)
        CALL GAD_CALC_RHS(
     I           bi, bj, iMin,iMax,jMin,jMax, k, kM1, kUp, kDown,
     I           xA, yA, maskUp, uFld(1-OLx,1-OLy,k),
     I           vFld(1-OLx,1-OLy,k), wFld(1-OLx,1-OLy,k),
     I           uTrans, vTrans, rTrans, rTransKp,
     I           diffKhT, diffK4T, KappaRk(1-OLx,1-OLy,k), diffKr4T,
     I           theta(1-OLx,1-OLy,1,bi,bj),
     I           gtNm(1-OLx,1-OLy,1,bi,bj,m2), dTtracerLev,
     I           GAD_TEMPERATURE, tempAdvScheme, tempVertAdvScheme,
     I           calcAdvection, tempImplVertAdv, AdamsBashforth_T,
     I           tempVertDiff4, useGMRedi, useKPP, temp_stayPositive,
     O           fZon, fMer,
     U           fVer, gT_loc,
     I           myTime, myIter, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL GAD_CALC_RHS(
     I           bi, bj, iMin,iMax,jMin,jMax, k, kM1, kUp, kDown,
     I           xA, yA, maskUp, uFld(1-OLx,1-OLy,k),
     I           vFld(1-OLx,1-OLy,k), wFld(1-OLx,1-OLy,k),
     I           uTrans, vTrans, rTrans, rTransKp,
     I           diffKhT, diffK4T, KappaRk(1-OLx,1-OLy,k), diffKr4T,
     I           theta(1-OLx,1-OLy,1,bi,bj),
     I           gtNm1(1-OLx,1-OLy,1,bi,bj), dTtracerLev,
     I           GAD_TEMPERATURE, tempAdvScheme, tempVertAdvScheme,
     I           calcAdvection, tempImplVertAdv, AdamsBashforth_T,
     I           tempVertDiff4, useGMRedi, useKPP, temp_stayPositive,
     O           fZon, fMer,
     U           fVer, gT_loc,
     I           myTime, myIter, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */

C--   External thermal forcing term(s) inside Adams-Bashforth:
        IF ( tempForcing .AND. tracForcingOutAB.NE.1 ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gT_loc(i,j,k) = gT_loc(i,j,k) + gtForc(i,j)
           ENDDO
          ENDDO
        ENDIF

        IF ( AdamsBashforthGt ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
          CALL ADAMS_BASHFORTH3(
     I                          bi, bj, k, Nr,
     U                          gT_loc, gtNm,
     O                          gt_AB,
     I                          tempStartAB, iterNb, myThid )
#else
          CALL ADAMS_BASHFORTH2(
     I                          bi, bj, k, Nr,
     U                          gT_loc, gtNm1(1-OLx,1-OLy,1,bi,bj),
     O                          gt_AB,
     I                          tempStartAB, iterNb, myThid )
#endif
#ifdef ALLOW_DIAGNOSTICS
          IF ( diagAB_tend ) THEN
            CALL DIAGNOSTICS_FILL(gt_AB,'AB_gT   ',k,1,2,bi,bj,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF

C--   External thermal forcing term(s) outside Adams-Bashforth:
        IF ( tempForcing .AND. tracForcingOutAB.EQ.1 ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gT_loc(i,j,k) = gT_loc(i,j,k) + gtForc(i,j)
           ENDDO
          ENDDO
        ENDIF

#ifdef NONLIN_FRSURF
        IF (nonlinFreeSurf.GT.0) THEN
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gT_loc,
     I                            myThid )
         IF ( AdamsBashforthGt ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gtNm(:,:,k,bi,bj,1) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
CADJ STORE gtNm(:,:,k,bi,bj,2) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
# endif
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gtNm(1-OLx,1-OLy,1,bi,bj,1),
     I                            myThid )
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gtNm(1-OLx,1-OLy,1,bi,bj,2),
     I                            myThid )
#else
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gtNm1(1-OLx,1-OLy,1,bi,bj),
     I                            myThid )
#endif
         ENDIF
        ENDIF
#endif /* NONLIN_FRSURF */

C-    end of vertical index (k) loop (Nr:1)
      ENDDO

#ifdef ALLOW_DOWN_SLOPE
      IF ( useDOWN_SLOPE ) THEN
        IF ( usingPCoords ) THEN
          CALL DWNSLP_APPLY(
     I                  GAD_TEMPERATURE, bi, bj, kSurfC,
     I                  theta(1-OLx,1-OLy,1,bi,bj),
     U                  gT_loc,
     I                  recip_hFac, recip_rA, recip_drF,
     I                  dTtracerLev, myTime, myIter, myThid )
        ELSE
          CALL DWNSLP_APPLY(
     I                  GAD_TEMPERATURE, bi, bj, kLowC,
     I                  theta(1-OLx,1-OLy,1,bi,bj),
     U                  gT_loc,
     I                  recip_hFac, recip_rA, recip_drF,
     I                  dTtracerLev, myTime, myIter, myThid )
        ENDIF
      ENDIF
#endif /* ALLOW_DOWN_SLOPE */

C-    Integrate forward in time, storing in gT_loc:  gT <= T + dt*gT
      CALL TIMESTEP_TRACER(
     I                  bi, bj, dTtracerLev,
     I                  theta(1-OLx,1-OLy,1,bi,bj),
     U                  gT_loc,
     I                  myTime, myIter, myThid )

C--   Implicit vertical advection & diffusion

#ifdef INCLUDE_IMPLVERTADV_CODE
      IF ( tempImplVertAdv .OR. implicitDiffusion ) THEN
C     to recover older (prior to 2016-10-05) results:
c     IF ( tempImplVertAdv ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRk(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE gT_loc(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE wFld(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE recip_hFac(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        CALL GAD_IMPLICIT_R(
     I         tempImplVertAdv, tempVertAdvScheme, GAD_TEMPERATURE,
     I         dTtracerLev, kappaRk, recip_hFac, wFld,
     I         theta(1-OLx,1-OLy,1,bi,bj),
     U         gT_loc,
     I         bi, bj, myTime, myIter, myThid )
      ELSEIF ( implicitDiffusion ) THEN
#else /* INCLUDE_IMPLVERTADV_CODE */
      IF     ( implicitDiffusion ) THEN
#endif /* INCLUDE_IMPLVERTADV_CODE */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRk(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE gT_loc(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         GAD_TEMPERATURE, kappaRk, recip_hFac,
     U         gT_loc,
     I         myThid )
      ENDIF

#ifdef ALLOW_TIMEAVE
      useVariableK = useKPP .OR. usePP81 .OR. useKL10 .OR. useMY82
     &       .OR. useGGL90 .OR. useGMredi .OR. ivdc_kappa.NE.0.
      IF ( taveFreq.GT.0. .AND. useVariableK
     &                    .AND.implicitDiffusion ) THEN
        CALL TIMEAVE_CUMUL_DIF_1T( TdiffRtave,
     I                        gT_loc, kappaRk,
     I                        Nr, 3, deltaTClock, bi, bj, myThid )
      ENDIF
#endif /* ALLOW_TIMEAVE */

      IF ( AdamsBashforth_T ) THEN
C-    Save current tracer field (for AB on tracer) and then update tracer
#ifdef ALLOW_ADAMSBASHFORTH_3
        CALL CYCLE_AB_TRACER(
     I             bi, bj, gT_loc,
     U             theta(1-OLx,1-OLy,1,bi,bj),
     O             gtNm(1-OLx,1-OLy,1,bi,bj,m2),
     I             myTime, myIter, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL CYCLE_AB_TRACER(
     I             bi, bj, gT_loc,
     U             theta(1-OLx,1-OLy,1,bi,bj),
     O             gtNm1(1-OLx,1-OLy,1,bi,bj),
     I             myTime, myIter, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      ELSE
C-    Update tracer fields:  T(n) = T**
        CALL CYCLE_TRACER(
     I             bi, bj,
     O             theta(1-OLx,1-OLy,1,bi,bj),
     I             gT_loc, myTime, myIter, myThid )
      ENDIF

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
