C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_integrate.F,v 1.51 2013/12/06 01:56:52 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_INTEGRATE

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_INTEGRATE(
     I                    bi, bj,
     I                    uFld, vFld, wFld,
     U                    KappaRk,
     I                    myTime, myIter, myThid )

C !DESCRIPTION:
C     Calculates tendency for passive tracers and integrates forward
C     in time.

C !USES: ===============================================================
#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
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
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi, bj               :: tile indices
C  uFld, vFld, wFld     :: Local copy of velocity field (3 components)
C  KappaRk              :: vertical diffusion used for one passive tracer
C  myTime               :: model time
C  myIter               :: time-step number
C  myThid               :: thread number
      INTEGER bi, bj
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer              :: tracer index
C  iMin,iMax,jMin,jMax  :: loop ranges
C  k                    :: vertical level number
C  kUp,kDown            :: toggle indices for even/odd level fluxes
C  kM1                  :: =min(1,k-1)
C  GAD_TR               :: passive tracer id (GAD_TR1+iTracer-1)
C  xA                   :: face area at U points in level k
C  yA                   :: face area at V points in level k
C  maskUp               :: mask for vertical transport
C  uTrans               :: zonal transport in level k
C  vTrans               :: meridional transport in level k
C  rTrans               :: vertical volume transport at interface k
C  rTransKp             :: vertical volume transport at interface k+1
C  fVerT                :: passive tracer vertical flux
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
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL gTr_AB  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL calcAdvection
      INTEGER iterNb
      _RL dummy(Nr)
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
C-    Functions:
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif /* ALLOW_DIAGNOSTICS */
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Loop ranges for daughter routines
      iMin = 1-OLx+2
      iMax = sNx+OLx-1
      jMin = 1-OLy+2
      jMax = sNy+OLy-1

C-    Compute iter at beginning of ptracer time step
#ifdef ALLOW_LONGSTEP
      iterNb = myIter - LS_nIter + 1
      IF (LS_whenToSample.GE.2) iterNb = myIter - LS_nIter
#else
      iterNb = myIter
      IF (staggerTimeStep) iterNb = myIter - 1
#endif

C--   Loop over tracers
      DO iTracer=1,PTRACERS_numInUse

C-    Initialise tracer tendency to zero
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gPTr(i,j,k,bi,bj,iTracer) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
       IF ( PTRACERS_StepFwd(iTracer) ) THEN
        GAD_TR = GAD_TR1 + iTracer - 1

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

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           fVerT(i,j,1) = 0. _d 0
           fVerT(i,j,2) = 0. _d 0
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

        IF ( .NOT.implicitDiffusion ) THEN
          CALL CALC_3D_DIFFUSIVITY(
     I         bi, bj, iMin,iMax,jMin,jMax,
     I         GAD_TR,
     I         PTRACERS_useGMRedi(iTracer), PTRACERS_useKPP(iTracer),
     O         kappaRk,
     I         myThid)
        ENDIF

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
     I                       PTRACERS_ImplVertAdv(iTracer),
     I                       PTRACERS_advScheme(iTracer),
     I                       PTRACERS_advScheme(iTracer),
     I                       GAD_TR1+iTracer-1,
     I                       PTRACERS_dTLev,
     I                       uFld, vFld, wFld,
     I                       pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     U                       _Ptracers_som(:,:,:,:,:,:,iTracer),
     O                       gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I                       bi, bj, myTime, myIter, myThid )
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
     I                        GAD_TR1+iTracer-1,
     I                        PTRACERS_dTLev,
     I                        uFld, vFld, wFld,
     I                        pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     O                        gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I                        bi, bj, myTime, myIter, myThid )
        ENDIF
#endif /* DISABLE_MULTIDIM_ADVECTION */

C-    Start vertical index (k) loop (Nr:1)
        calcAdvection = .NOT.PTRACERS_MultiDimAdv(iTracer)
        DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = (iptrkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

          kM1  = MAX(1,k-1)
          kUp  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fVerT(:,:,:) = comlev1_bibj_k_ptracers,
CADJ &     key = kkey, byte = isbyte, kind = isbyte
CADJ STORE gPtr(:,:,k,bi,bj,iTracer) = comlev1_bibj_k_ptracers,
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

C-    Calculate active tracer tendencies (gPtr) due to internal processes
C      (advection, [explicit] diffusion, parameterizations,...)
          CALL GAD_CALC_RHS(
     I             bi,bj, iMin,iMax,jMin,jMax, k,kM1, kUp,kDown,
     I             xA, yA, maskUp, uFld(1-OLx,1-OLy,k),
     I             vFld(1-OLx,1-OLy,k), wFld(1-OLx,1-OLy,k),
     I             uTrans, vTrans, rTrans, rTransKp,
     I             PTRACERS_diffKh(iTracer),
     I             PTRACERS_diffK4(iTracer),
     I             KappaRk(1-OLx,1-OLy,k), dummy,
     I             gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     I             pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     I             PTRACERS_dTLev, GAD_TR,
     I             PTRACERS_advScheme(iTracer),
     I             PTRACERS_advScheme(iTracer),
     I             calcAdvection, PTRACERS_ImplVertAdv(iTracer),
     I             .FALSE., .FALSE.,
     I             PTRACERS_useGMRedi(iTracer),
     I             PTRACERS_useKPP(iTracer),
     U             fVerT, gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I             myTime, myIter, myThid )

C-    External forcing term(s)
          IF ( tracForcingOutAB.NE.1 )
     &      CALL PTRACERS_FORCING(
     I                    bi,bj,iMin,iMax,jMin,jMax,k,iTracer,
     U                    gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I                    surfaceForcingPTr(1-OLx,1-OLy,1,1,iTracer),
     I                    myIter, myTime, myThid )

C-    If using Adams-Bashforth II, then extrapolate tendencies
C     gPtr is now the tracer tendency for explicit advection/diffusion

C     If matrix is being computed, skip call to S/R ADAMS_BASHFORTH2 to
C     prevent gPtr from being replaced by the average of gPtr and gpTrNm1.
          IF ( .NOT.useMATRIX .AND.
     &         PTRACERS_AdamsBashGtr(iTracer) ) THEN

           CALL ADAMS_BASHFORTH2(
     I                      bi, bj, k, Nr,
     U                      gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     U                      gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     U                      gTr_AB,
     I                      PTRACERS_startAB(iTracer), iterNb, myThid )
#ifdef ALLOW_DIAGNOSTICS
           IF ( useDiagnostics ) THEN
             diagSufx = GAD_DIAG_SUFX( GAD_TR, myThid )
             diagName = 'AB_g'//diagSufx
             CALL DIAGNOSTICS_FILL(gTr_AB,diagName,k,1,2,bi,bj,myThid)
           ENDIF
#endif /* ALLOW_DIAGNOSTICS */
          ENDIF

C-    External forcing term(s)
          IF ( tracForcingOutAB.EQ.1 )
     &      CALL PTRACERS_FORCING(
     I                    bi,bj,iMin,iMax,jMin,jMax,k,iTracer,
     U                    gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I                    surfaceForcingPTr(1-OLx,1-OLy,1,1,iTracer),
     I                    myIter, myTime, myThid )

#ifdef NONLIN_FRSURF
C-    Account for change in level thickness
          IF (nonlinFreeSurf.GT.0) THEN
           CALL FREESURF_RESCALE_G(
     I                             bi,bj,k,
     U                             gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I                             myThid )
           IF ( PTRACERS_AdamsBashGtr(iTracer) )
     &     CALL FREESURF_RESCALE_G(
     I                             bi,bj,k,
     U                             gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     I                             myThid )
          ENDIF
#endif /* NONLIN_FRSURF */

C-    Integrate forward in time, storing in gPtr:  G=T+dt*G
          CALL TIMESTEP_TRACER(
     I                         bi, bj, k, PTRACERS_dTLev(k),
     I                         pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     U                         gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I                         myIter, myThid )

C-    end of vertical index (k) loop (Nr:1)
        ENDDO

C--   end of tracer loop
       ENDIF
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
