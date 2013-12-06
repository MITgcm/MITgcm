C $Header: /u/gcmpack/MITgcm/model/src/temp_integrate.F,v 1.4 2013/12/06 01:55:42 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: TEMP_INTEGRATE
C     !INTERFACE:
      SUBROUTINE TEMP_INTEGRATE(
     I           bi, bj,
     I           uFld, vFld, wFld,
     U           KappaRk,
     I           myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE TEMP_INTEGRATE
C     | o Calculate tendency for temperature
C     |   and integrates forward in time.
C     *==========================================================*
C     | A procedure called EXTERNAL_FORCING_T is called from
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
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
# include "GAD_SOM_VARS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj,   :: tile indices
C     uFld,vFld :: Local copy of horizontal velocity field
C     wFld      :: Local copy of vertical velocity field
C     KappaRk   :: Vertical diffusion for Tempertature
C     myTime    :: current time
C     myIter    :: current iteration number
C     myThid    :: my Thread Id. number
      INTEGER bi, bj
      _RL uFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     iMin,iMax :: Range of points for which calculation
C     jMin,jMax ::       results will be set.
C     k         :: vertical index
C     kM1       :: =k-1 for k>1, =1 for k=1
C     kUp       :: index into 2 1/2D array, toggles between 1|2
C     kDown     :: index into 2 1/2D array, toggles between 2|1
C     xA        :: Tracer cell face area normal to X
C     yA        :: Tracer cell face area normal to X
C     maskUp    :: Land/water mask for Wvel points (interface k)
C     uTrans    :: Zonal volume transport through cell face
C     vTrans    :: Meridional volume transport through cell face
C     rTrans    ::   Vertical volume transport at interface k
C     rTransKp  :: Vertical volume transport at inteface k+1
C     fVerT     :: Flux of temperature (T) in the vertical direction
C                  at the upper(U) and lower(D) faces of a cell.
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
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL gt_AB   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL calcAdvection
      INTEGER iterNb
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER m1, m2
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Loop ranges for daughter routines
      iMin = 1-OLx+2
      iMax = sNx+OLx-1
      jMin = 1-OLy+2
      jMax = sNy+OLy-1

      iterNb = myIter
      IF (staggerTimeStep) iterNb = myIter -1

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          itdkey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C-    Tracer tendency needs to be set to zero (moved here from gad_calc_rhs):
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         gT(i,j,k,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
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

#ifdef INCLUDE_CALC_DIFFUSIVITY_CALL
      IF ( .NOT. implicitDiffusion ) THEN
        CALL CALC_3D_DIFFUSIVITY(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         GAD_TEMPERATURE, useGMredi, useKPP,
     O         kappaRk,
     I         myThid )
      ENDIF
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
     I             tempImplVertAdv, tempAdvScheme, tempVertAdvScheme,
     I             GAD_TEMPERATURE, dTtracerLev,
     I             uFld, vFld, wFld, theta,
     U             som_T,
     O             gT,
     I             bi, bj, myTime, myIter, myThid )
      ELSEIF (tempMultiDimAdvec) THEN
#else /* GAD_ALLOW_TS_SOM_ADV */
      IF (tempMultiDimAdvec) THEN
#endif /* GAD_ALLOW_TS_SOM_ADV */
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GAD_ADVECTION',myThid)
# endif
        CALL GAD_ADVECTION(
     I             tempImplVertAdv, tempAdvScheme, tempVertAdvScheme,
     I             GAD_TEMPERATURE, dTtracerLev,
     I             uFld, vFld, wFld, theta,
     O             gT,
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
CADJ STORE fVerT(:,:,:) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
CADJ STORE gT(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey,
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

#ifdef ALLOW_ADAMSBASHFORTH_3
        m1 = 1 + MOD(iterNb+1,2)
        m2 = 1 + MOD( iterNb ,2)
        CALL GAD_CALC_RHS(
     I           bi, bj, iMin,iMax,jMin,jMax, k, kM1, kUp, kDown,
     I           xA, yA, maskUp, uFld(1-OLx,1-OLy,k),
     I           vFld(1-OLx,1-OLy,k), wFld(1-OLx,1-OLy,k),
     I           uTrans, vTrans, rTrans, rTransKp,
     I           diffKhT, diffK4T, KappaRk(1-OLx,1-OLy,k), diffKr4T,
     I           gtNm(1-OLx,1-OLy,1,1,1,m2), theta, dTtracerLev,
     I           GAD_TEMPERATURE, tempAdvScheme, tempVertAdvScheme,
     I           calcAdvection, tempImplVertAdv, AdamsBashforth_T,
     I           tempVertDiff4, useGMRedi, useKPP,
     U           fVerT, gT,
     I           myTime, myIter, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL GAD_CALC_RHS(
     I           bi, bj, iMin,iMax,jMin,jMax, k, kM1, kUp, kDown,
     I           xA, yA, maskUp, uFld(1-OLx,1-OLy,k),
     I           vFld(1-OLx,1-OLy,k), wFld(1-OLx,1-OLy,k),
     I           uTrans, vTrans, rTrans, rTransKp,
     I           diffKhT, diffK4T, KappaRk(1-OLx,1-OLy,k), diffKr4T,
     I           gtNm1, theta, dTtracerLev,
     I           GAD_TEMPERATURE, tempAdvScheme, tempVertAdvScheme,
     I           calcAdvection, tempImplVertAdv, AdamsBashforth_T,
     I           tempVertDiff4, useGMRedi, useKPP,
     U           fVerT, gT,
     I           myTime, myIter, myThid )
#endif

C--   External thermal forcing term(s) inside Adams-Bashforth:
        IF ( tempForcing .AND. tracForcingOutAB.NE.1 )
     &    CALL EXTERNAL_FORCING_T(
     I         iMin, iMax, jMin, jMax, bi, bj, k,
     I         myTime, myThid )

        IF ( AdamsBashforthGt ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
          CALL ADAMS_BASHFORTH3(
     I                          bi, bj, k, Nr,
     U                          gT, gtNm, gt_AB,
     I                          tempStartAB, iterNb, myThid )
#else
          CALL ADAMS_BASHFORTH2(
     I                          bi, bj, k, Nr,
     U                          gT, gtNm1, gt_AB,
     I                          tempStartAB, iterNb, myThid )
#endif
#ifdef ALLOW_DIAGNOSTICS
          IF ( useDiagnostics ) THEN
            CALL DIAGNOSTICS_FILL(gt_AB,'AB_gT   ',k,1,2,bi,bj,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF

C--   External thermal forcing term(s) outside Adams-Bashforth:
        IF ( tempForcing .AND. tracForcingOutAB.EQ.1 )
     &    CALL EXTERNAL_FORCING_T(
     I         iMin, iMax, jMin, jMax, bi, bj, k,
     I         myTime, myThid )

#ifdef NONLIN_FRSURF
        IF (nonlinFreeSurf.GT.0) THEN
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gT,
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
     U                            gtNm(1-OLx,1-OLy,1,1,1,1),
     I                            myThid )
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gtNm(1-OLx,1-OLy,1,1,1,2),
     I                            myThid )
#else
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gtNm1,
     I                            myThid )
#endif
         ENDIF
        ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_ADAMSBASHFORTH_3
        IF ( AdamsBashforth_T ) THEN
          CALL TIMESTEP_TRACER(
     I           bi, bj, k, dTtracerLev(k),
     I           gtNm(1-OLx,1-OLy,1,1,1,m2),
     U           gT,
     I           myIter, myThid )
        ELSE
#endif
          CALL TIMESTEP_TRACER(
     I           bi, bj, k, dTtracerLev(k),
     I           theta,
     U           gT,
     I           myIter, myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
        ENDIF
#endif

C-    end of vertical index (k) loop (Nr:1)
      ENDDO

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
