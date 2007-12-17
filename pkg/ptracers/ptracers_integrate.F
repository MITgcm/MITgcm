C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_integrate.F,v 1.36 2007/12/17 22:03:15 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_INTEGRATE

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_INTEGRATE(
     I                    bi,bj,k,
     I                    xA, yA, maskUp, uFld, vFld, wFld,
     I                    uTrans, vTrans, rTrans, rTransKp1,
     I                    KappaRtr,
     U                    rFlx,
     I                    myTime,myIter,myThid )

C !DESCRIPTION:
C     Calculates tendency for passive tracers and integrates forward
C     in time.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_RESTART.h"
#include "PTRACERS_FIELDS.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level number
C  xA                   :: face area at U points in level k
C  yA                   :: face area at V points in level k
C  maskUp               :: mask for vertical transport
C  uFld,vFld,wFld       :: Local copy of velocity field (3 components)
C  uTrans               :: zonal transport in level k
C  vTrans               :: meridional transport in level k
C  rTrans               :: vertical volume transport at interface k
C  rTransKp1            :: vertical volume transport at interface k+1
C  KappaRtr             :: vertical diffusion of passive tracers, interf k
C  rFlx                 :: vertical flux
C  myTime               :: model time
C  myIter               :: time-step number
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRtr(1-Olx:sNx+Olx,1-Oly:sNy+Oly,PTRACERS_num)
      _RL rFlx    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,PTRACERS_num)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer              :: tracer index
C  iMin,iMax,jMin,jMax  :: loop ranges
C  kUp,kDown            :: toggle indices for even/odd level fluxes
C  km1                  :: =min(1,k-1)
C  GAD_TR               :: passive tracer id (GAD_TR1+iTracer-1)
      INTEGER iTracer
      INTEGER iMin,iMax,jMin,jMax
      INTEGER kUp,kDown,km1
      INTEGER GAD_TR
      LOGICAL calcAdvection
      INTEGER iterNb
CEOP

C Loop ranges for daughter routines
      iMin = 1-OLx+2
      iMax = sNx+OLx-1
      jMin = 1-OLy+2
      jMax = sNy+OLy-1

      km1  = MAX(1,k-1)
      kUp  = 1+MOD(k+1,2)
      kDown= 1+MOD(k,2)

C Loop over tracers
      DO iTracer=1,PTRACERS_numInUse

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
          kkey = (iptrkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
      rFlx(1,1,kDown,iTracer) = rFlx(1,1,kDown,iTracer)
c
CADJ STORE pTracer(:,:,k,bi,bj,iTracer)
CADJ &      = comlev1_bibj_k_ptracers, key=kkey, byte=isbyte
CADJ STORE gpTrNm1(:,:,k,bi,bj,iTracer)
CADJ &      = comlev1_bibj_k_ptracers, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C Calculate active tracer tendencies (gPtr) due to internal processes
C (advection, [explicit] diffusion, parameterizations,...)
       calcAdvection = .NOT.PTRACERS_MultiDimAdv(iTracer)
       GAD_TR = GAD_TR1 + iTracer - 1
       CALL GAD_CALC_RHS(
     I                   bi,bj,iMin,iMax,jMin,jMax,k,km1,kUp,kDown,
     I                   xA, yA, maskUp, uFld, vFld, wFld,
     I                   uTrans, vTrans, rTrans, rTransKp1,
     I                   PTRACERS_diffKh(iTracer),
     I                   PTRACERS_diffK4(iTracer),
     I                   KappaRtr(1-Olx,1-Oly,iTracer),
     I                   gpTrNm1(1-Olx,1-Oly,1,1,1,iTracer),
     I                   pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     I                   GAD_TR,
     I                   PTRACERS_advScheme(iTracer),
     I                   PTRACERS_advScheme(iTracer),
     I                   calcAdvection, PTRACERS_ImplVertAdv(iTracer),
     I                   .FALSE.,
     I                   PTRACERS_useGMRedi(iTracer),
     I                   PTRACERS_useKPP(iTracer),
     U                   rFlx(1-Olx,1-Oly,1,iTracer),
     U                   gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                   myTime, myIter, myThid )

C External forcing term(s)
       IF ( tracForcingOutAB.NE.1 )
     &   CALL PTRACERS_FORCING(
     I                      bi,bj,iMin,iMax,jMin,jMax,k,iTracer,
     U                      gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                      surfaceForcingPTr(1-Olx,1-Oly,1,1,iTracer),
     I                      myIter,myTime,myThid)

C If using Adams-Bashforth II, then extrapolate tendencies
C gPtr is now the tracer tendency for explicit advection/diffusion
      IF ( PTRACERS_AdamsBashGtr(iTracer) ) THEN
#ifdef ALLOW_MATRIX
C  If matrix is being computed, block call to S/R ADAMS_BASHFORTH2 to
C  prevent gPtr from being replaced by the average of gPtr and gpTrNm1.
        IF (.NOT.useMATRIX) THEN
#endif
        iterNb = myIter
        IF (staggerTimeStep) iterNb = myIter - 1
        CALL ADAMS_BASHFORTH2(
     I                      bi,bj,K,
     U                      gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     U                      gpTrNm1(1-Olx,1-Oly,1,1,1,iTracer),
     I                      PTRACERS_startAB(iTracer), iterNb, myThid )
#ifdef ALLOW_MATRIX
        ENDIF
#endif
      ENDIF

C External forcing term(s)
       IF ( tracForcingOutAB.EQ.1 )
     &   CALL PTRACERS_FORCING(
     I                      bi,bj,iMin,iMax,jMin,jMax,k,iTracer,
     U                      gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                      surfaceForcingPTr(1-Olx,1-Oly,1,1,iTracer),
     I                      myIter,myTime,myThid)

#ifdef NONLIN_FRSURF
C Account for change in level thickness
      IF (nonlinFreeSurf.GT.0) THEN
        CALL FREESURF_RESCALE_G(
     I                          bi,bj,K,
     U                          gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                          myThid )
        IF ( PTRACERS_AdamsBashGtr(iTracer) ) THEN
     &  CALL FREESURF_RESCALE_G(
     I                          bi,bj,K,
     U                          gpTrNm1(1-Olx,1-Oly,1,1,1,iTracer),
     I                          myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

C Integrate forward in time, storing in gPtr:  G=T+dt*G
         CALL TIMESTEP_TRACER(
     I                        bi,bj,iMin,iMax,jMin,jMax,k,
     I                        PTRACERS_advScheme(iTracer),
     I                        pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     I                        gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I                        myIter,myThid )

#ifdef   ALLOW_OBCS
C Apply open boundary conditions
         IF (useOBCS) THEN
          CALL OBCS_APPLY_PTRACER(
     I         bi, bj, k, iTracer,
     U         gPtr(1-Olx,1-Oly,k,bi,bj,iTracer),
     I         myThid )
         ENDIF
#endif   /* ALLOW_OBCS */
C end of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
