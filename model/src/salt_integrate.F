C $Header: /u/gcmpack/MITgcm/model/src/salt_integrate.F,v 1.2 2013/11/21 00:16:55 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_INTEGRATE
C     !INTERFACE:
      SUBROUTINE SALT_INTEGRATE(
     I           bi, bj, iMin, iMax, jMin, jMax,
     I           uFld, vFld, wFld, KappaRk,
     I           myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SALT_INTEGRATE
C     | o Calculate tendency for salt
C     |   and integrates forward in time.
C     *==========================================================*
C     | A procedure called EXTERNAL_FORCING_S is called from
C     | here. These procedures can be used to add per problem
C     | E-P  flux source terms.
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
C     | diffusive term is formulated so that isopycnal mixing and
C     | GM-style subgrid-scale terms can be incorporated by
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
#include "GAD.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj,   :: tile indices
C     iMin,iMax, jMin,jMax :: Range of points for which calculation
C                                       results will be set.
C     uFld,vFld :: Local copy of horizontal velocity field
C     wFld      :: Local copy of vertical velocity field
C     KappaRk   :: Vertical diffusion for Salinity
C     myTime    :: current time
C     myIter    :: current iteration number
C     myThid    :: my Thread Id. number

      INTEGER bi,bj,iMin,iMax,jMin,jMax
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
C     fVerS     :: Flux of salt (S) in the vertical direction
C                  at the upper(U) and lower(D) faces of a cell.
      INTEGER i, j, k
      INTEGER kUp, kDown, kM1
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL gs_AB   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL calcAdvection
      INTEGER iterNb
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER m1, m2
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
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      calcAdvection = saltAdvection .AND. .NOT.saltMultiDimAdvec
      iterNb = myIter
      IF (staggerTimeStep) iterNb = myIter - 1

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         fVerS(i,j,1) = 0. _d 0
         fVerS(i,j,2) = 0. _d 0
#ifdef ALLOW_AUTODIFF
         rTrans(i,j) = 0. _d 0
#endif /* ALLOW_AUTODIFF */
       ENDDO
      ENDDO

      DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC
        kkey = (itdkey-1)*Nr + k
#endif
        kM1  = MAX(1,k-1)
        kUp  = 1+MOD(k+1,2)
        kDown= 1+MOD(k,2)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rtrans(:,:) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
CADJ STORE fVerS(:,:,:) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
CADJ STORE gS(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
# ifdef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gsNm(:,:,k,bi,bj,1) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
CADJ STORE gsNm(:,:,k,bi,bj,2) = comlev1_bibj_k, key=kkey,
CADJ &     kind = isbyte
# else
CADJ STORE gsNm1(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey,
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
     I           diffKhS, diffK4S, KappaRk(1-OLx,1-OLy,k), diffKr4S,
     I           gsNm(1-OLx,1-OLy,1,1,1,m2), salt, dTtracerLev,
     I           GAD_SALINITY, saltAdvScheme, saltVertAdvScheme,
     I           calcAdvection, saltImplVertAdv, AdamsBashforth_S,
     I           saltVertDiff4, useGMRedi, useKPP,
     U           fVerS, gS,
     I           myTime, myIter, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL GAD_CALC_RHS(
     I           bi, bj, iMin,iMax,jMin,jMax, k, kM1, kUp, kDown,
     I           xA, yA, maskUp, uFld(1-OLx,1-OLy,k),
     I           vFld(1-OLx,1-OLy,k), wFld(1-OLx,1-OLy,k),
     I           uTrans, vTrans, rTrans, rTransKp,
     I           diffKhS, diffK4S, KappaRk(1-OLx,1-OLy,k), diffKr4S,
     I           gsNm1, salt, dTtracerLev,
     I           GAD_SALINITY, saltAdvScheme, saltVertAdvScheme,
     I           calcAdvection, saltImplVertAdv, AdamsBashforth_S,
     I           saltVertDiff4, useGMRedi, useKPP,
     U           fVerS, gS,
     I           myTime, myIter, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */

C--   External salinity forcing term(s) inside Adams-Bashforth:
        IF ( saltForcing .AND. tracForcingOutAB.NE.1 )
     &    CALL EXTERNAL_FORCING_S(
     I         iMin, iMax, jMin, jMax, bi, bj, k,
     I         myTime, myThid )

        IF ( AdamsBashforthGs ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
          CALL ADAMS_BASHFORTH3(
     I                          bi, bj, k, Nr,
     U                          gS, gsNm, gs_AB,
     I                          saltStartAB, iterNb, myThid )
#else
          CALL ADAMS_BASHFORTH2(
     I                          bi, bj, k, Nr,
     U                          gS, gsNm1, gs_AB,
     I                          saltStartAB, iterNb, myThid )
#endif
#ifdef ALLOW_DIAGNOSTICS
          IF ( useDiagnostics ) THEN
            CALL DIAGNOSTICS_FILL(gs_AB,'AB_gS   ',k,1,2,bi,bj,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF

C--   External salinity forcing term(s) outside Adams-Bashforth:
        IF ( saltForcing .AND. tracForcingOutAB.EQ.1 )
     &    CALL EXTERNAL_FORCING_S(
     I         iMin, iMax, jMin, jMax, bi, bj, k,
     I         myTime, myThid )

#ifdef NONLIN_FRSURF
        IF (nonlinFreeSurf.GT.0) THEN
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gS,
     I                            myThid )
         IF ( AdamsBashforthGs ) THEN
#ifdef ALLOW_ADAMSBASHFORTH_3
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gsNm(:,:,k,bi,bj,1) = comlev1_bibj_k, key=kkey,
CADJ &     byte=isbyte,  kind = isbyte
CADJ STORE gsNm(:,:,k,bi,bj,2) = comlev1_bibj_k, key=kkey,
CADJ &     kind = isbyte
# endif
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gsNm(1-OLx,1-OLy,1,1,1,1),
     I                            myThid )
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gsNm(1-OLx,1-OLy,1,1,1,2),
     I                            myThid )
#else
          CALL FREESURF_RESCALE_G(
     I                            bi, bj, k,
     U                            gsNm1,
     I                            myThid )
#endif
         ENDIF
        ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_ADAMSBASHFORTH_3
        IF ( AdamsBashforth_S ) THEN
          CALL TIMESTEP_TRACER(
     I           bi, bj, k, dTtracerLev(k),
     I           gsNm(1-OLx,1-OLy,1,1,1,m2),
     U           gS,
     I           myIter, myThid )
        ELSE
#endif
          CALL TIMESTEP_TRACER(
     I           bi, bj, k, dTtracerLev(k),
     I           salt,
     U           gS,
     I           myIter, myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
        ENDIF
#endif

C-    end of vertical index (k) loop (Nr:1)
      ENDDO

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
