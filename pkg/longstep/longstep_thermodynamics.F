C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_thermodynamics.F,v 1.2 2010/01/12 23:55:48 jahn Exp $
C $Name:  $

#include "LONGSTEP_OPTIONS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_GMREDI
#  include "GMREDI_OPTIONS.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

CBOP
C     !ROUTINE: LONGSTEP_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE LONGSTEP_THERMODYNAMICS(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_THERMODYNAMICS
C     | o Controlling routine for the prognostics of passive tracers
C     |   with longer time step.
C     *===========================================================
C     | This is a copy of THERMODYNAMICS, but only with the
C     | parts relevant to ptracers, and dynamics fields replaced
C     | by their longstep averages.
C     | When THERMODYNAMICS is changed, this routine probably has
C     | to be changed too :(
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#include "LONGSTEP_PARAMS.h"
#include "LONGSTEP.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
#endif
#ifdef ALLOW_TIMEAVE
# include "TIMEAVE_STATV.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
# include "FFIELDS.h"
# include "SURFACE.h"
# include "EOS.h"
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_LONGSTEP
C     !LOCAL VARIABLES:
C     == Local variables
C     xA, yA                 - Per block temporaries holding face areas
C     uFld, vFld, wFld       - Local copy of velocity field (3 components)
C     uTrans, vTrans, rTrans - Per block temporaries holding flow transport
C                              o uTrans: Zonal transport
C                              o vTrans: Meridional transport
C                              o rTrans: Vertical transport
C     rTransKp1                o vertical volume transp. at interface k+1
C     maskUp                   o maskUp: land/water mask for W points
C     fVer[STUV]               o fVer: Vertical flux term - note fVer
C                                      is "pipelined" in the vertical
C                                      so we need an fVer for each
C                                      variable.
C     kappaRT,       - Total diffusion in vertical at level k, for T and S
C     kappaRS          (background + spatially varying, isopycnal term).
C     kappaRTr       - Total diffusion in vertical at level k,
C                      for each passive Tracer
C     kappaRk        - Total diffusion in vertical, all levels, 1 tracer
C     useVariableK   = T when vertical diffusion is not constant
C     iMin, iMax     - Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj
C     k, kup,        - Index for layer above and below. kup and kDown
C     kDown, km1       are switched with layer to be the appropriate
C                      index into fVerTerm.
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL kappaRT (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL kappaRS (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
#ifdef ALLOW_PTRACERS
      _RL fVerP   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,PTRACERS_num)
      _RL kappaRTr(1-Olx:sNx+Olx,1-Oly:sNy+Oly,PTRACERS_num)
#endif
      _RL kappaRk (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, km1, kup, kDown
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER iterNb, m1, m2
#endif
#ifdef ALLOW_TIMEAVE
      LOGICAL useVariableK
#endif
#ifdef ALLOW_PTRACERS
      INTEGER iTracer, ip
#endif
      INTEGER trIter
CEOP

#ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_ENTER('LONGSTEP_THERMODYNAMICS',myThid)
#endif

C     time for a ptracer time step?
      IF ( LS_doTimeStep ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
C--   dummy statement to end declaration part
      ikey = 1
      itdkey = 1
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAMC
CHPF$  INDEPENDENT, NEW (rTrans,fVerT,fVerS
CHPF$&                  ,utrans,vtrans,xA,yA
CHPF$&                  ,kappaRT,kappaRS
CHPF$&                  )
# ifdef ALLOW_PTRACERS
CHPF$  INDEPENDENT, NEW (fVerP,kappaRTr)
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

       DO bi=myBxLo(myThid),myBxHi(myThid)

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

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xA(i,j)        = 0. _d 0
          yA(i,j)        = 0. _d 0
          uTrans(i,j)    = 0. _d 0
          vTrans(i,j)    = 0. _d 0
          rTrans (i,j)   = 0. _d 0
          rTransKp1(i,j) = 0. _d 0
         ENDDO
        ENDDO

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C This is currently also used by IVDC and Diagnostics
           kappaRk(i,j,k)    = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_PTRACERS
        IF ( usePTRACERS ) THEN
         DO ip=1,PTRACERS_num
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             fVerP  (i,j,1,ip) = 0. _d 0
             fVerP  (i,j,2,ip) = 0. _d 0
             kappaRTr(i,j,ip)  = 0. _d 0
            ENDDO
           ENDDO
         ENDDO
C-      set tracer tendency to zero:
         DO iTracer=1,PTRACERS_num
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gPTr(i,j,k,bi,bj,itracer) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
#endif

        iMin = 1-OLx
        iMax = sNx+OLx
        jMin = 1-OLy
        jMax = sNy+OLy

C     need to recompute surfaceForcingPtr using LS_fwFlux
        CALL LONGSTEP_FORCING_SURF(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime,myIter,myThid )

#ifdef ALLOW_AUTODIFF_TAMC
cph avoids recomputation of integrate_for_w
CADJ STORE wvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Attention: by defining "SINGLE_LAYER_MODE" in CPP_OPTIONS.h
C--     MOST of THERMODYNAMICS will be disabled
#ifndef SINGLE_LAYER_MODE

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
# if ((defined ALLOW_DEPTH_CONTROL) || (defined NONLIN_FRSURF))
CADJ STORE gtnm1(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE gsnm1(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

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

C Since passive tracers are configurable separately from T,S we
C call the multi-dimensional method for PTRACERS regardless
C of whether multiDimAdvection is set or not.
#ifdef ALLOW_PTRACERS
        IF ( usePTRACERS ) THEN
#ifdef ALLOW_DEBUG
          IF ( debugLevel .GE. debLevB )
     &     CALL DEBUG_CALL('PTRACERS_ADVECTION',myThid)
#endif
         CALL PTRACERS_ADVECTION(
     I             bi,bj,myIter,myTime,myThid )
        ENDIF
#endif /* ALLOW_PTRACERS */
#endif /* DISABLE_MULTIDIM_ADVECTION */

#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_MSG('ENTERING DOWNWARD K LOOP IN LONGSTEP',
     &                   myThid)
#endif

C--     Start of thermodynamics loop
        DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC
C? Patrick Is this formula correct?
cph Yes, but I rewrote it.
cph Also, the kappaR? need the index and subscript k!
         kkey = (itdkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

C--       km1    Points to level above k (=k-1)
C--       kup    Cycles through 1,2 to point to layer above
C--       kDown  Cycles through 2,1 to point to current layer

          km1  = MAX(1,k-1)
          kup  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

          iMin = 1-OLx
          iMax = sNx+OLx
          jMin = 1-OLy
          jMax = sNy+OLy

          IF (k.EQ.Nr) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             rTransKp1(i,j) = 0. _d 0
            ENDDO
           ENDDO
          ELSE
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             rTransKp1(i,j) = rTrans(i,j)
            ENDDO
           ENDDO
          ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rTransKp1(:,:) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

C--       Get temporary terms used by tendency routines :
C-        Calculate horizontal "volume transport" through tracer cell face
C         anelastic: uTrans,vTrans are scaled by rhoFacC (~ mass transport)
          CALL CALC_COMMON_FACTORS (
     I         LS_uVel, LS_vVel,
     O         uFld, vFld, uTrans, vTrans, xA, yA,
     I         k,bi,bj, myThid )

C-        Calculate vertical "volume transport" through tracer cell face
          IF (k.EQ.1) THEN
C-         Surface interface :
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             wFld(i,j)   = 0. _d 0
             maskUp(i,j) = 0. _d 0
             rTrans(i,j) = 0. _d 0
            ENDDO
           ENDDO
          ELSE
C-         Interior interface :
C          anelastic: rTrans is scaled by rhoFacF (~ mass transport)
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             wFld(i,j)   = LS_wVel(i,j,k,bi,bj)
             maskUp(i,j) = maskC(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
             rTrans(i,j) = wFld(i,j)*rA(i,j,bi,bj)*maskUp(i,j)
     &                              *deepFac2F(k)*rhoFacF(k)
            ENDDO
           ENDDO
          ENDIF

#ifdef ALLOW_GMREDI
C--   Residual transp = Bolus transp + Eulerian transp
          IF (useGMRedi) THEN
            CALL GMREDI_CALC_UVFLOW(
     U                  uFld, vFld, uTrans, vTrans,
     I                  k, bi, bj, myThid )
            IF (K.GE.2) THEN
              CALL GMREDI_CALC_WFLOW(
     U                  wFld, rTrans,
     I                  k, bi, bj, myThid )
            ENDIF
          ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE wfld(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
# ifdef GM_BOLUS_ADVEC
CADJ STORE ufld(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE vfld(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE uTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE vTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
# endif /* ALLOW_AUTODIFF_TAMC */
#endif /* ALLOW_GMREDI */

          iMin = 1-OLx+2
          iMax = sNx+OLx-1
          jMin = 1-OLy+2
          jMax = sNy+OLy-1

C--      Calculate active tracer tendencies (gT,gS,...)
C        and step forward storing result in gT, gS, etc.
C--
# ifdef ALLOW_AUTODIFF_TAMC
#  if ((defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)) && (defined ALLOW_GMREDI)
#   ifdef GM_NON_UNITY_DIAGONAL
CADJ STORE kux(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE kvy(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#   endif
#   ifdef GM_EXTRA_DIAGONAL
CADJ STORE kuz(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE kvz(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#   endif
#  endif
# endif /* ALLOW_AUTODIFF_TAMC */
C
#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
cph-test
CADJ STORE uFld(:,:), vFld(:,:), wFld(:,:)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE uTrans(:,:), vTrans(:,:)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE xA(:,:), yA(:,:)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */
C
#ifdef ALLOW_PTRACERS
         IF ( usePTRACERS ) THEN
           IF ( .NOT.implicitDiffusion ) THEN
             CALL PTRACERS_CALC_DIFF(
     I            bi,bj,iMin,iMax,jMin,jMax,k,
     I            maskUp,
     O            kappaRTr,
     I            myThid)
           ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRTr(:,:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
           CALL PTRACERS_INTEGRATE(
     I          bi,bj,k,
     I          xA, yA, maskUp, uFld, vFld, wFld,
     I          uTrans, vTrans, rTrans, rTransKp1,
     I          kappaRTr,
     U          fVerP,
     I          myTime,myIter,myThid)
         ENDIF
#endif /* ALLOW_PTRACERS */

C--     end of thermodynamic k loop (Nr:1)
        ENDDO

#ifdef ALLOW_DOWN_SLOPE
#ifdef ALLOW_PTRACERS
        IF ( usePTRACERS .AND. useDOWN_SLOPE ) THEN
          CALL PTRACERS_DWNSLP_APPLY(
     I                  bi, bj, myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_PTRACERS */
#endif /* ALLOW_DOWN_SLOPE */

C       All explicit advection/diffusion/sources should now be
C       done. The updated tracer field is in gPtr. Accumalate
C       explicit tendency and also reset gPtr to initial tracer
C       field for implicit matrix calculation

#ifdef ALLOW_MATRIX
        IF (useMATRIX)
     &    CALL MATRIX_STORE_TENDENCY_EXP(bi,bj, myTime,myIter,myThid)
#endif

        iMin = 1
        iMax = sNx
        jMin = 1
        jMax = sNy

#ifdef ALLOW_PTRACERS
        IF     ( usePTRACERS ) THEN
C--     Vertical advection/diffusion (implicit) for passive tracers
           CALL PTRACERS_IMPLICIT(
     U                             kappaRk,
     I                             bi, bj, myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_PTRACERS */

#endif /* SINGLE_LAYER_MODE */

C--   end bi,bj loops.
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      If (debugMode) THEN
       CALL DEBUG_STATS_RL(Nr,LS_uVel,'LS_Uvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,LS_vVel,'LS_Vvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,LS_wVel,'LS_Wvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,LS_theta,'LS_Theta (THERMODYNAMICS)',
     &                     myThid)
       CALL DEBUG_STATS_RL(Nr,LS_salt,'LS_Salt (THERMODYNAMICS)',myThid)
#ifdef ALLOW_PTRACERS
       IF ( usePTRACERS ) THEN
         CALL PTRACERS_DEBUG(myThid)
       ENDIF
#endif /* ALLOW_PTRACERS */
      ENDIF
#endif /* ALLOW_DEBUG */

C     LS_doTimeStep
      ENDIF

#ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_LEAVE('LONGSTEP_THERMODYNAMICS',myThid)
#endif

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
