#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: SOLVE_FOR_PRESSURE
C     !INTERFACE:
      SUBROUTINE SOLVE_FOR_PRESSURE( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SOLVE_FOR_PRESSURE
C     | o Controls inversion of two and/or three-dimensional
C     |   elliptic problems for the pressure field.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
# include "NH_VARS.h"
#endif
#ifdef ALLOW_CD_CODE
# include "CD_CODE_VARS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of SOLVE_FOR_PRESSURE
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !LOCAL VARIABLES:
      INTEGER i,j,k,bi,bj
      INTEGER ks
      INTEGER numIters, nIterMin
      _RL firstResidual, minResidualSq, lastResidual
      _RL tmpFac
      _RL sumEmP, tileEmP(nSx,nSy)
      LOGICAL putPmEinXvector
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C--   formerly part of header file SOLVE_FOR_PRESSURE.h (but not common block)
C     cg2d_x  :: Solution vector of the 2-D solver equation A.x=B
C     cg2d_b  :: Right Hand side vector of the 2-D solver equation A.x=B
      _RL cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_NONHYDROSTATIC
C--   formerly part of header file SOLVE_FOR_PRESSURE3D.h, but do not need
C     to be in common block --> changed to local var.
C     cg3d_x  :: Solution vector of the 3-D solver equation A.x=B
C     cg3d_b  :: Right Hand side vector of the 3-D solver equation A.x=B
c     _RL  cg3d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      LOGICAL oldFreeSurfTerm
#else /* ALLOW_NONHYDROSTATIC */
      _RL     cg3d_b(1)
#endif /* ALLOW_NONHYDROSTATIC */
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      _RL tmpVar (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_DIAGNOSTICS */
CEOP

#ifdef ALLOW_NONHYDROSTATIC
        oldFreeSurfTerm = use3Dsolver .AND. .NOT.exactConserv
#else
        cg3d_b(1) = 0.
#endif

C deepAtmosphere & useRealFreshWaterFlux: only valid if deepFac2F(ksurf)=1
C anelastic (always Z-coordinate):
C     1) assume that rhoFacF(1)=1 (and ksurf == 1);
C        (this reduces the number of lines of code to modify)
C     2) (a) 2-D continuity eq. compute div. of mass transport (<- add rhoFac)
C        (b) gradient of surf.Press in momentum eq. (<- add 1/rhoFac)
C       => 2 factors cancel in elliptic eq. for Phi_s ,
C       but 1rst factor(a) remains in RHS cg2d_b.

C--   Initialise the Vector solution with etaN + deltaT*Global_mean_PmE
C     instead of simply etaN ; This can speed-up the solver convergence in
C     the case where |Global_mean_PmE| is large.
      putPmEinXvector = .FALSE.
c     putPmEinXvector = useRealFreshWaterFlux.AND.fluidIsWater

      IF ( myIter.EQ.1+nIter0 .AND. debugLevel .GE. debLevA ) THEN
        _BEGIN_MASTER( myThid )
        ioUnit = standardMessageUnit
        WRITE(msgBuf,'(2A,L5)') 'SOLVE_FOR_PRESSURE:',
     &       ' putPmEinXvector =', putPmEinXvector
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#ifdef ALLOW_NONHYDROSTATIC
        WRITE(msgBuf,'(2A,L5)') 'SOLVE_FOR_PRESSURE:',
     &       ' oldFreeSurfTerm =', oldFreeSurfTerm
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#endif
        _END_MASTER( myThid )
      ENDIF

C--   Save previous solution & Initialise Vector solution and source term :
      sumEmP = 0.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef ALLOW_CD_CODE
          etaNm1(i,j,bi,bj) = etaN(i,j,bi,bj)
#endif
          cg2d_x(i,j,bi,bj) = Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
          cg2d_b(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_NONHYDROSTATIC
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           cg3d_b(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_NONHYDROSTATIC */
        IF (useRealFreshWaterFlux.AND.fluidIsWater) THEN
         tmpFac = freeSurfFac*mass2rUnit*implicDiv2DFlow
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) =
     &       tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)/deltaTMom
          ENDDO
         ENDDO
        ENDIF
        IF ( putPmEinXvector ) THEN
         tileEmP(bi,bj) = 0.
         DO j=1,sNy
          DO i=1,sNx
            tileEmP(bi,bj) = tileEmP(bi,bj)
     &                     + rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
     &                                    *maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO
      IF ( putPmEinXvector ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileEmP, sumEmP, myThid )
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( putPmEinXvector ) THEN
          tmpFac = 0.
          IF (globalArea.GT.0.) tmpFac =
     &      freeSurfFac*deltaTFreeSurf*mass2rUnit*sumEmP/globalArea
          DO j=1,sNy
           DO i=1,sNx
            cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)
     &                        - tmpFac*Bo_surf(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
C- RHS: similar to the divergence of the vertically integrated mass transport:
C       del_i { Sum_k [ rhoFac.(dr.hFac).(dy.deepFac).(u*) ] }  / deltaT
        DO k=Nr,1,-1
         CALL CALC_DIV_GHAT(
     I                       bi,bj,k,
     U                       cg2d_b, cg3d_b,
     I                       myThid )
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_NONHYDROSTATIC
        IF ( oldFreeSurfTerm ) THEN
C--   Add source term arising from w=d/dt (p_s + p_nh)
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           IF ( ks.LE.Nr ) THEN
            cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*recip_Bo(i,j,bi,bj) )
            cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*recip_Bo(i,j,bi,bj) )
           ENDIF
          ENDDO
         ENDDO
        ELSEIF ( exactConserv ) THEN
#else
C--   Add source term arising from w=d/dt (p_s)
        IF ( exactConserv ) THEN
#endif /* ALLOW_NONHYDROSTATIC */
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
     &         * etaH(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
     &         * etaN(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_OBCS
C- Note: solver matrix is trivial outside OB region (main diagonal only)
C     => no real need to reset RHS (=cg2d_b) & cg2d_x, except that:
C    a) normalisation is fct of Max(RHS), which can be large ouside OB region
C      (would be different if we were solving for increment of eta/g
C       instead of directly for eta/g).
C       => need to reset RHS to ensure that interior solution does not depend
C       on ouside OB region.
C    b) provide directly the trivial solution cg2d_x == 0 for outside OB region
C      (=> no residual => no effect on solver convergence and interior solution)
        IF (useOBCS) THEN
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)*maskInC(i,j,bi,bj)
           cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)*maskInC(i,j,bi,bj)
         ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS */
C-    end bi,bj loops
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
       CALL DEBUG_STATS_RL(1,cg2d_b,'cg2d_b (SOLVE_FOR_PRESSURE)',
     &                        myThid)
      ENDIF
#endif
      IF ( DIFFERENT_MULTIPLE(diagFreq, myTime, deltaTClock) ) THEN
       CALL WRITE_FLD_XY_RL( 'cg2d_b', 'I10', cg2d_b, myIter, myThid )
      ENDIF

C--   Find the surface pressure using a two-dimensional conjugate
C     gradient solver. See CG2D.h for the interface to this routine.
C     In rare cases of a poor solver convergence, better to select the
C     solver minimum-residual solution (instead of the last-iter solution)
C     by setting cg2dUseMinResSol=1 (<-> nIterMin=0 in input)
      numIters      = cg2dMaxIters
      nIterMin      = cg2dUseMinResSol - 1
c     CALL TIMER_START('CG2D   [SOLVE_FOR_PRESSURE]',myThid)
#ifdef DISCONNECTED_TILES
C--   Call the disconnected-tile (no EXCH) version of cg2d
      CALL CG2D_EX0(
     U           cg2d_b, cg2d_x,
     O           firstResidual, minResidualSq, lastResidual,
     U           numIters, nIterMin,
     I           myThid )
#else /* not DISCONNECTED_TILES = default */
      IF ( useSRCGSolver ) THEN
C--   Call the single reduce CG solver
#ifdef ALLOW_SRCG
       CALL CG2D_SR(
     U           cg2d_b, cg2d_x,
     O           firstResidual, minResidualSq, lastResidual,
     U           numIters, nIterMin,
     I           myThid )
#endif /* ALLOW_SRCG */
#ifdef ALLOW_CG2D_NSA
      ELSEIF ( useNSACGSolver ) THEN
C--   Call the not-self-adjoint version of cg2d
       CALL CG2D_NSA(
     U           cg2d_b, cg2d_x,
     O           firstResidual, minResidualSq, lastResidual,
     U           numIters, nIterMin,
     I           myThid )
#endif
      ELSE
C--   Call the default CG solver
       CALL CG2D(
     U           cg2d_b, cg2d_x,
     O           firstResidual, minResidualSq, lastResidual,
     U           numIters, nIterMin,
     I           myThid )
      ENDIF
#endif /* DISCONNECTED_TILES */
      _EXCH_XY_RL( cg2d_x, myThid )
#ifdef ALLOW_AUTODIFF
#ifndef ALLOW_TAPENADE
      IF (  .NOT. useNSACGSolver .AND. cg2dFullAdjoint )
     &     CALL CG2D_STORE( cg2d_x, .TRUE., myThid )
#endif /* ALLOW_TAPENADE */
#endif
c     CALL TIMER_STOP ('CG2D   [SOLVE_FOR_PRESSURE]',myThid)

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
       CALL DEBUG_STATS_RL(1,cg2d_x,'cg2d_x (SOLVE_FOR_PRESSURE)',
     &                        myThid)
      ENDIF
#endif

C- dump CG2D output at monitorFreq (to reduce size of STD-OUTPUT files) :
      IF ( DIFFERENT_MULTIPLE(monitorFreq,myTime,deltaTClock)
     &   ) THEN
       IF ( debugLevel .GE. debLevA ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A20,1PE23.14)') 'cg2d_init_res =',firstResidual
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        WRITE(msgBuf,'(A27,2I8)')
     &       'cg2d_iters(min,last) =', nIterMin, numIters
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        IF ( minResidualSq.GE.0. ) THEN
         minResidualSq = SQRT(minResidualSq)
         WRITE(msgBuf,'(A20,1PE23.14)') 'cg2d_min_res  =',minResidualSq
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        ENDIF
        WRITE(msgBuf,'(A20,1PE23.14)') 'cg2d_last_res =',lastResidual
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        _END_MASTER( myThid )
       ENDIF
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
C--   Fill diagnostics
      IF ( useDiagnostics .AND. implicSurfPress.NE.oneRL ) THEN
        diagName = 'PHI_SURF'
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             tmpVar(i,j) = implicSurfPress * cg2d_x(i,j,bi,bj)
     &          + (oneRL - implicSurfPress)* Bo_surf(i,j,bi,bj)
     &                                     * etaN(i,j,bi,bj)
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL( tmpVar,diagName,1,1,2,bi,bj,myThid )
          ENDDO
         ENDDO
        ENDIF
      ELSEIF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( cg2d_x,'PHI_SURF', 0,1, 0,1,1, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   Transfert the 2D-solution to "etaN" :
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          etaN(i,j,bi,bj) = recip_Bo(i,j,bi,bj)*cg2d_x(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
      IF ( use3Dsolver ) THEN
       IF ( DIFFERENT_MULTIPLE(diagFreq, myTime, deltaTClock) ) THEN
        CALL WRITE_FLD_XY_RL( 'cg2d_x','I10', cg2d_x, myIter, myThid )
       ENDIF

C--   Solve for a three-dimensional pressure term (NH or IGW or both ).
C     see CG3D.h for the interface to this routine.

C--   Finish updating cg3d_b: 1) Add EmPmR contribution to top level cg3d_b:
C                             2) Update or Add free-surface contribution
C                             3) increment in horiz velocity due to new cg2d_x
C                             4) add vertical velocity contribution.
       CALL PRE_CG3D(
     I                oldFreeSurfTerm,
     I                cg2d_x,
     U                cg3d_b,
     I                myTime, myIter, myThid )

#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevD ) THEN
        CALL DEBUG_STATS_RL(Nr,cg3d_b,'cg3d_b (SOLVE_FOR_PRESSURE)',
     &                         myThid)
       ENDIF
#endif
       IF ( DIFFERENT_MULTIPLE( diagFreq, myTime, deltaTClock) ) THEN
        CALL WRITE_FLD_XYZ_RL('cg3d_b','I10', cg3d_b, myIter,myThid )
       ENDIF

       firstResidual=0.
       lastResidual=0.
       numIters=cg3dMaxIters
       CALL TIMER_START('CG3D   [SOLVE_FOR_PRESSURE]',myThid)
#ifdef DISCONNECTED_TILES
       CALL CG3D_EX0(
     U            cg3d_b, phi_nh,
     O            firstResidual, lastResidual,
     U            numIters,
     I            myIter, myThid )
#else /* not DISCONNECTED_TILES = default */
       CALL CG3D(
     U            cg3d_b, phi_nh,
     O            firstResidual, lastResidual,
     U            numIters,
     I            myIter, myThid )
#endif /* DISCONNECTED_TILES */
       _EXCH_XYZ_RL( phi_nh, myThid )
       CALL TIMER_STOP ('CG3D   [SOLVE_FOR_PRESSURE]',myThid)

       IF ( DIFFERENT_MULTIPLE(monitorFreq,myTime,deltaTClock)
     &    ) THEN
        IF ( debugLevel .GE. debLevA ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(A20,1PE23.14)') 'cg3d_init_res =',firstResidual
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
         WRITE(msgBuf,'(A27,I16)') 'cg3d_iters (last) =   ',numIters
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
         WRITE(msgBuf,'(A20,1PE23.14)') 'cg3d_last_res =',lastResidual
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
         _END_MASTER( myThid )
        ENDIF
       ENDIF

C--   Separate the Hydrostatic Surface Pressure adjusment (=> put it in dPhiNH)
C     from the Non-hydrostatic pressure (since cg3d_x contains both contribution)
       IF ( nonHydrostatic .AND. exactConserv ) THEN
        IF ( DIFFERENT_MULTIPLE( diagFreq, myTime, deltaTClock) ) THEN
         CALL WRITE_FLD_XYZ_RL('cg3d_x','I10', phi_nh, myIter,myThid )
        ENDIF
        CALL POST_CG3D(
     I                  myTime, myIter, myThid )
       ENDIF

      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

#ifdef ALLOW_SHOWFLOPS
      CALL SHOWFLOPS_INSOLVE( myThid)
#endif

      RETURN
      END
