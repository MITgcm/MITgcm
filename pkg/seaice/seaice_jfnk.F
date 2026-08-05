#include "SEAICE_OPTIONS.h"

C--  File seaice_jfnk.F: seaice jfnk dynamical solver S/R:
C--   Contents
C--   o SEAICE_JFNK
C--   o SEAICE_JFNK_UPDATE

CBOP
C     !ROUTINE: SEAICE_JFNK
C     !INTERFACE:
      SUBROUTINE SEAICE_JFNK( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_JFNK
C     | o Ice dynamics using a Jacobian-free Newton-Krylov solver
C     |   following J.-F. Lemieux et al. Improving the numerical
C     |   convergence of viscous-plastic sea ice models with the
C     |   Jacobian-free Newton-Krylov method. J. Comp. Phys. 229,
C     |   2840-2852 (2010).
C     | o The logic follows JFs code.
C     *==========================================================*
C     | written by Martin Losch, Oct 2012
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if ( defined SEAICE_CGRID && defined SEAICE_ALLOW_JFNK )
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj :: loop indices
      INTEGER i,j,bi,bj
C     loop indices
      INTEGER newtonIter
      INTEGER krylovIter, krylovFails
      INTEGER totalKrylovItersLoc, totalNewtonItersLoc
C     FGMRES parameters
C     im      :: size of Krylov space
C     ifgmres :: interation counter
      INTEGER im
      PARAMETER ( im = 50 )
      INTEGER ifgmres
C     FGMRES flag that determines amount of output messages of fgmres
      INTEGER iOutFGMRES
C     FGMRES flag that indicates what fgmres wants us to do next
      INTEGER iCode
      _RL     JFNKresidual
      _RL     JFNKresidualKm1
C     parameters to compute convergence criterion
      _RL     JFNKgamma_lin
      _RL     FGMRESeps
      _RL     JFNKtol
C     backward differences extrapolation factors
      _RL bdfFac, bdfAlpha
C
      _RL     recip_deltaT
      LOGICAL JFNKconverged, krylovConverged
      LOGICAL writeNow
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     u/vIceRes :: residual of sea-ice momentum equations
      _RL uIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     extra time level required for backward difference time stepping
      _RL duIcNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIcNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     du/vIce   :: ice velocity increment to be added to u/vIce
      _RL duIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     precomputed (= constant per Newton iteration) versions of
C     zeta, eta, and DWATN, press
      _RL zetaPre (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaZPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaPre  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaZPre (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dwatPre (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     work arrays
      _RL rhs(nVec,nSx,nSy), sol(nVec,nSx,nSy)
      _RL vv(nVec,im+1,nSx,nSy), w(nVec,im,nSx,nSy)
      _RL wk1(nVec,nSx,nSy), wk2(nVec,nSx,nSy)
CEOP

C     Initialise
      newtonIter          = 0
      krylovFails         = 0
      totalKrylovItersLoc = 0
      JFNKconverged       = .FALSE.
      JFNKtol             = 0. _d 0
      JFNKresidual        = 0. _d 0
      JFNKresidualKm1     = 0. _d 0
      FGMRESeps           = 0. _d 0
      recip_deltaT        = 1. _d 0 / SEAICE_deltaTdyn

      iOutFGMRES=0
C     with iOutFgmres=1, seaice_fgmres prints the residual at each iteration
      IF ( debugLevel.GE.debLevC .AND.
     &     DIFFERENT_MULTIPLE( SEAICE_monFreq, myTime, deltaTClock ) )
     &     iOutFGMRES=1

C     backward difference extrapolation factors
      bdfFac = 0. _d 0
      IF ( SEAICEuseBDF2 ) THEN
       IF ( myIter.EQ.nIter0 .AND. SEAICEmomStartBDF.EQ.0 ) THEN
        bdfFac = 0. _d 0
       ELSE
        bdfFac = 0.5 _d 0
       ENDIF
      ENDIF
      bdfAlpha = 1. _d 0 + bdfFac

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          uIceRes(I,J,bi,bj) = 0. _d 0
          vIceRes(I,J,bi,bj) = 0. _d 0
          duIce  (I,J,bi,bj) = 0. _d 0
          dvIce  (I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
C     cycle ice velocities
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          duIcNm1(I,J,bi,bj) = uIce(I,J,bi,bj) * bdfAlpha
     &         + ( uIce(I,J,bi,bj) - uIceNm1(I,J,bi,bj) ) * bdfFac
          dvIcNm1(I,J,bi,bj) = vIce(I,J,bi,bj) * bdfAlpha
     &         + ( vIce(I,J,bi,bj) - vIceNm1(I,J,bi,bj) ) * bdfFac
          uIceNm1(I,J,bi,bj) = uIce(I,J,bi,bj)
          vIceNm1(I,J,bi,bj) = vIce(I,J,bi,bj)
         ENDDO
        ENDDO
C     As long as IMEX is not properly implemented leave this commented out
CML        IF ( .NOT.SEAICEuseIMEX ) THEN
C     Compute things that do no change during the Newton iteration:
C     sea-surface tilt and wind stress:
C     FORCEX/Y0 - mass*(1.5*u/vIceNm1+0.5*(u/vIceNm1-u/vIceNm2))/deltaT
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          FORCEX(I,J,bi,bj) = FORCEX0(I,J,bi,bj)
     &         + seaiceMassU(I,J,bi,bj)*duIcNm1(I,J,bi,bj)*recip_deltaT
          FORCEY(I,J,bi,bj) = FORCEY0(I,J,bi,bj)
     &         + seaiceMassV(I,J,bi,bj)*dvIcNm1(I,J,bi,bj)*recip_deltaT
         ENDDO
        ENDDO
CML        ENDIF
       ENDDO
      ENDDO
C     Start nonlinear Newton iteration: outer loop iteration
      DO WHILE ( newtonIter.LT.SEAICEnonLinIterMax .AND.
     &     .NOT.JFNKconverged )
       newtonIter = newtonIter + 1
C     Compute initial residual F(u), (includes computation of global
C     variables DWATN, zeta, and eta)
       IF ( newtonIter .EQ. 1 ) CALL SEAICE_JFNK_UPDATE(
     I      duIce, dvIce,
     U      uIce, vIce, JFNKresidual,
     O      uIceRes, vIceRes,
     I      newtonIter, myTime, myIter, myThid )
C     local copies of precomputed coefficients that are to stay
C     constant for the preconditioner
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           zetaPre(I,J,bi,bj) =  zeta(I,J,bi,bj)
           zetaZPre(I,J,bi,bj)= zetaZ(I,J,bi,bj)
            etaPre(I,J,bi,bj) =   eta(I,J,bi,bj)
           etaZPre(I,J,bi,bj) =  etaZ(I,J,bi,bj)
           dwatPre(I,J,bi,bj) = DWATN(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C     compute convergence criterion for linear preconditioned FGMRES
       JFNKgamma_lin = JFNKgamma_lin_max
       IF ( newtonIter.GT.1.AND.newtonIter.LE.SEAICE_JFNK_tolIter
     &      .AND.JFNKresidual.LT.JFNKres_t ) THEN
C     Eisenstat and Walker (1996), eq.(2.6)
        JFNKgamma_lin = SEAICE_JFNKphi
     &       *( JFNKresidual/JFNKresidualKm1 )**SEAICE_JFNKalpha
        JFNKgamma_lin = min(JFNKgamma_lin_max, JFNKgamma_lin)
        JFNKgamma_lin = max(JFNKgamma_lin_min, JFNKgamma_lin)
       ENDIF
C     save the residual for the next iteration
       JFNKresidualKm1 = JFNKresidual

C     The Krylov iteration using FGMRES, the preconditioner is LSOR
C     for now. The code is adapted from SEAICE_LSR, but heavily stripped
C     down.
C     krylovIter is mapped into "its" in seaice_fgmres and is incremented
C     in that routine
       krylovIter    = 0
       iCode         = 0

       JFNKconverged = JFNKresidual.LT.JFNKtol
     &      .OR.JFNKresidual.EQ.0. _d 0

C     do Krylov loop only if convergence is not reached

       IF ( .NOT.JFNKconverged ) THEN

C     start Krylov iteration (FGMRES)

        krylovConverged = .FALSE.
        FGMRESeps = JFNKgamma_lin * JFNKresidual
C     map first guess sol; it is zero because the solution is a correction
       CALL SEAICE_MAP2VEC(nVec,duIce,dvIce,sol,.TRUE.,myThid)
C     map rhs and change its sign because we are solving J*u = -F
        CALL SEAICE_MAP2VEC(nVec,uIceRes,vIceRes,rhs,.TRUE.,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,nVec
           rhs(j,bi,bj) = - rhs(j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
        DO WHILE ( .NOT.krylovConverged )
C     solution vector sol = du/vIce
C     residual vector (rhs) Fu = u/vIceRes
C     output work vectors wk1, -> input work vector wk2

C     map preconditioner results or Jacobian times vector,
C     stored in du/vIce to wk2, for iCode=0, wk2 is set to zero,
C     because du/vIce = 0
         CALL SEAICE_MAP2VEC(nVec,duIce,dvIce,wk2,.TRUE.,myThid)

         CALL SEAICE_FGMRES (nVec,im,rhs,sol,ifgmres,krylovIter,
     U        vv,w,wk1,wk2,
     I        FGMRESeps,SEAICElinearIterMax,iOutFGMRES,
     U        iCode,
     I        myThid)

         IF ( iCode .EQ. 0 ) THEN
C     map sol(ution) vector to du/vIce
          CALL SEAICE_MAP2VEC(nVec,duIce,dvIce,sol,.FALSE.,myThid)
         ELSE
C     map work vector to du/vIce to either compute a preconditioner
C     solution (wk1=rhs) or a Jacobian times wk1
          CALL SEAICE_MAP2VEC(nVec,duIce,dvIce,wk1,.FALSE.,myThid)
         ENDIF
C     Fill overlaps in updated fields
         CALL EXCH_UV_XY_RL( duIce, dvIce,.TRUE.,myThid)
C     FGMRES returns iCode either asking for an new preconditioned vector
C     or product of matrix (Jacobian) times vector. For iCode = 0, terminate
C     iteration
         IF (iCode.EQ.1) THEN
C     Call preconditioner
          IF ( SEAICEpreconLinIter .GT. 0 )
     &         CALL SEAICE_PRECONDITIONER(
     U         duIce, dvIce,
     I         zetaPre, etaPre, etaZpre, zetaZpre, dwatPre,
     I         newtonIter, krylovIter, myTime, myIter, myThid )
         ELSEIF (iCode.GE.2) THEN
C     Compute Jacobian times vector
          CALL SEAICE_JACVEC(
     I         uIce, vIce, uIceRes, vIceRes,
     U         duIce, dvIce,
     I         newtonIter, krylovIter, myTime, myIter, myThid )
         ENDIF
         krylovConverged = iCode.EQ.0
C     End of Krylov iterate
        ENDDO
        totalKrylovItersLoc = totalKrylovItersLoc + krylovIter
C     some output diagnostics
        IF ( debugLevel.GE.debLevA ) THEN
         _BEGIN_MASTER( myThid )
         totalNewtonItersLoc =
     &        SEAICEnonLinIterMax*(myIter-nIter0)+newtonIter
         WRITE(msgBuf,'(2A,2(1X,I6),2E12.5)')
     &        ' S/R SEAICE_JFNK: Newton iterate / total, ',
     &        'JFNKgamma_lin, initial norm = ',
     &        newtonIter, totalNewtonItersLoc,
     &        JFNKgamma_lin,JFNKresidual
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(3(A,I6))')
     &        ' S/R SEAICE_JFNK: Newton iterate / total = ',newtonIter,
     &        ' / ', totalNewtonItersLoc,
     &        ', Nb. of FGMRES iterations = ', krylovIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
        ENDIF
        IF ( krylovIter.EQ.SEAICElinearIterMax ) THEN
         krylovFails = krylovFails + 1
        ENDIF
C     Set the stopping criterion for the Newton iteration and the
C     criterion for the transition from accurate to approximate FGMRES
        IF ( newtonIter .EQ. 1 ) THEN
         JFNKtol=SEAICEnonLinTol*JFNKresidual
         IF ( JFNKres_tFac .NE. UNSET_RL )
     &        JFNKres_t = JFNKresidual * JFNKres_tFac
        ENDIF
C     Update linear solution vector and return to Newton iteration
C     Do a linesearch if necessary, and compute a new residual.
C     Note that it should be possible to do the following operations
C     at the beginning of the Newton iteration, thereby saving us from
C     the extra call of seaice_jfnk_update, but unfortunately that
C     changes the results, so we leave the stuff here for now.
        CALL SEAICE_JFNK_UPDATE(
     I       duIce, dvIce,
     U       uIce, vIce, JFNKresidual,
     O       uIceRes, vIceRes,
     I       newtonIter, myTime, myIter, myThid )
C     reset du/vIce here instead of setting sol = 0 in seaice_fgmres_driver
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO J=1-OLy,sNy+OLy
           DO I=1-OLx,sNx+OLx
            duIce(I,J,bi,bj)= 0. _d 0
            dvIce(I,J,bi,bj)= 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
C     end of Newton iterate
      ENDDO

C--   Output diagnostics

      IF ( SEAICE_monFreq .GT. 0. _d 0 ) THEN
C     Only Master Thread updates counters in common block:
      _BEGIN_MASTER(myThid)
C     Cumulate some diagnostic counters for the JFNK solver
       totalJFNKtimeSteps = totalJFNKtimeSteps + 1
       totalNewtonIters   = totalNewtonIters + newtonIter
       totalKrylovIters   = totalKrylovIters + totalKrylovItersLoc
C     Record failure
       totalKrylovFails   = totalKrylovFails + krylovFails
       IF ( newtonIter .EQ. SEAICEnonLinIterMax ) THEN
        totalNewtonFails = totalNewtonFails + 1
       ENDIF
       _END_MASTER( myThid )
      ENDIF
C     Decide whether it is time to dump and reset the counter
      writeNow = DIFFERENT_MULTIPLE(SEAICE_monFreq,
     &     myTime+deltaTClock, deltaTClock)
#ifdef ALLOW_CAL
      IF ( useCAL ) THEN
       CALL CAL_TIME2DUMP(
     I      zeroRL, SEAICE_monFreq,  deltaTClock,
     U      writeNow,
     I      myTime+deltaTClock, myIter+1, myThid )
      ENDIF
#endif
      IF ( writeNow ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(A)')
     &' // ======================================================='
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') ' // Begin JFNK statistics'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &' // ======================================================='
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %JFNK_MON: time step              = ', myIter+1
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %JFNK_MON: Nb. of time steps      = ', totalJFNKtimeSteps
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %JFNK_MON: Nb. of Newton steps    = ', totalNewtonIters
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %JFNK_MON: Nb. of Krylov steps    = ', totalKrylovIters
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %JFNK_MON: Nb. of Newton failures = ', totalNewtonFails
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %JFNK_MON: Nb. of Krylov failures = ', totalKrylovFails
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &' // ======================================================='
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') ' // End JFNK statistics'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &' // ======================================================='
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
C     Reset and start again
       totalJFNKtimeSteps = 0
       totalNewtonIters   = 0
       totalKrylovIters   = 0
       totalKrylovFails   = 0
       totalNewtonFails   = 0
       _END_MASTER( myThid )
      ENDIF

C     Print more debugging information
      IF ( debugLevel.GE.debLevA ) THEN
       IF ( newtonIter .EQ. SEAICEnonLinIterMax ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A,I10)')
     &       ' S/R SEAICE_JFNK: JFNK did not converge in timestep ',
     &       myIter+1
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
       ENDIF
       IF ( krylovFails .GT. 0 ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A,I4,A,I10)')
     &       ' S/R SEAICE_JFNK: FGMRES did not converge ',
     &       krylovFails, ' times in timestep ', myIter+1
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
       ENDIF
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(A,I6,A,I10)')
     &      ' S/R SEAICE_JFNK: Total number FGMRES iterations = ',
     &      totalKrylovItersLoc, ' in timestep ', myIter+1
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_JFNK_UPDATE
C     !INTERFACE:

      SUBROUTINE SEAICE_JFNK_UPDATE(
     I     duIce, dvIce,
     U     uIce, vIce, JFNKresidual,
     O     uIceRes, vIceRes,
     I     newtonIter, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_JFNK_UPDATE
C     | o Update velocities with incremental solutions of FGMRES
C     | o compute residual of updated solutions and do
C     | o linesearch:
C     |   reduce update until residual is smaller than previous
C     |   one (input)
C     *==========================================================*
C     | written by Martin Losch, Jan 2013
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     newtonIter :: current iterate of Newton iteration
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER newtonIter
C     JFNKresidual :: Residual at the beginning of the FGMRES iteration,
C                     changes with newtonIter (updated)
      _RL     JFNKresidual
C     du/vIce   :: ice velocity increment to be added to u/vIce (input)
      _RL duIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vIce    :: ice velocity increment to be added to u/vIce (updated)
      _RL uIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vIceRes :: residual of sea-ice momentum equations (output)
      _RL uIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj :: loop indices
      INTEGER i,j,bi,bj
      INTEGER l
      _RL     resLoc, facLS
      LOGICAL doLineSearch
C     nVec    :: size of the input vector(s)
C     resTmp  :: vector version of the residuals
      INTEGER nVec
      PARAMETER ( nVec  = 2*sNx*sNy )
      _RL resTmp (nVec,1,nSx,nSy)

      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C     Initialise some local variables
      l = 0
      resLoc = JFNKresidual
      facLS = 1. _d 0
      doLineSearch = .TRUE.
      DO WHILE ( doLineSearch )
C     The computation of the line search factor is complicated because
C     we overwrite the solution, so that we have to reconstruct what
C     should be just u_l = u_0 + (1-gamma)**l *du. Instead, after adding
C     du/vIce in the first iteration (l=0), we substract a decreasing
C     part of du/vIce from u/vIce in the next iterations until the
C     residual is smaller than the initial residual.
       IF ( l .GT. 0 ) facLS = - SEAICE_JFNK_lsGamma
     &       *(1. _d 0 - SEAICE_JFNK_lsGamma)**(l-1)
C     Create update
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           uIce(I,J,bi,bj) = uIce(I,J,bi,bj)+facLS*duIce(I,J,bi,bj)
           vIce(I,J,bi,bj) = vIce(I,J,bi,bj)+facLS*dvIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C     Compute current residual F(u), (includes re-computation of global
C     variables DWATN, zeta, and eta, i.e. they are different after this)
       CALL SEAICE_CALC_RESIDUAL(
     I      uIce, vIce,
     O      uIceRes, vIceRes,
     I      newtonIter, 0, myTime, myIter, myThid )
C     Important: Compute the norm of the residual using the same scalar
C     product that SEAICE_FGMRES does
       CALL SEAICE_MAP2VEC(nVec,uIceRes,vIceRes,resTmp,.TRUE.,myThid)
       CALL SEAICE_SCALPROD(nVec,1,1,1,resTmp,resTmp,resLoc,myThid)
       resLoc = SQRT(resLoc)
C     Determine, if we need more iterations
       doLineSearch = resLoc .GE. JFNKresidual
C     Limit the maximum number of iterations arbitrarily to
C     SEAICE_JFNK_lsLmax (default is 4)
       doLineSearch = doLineSearch .AND. l .LE. SEAICE_JFNK_lsLmax
C     For the first iteration du/vIce = 0 and there will be no
C     improvement of the residual possible, so we do only the first
C     iteration
       IF ( newtonIter .EQ. 1 ) doLineSearch = .FALSE.
C     Only start a linesearch after some Newton iterations
       IF ( newtonIter .LE. SEAICE_JFNK_lsIter ) doLineSearch = .FALSE.
C     some output diagnostics
       IF ( debugLevel.GE.debLevA .AND. doLineSearch ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(2A,2(1X,I6),1X,3E12.5)')
     &       ' S/R SEAICE_JFNK_UPDATE: Newton iter, LSiter, ',
     &       'facLS, JFNKresidual, resLoc = ',
     &        newtonIter, l, facLS, JFNKresidual, resLoc
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
       ENDIF
C     Increment counter
       l = l + 1
      ENDDO
C     This is the new residual
      JFNKresidual = resLoc

#endif /* SEAICE_CGRID and SEAICE_ALLOW_JFNK */

      RETURN
      END
