#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C--  File seaice_krylov.F: seaice krylov dynamical solver S/R:

CBOP
C     !ROUTINE: SEAICE_KRYLOV
C     !INTERFACE:
      SUBROUTINE SEAICE_KRYLOV( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_KRYLOV
C     | o Picard solver for ice dynamics using a preconditioned
C     |   KRYLOV (Generalized Minimum RESidual=GMRES) method for
C     |   solving the linearised system following J.-F. Lemieux
C     |   et al., JGR 113, doi:10.1029/2007JC004680, 2008.
C     *==========================================================*
C     | written by Martin Losch, Jan 2016
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

#if ( defined SEAICE_CGRID && defined SEAICE_ALLOW_KRYLOV )
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj :: loop indices
      INTEGER i,j,bi,bj
C     loop indices
      INTEGER picardIter
      INTEGER krylovIter, krylovFails
      INTEGER krylovIterMax, picardIterMax
      INTEGER totalKrylovItersLoc, totalPicardItersLoc
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
      _RL     picardResidual
      _RL     picardResidualKm1
C     parameters to compute convergence criterion
      _RL     krylovLinTol
      _RL     FGMRESeps
      _RL     picardTol
C     backward differences extrapolation factors
      _RL bdfFac, bdfAlpha
C
      _RL     recip_deltaT
      LOGICAL picardConverged, krylovConverged
      LOGICAL writeNow
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _RL uIceLin(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLin(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     extra time level required for backward difference time stepping
      _RL duIcNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIcNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vWork   :: work arrays
      _RL uWork  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vWork  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vIceLHS :: left hand side of momentum equation (A*x)
      _RL uIceLHS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLHS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vIceRHS :: right hand side of momentum equation (b)
      _RL uIceRHS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceRHS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     helper array
      _RL resTmp (nVec,1,nSx,nSy)
C     work arrays
      _RL rhs(nVec,nSx,nSy), sol(nVec,nSx,nSy)
      _RL vv(nVec,im+1,nSx,nSy), w(nVec,im,nSx,nSy)
      _RL wk1(nVec,nSx,nSy), wk2(nVec,nSx,nSy)
CEOP

C     Initialise
      picardIter          = 0
      krylovFails         = 0
      totalKrylovItersLoc = 0
      picardConverged     = .FALSE.
      picardTol           = 0. _d 0
      picardResidual      = 0. _d 0
      picardResidualKm1   = 0. _d 0
      FGMRESeps           = 0. _d 0
      recip_deltaT        = 1. _d 0 / SEAICE_deltaTdyn

      krylovIterMax       = SEAICElinearIterMax
      picardIterMax       = SEAICEnonLinIterMax
      IF ( SEAICEusePicardAsPrecon ) THEN
       krylovIterMax       = SEAICEpreconlinIter
       picardIterMax       = SEAICEpreconNL_Iter
      ENDIF

      iOutFGMRES=0
C     with iOutFgmres=1, seaice_fgmres prints the residual at each iteration
      IF ( debugLevel.GE.debLevC .AND.
     &     .NOT.SEAICEusePicardAsPrecon .AND.
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
          uIceLHS(I,J,bi,bj) = 0. _d 0
          vIceLHS(I,J,bi,bj) = 0. _d 0
          uIceRHS(I,J,bi,bj) = 0. _d 0
          vIceRHS(I,J,bi,bj) = 0. _d 0
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
          uIceLin(I,J,bi,bj) = uIce(I,J,bi,bj)
          vIceLin(I,J,bi,bj) = vIce(I,J,bi,bj)
         ENDDO
        ENDDO
C     Compute things that do no change during the OL iteration:
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
C     Start nonlinear Picard iteration: outer loop iteration
      DO WHILE ( picardIter.LT.picardIterMax .AND.
     &     .NOT.picardConverged )
       picardIter = picardIter + 1
C     smooth ice velocities in time for computation of
C     the non-linear drag coefficents
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIceLin(I,J,bi,bj) = 0.5 _d 0 *
     &          (uIce(I,J,bi,bj) + uIceLin(I,J,bi,bj))
           vIceLin(I,J,bi,bj) = 0.5 _d 0 *
     &          (vIce(I,J,bi,bj) + vIceLin(I,J,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C     u/vIce have changed in Picard iteration so that new drag
C     coefficients and viscosities are required (that will not change in
C     the Krylov iteration)
       CALL SEAICE_OCEANDRAG_COEFFS(
     I      uIceLin, vIceLin, HEFFM,
     O      DWATN,
     I      0, myTime, myIter, myThid )
#ifdef SEAICE_ALLOW_BOTTOMDRAG
       CALL SEAICE_BOTTOMDRAG_COEFFS(
     I      uIceLin, vIceLin, HEFFM,
#ifdef SEAICE_ITD
     I      HEFFITD, AREAITD, AREA,
#else
     I      HEFF, AREA,
#endif
     O      CbotC,
     I      0, myTime, myIter, myThid )
#endif /* SEAICE_ALLOW_BOTTOMDRAG */
       CALL SEAICE_CALC_STRAINRATES(
     I      uIceLin, vIceLin,
     O      e11, e22, e12,
     I      0, myTime, myIter, myThid )
       CALL SEAICE_CALC_VISCOSITIES(
     I      e11, e22, e12, SEAICE_zMin, SEAICE_zMax, HEFFM, press0,
     I      tensileStrFac,
     O      eta, etaZ, zeta, zetaZ, press, deltaC,
     I      0, myTime, myIter, myThid )
C     compute rhs that does not change during Krylov iteration
       CALL SEAICE_CALC_RHS(
     O      uIceRHS, vIceRHS,
     I      picardIter, 0, myTime, myIter, myThid )
C     compute rhs for initial residual
       CALL SEAICE_CALC_LHS(
     I         uIce, vIce,
     O         uIceLHS, vIceLHS,
     I         picardIter, myTime, myIter, myThid )
C     Calculate the residual
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           uIceLHS(I,J,bi,bj) = uIceLHS(I,J,bi,bj) - uIceRHS(I,J,bi,bj)
           vIceLHS(I,J,bi,bj) = vIceLHS(I,J,bi,bj) - vIceRHS(I,J,bi,bj)
C     save u/vIceLin as k-2nd step for linearization (does not work properly)
CML           uIceLin(I,J,bi,bj) = uIce(I,J,bi,bj)
CML           vIceLin(I,J,bi,bj) = vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C     Important: Compute the norm of the residual using the same scalar
C     product that SEAICE_FGMRES does
       CALL SEAICE_MAP2VEC(nVec,uIceLHS,vIceLHS,resTmp,.TRUE.,myThid)
       CALL SEAICE_SCALPROD(nVec,1,1,1,resTmp,resTmp,
     &      picardResidual,myThid)
       picardResidual = SQRT(picardResidual)

C     compute convergence criterion for linear preconditioned FGMRES
       krylovLinTol = JFNKgamma_lin_max
C     the best method is still not clear to me
C     this is described in Lemieux et al 2008
CML       IF ( picardIter .EQ. 1 ) krylovLinTol = 1./10.
CML       IF ( picardIter .EQ. 2 ) krylovLinTol = 1./20.
CML       IF ( picardIter .EQ. 3 ) krylovLinTol = 1./20.
CML       IF ( picardIter .EQ. 4 ) krylovLinTol = 1./30.
CML       IF ( picardIter .EQ. 5 ) krylovLinTol = 1./30.
CML       IF ( picardIter .EQ. 6 ) krylovLinTol = 1./30.
CML       IF ( picardIter .EQ. 7 ) krylovLinTol = 1./30.
CML       IF ( picardIter .EQ. 8 ) krylovLinTol = 1./40.
CML       IF ( picardIter .EQ. 9 ) krylovLinTol = 1./50.
CML       IF ( picardIter .GT. 9 ) krylovLinTol = 1./80.
C     this is used with the JFNK solver, but the Picard-Krylov solver
C     converges too slowly for this scheme
CML       IF ( picardIter.GT.1.AND.picardResidual.LT.JFNKres_t ) THEN
CMLC     Eisenstat and Walker (1996), eq.(2.6)
CML        krylovLinTol = SEAICE_JFNKphi
CML     &       *( picardResidual/picardResidualKm1 )**SEAICE_JFNKalpha
CML        krylovLinTol = min(JFNKgamma_lin_max, krylovLinTol)
CML        krylovLinTol = max(JFNKgamma_lin_min, krylovLinTol)
CML       ENDIF
CML       krylovLinTol = 1. _d -1
C     save the residual for the next iteration
       picardResidualKm1 = picardResidual

C     The Krylov iteration uses FGMRES, the preconditioner is LSOR
C     for now. The code is adapted from SEAICE_LSR, but heavily stripped
C     down.
C     krylovIter is mapped into "its" in seaice_fgmres and is incremented
C     in that routine
       krylovIter    = 0
       iCode         = 0

       picardConverged = picardResidual.LT.picardTol
     &      .OR.picardResidual.EQ.0. _d 0

C     do Krylov loop only if convergence is not reached

       IF ( .NOT.picardConverged ) THEN

C     start Krylov iteration (FGMRES)

        krylovConverged = .FALSE.
        FGMRESeps = krylovLinTol * picardResidual
        CALL SEAICE_MAP2VEC(nVec,uIce,vIce,sol,.TRUE.,myThid)
        CALL SEAICE_MAP2VEC(nVec,uIceRHS,vIceRHS,rhs,.TRUE.,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            uWork(i,j,bi,bj) = 0. _d 0
            vWork(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        DO WHILE ( .NOT.krylovConverged )
C     solution vector sol = u/vIce
C     residual vector (rhs) Fu = u/vIceRHS
C     output work vectors wk1, -> input work vector wk2

C     map results to wk2
         CALL SEAICE_MAP2VEC(nVec,uWork,vWork,wk2,.TRUE.,myThid)

         CALL SEAICE_FGMRES (nVec,im,rhs,sol,ifgmres,krylovIter,
     U        vv,w,wk1,wk2,
     I        FGMRESeps,krylovIterMax,iOutFGMRES,
     U        iCode,
     I        myThid)
C
         IF ( iCode .EQ. 0 ) THEN
C     map sol(ution) vector to u/vIce
          CALL SEAICE_MAP2VEC(nVec,uIce,vIce,sol,.FALSE.,myThid)
          CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)
         ELSE
C     map work vector to du/vIce to either compute a preconditioner
C     solution (wk1=rhs) or a matrix times wk1
          CALL SEAICE_MAP2VEC(nVec,uWork,vWork,wk1,.FALSE.,myThid)
          CALL EXCH_UV_XY_RL( uWork, vWork,.TRUE.,myThid)
         ENDIF

C     FGMRES returns iCode either asking for an new preconditioned vector
C     or product of matrix times vector. For iCode = 0, terminate
C     iteration
         IF (iCode.EQ.1) THEN
C     Call preconditioner
          IF ( SEAICEpreconLinIter .GT. 0 )
     &         CALL SEAICE_PRECONDITIONER(
     U         uWork, vWork,
     I         zeta, eta, etaZ, zetaZ, dwatn,
     I         picardIter, krylovIter, myTime, myIter, myThid )
         ELSEIF (iCode.GE.2) THEN
C     Compute lhs of equations (A*x)
          CALL SEAICE_CALC_STRAINRATES(
     I         uWork, vWork,
     O         e11, e22, e12,
     I         krylovIter, myTime, myIter, myThid )
          CALL SEAICE_CALC_LHS(
     I         uWork, vWork,
     O         uIceLHS, vIceLHS,
     I         picardIter, myTime, myIter, myThid )
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              uWork(i,j,bi,bj) = uIceLHS(i,j,bi,bj)
              vWork(i,j,bi,bj) = vIceLHS(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         krylovConverged = iCode.EQ.0
C     End of Krylov iterate
        ENDDO
        totalKrylovItersLoc = totalKrylovItersLoc + krylovIter
C     some output diagnostics
        IF ( debugLevel.GE.debLevA
     &       .AND. .NOT.SEAICEusePicardAsPrecon ) THEN
         _BEGIN_MASTER( myThid )
         totalPicardItersLoc =
     &        picardIterMax*(myIter-nIter0)+picardIter
         WRITE(msgBuf,'(2A,2(1X,I6),2E12.5)')
     &        ' S/R SEAICE_KRYLOV: Picard iterate / total, ',
     &        'KRYLOVgamma_lin, initial norm = ',
     &        picardIter, totalPicardItersLoc,
     &        krylovLinTol,picardResidual
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(3(A,I6))')
     &        ' S/R SEAICE_KRYLOV: Picard iterate / total = ',
     &        picardIter, ' / ', totalPicardItersLoc,
     &        ', Nb. of FGMRES iterations = ', krylovIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
        ENDIF
        IF ( krylovIter.EQ.krylovIterMax ) THEN
         krylovFails = krylovFails + 1
        ENDIF
C     Set the stopping criterion for the Picard iteration and the
C     criterion for the transition from accurate to approximate FGMRES
        IF ( picardIter .EQ. 1 ) THEN
         picardTol=SEAICEnonLinTol*picardResidual
         IF ( JFNKres_tFac .NE. UNSET_RL )
     &        JFNKres_t = picardResidual * JFNKres_tFac
        ENDIF
       ENDIF
C     end of Picard iterate
      ENDDO

C--   Output diagnostics

      IF ( SEAICE_monFreq .GT. 0. _d 0 ) THEN
C     Only Master Thread updates counters in common block:
      _BEGIN_MASTER(myThid)
C     Cumulate some diagnostic counters for the Krylov solver
       totalJFNKtimeSteps = totalJFNKtimeSteps + 1
       totalNewtonIters   = totalNewtonIters + picardIter
       totalKrylovIters   = totalKrylovIters + totalKrylovItersLoc
C     Record failure
       totalKrylovFails   = totalKrylovFails + krylovFails
       IF ( picardIter .EQ. picardIterMax ) THEN
        totalNewtonfails = totalNewtonfails + 1
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
       WRITE(msgBuf,'(A)') ' // Begin KRYLOV statistics'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &' // ======================================================='
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %KRYLOV_MON: time step              = ', myIter+1
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %KRYLOV_MON: Nb. of time steps      = ',
     &      totalJFNKtimeSteps
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %KRYLOV_MON: Nb. of Picard steps    = ', totalNewtonIters
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %KRYLOV_MON: Nb. of Krylov steps    = ', totalKrylovIters
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %KRYLOV_MON: Nb. of Picard failures = ', totalNewtonfails
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,I10)')
     &      ' %KRYLOV_MON: Nb. of Krylov failures = ', totalKrylovFails
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &' // ======================================================='
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') ' // End KRYLOV statistics'
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
       totalNewtonfails   = 0
       _END_MASTER( myThid )
      ENDIF

C     Print more debugging information
      IF ( debugLevel.GE.debLevA
     &     .AND. .NOT.SEAICEusePicardAsPrecon ) THEN
       IF ( picardIter .EQ. picardIterMax ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A,I10)')
     &       ' S/R SEAICE_KRYLOV: Solver did not converge in timestep ',
     &       myIter+1
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
       ENDIF
       IF ( krylovFails .GT. 0 ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A,I4,A,I10)')
     &       ' S/R SEAICE_KRYLOV: FGMRES did not converge ',
     &       krylovFails, ' times in timestep ', myIter+1
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
       ENDIF
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(A,I6,A,I10)')
     &      ' S/R SEAICE_KRYLOV: Total number FGMRES iterations = ',
     &      totalKrylovItersLoc, ' in timestep ', myIter+1
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )
      ENDIF

#endif /* SEAICE_CGRID and SEAICE_ALLOW_KRYLOV */

      RETURN
      END
