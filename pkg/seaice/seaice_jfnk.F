C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_jfnk.F,v 1.10 2012/11/26 08:04:50 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_JFNK
C     !INTERFACE:
      SUBROUTINE SEAICE_JFNK( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_JFKF
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

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if ( (defined SEAICE_CGRID) && \
      (defined SEAICE_ALLOW_JFNK) && \
      (defined SEAICE_ALLOW_DYNAMICS) )
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     i,j,bi,bj :: loop indices
      INTEGER i,j,bi,bj
C     loop indices
      INTEGER newtonIter
      INTEGER krylovIter, krylovFails
      INTEGER totalKrylovItersLoc
C     FGMRES flag that determines amount of output messages of fgmres
      INTEGER iOutFGMRES
C     FGMRES flag that indicates what fgmres wants us to do next
      INTEGER iCode
      _RL     JFNKresidual, JFNKresidualTile(nSx,nSy)
      _RL     JFNKresidualKm1
C     parameters to compute convergence criterion
      _RL     phi_e, alp_e, JFNKgamma_lin
      _RL     FGMRESeps
      _RL     JFNKtol
C     
      _RL     recip_deltaT
      LOGICAL JFNKconverged, krylovConverged
      LOGICAL writeNow
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C
C     u/vIceRes :: residual of sea-ice momentum equations
      _RL uIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     du/vIce   :: ice velocity increment to be added to u/vIce
      _RL duIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     precomputed (= constant per Newton iteration) versions of 
C     zeta, eta, and DWATN, press
      _RL zetaPre (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaPre  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaZPre (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dwatPre (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
C     iOutFgmres=1 gives a little bit of output
      IF ( debugLevel.GE.debLevA .AND.
     &     DIFFERENT_MULTIPLE( SEAICE_monFreq, myTime, deltaTClock ) )
     &     iOutFGMRES=1

C     
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          uIceRes(I,J,bi,bj) = 0. _d 0
          vIceRes(I,J,bi,bj) = 0. _d 0
          duIce  (I,J,bi,bj) = 0. _d 0
          dvIce  (I,J,bi,bj) = 0. _d 0
          uIceNm1(I,J,bi,bj) = uIce(I,J,bi,bj)
          vIceNm1(I,J,bi,bj) = vIce(I,J,bi,bj)
         ENDDO
        ENDDO
C     Compute things that do no change during the Newton iteration:
C     sea-surface tilt and wind stress: 
C     FORCEX/Y0 - mass*(u/vIceNm1)/deltaT
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          FORCEX(I,J,bi,bj) = FORCEX0(I,J,bi,bj)
     &         + seaiceMassU(I,J,bi,bj)*uIceNm1(I,J,bi,bj)*recip_deltaT    
          FORCEY(I,J,bi,bj) = FORCEY0(I,J,bi,bj)
     &         + seaiceMassV(I,J,bi,bj)*vIceNm1(I,J,bi,bj)*recip_deltaT    
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Start nonlinear Newton iteration: outer loop iteration
      DO WHILE ( newtonIter.LT.SEAICEnewtonIterMax .AND.
     &     .NOT.JFNKconverged )
       newtonIter = newtonIter + 1
C     Compute initial residual F(u), (includes computation of global
C     variables DWATN, zeta, and eta)
       CALL SEAICE_CALC_RESIDUAL( 
     I      uIce, vIce, 
     O      uIceRes, vIceRes, 
     I      newtonIter, 0, myTime, myIter, myThid )
       CALL EXCH_UV_XY_RL( uIceRes, vIceRes,.TRUE.,myThid)
C     local copies of precomputed coefficients that are to stay
C     constant for the preconditioner
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           zetaPre(I,J,bi,bj) =  zeta(I,J,bi,bj)
            etaPre(I,J,bi,bj) =   eta(I,J,bi,bj)
           etaZPre(I,J,bi,bj) =  etaZ(I,J,bi,bj)
           dwatPre(I,J,bi,bj) = DWATN(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C     
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         JFNKresidualTile(bi,bj) = 0. _d 0
         DO J=1,sNy
          DO I=1,sNx
#ifdef CG2D_SINGLECPU_SUM
           JFNKlocalBuf(I,J,bi,bj) = 
#else
           JFNKresidualTile(bi,bj) = JFNKresidualTile(bi,bj) + 
#endif
     &          uIceRes(I,J,bi,bj)*uIceRes(I,J,bi,bj) +
     &          vIceRes(I,J,bi,bj)*vIceRes(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       JFNKresidual = 0. _d 0
#ifdef CG2D_SINGLECPU_SUM
       CALL GLOBAL_SUM_SINGLECPU_RL(
     &         JFNKlocalBuf,JFNKresidual, 0, 0, myThid)
#else
       CALL GLOBAL_SUM_TILE_RL( JFNKresidualTile,JFNKresidual,myThid )
#endif
       JFNKresidual = SQRT(JFNKresidual)
C     compute convergence criterion for linear preconditioned FGMRES
       JFNKgamma_lin = JFNKgamma_lin_max
       IF ( newtonIter.GT.1.AND.newtonIter.LE.100
     &      .AND.JFNKresidual.LT.JFNKres_t ) THEN
C     Eisenstat, 1996, equ.(2.6)      
        phi_e = 1. _d 0
        alp_e = 1. _d 0
        JFNKgamma_lin = phi_e*( JFNKresidual/JFNKresidualKm1 )**alp_e
        JFNKgamma_lin = min(JFNKgamma_lin_max, JFNKgamma_lin)
        JFNKgamma_lin = max(JFNKgamma_lin_min, JFNKgamma_lin)
       ENDIF
C     save the residual for the next iteration
       JFNKresidualKm1 = JFNKresidual
C
C     The Krylov iteration using FGMRES, the preconditioner is LSOR
C     for now. The code is adapted from SEAICE_LSR, but heavily stripped
C     down.
C     krylovIter is mapped into "its" in seaice_fgmres and is incremented
C     in that routine
       krylovIter    = 0
       iCode         = 0
       IF ( debugLevel.GE.debLevA ) THEN  
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(2A,2(1XI6),2E12.5)') 
     &       ' S/R SEAICE_JFNK: newtonIter,',
     &       ' total newtonIter, JFNKgamma_lin, initial norm = ',
     &       newtonIter,SEAICEnewtonIterMax*(myIter-nIter0)+newtonIter, 
     &       JFNKgamma_lin, JFNKresidual
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
       ENDIF
C
       JFNKconverged = JFNKresidual.LT.JFNKtol
C
C     do Krylov loop only if convergence is not reached
C
       IF ( .NOT.JFNKconverged ) THEN
C
C     start Krylov iteration (FGMRES)
C
        krylovConverged = .FALSE.
        FGMRESeps = JFNKgamma_lin * JFNKresidual
        DO WHILE ( .NOT.krylovConverged ) 
C     solution vector sol = du/vIce
C     residual vector (rhs) Fu = u/vIceRes
C     output work vectors wk1, -> input work vector wk2 
C     
         CALL SEAICE_FGMRES_DRIVER(
     I        uIceRes, vIceRes, 
     U        duIce, dvIce, iCode,
     I        FGMRESeps, iOutFGMRES,
     I        newtonIter, krylovIter, myTime, myIter, myThid )
C     FGMRES returns iCode either asking for an new preconditioned vector
C     or product of matrix (Jacobian) times vector. For iCode = 0, terminate
C     iteration
         IF (iCode.EQ.1) THEN
C     Call preconditioner 
          IF ( SOLV_MAX_ITERS .GT. 0 )
     &         CALL SEAICE_PRECONDITIONER( 
     U         duIce, dvIce, 
     I         zetaPre, etaPre, etaZpre, dwatPre, 
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
         WRITE(msgBuf,'(3(A,I6))')
     &        ' S/R SEAICE_JFNK: Newton iterate / total = ', newtonIter, 
     &        ' / ', SEAICEnewtonIterMax*(myIter-nIter0)+newtonIter, 
     &        ', Nb. of FGMRES iterations = ', krylovIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
        ENDIF
        IF ( krylovIter.EQ.SEAICEkrylovIterMax ) THEN
         krylovFails = krylovFails + 1
        ENDIF
C     Update linear solution vector and return to Newton iteration
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO J=1-Oly,sNy+Oly
           DO I=1-Olx,sNx+Olx
            uIce(I,J,bi,bj) = uIce(I,J,bi,bj)+duIce(I,J,bi,bj)
            vIce(I,J,bi,bj) = vIce(I,J,bi,bj)+dvIce(I,J,bi,bj)
C     reset du/vIce here instead of setting sol = 0 in seaice_fgmres_driver
            duIce(I,J,bi,bj)= 0. _d 0
            dvIce(I,J,bi,bj)= 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C     Set the stopping criterion for the Newton iteration
        IF ( newtonIter .EQ. 1 ) JFNKtol=JFNKgamma_nonlin*JFNKresidual
       ENDIF
C     end of Newton iterate
      ENDDO
C
C--   Output diagnostics
C
      IF ( SEAICE_monFreq .GT. 0. _d 0 ) THEN
C     Count iterations
       totalJFNKtimeSteps = totalJFNKtimeSteps + 1
       totalNewtonIters   = totalNewtonIters + newtonIter
       totalKrylovIters   = totalKrylovIters + totalKrylovItersLoc
C     Record failure
       totalKrylovFails   = totalKrylovFails + krylovFails
       IF ( newtonIter .EQ. SEAICEnewtonIterMax ) THEN
        totalNewtonFails = totalNewtonFails + 1 
       ENDIF
      ENDIF
C     Decide whether it is time to dump and reset the counter
      writeNow = DIFFERENT_MULTIPLE(SEAICE_monFreq,
     &     myTime+deltaTClock, deltaTClock) 
#ifdef ALLOW_CAL
      IF ( useCAL ) THEN
       CALL CAL_TIME2DUMP( 
     I      zeroRL, SEAICE_monFreq,  deltaTClock,
     U      writeNow,
     I      myTime+deltaTclock, myIter+1, myThid )
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
       WRITE(msgBuf,'(A)') ' // Begin JFNK statistics'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') 
     &' // ======================================================='
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )
C     reset and start again
       totalJFNKtimeSteps = 0
       totalNewtonIters   = 0
       totalKrylovIters   = 0
       totalKrylovFails   = 0
       totalNewtonFails   = 0
      ENDIF

C     Print more debugging information
      IF ( debugLevel.GE.debLevA ) THEN
       IF ( newtonIter .EQ. SEAICEnewtonIterMax ) THEN
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

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_JFNK */

      RETURN
      END
