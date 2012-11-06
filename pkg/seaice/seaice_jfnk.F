C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_jfnk.F,v 1.3 2012/11/06 12:53:14 mlosch Exp $
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

C     i,j,bi,bj :: loop indices
      INTEGER i,j,bi,bj
C     loop indices
      INTEGER newtonIter, newtonIterFail
      INTEGER krylovIter, krylovIterFail
      INTEGER totalKrylovIter
C     FGMRES flag that indicates what to do next
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
      _RL dwatPre (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pressPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEOP

C     Initialise
      newtonIter      = 0
      newtonIterFail  = 0
      krylovIterFail  = 0
      totalKrylovIter = 0
      JFNKconverged   = .FALSE.
      JFNKtol         = 0. _d 0
      JFNKresidual    = 0. _d 0
      JFNKresidualKm1 = 0. _d 0
      FGMRESeps       = 0. _d 0
      recip_deltaT    = 1. _d 0 / SEAICE_deltaTdyn
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
            dwatPre(I,J,bi,bj) = DWATN(I,J,bi,bj)
           pressPre(I,J,bi,bj) = press(I,J,bi,bj)
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
        WRITE(msgBuf,'(2A,2(1XI6),2E12.5)') 
     &       ' S/R SEAICE_JFNK: newtonIter,',
     &       ' total newtonIter, JFNKgamma_lin, initial norm = ',
     &       newtonIter,SEAICEnewtonIterMax*(myIter-nIter0)+newtonIter, 
     &       JFNKgamma_lin, JFNKresidual
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
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
     I        FGMRESeps,  
     I        newtonIter, krylovIter, myTime, myIter, myThid )
C     FGMRES returns iCode either asking for an new preconditioned vector
C     or product of matrix (Jacobian) times vector. For iCode = 0, terminate
C     iteration
         IF (iCode.EQ.1) THEN
C     Call preconditioner
          CALL SEAICE_PRECONDITIONER( 
     U         duIce, dvIce, 
     I         zetaPre, etaPre, dwatPre, pressPre,
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
        totalKrylovIter = totalKrylovIter + krylovIter
C     some output diagnostics
        IF ( debugLevel.GE.debLevA ) THEN
         WRITE(msgBuf,'(3(A,I6))')
     &        ' S/R SEAICE_JFNK: Newton iterate / total = ', newtonIter, 
     &        ' / ', SEAICEnewtonIterMax*(myIter-nIter0)+newtonIter, 
     &        ', Nb. of FGMRES iterations = ', krylovIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
        ENDIF
        IF ( krylovIter.EQ.SEAICEkrylovIterMax ) THEN
         krylovIterFail = krylovIterFail + 1
        ENDIF
C     Update linear solution vector and return to Newton iteration
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO J=1-Oly,sNy+Oly
           DO I=1-Olx,sNx+Olx
            uIce(I,J,bi,bj) = uIce(I,J,bi,bj)+duIce(I,J,bi,bj)
            vIce(I,J,bi,bj) = vIce(I,J,bi,bj)+dvIce(I,J,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C     Set the stopping criterion for the Newton iteration
        IF ( newtonIter .EQ. 1 ) JFNKtol=JFNKgamma_nonlin*JFNKresidual
       ENDIF
C     end of Newton iterate
      ENDDO
C     some output diagnostics
      IF ( debugLevel.GE.debLevA ) THEN
C     Record failure
       IF ( newtonIter .EQ. SEAICEnewtonIterMax ) THEN
        newtonIterFail = newtonIterFail + 1 
        WRITE(msgBuf,'(A,I10)') 
     &       ' S/R SEAICE_JFNK: JFNK did not converge in timestep ',
     &       myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( krylovIterFail .GT. 0 ) THEN
        WRITE(msgBuf,'(A,I4,A,I10)') 
     &       ' S/R SEAICE_JFNK: FGMRES did not converge ',
     &       krylovIterFail, ' times in timestep ', myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
       ENDIF
       WRITE(msgBuf,'(A,I6)') 
     &      ' S/R SEAICE_JFNK: Total number FGMRES iterations = ',
     &      totalKrylovIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
       
      ENDIF

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_JFNK */

      RETURN
      END
