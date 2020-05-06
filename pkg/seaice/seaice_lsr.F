#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#else
# define OBCS_UVICE_OLD
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C--  File seaice_lsr.F: seaice LSR dynamical solver S/R:
C--   Contents
C--   o SEAICE_LSR
C--   o SEAICE_RESIDUAL
C--   o SEAICE_LSR_CALC_COEFFS
C--   o SEAICE_LSR_RHSU
C--   o SEAICE_LSR_RHSV
C--   o SEAICE_LSR_TRIDIAGU
C--   o SEAICE_LSR_TRIDIAGV

CBOP
C     !ROUTINE: SEAICE_LSR
C     !INTERFACE:
      SUBROUTINE SEAICE_LSR( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_LSR
C     | o Solve ice momentum equation with an LSR dynamics solver
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997
C     |    and Zhang and Rothrock, MWR, 131,  845- 861, 2003)
C     |   Written by Jinlun Zhang, PSC/UW, Feb-2001
C     |                     zhang@apl.washington.edu
C     *==========================================================*
C     | C-grid version by Martin Losch
C     |   Since 2009/03/18: finite-Volume discretization of
C     |   stress divergence that includes all metric terms
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
# include "AUTODIFF_PARAMS.h"
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime     :: Simulation time
C     myIter     :: Simulation timestep number
C     myThid     :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj  :: Loop counters

      INTEGER ipass, ipass0
      INTEGER i, j, m, bi, bj
      INTEGER iMin, iMax, jMin, jMax
      INTEGER ICOUNT1, ICOUNT2, linearIterLoc, nonLinIterLoc
      INTEGER kSrf
      INTEGER SOLV_MAX_TMP
      LOGICAL doIterate4u, doIterate4v
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _RL WFAU, WFAV, WFAU2, WFAV2
      _RL S1, S2, S1A, S2A
      _RL recip_deltaT

C     diagonals of coefficient matrices
      _RL AU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     RHS of TriDiag Linear system (both directions => 5 coef A,B,C & Rt1,2)
      _RL rhsU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef SEAICE_GLOBAL_3DIAG_SOLVER
C     RHS of TriDiag Linear system (1 direction only => 3 coef A,B,C)
      _RL rhsUx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsVy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iSubIter
      INTEGER errCode
#endif
C     coefficients for lateral points, u(j+/-1)
      _RL uRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, v(i+/-1)
      _RL vRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     abbreviations
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     symmetric drag coefficient
      _RL dragSym     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     intermediate time level centered (C) between n and n-1
      _RL uIceC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     auxillary fields
      _RL uTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     fractional area at velocity points
      _RL areaW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL areaS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef SEAICE_ALLOW_MOM_ADVECTION
C     tendency due to advection of momentum
      _RL gUmom(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gVmom(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /*  SEAICE_ALLOW_MOM_ADVECTION */
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER itmpkey, itmpkey2, itmpkey3
#endif
      _RL COSWAT
      _RS SINWAT
      _RL UERR
#ifdef SEAICE_ALLOW_CHECK_LSR_CONVERGENCE
      _RL resnorm, EKnorm, counter
#endif /* SEAICE_ALLOW_CHECK_LSR_CONVERGENCE */
      LOGICAL printResidual
      _RL residUini, residVini, residUend, residVend
#ifdef SEAICE_ALLOW_FREEDRIFT
      _RL uRes (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRes (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL errIni, errFD, errSum
      _RL residU_fd, residV_fd
      _RL residUmix, residVmix
#endif /* SEAICE_ALLOW_FREEDRIFT */
      _RL areaMinLoc

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      printResidual = debugLevel.GE.debLevA
     &  .AND. DIFFERENT_MULTIPLE( SEAICE_monFreq, myTime, deltaTClock )

C     extra overlap for (restricted) additive Schwarz method
      jMin = 1   - SEAICE_OLy
      jMax = sNy + SEAICE_OLy
      iMin = 1   - SEAICE_OLx
      iMax = sNx + SEAICE_OLx
C
      linearIterLoc = SEAICElinearIterMax
      nonLinIterLoc = SEAICEnonLinIterMax
      IF ( SEAICEusePicardAsPrecon ) THEN
       linearIterLoc = SEAICEpreconlinIter
       nonLinIterLoc = SEAICEpreconNL_Iter
      ENDIF
C
      recip_deltaT = 1. _d 0 / SEAICE_deltaTdyn

#ifdef ALLOW_AUTODIFF_TAMC
cph break artificial dependencies
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          deltaC (i,j,bi,bj) = 0. _d 0
          press  (i,j,bi,bj) = 0. _d 0
          zeta   (i,j,bi,bj) = 0. _d 0
          zetaZ  (i,j,bi,bj) = 0. _d 0
          eta    (i,j,bi,bj) = 0. _d 0
          etaZ   (i,j,bi,bj) = 0. _d 0
          uIceC  (i,j,bi,bj) = 0. _d 0
          vIceC  (i,j,bi,bj) = 0. _d 0
          uIceNm1(i,j,bi,bj) = 0. _d 0
          vIceNm1(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

C     initialise fractional areas at velocity points
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          areaW(I,J,bi,bj) = 1. _d 0
          areaS(I,J,bi,bj) = 1. _d 0
         ENDDO
        ENDDO
        areaMinLoc = 0. _d 0
        IF ( SEAICEscaleSurfStress ) THEN
         areaMinLoc = 1. _d -10
CML         areaMinLoc = SEAICE_EPS
         DO J=jMin,jMax
          DO I=iMin,iMax
           areaW(I,J,bi,bj) =
     &          0.5 _d 0*(AREA(I,J,bi,bj)+AREA(I-1,J,bi,bj))
           areaS(I,J,bi,bj) =
     &          0.5 _d 0*(AREA(I,J,bi,bj)+AREA(I,J-1,bi,bj))
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

      ipass0 = 2
C     modify time stepping a little if more than the default
C     modified Eulerian time step is used.
      IF ( nonLinIterLoc.GT.2 ) ipass0 = 1
#ifdef ALLOW_AUTODIFF_TAMC
      DO ipass=1,MPSEUDOTIMESTEPS
       itmpkey = (ikey_dynamics-1)*MPSEUDOTIMESTEPS + ipass
#else
      DO ipass=1,nonLinIterLoc
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef SEAICE_ALLOW_FREEDRIFT
CADJ STORE uice_fd, vice_fd = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
CADJ STORE ures,vres = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
# endif
CADJ STORE utmp,vtmp = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
#endif /* ALLOW_AUTODIFF_TAMC */

      IF ( ipass .LE. nonLinIterLoc ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
CADJ STORE vice = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
CADJ STORE uicenm1 = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
CADJ STORE vicenm1 = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
#endif /* ALLOW_AUTODIFF_TAMC */

C     surface level
      kSrf = 1
C--   introduce turning angles
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

C SET SOME VALUES
      WFAU=SEAICE_LSRrelaxU
      WFAV=SEAICE_LSRrelaxV
      WFAU2=ZERO
      WFAV2=ZERO
      S1 = 0.
      S2 = 0.
      S1A=0.80 _d 0
      S2A=0.80 _d 0

      IF ( ipass .EQ. 1 ) THEN
C NOW DO PREDICTOR TIME STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIceNm1(I,J,bi,bj) = uIce(I,J,bi,bj)
           vIceNm1(I,J,bi,bj) = vIce(I,J,bi,bj)
           uIceC  (I,J,bi,bj) = uIce(I,J,bi,bj)
           vIceC  (I,J,bi,bj) = vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
C NOW DO MODIFIED EULER STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIce (I,J,bi,bj)=HALF*(uIce(I,J,bi,bj)+uIceNm1(i,j,bi,bj))
           vIce (I,J,bi,bj)=HALF*(vIce(I,J,bi,bj)+vIceNm1(i,j,bi,bj))
           uIceC(I,J,bi,bj)=uIce(I,J,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( ipass .GT. ipass0 ) THEN
C     for additional (pseudo-time)steps update u/vIceNm1
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIceNm1(i,j,bi,bj)=uIce(I,J,bi,bj)
           vIceNm1(i,j,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
C     That is an important one! Note, that
C     * lsr is called multiple times (twice by default), thus the
C       ipass index is part of itmpkey
C     * this storing is still outside the lsr iteration loop
CADJ STORE uice = comlev1_dynsol, kind=isbyte, key = itmpkey
CADJ STORE vice = comlev1_dynsol, kind=isbyte, key = itmpkey
#endif /* ALLOW_AUTODIFF_TAMC */

      CALL SEAICE_CALC_STRAINRATES(
     I     uIceC, vIceC,
     O     e11, e22, e12,
     I     ipass, myTime, myIter, myThid )

      CALL SEAICE_CALC_VISCOSITIES(
     I     e11, e22, e12, zMin, zMax, HEFFM, press0, tensileStrFac,
     O     eta, etaZ, zeta, zetaZ, press, deltaC,
     I     ipass, myTime, myIter, myThid )

      CALL SEAICE_OCEANDRAG_COEFFS(
     I     uIceC, vIceC, HEFFM,
     O     DWATN,
     I     ipass, myTime, myIter, myThid )

#ifdef SEAICE_ALLOW_BOTTOMDRAG
      IF (SEAICEbasalDragK2.GT.0. _d 0) CALL SEAICE_BOTTOMDRAG_COEFFS(
     I     uIceC, vIceC, HEFFM,
#ifdef SEAICE_ITD
     I     HEFFITD, AREAITD, AREA,
#else
     I     HEFF, AREA,
#endif
     O     CbotC,
     I     ipass, myTime, myIter, myThid )
#endif /* SEAICE_ALLOW_BOTTOMDRAG */
C
C     some abbreviations
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=jMin-1,jMax
         DO I=iMin-1,iMax
          etaPlusZeta (I,J,bi,bj) = ETA (I,J,bi,bj)+ZETA(I,J,bi,bj)
          zetaMinusEta(I,J,bi,bj) = ZETA(I,J,bi,bj)-ETA (I,J,bi,bj)
         ENDDO
        ENDDO
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          dragSym(I,J,bi,bj) = DWATN(I,J,bi,bj)*COSWAT
#ifdef SEAICE_ALLOW_BOTTOMDRAG
     &         +CbotC(I,J,bi,bj)
#endif /* SEAICE_ALLOW_BOTTOMDRAG */
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C
C     Set up of right hand sides of momentum equations starts here
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=jMin,jMax
         DO I=iMin,iMax
C     set up anti symmetric drag force and add in current force
C     ( remember to average to correct velocity points )
          FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+
     &         ( 0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &         COSWAT * uVel(I,J,kSrf,bi,bj)
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &         ( DWATN(I  ,J,bi,bj) * 0.5 _d 0 *
     &          (vVel(I  ,J  ,kSrf,bi,bj)-vIceC(I  ,J  ,bi,bj)
     &          +vVel(I  ,J+1,kSrf,bi,bj)-vIceC(I  ,J+1,bi,bj))
     &         + DWATN(I-1,J,bi,bj) * 0.5 _d 0 *
     &          (vVel(I-1,J  ,kSrf,bi,bj)-vIceC(I-1,J  ,bi,bj)
     &          +vVel(I-1,J+1,kSrf,bi,bj)-vIceC(I-1,J+1,bi,bj))
     &         ) ) * areaW(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+
     &         ( 0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &         COSWAT * vVel(I,J,kSrf,bi,bj)
     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I,J  ,bi,bj) * 0.5 _d 0 *
     &          (uVel(I  ,J  ,kSrf,bi,bj)-uIceC(I  ,J  ,bi,bj)
     &          +uVel(I+1,J  ,kSrf,bi,bj)-uIceC(I+1,J  ,bi,bj))
     &         + DWATN(I,J-1,bi,bj) * 0.5 _d 0 *
     &          (uVel(I  ,J-1,kSrf,bi,bj)-uIceC(I  ,J-1,bi,bj)
     &          +uVel(I+1,J-1,kSrf,bi,bj)-uIceC(I+1,J-1,bi,bj))
     &         ) ) * areaS(I,J,bi,bj)
         ENDDO
        ENDDO
        DO j=jMin,jMax
         DO i=iMin,iMax
C-    add Coriolis term
          FORCEX(I,J,bi,bj) = FORCEX(I,J,bi,bj) + HALF*
     &         ( seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &          *0.5 _d 0*( vIceC( i ,j,bi,bj)+vIceC( i ,j+1,bi,bj) )
     &         + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &          *0.5 _d 0*( vIceC(i-1,j,bi,bj)+vIceC(i-1,j+1,bi,bj) ) )
          FORCEY(I,J,bi,bj) = FORCEY(I,J,bi,bj) - HALF*
     &         ( seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &         *0.5 _d 0*( uIceC(i  ,j  ,bi,bj)+uIceC(i+1,  j,bi,bj) )
     &         + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &         *0.5 _d 0*( uIceC(i  ,j-1,bi,bj)+uIceC(i+1,j-1,bi,bj) ) )
         ENDDO
        ENDDO
C      this is the rhs contribution of the time derivative
        DO j=jMin,jMax
         DO i=iMin,iMax
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         +seaiceMassU(I,J,bi,bj)*recip_deltaT
     &         *uIceNm1(i,j,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         +seaiceMassV(I,J,bi,bj)*recip_deltaT
     &         *vIceNm1(i,j,bi,bj)
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
         ENDDO
        ENDDO
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C
C     u-equation
C
        DO j=jMin,jMax
         DO i=iMin,iMax
          rhsU(I,J,bi,bj) = FORCEX(I,J,bi,bj)
         ENDDO
        ENDDO
        CALL SEAICE_LSR_RHSU(
     I       zetaMinusEta, etaPlusZeta, etaZ, zetaZ, press,
     I       uIceC, vIceC,
     U       rhsU,
     I       iMin, iMax, jMin, jMax, bi, bj, myThid )
C
C     v-equation
C
        DO j=jMin,jMax
         DO i=iMin,iMax
          rhsV(I,J,bi,bj) = FORCEY(I,J,bi,bj)
         ENDDO
        ENDDO
        CALL SEAICE_LSR_RHSV(
     I       zetaMinusEta, etaPlusZeta, etaZ, zetaZ, press,
     I       uIceC, vIceC,
     U       rhsV,
     I       iMin, iMax, jMin, jMax, bi, bj, myThid )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef SEAICE_ALLOW_MOM_ADVECTION
        IF ( SEAICEmomAdvection ) THEN
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           gUmom(I,J) = 0. _d 0
           gVmom(I,J) = 0. _d 0
          ENDDO
         ENDDO
         CALL SEAICE_MOM_ADVECTION(
     I        bi,bj,iMin,iMax,jMin,jMax,
     I        uIceC, vIceC,
     O        gUmom, gVmom,
     I        myTime, myIter, myThid )
         DO J=jMin,jMax
          DO I=jMin,jMax
           rhsU(I,J,bi,bj) = rhsU(I,J,bi,bj) + gUmom(I,J)
           rhsV(I,J,bi,bj) = rhsV(I,J,bi,bj) + gVmom(I,J)
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_ALLOW_MOM_ADVECTION */
       ENDDO
      ENDDO
C
C     Set up of right hand sides of momentum equations ends here
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C
C     calculate coefficients of tridiagonal matrices for both u- and
C     v-equations
C
      CALL SEAICE_LSR_CALC_COEFFS(
     I     etaPlusZeta, zetaMinusEta, etaZ, zetaZ, dragSym,
     O     AU, BU, CU, AV, BV, CV, uRt1, uRt2, vRt1, vRt2,
     I     iMin, iMax, jMin, jMax, myTime, myIter, myThid )

#ifndef OBCS_UVICE_OLD
C--     prevent tri-diagonal solver from modifying OB values:
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=jMin,jMax
         DO I=iMin,iMax
          IF ( maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj) .EQ. 0. ) THEN
           AU(I,J,bi,bj)   = ZERO
           BU(I,J,bi,bj)   = ONE
           CU(I,J,bi,bj)   = ZERO
           uRt1(I,J,bi,bj) = ZERO
           uRt2(I,J,bi,bj) = ZERO
           rhsU(I,J,bi,bj) = uIce(I,J,bi,bj)
          ENDIF
          IF ( maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj) .EQ. 0. ) THEN
           AV(I,J,bi,bj)   = ZERO
           BV(I,J,bi,bj)   = ONE
           CV(I,J,bi,bj)   = ZERO
           vRt1(I,J,bi,bj) = ZERO
           vRt2(I,J,bi,bj) = ZERO
           rhsV(I,J,bi,bj) = vIce(I,J,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* OBCS_UVICE_OLD */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &        'Uice pre  iter (SEAICE_LSR', MOD(ipass,1000), ')'
        CALL DEBUG_STATS_RL( 1, UICE, msgBuf, myThid )
        WRITE(msgBuf,'(A,I3,A)')
     &        'Vice pre  iter (SEAICE_LSR', MOD(ipass,1000), ')'
        CALL DEBUG_STATS_RL( 1, VICE, msgBuf, myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

C--   Calculate initial residual of the linearised system
      IF ( printResidual .OR. LSR_mixIniGuess.GE.1 )
     & CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uIce, vIce,
     O                  residUini, residVini, uTmp, vTmp,
     I                  printResidual, myIter, myThid )

#ifdef SEAICE_ALLOW_FREEDRIFT
      IF ( printResidual .OR. LSR_mixIniGuess.GE.1 ) THEN
       IF ( LSR_mixIniGuess.GE.1 ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=jMin,jMax
           DO i=iMin,iMax
            uIce_fd(i,j,bi,bj) = FORCEX(i,j,bi,bj)
     &         / ( 1. _d 0 - seaiceMaskU(i,j,bi,bj)
     &           + seaiceMassU(i,j,bi,bj)*recip_deltaT
     &           + HALF*( dragSym(i,j,bi,bj) + dragSym(i-1,j,bi,bj) )
     &                 * MAX(areaW(i,j,bi,bj),areaMinLoc)
     &           )
            vIce_fd(i,j,bi,bj) = FORCEY(i,j,bi,bj)
     &         / ( 1. _d 0 - seaiceMaskV(i,j,bi,bj)
     &           + seaiceMassV(i,j,bi,bj)*recip_deltaT
     &           + HALF*( dragSym(i,j,bi,bj) + dragSym(i,j-1,bi,bj) )
     &                 * MAX(areaS(i,j,bi,bj),areaMinLoc)
     &           )
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL EXCH_UV_XY_RL( uIce_fd, vIce_fd, .TRUE., myThid )
       ENDIF
       IF ( LSR_mixIniGuess.GE.0 )
     &  CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uIce_fd, vIce_fd,
     O                  residU_fd, residV_fd, uRes, vRes,
     I                  printResidual, myIter, myThid )
      ENDIF
      IF ( LSR_mixIniGuess.GE.2 ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice, vice = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
CADJ STORE utmp, vtmp = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
# endif /* ALLOW_AUTODIFF_TAMC */
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=jMin,jMax
          DO i=iMin,iMax
           errIni = uTmp(i,j,bi,bj)*uTmp(i,j,bi,bj)
           errFD  = uRes(i,j,bi,bj)*uRes(i,j,bi,bj)
           IF ( LSR_mixIniGuess.GE.4 ) THEN
            errIni = errIni*errIni
            errFD  = errFD *errFD
           ENDIF
           errSum = ( errIni + errFD )
     &          *maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj)
           IF ( errSum.GT.0. ) THEN
            uIce(i,j,bi,bj) = ( errFD *uIce(i,j,bi,bj)
     &                        + errIni*uIce_fd(i,j,bi,bj)
     &                        )/errSum
           ENDIF
           errIni = vTmp(i,j,bi,bj)*vTmp(i,j,bi,bj)
           errFD  = vRes(i,j,bi,bj)*vRes(i,j,bi,bj)
           IF ( LSR_mixIniGuess.GE.4 ) THEN
            errIni = errIni*errIni
            errFD  = errFD *errFD
           ENDIF
           errSum = ( errIni + errFD )
     &          *maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj)
           IF ( errSum.GT.0. ) THEN
            vIce(i,j,bi,bj) = ( errFD *vIce(i,j,bi,bj)
     &                        + errIni*vIce_fd(i,j,bi,bj)
     &                        )/errSum
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL EXCH_UV_XY_RL( uIce, vIce, .TRUE., myThid )
#ifndef ALLOW_AUTODIFF_TAMC
C     Here residU/Vmix and u/vTmp are computed but never used for anything
C     but reporting to STDOUT. Still TAF thinks this requires recomputations
C     so we hide this part of the code from TAF.
       IF ( printResidual ) THEN
        CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uIce, vIce,
     O                  residUmix, residVmix, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
       ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */
      ENDIF
#endif /* SEAICE_ALLOW_FREEDRIFT */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C NOW DO ITERATION

cph--- iteration starts here
cph--- need to kick out goto
      doIterate4u = .TRUE.
      doIterate4v = .TRUE.

C ITERATION START -----------------------------------------------------
C     Ideally, we would like to use the loop-directive for this
C     iterative loop, but (unsurprisingly) it does not seem to work for
C     this convoluted case of mixed iterations, so it is commented out
CML#ifdef ALLOW_AUTODIFF_TAMC
CMLCADJ LOOP = iteration uice, vice = comlev1_lsr
CML#endif /* ALLOW_AUTODIFF_TAMC */

#if (defined (ALLOW_AUTODIFF_TAMC) && defined (SEAICE_LSR_ADJOINT_ITER))
      IF ( inAdMode ) THEN
        SOLV_MAX_TMP = SOLV_MAX_FIXED
      ELSE
        SOLV_MAX_TMP = linearIterLoc
      ENDIF
#else
      SOLV_MAX_TMP = linearIterLoc
#endif

      ICOUNT1 = SOLV_MAX_TMP
      ICOUNT2 = SOLV_MAX_TMP

      DO m = 1, SOLV_MAX_TMP

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef SEAICE_LSR_ADJOINT_ITER
       itmpkey2 = (itmpkey-1)*SOLV_MAX_FIXED + MIN(m,SOLV_MAX_FIXED)
# else
       itmpkey2 = itmpkey
# endif /* SEAICE_LSR_ADJOINT_ITER */
C     Storing these scalars and logicals is necessary to do an exact
C     backward iteration
CADJ STORE wfau, wfav = comlev1_lsr,  kind=isbyte, key = itmpkey2
CADJ STORE doIterate4u, doIterate4v = comlev1_lsr, key = itmpkey2
#endif /* ALLOW_AUTODIFF_TAMC */

       IF ( doIterate4u .OR. doIterate4v ) THEN

        IF ( useCubedSphereExchange ) THEN
          doIterate4u = .TRUE.
          doIterate4v = .TRUE.
        ENDIF

# ifdef ALLOW_AUTODIFF_TAMC
C     Note that itmpkey2 can get very large when SEAICE_LSR_ADJOINT_ITER
C     is defined for an accurate adjoint, otherwise it is the same for
C     each m, so that the tapes get overwritten for each iterate and
C     the adjoint is necessarily wrong.
CADJ STORE uice, vice = comlev1_lsr, kind=isbyte, key = itmpkey2
# endif /* ALLOW_AUTODIFF_TAMC */

#ifdef SEAICE_GLOBAL_3DIAG_SOLVER
        IF ( SEAICEuseMultiTileSolver ) THEN

         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           IF ( doIterate4u ) THEN
            DO j=jMin,jMax
             DO i=iMin,iMax
              uTmp(i,j,bi,bj)  = uIce(i,j,bi,bj)
              rhsUx(i,j,bi,bj) = ( rhsU(i,j,bi,bj)
     &             + uRt1(i,j,bi,bj)*uIce(i,j-1,bi,bj)
     &             + uRt2(i,j,bi,bj)*uIce(i,j+1,bi,bj)
     &                           )*seaiceMaskU(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDIF
           IF ( doIterate4v ) THEN
            DO j=jMin,jMax
             DO i=iMin,iMax
              vTmp(i,j,bi,bj)  = vIce(i,j,bi,bj)
              rhsVy(i,j,bi,bj) = ( rhsV(i,j,bi,bj)
     &             + vRt1(i,j,bi,bj)*vIce(i-1,J,bi,bj)
     &             + vRt2(i,j,bi,bj)*vIce(i+1,J,bi,bj)
     &                           )*seaiceMaskU(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDIF
C     end bi,bj-loops
          ENDDO
         ENDDO

         iSubIter = SOLV_MAX_TMP*(ipass-1) + m
         CALL SOLVE_UV_TRIDIAGO(
     I                 1, OLx, doIterate4u, doIterate4v,
     I                 AU, BU, CU, rhsUx,
     I                 AV, BV, CV, rhsVy,
     O                 uIce, vIce, errCode,
     I                 iSubIter, myIter, myThid )

         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           IF ( doIterate4u ) THEN
            DO j=jMin,jMax
             DO i=iMin,iMax
              uIce(i,j,bi,bj) = uTmp(i,j,bi,bj)
     &            + WFAU*( uIce(i,j,bi,bj)-uTmp(i,j,bi,bj) )
             ENDDO
            ENDDO
           ENDIF
           IF ( doIterate4v ) THEN
            DO j=jMin,jMax
             DO i=iMin,iMax
              vIce(i,j,bi,bj) = vTmp(i,j,bi,bj)
     &            + WFAV*( vIce(i,j,bi,bj)-vTmp(i,j,bi,bj) )
             ENDDO
            ENDDO
           ENDIF
C     end bi,bj-loops
          ENDDO
         ENDDO

        ELSE
#endif /* SEAICE_GLOBAL_3DIAG_SOLVER */

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = itmpkey2 - 1
          itmpkey3 = (act1 + 1) + act2*max1
     &                          + act3*max1*max2
     &                          + act4*max1*max2*max3
CADJ STORE uice(:,:,bi,bj) = comlev1_bibj_lsr,kind=isbyte,key=itmpkey3
CADJ STORE vice(:,:,bi,bj) = comlev1_bibj_lsr,kind=isbyte,key=itmpkey3
#endif /* ALLOW_AUTODIFF_TAMC */
C-jmc: get less TAF warnings when always (no if doIterate) saving uIce,vIce:
C     save u/vIce prior to iteration
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            uTmp(I,J,bi,bj)=uIce(I,J,bi,bj)
            vTmp(I,J,bi,bj)=vIce(I,J,bi,bj)
           ENDDO
          ENDDO

          IF ( doIterate4u ) THEN
C Solve for uIce :
           CALL SEAICE_LSR_TRIDIAGU(
     I          AU, BU, CU, uRt1, uRt2, rhsU, uTmp, seaiceMaskU, WFAU,
     U          uIce,
     I          iMin, iMax, jMin, jMax, bi, bj, myTime, myIter, myThid )
          ENDIF

          IF ( doIterate4v ) THEN
C Solve for vIce
           CALL SEAICE_LSR_TRIDIAGV(
     I          AV, BV, CV, vRt1, vRt2, rhsV, vTmp, seaiceMaskV, WFAV,
     U          vIce,
     I          iMin, iMax, jMin, jMax, bi, bj, myTime, myIter, myThid )
          ENDIF

C     end bi,bj-loops
         ENDDO
        ENDDO

#ifdef SEAICE_GLOBAL_3DIAG_SOLVER
        ENDIF
#endif /* SEAICE_GLOBAL_3DIAG_SOLVER */

        IF ( doIterate4u.AND.MOD(m,SOLV_NCHECK).EQ.0) THEN
         S1=ZERO
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO J=1,sNy
            DO I=1,sNx
             UERR=(uIce(I,J,bi,bj)-uTmp(I,J,bi,bj))
     &               * seaiceMaskU(I,J,bi,bj)
             S1=MAX(ABS(UERR),S1)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _GLOBAL_MAX_RL( S1, myThid )
c        WRITE(standardMessageUnit,'(A,2I6,1P4E16.9)')
c    &   ' U iters,error,WF = ',ipass,M,S1,S1A,WFAU
C SAFEGUARD AGAINST BAD FORCING ETC
         IF(m.GT.1.AND.S1.GT.S1A) WFAU=WFAU2
         S1A=S1
         IF(S1.LT.LSR_ERROR) THEN
          ICOUNT1=m
          doIterate4u = .FALSE.
         ENDIF
        ENDIF

        IF ( doIterate4v.AND.MOD(m,SOLV_NCHECK).EQ.0) THEN
         S2=ZERO
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO J=1,sNy
            DO I=1,sNx
             UERR=(vIce(I,J,bi,bj)-vTmp(I,J,bi,bj))
     &               * seaiceMaskV(I,J,bi,bj)
             S2=MAX(ABS(UERR),S2)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _GLOBAL_MAX_RL( S2, myThid )
C SAFEGUARD AGAINST BAD FORCING ETC
         IF(m.GT.1.AND.S2.GT.S2A) WFAV=WFAV2
         S2A=S2
         IF(S2.LT.LSR_ERROR) THEN
          ICOUNT2=m
          doIterate4v = .FALSE.
         ENDIF
        ENDIF

        CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

C--    end doIterate4u or doIterate4v
       ENDIF
      ENDDO
C ITERATION END -----------------------------------------------------

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifndef ALLOW_AUTODIFF_TAMC
C     Here residU/Vend and u/vTmp are computed but never used for anything
C     but reporting to STDOUT. Still TAF thinks this requires recomputations
C     so we hide this part of the code from TAF.
      IF ( printResidual ) THEN
C--   Calculate final residual of the linearised system
        CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uIce, vIce,
     O                  residUend, residVend, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
        _BEGIN_MASTER( myThid )
        WRITE(standardMessageUnit,'(A,1X,I4,1P3E16.8)')
     &    ' SEAICE_LSR: Residual Initial ipass,Uice,Vice=',
     &        ipass, residUini, residVini
#ifdef SEAICE_ALLOW_FREEDRIFT
        IF ( LSR_mixIniGuess.GE.0 )
     &  WRITE(standardMessageUnit,'(A,1P3E16.8)')
     &    ' SEAICE_LSR: Residual FrDrift U_fd,V_fd=',
     &        residU_fd, residV_fd
        IF ( LSR_mixIniGuess.GE.2 )
     &  WRITE(standardMessageUnit,'(A,1P3E16.8)')
     &    ' SEAICE_LSR: Residual Mix-ini Uice,Vice=',
     &        residUmix, residVmix
#endif /* SEAICE_ALLOW_FREEDRIFT */
        WRITE(standardMessageUnit,'(A,I4,A,I6,1P2E16.8)')
     &    ' SEAICE_LSR (ipass=',ipass,') iters,dU,Resid=',
     &      ICOUNT1, S1, residUend
        WRITE(standardMessageUnit,'(A,I4,A,I6,1P2E16.8)')
     &    ' SEAICE_LSR (ipass=',ipass,') iters,dV,Resid=',
     &      ICOUNT2, S2, residVend
        _END_MASTER( myThid )
      ENDIF
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &        'Uice post iter (SEAICE_LSR', MOD(ipass,1000), ')'
        CALL DEBUG_STATS_RL( 1, UICE, msgBuf, myThid )
        WRITE(msgBuf,'(A,I3,A)')
     &        'Vice post iter (SEAICE_LSR', MOD(ipass,1000), ')'
        CALL DEBUG_STATS_RL( 1, VICE, msgBuf, myThid )
      ENDIF
#endif /* ALLOW_DEBUG */
      IF ( doIterate4u .OR. doIterate4v ) THEN
        WRITE(msgBuf,'(2A,I10,A,I4,A)') '** WARNING ** SEAICE_LSR ',
     &    '(it=', myIter, ',', ipass,') did not converge :'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2(A,I6,0PF6.3,1PE14.6))')
     &      ' nIt,wFU,dU=', ICOUNT1, WFAU, S1,
     &    ' ; nIt,wFV,dV=', ICOUNT2, WFAV, S2
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

C     APPLY MASKS
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          uIce(I,J,bi,bj)=uIce(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          vIce(I,J,bi,bj)=vIce(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
CML      CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

#ifdef SEAICE_ALLOW_CHECK_LSR_CONVERGENCE
      IF ( debugLevel .GE. debLevC ) THEN
       resnorm = 0. _d 0
       EKnorm  = 0. _d 0
       counter = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C--   Compute residual norm and print
         DO j=1,sNy
          DO i=1,sNx
           resnorm = resnorm + 0.5 _d 0 *
     &          ( ( (uIceNm1(i,j,bi,bj)+uIceNm1(i+1,j,bi,bj))
     &            - (uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj)) )**2
     &          + ( (vIceNm1(i,j,bi,bj)+vIceNm1(i,j+1,bi,bj))
     &            - (vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj)) )**2 )
           IF ( area(i,j,bi,bj) .gt. 0.5 _d 0 ) THEN
            EKnorm = EKnorm + 0.5 _d 0 * heff(i,j,bi,bj) *
     &           ( ( (uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj)) )**2
     &           + ( (vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj)) )**2 )
            counter = counter + 1. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _GLOBAL_SUM_RL( resnorm, myThid )
       _GLOBAL_SUM_RL( EKnorm, myThid )
       _GLOBAL_SUM_RL( counter, myThid )
       IF ( counter .gt. 0. _d 0 ) EKnorm = EKnorm/counter
       _BEGIN_MASTER( myThid )
       WRITE(standardMessageUnit,'(A,I7,1X,2E22.14)')
     &      'S/R SEAICE_LSR: IPSEUDO, RESNORM, EKNORM = ',
     &      ipass, sqrt(resnorm), EKnorm
       _END_MASTER( myThid )
      ENDIF
#endif /* SEAICE_ALLOW_CHECK_LSR_CONVERGENCE */

      ENDIF
C     end outer ipass pseudo-time stepping loop
      ENDDO

      IF ( useHB87StressCoupling ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
C     These directives do not remove any recomputation warnings but avoid
C     recomputing the entire LSR loop before evaluating this code block
CADJ STORE uice, vice, eta, etaz, zeta = comlev1_dynsol,
CADJ &     kind=isbyte, key = ikey_dynamics
# endif /* ALLOW_AUTODIFF_TAMC */
C     compute the divergence of stress here to be used later
C
C     compute strain rate from latest velocities
       CALL SEAICE_CALC_STRAINRATES(
     I       uIce, vIce,
     O       e11, e22, e12,
     I       3, myTime, myIter, myThid )
C     compute internal stresses with updated ice velocities
C     and evaluate divergence of stress and apply to forcing
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         CALL SEAICE_CALC_STRESSDIV(
     I        e11, e22, e12, press, zeta, eta, etaZ,
     O        stressDivergenceX, stressDivergenceY,
     I        bi, bj, myTime, myIter, myThid )
        ENDDO
       ENDDO
C     endif  useHB87StressCoupling
      ENDIF

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_RESIDUAL
C     !INTERFACE:
      SUBROUTINE SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uFld, vFld,
     O                  residU, residV, uRes, vRes,
     I                  calcMeanResid, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_RESIDUAL
C     | o Compute Linear solver residual
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     calcMeanResid :: also compute mean residual (=RMS)
C     myIter        :: Simulation timestep number
C     myThid        :: my Thread Id. number
C--   RHS
      _RL rhsU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C-    coefficients for lateral points, u(j+/-1)
      _RL uRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C-    coefficients for lateral points, v(i+/-1)
      _RL vRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C-    diagonals of coefficient matrices
      _RL AU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C--   seaice velocity
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C-    residual (output)
      _RL residU, residV
      _RL uRes (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRes (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      LOGICAL calcMeanResid
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj  :: Loop counters

      INTEGER i, j, bi, bj

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Calculate residual of the linearised system
      residU = 0.
      residV = 0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           uRes(i,j,bi,bj) = rhsU(i,j,bi,bj)
     &            + uRt1(i,j,bi,bj)*uFld(i,j-1,bi,bj)
     &            + uRt2(i,j,bi,bj)*uFld(i,j+1,bi,bj)
     &            - ( AU(i,j,bi,bj)*uFld(i-1,j,bi,bj)
     &              + BU(i,j,bi,bj)*uFld( i ,j,bi,bj)
     &              + CU(i,j,bi,bj)*uFld(i+1,j,bi,bj)
     &              )
           vRes(i,j,bi,bj) = rhsV(i,j,bi,bj)
     &            + vRt1(i,j,bi,bj)*vFld(i-1,j,bi,bj)
     &            + vRt2(i,j,bi,bj)*vFld(i+1,j,bi,bj)
     &            - ( AV(i,j,bi,bj)*vFld(i,j-1,bi,bj)
     &              + BV(i,j,bi,bj)*vFld(i, j ,bi,bj)
     &              + CV(i,j,bi,bj)*vFld(i,j+1,bi,bj)
     &              )
          ENDDO
         ENDDO
         IF ( calcMeanResid ) THEN
          DO j=1,sNy
           DO i=1,sNx
            residU = residU + uRes(i,j,bi,bj)*uRes(i,j,bi,bj)
     &                       *rAw(i,j,bi,bj)*maskInW(i,j,bi,bj)
#ifndef OBCS_UVICE_OLD
     &                       *maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj)
#endif
            residV = residV + vRes(i,j,bi,bj)*vRes(i,j,bi,bj)
     &                       *rAs(i,j,bi,bj)*maskInS(i,j,bi,bj)
#ifndef OBCS_UVICE_OLD
     &                       *maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj)
#endif
           ENDDO
          ENDDO
         ENDIF
       ENDDO
      ENDDO
      IF ( calcMeanResid ) THEN
       _GLOBAL_SUM_RL( residU, myThid )
       _GLOBAL_SUM_RL( residV, myThid )
C     scale residuals by globalArea so that they do not get ridiculously large
       IF ( residU.GT.0. ) residU = SQRT(residU/globalArea)
       IF ( residV.GT.0. ) residV = SQRT(residV/globalArea)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */

      RETURN
      END

CBOP
C     !ROUTINE: SEAICE_LSR_CALC_COEFFS
C     !INTERFACE:
      SUBROUTINE SEAICE_LSR_CALC_COEFFS(
     I     etaPlusZeta, zetaMinusEta, etaZloc, zetaZloc, dragSym,
     O     AU, BU, CU, AV, BV, CV, uRt1, uRt2, vRt1, vRt2,
     I     iMin, iMax, jMin, jMax, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_LSR_CALC_COEFFS
C     | o Calculate coefficient matrix for LSR solver
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime     :: Simulation time
C     myIter     :: Simulation timestep number
C     myThid     :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER iMin, iMax, jMin, jMax
C     combinations of bulk and shear viscosity at C points
C     abbreviations
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     shear and bulk viscosity at Z points
      _RL  etaZloc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaZloc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     symmetric ice-ocean stress coefficient
      _RL dragSym     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients of ice velocities in coefficient matrix
C     for both U and V-equation
C     diagonals of coefficient matrices
      _RL AU          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BU          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CU          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AV          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BV          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CV          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, u(j+/-1)
      _RL uRt1        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uRt2        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, v(i+/-1)
      _RL vRt1        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRt2        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEOP

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj  :: Loop counters

      INTEGER i, j, bi, bj
      _RL hFacM, hFacP
C     backward difference extrapolation factor (includes 1/deltaT)
      _RL bdfAlphaOverDt
C     strongly implicit coupling factor
      _RL strImpCplFac

C     fractional area at velocity points
      _RL areaW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL areaS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     coefficients of ice velocities in coefficient matrix
C     for both U and V-equation
C     XX: double derivative in X
C     YY: double derivative in Y
C     XM: metric term with derivative in X
C     YM: metric term with derivative in Y
      _RL UXX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL UYY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL UXM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL UYM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VXX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VYY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VXM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VYM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     backward difference extrapolation factor
      bdfAlphaOverDt = 1. _d 0
      IF ( SEAICEuseBDF2 ) THEN
       IF ( myIter.EQ.nIter0 .AND. SEAICEmomStartBDF.EQ.0 ) THEN
        bdfAlphaOverDt = 1. _d 0
       ELSE
        bdfAlphaOverDt = 1.5 _d 0
       ENDIF
      ENDIF
C
      bdfAlphaOverDt = bdfAlphaOverDt / SEAICE_deltaTdyn
C
      strImpCplFac = 0. _d 0
      IF ( SEAICEuseStrImpCpl ) strImpCplFac = 1. _d 0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( SEAICEscaleSurfStress ) THEN
         DO J=jMin,jMax
          DO I=iMin,iMax
           areaW(I,J) = 0.5 _d 0*(AREA(I,J,bi,bj)+AREA(I-1,J,bi,bj))
           areaS(I,J) = 0.5 _d 0*(AREA(I,J,bi,bj)+AREA(I,J-1,bi,bj))
          ENDDO
         ENDDO
        ELSE
         DO J=jMin,jMax
          DO I=iMin,iMax
           areaW(I,J) = 1. _d 0
           areaS(I,J) = 1. _d 0
          ENDDO
         ENDDO
        ENDIF
C
C     coefficients of uIce(I,J) and vIce(I,J) belonging to ...
C
        DO J=jMin,jMax
         DO I=iMin-1,iMax
C     ... d/dx (eta+zeta) d/dx u
          UXX(I,J) = _dyF(I,J,bi,bj) * etaPlusZeta(I,J,bi,bj)
     &         * _recip_dxF(I,J,bi,bj)
C     ... d/dx (zeta-eta) k1 u
          UXM(I,J) = _dyF(I,J,bi,bj) * zetaMinusEta(I,J,bi,bj)
     &         * k1AtC(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=jMin,jMax+1
         DO I=iMin,iMax
C     ... d/dy eta d/dy u
          UYY(I,J) = _dxV(I,J,bi,bj) *
     &         ( etaZloc(I,J,bi,bj) + strImpCplFac*zetaZloc(I,J,bi,bj) )
     &         * _recip_dyU(I,J,bi,bj)
C     ... d/dy eta k2 u
          UYM(I,J) = _dxV(I,J,bi,bj) * etaZloc(I,J,bi,bj)
     &         * k2AtZ(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=jMin,jMax
         DO I=iMin,iMax+1
C     ... d/dx eta dv/dx
          VXX(I,J) = _dyU(I,J,bi,bj) *
     &         ( etaZloc(I,J,bi,bj) + strImpCplFac*zetaZloc(I,J,bi,bj) )
     &         * _recip_dxV(I,J,bi,bj)
C     ... d/dx eta k1 v
          VXM(I,J) = _dyU(I,J,bi,bj) * etaZloc(I,J,bi,bj)
     &         * k1AtZ(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=jMin-1,jMax
         DO I=iMin,iMax
C     ... d/dy eta+zeta dv/dy
          VYY(I,J) = _dxF(I,J,bi,bj) * etaPlusZeta(I,J,bi,bj)
     &         * _recip_dyF(I,J,bi,bj)
C     ... d/dy (zeta-eta) k2 v
          VYM(I,J) = _dxF(I,J,bi,bj) * zetaMinusEta(I,J,bi,bj)
     &         * k2AtC(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO

C--   Coefficients for solving uIce :

C     assemble coefficient matrix, beware of sign convention: because this
C     is the left hand side we calculate -grad(sigma), but the coefficients
C     of U(I,J+/-1) are counted on the right hand side
        DO J=jMin,jMax
         DO I=iMin,iMax
C     coefficients for UICE(I-1,J)
          AU(I,J,bi,bj)= ( - UXX(I-1,J) + UXM(I-1,J) )
     &         * seaiceMaskU(I,J,bi,bj)
C     coefficients for UICE(I+1,J)
          CU(I,J,bi,bj)= ( - UXX(I  ,J) - UXM(I  ,J) )
     &         * seaiceMaskU(I,J,bi,bj)
C     coefficients for UICE(I,J)
          BU(I,J,bi,bj)=(ONE - seaiceMaskU(I,J,bi,bj)) +
     &         ( UXX(I-1,J) + UXX(I,J) + UYY(I,J+1) + UYY(I,J)
     &         + UXM(I-1,J) - UXM(I,J) + UYM(I,J+1) - UYM(I,J)
     &         ) * seaiceMaskU(I,J,bi,bj)
C     coefficients of uIce(I,J-1)
          uRt1(I,J,bi,bj)= UYY(I,J  ) + UYM(I,J  )
C     coefficients of uIce(I,J+1)
          uRt2(I,J,bi,bj)= UYY(I,J+1) - UYM(I,J+1)
         ENDDO
        ENDDO

C     apply boundary conditions according to slip factor
C     for no slip, set u on boundary to zero: u(j+/-1)=-u(j)
C     for the free slip case sigma_12 = 0
        DO J=jMin,jMax
         DO I=iMin,iMax
          hFacM = seaiceMaskU(I,J-1,bi,bj)
          hFacP = seaiceMaskU(I,J+1,bi,bj)
C     copy contributions to coefficient of U(I,J)
C     beware of sign convection: uRt1/2 have the opposite sign convention
C     than BU, hence the minus sign
          BU(I,J,bi,bj)=BU(I,J,bi,bj) + seaiceMaskU(I,J,bi,bj) *
     &         ( ( 1. _d 0 - hFacM ) * ( UYY(I  ,J  ) + UYM(I  ,J  ) )
     &         + ( 1. _d 0 - hFacP ) * ( UYY(I  ,J+1) - UYM(I  ,J+1) ) )
C     reset coefficients of U(I,J-1) and U(I,J+1)
          uRt1(I,J,bi,bj) = uRt1(I,J,bi,bj) * hFacM
          uRt2(I,J,bi,bj) = uRt2(I,J,bi,bj) * hFacP
         ENDDO
        ENDDO

C     now we need to normalize everything by the grid cell area
        DO J=jMin,jMax
         DO I=iMin,iMax
          AU(I,J,bi,bj)   = AU(I,J,bi,bj)   * recip_rAw(I,J,bi,bj)
          CU(I,J,bi,bj)   = CU(I,J,bi,bj)   * recip_rAw(I,J,bi,bj)
C     here we need to add the contribution from the time derivative (in
C     bdfAlphaOverDt) and the symmetric drag term; must be done after
C     normalizing
          BU(I,J,bi,bj)    = BU(I,J,bi,bj)  * recip_rAw(I,J,bi,bj)
     &         + seaiceMaskU(I,J,bi,bj) *
     &         ( bdfAlphaOverDt*seaiceMassU(I,J,bi,bj)
     &         + 0.5 _d 0 * ( dragSym(I,  J,bi,bj)
     &                      + dragSym(I-1,J,bi,bj) )*areaW(I,J)
     &         )
          uRt1(I,J,bi,bj) = uRt1(I,J,bi,bj) * recip_rAw(I,J,bi,bj)
          uRt2(I,J,bi,bj) = uRt2(I,J,bi,bj) * recip_rAw(I,J,bi,bj)
         ENDDO
        ENDDO

C--   Coefficients for solving uIce :

C     assemble coefficient matrix, beware of sign convention: because this
C     is the left hand side we calculate -grad(sigma), but the coefficients
C     of V(I+/-1,J) are counted on the right hand side
        DO J=jMin,jMax
         DO I=iMin,iMax
C     coefficients for VICE(I,J-1)
          AV(I,J,bi,bj)=( - VYY(I,J-1) + VYM(I,J-1)
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for VICE(I,J+1)
          CV(I,J,bi,bj)=( - VYY(I,J  ) - VYM(I,J  )
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for VICE(I,J)
          BV(I,J,bi,bj)= (ONE - seaiceMaskV(I,J,bi,bj)) +
     &         ( VXX(I,J) + VXX(I+1,J) + VYY(I,J) + VYY(I,J-1)
     &         - VXM(I,J) + VXM(I+1,J) - VYM(I,J) + VYM(I,J-1)
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for V(I-1,J)
          vRt1(I,J,bi,bj) = VXX(I  ,J) + VXM(I  ,J)
C     coefficients for V(I+1,J)
          vRt2(I,J,bi,bj) = VXX(I+1,J) - VXM(I+1,J)
         ENDDO
        ENDDO

C     apply boundary conditions according to slip factor
C     for no slip, set u on boundary to zero: v(i+/-1)=-v(i)
C     for the free slip case sigma_12 = 0
        DO J=jMin,jMax
         DO I=iMin,iMax
          hFacM = seaiceMaskV(i-1,j,bi,bj)
          hFacP = seaiceMaskV(i+1,j,bi,bj)
C     copy contributions to coefficient of V(I,J)
C     beware of sign convection: vRt1/2 have the opposite sign convention
C     than BV, hence the minus sign
          BV(I,J,bi,bj)=BV(I,J,bi,bj) + seaiceMaskV(I,J,bi,bj) *
     &         ( ( 1. _d 0 - hFacM ) * ( VXX(I  ,J) + VXM(I  ,J) )
     &         + ( 1. _d 0 - hFacP ) * ( VXX(I+1,J) - VXM(I+1,J) ) )
C     reset coefficients of V(I-1,J) and V(I+1,J)
          vRt1(I,J,bi,bj) = vRt1(I,J,bi,bj) * hFacM
          vRt2(I,J,bi,bj) = vRt2(I,J,bi,bj) * hFacP
         ENDDO
        ENDDO

C     now we need to normalize everything by the grid cell area
        DO J=jMin,jMax
         DO I=iMin,iMax
          AV(I,J,bi,bj)   = AV(I,J,bi,bj)   * recip_rAs(I,J,bi,bj)
          CV(I,J,bi,bj)   = CV(I,J,bi,bj)   * recip_rAs(I,J,bi,bj)
C     here we need add the contribution from the time derivative (in
C     bdfAlphaOverDt) and the symmetric drag term; must be done after
C     normalizing
          BV(I,J,bi,bj)   = BV(I,J,bi,bj)   * recip_rAs(I,J,bi,bj)
     &         + seaiceMaskV(I,J,bi,bj) *
     &         ( bdfAlphaOverDt*seaiceMassV(I,J,bi,bj)
     &         + 0.5 _d 0 * ( dragSym(I,J,  bi,bj)
     &                      + dragSym(I,J-1,bi,bj) )*areaS(I,J)
     &         )
          vRt1(I,J,bi,bj) = vRt1(I,J,bi,bj) * recip_rAs(I,J,bi,bj)
          vRt2(I,J,bi,bj) = vRt2(I,J,bi,bj) * recip_rAs(I,J,bi,bj)
         ENDDO
        ENDDO

        IF ( ( useCubedSphereExchange .AND.
     &       (SEAICE_OLx.GT.0 .OR. SEAICE_OLy.GT.0) ) .OR.
     &       SEAICEscaleSurfStress ) THEN
C     this is clearly a hack: make sure that diagonals BU/BV are non-zero.
C     In my experience we only need to catch the case of SEAICE_OLx/y=OLx/y-2,
C     but for safety lets call this routine whenever SEAICE_OLx/y > 0
C     &       (SEAICE_OLx.EQ.OLx-2 .OR. SEAICE_OLy.EQ.OLy-2) ) THEN
C     When scaling the surface ice-ocean stress by AREA, then there will
C     be many cases of zero diagonal entries.
         DO J=jMin,jMax
          DO I=iMin,iMax
           IF (BU(I,J,bi,bj).EQ.0 _d 0) BU(I,J,bi,bj) = 1. _d 0
           IF (BV(I,J,bi,bj).EQ.0 _d 0) BV(I,J,bi,bj) = 1. _d 0
          ENDDO
         ENDDO
        ENDIF
C     bi/bj-loops
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SEAICE_LSR_RHSU
C     !INTERFACE:
      SUBROUTINE SEAICE_LSR_RHSU(
     I     zetaMinusEta, etaPlusZeta, etaZloc, zetaZloc, pressLoc,
     I     uIceC, vIceC,
     U     rhsU,
     I     iMin, iMax, jMin, jMax, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_LSR_RHSU
C     | o Set up stress contribution to rhs of u-momentum eq.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid     :: my Thread Id. number
      INTEGER myThid
      INTEGER iMin, iMax, jMin, jMax, bi, bj
C
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaZloc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaZloc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pressloc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uIceC       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceC       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsU        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j  :: Loop counters
      INTEGER I,J
      INTEGER kSrf
      _RL hFacM
      _RL sig11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      kSrf = 1
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        sig11(I,J) = 0. _d 0
        sig12(I,J) = 0. _d 0
       ENDDO
      ENDDO
C     contribution of sigma11 to rhs
      DO J=jMin,jMax
       DO I=iMin-1,iMax
        sig11(I,J) = zetaMinusEta(I,J,bi,bj)
     &       * ( vIceC(I,J+1,bi,bj) - vIceC(I,J,bi,bj) )
     &       * _recip_dyF(I,J,bi,bj)
     &       + etaPlusZeta(I,J,bi,bj) * k2AtC(I,J,bi,bj)
     &       * 0.5 _d 0 * ( vIceC(I,J+1,bi,bj) + vIceC(I,J,bi,bj) )
     &       - 0.5 _d 0 * pressLoc(I,J,bi,bj)
       ENDDO
      ENDDO
C     contribution of sigma12 to rhs of u-equation
      DO J=jMin,jMax+1
       DO I=iMin,iMax
        hFacM = seaiceMaskV(I,J,bi,bj) - seaiceMaskV(I-1,J,bi,bj)
        sig12(I,J) = etaZloc(I,J,bi,bj) * (
     &       ( vIceC(I,J,bi,bj) - vIceC(I-1,J,bi,bj) )
     &       * _recip_dxV(I,J,bi,bj)
     &       - k1AtZ(I,J,bi,bj)
     &       * 0.5 _d 0 * ( vIceC(I,J,bi,bj) + vIceC(I-1,J,bi,bj) )
     &       )
C     free slip conditions (sig12=0) are taken care of by masking sig12
     &       *HEFFM(I  ,J  ,bi,bj)*HEFFM(I-1,J  ,bi,bj)
     &       *HEFFM(I  ,J-1,bi,bj)*HEFFM(I-1,J-1,bi,bj)
C     no slip boundary conditions (v(i-1)=-v(i))
C     v(i)+v(i-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with v(i)-v(i-1)
     &       + etaZloc(I,J,bi,bj) * _recip_dxV(I,J,bi,bj)
     &       * ( vIceC(I,J,bi,bj) + vIceC(I-1,J,bi,bj) )
     &       * hFacM * 2. _d 0
       ENDDO
      ENDDO

      IF ( SEAICEuseStrImpCpl ) THEN
C     strictly speaking, this is not a contribution to sig12, but for
C     convenience we add the explicit term -zetaZ*du/dy here;
C     we do not have to add any metric terms, because they cancel.
       DO J=jMin,jMax+1
        DO I=iMin,iMax
         hFacM = seaiceMaskV(I,J,bi,bj) - seaiceMaskV(I-1,J,bi,bj)
         sig12(I,J) = sig12(I,J) - zetaZloc(I,J,bi,bj) * (
     &        ( uIceC(I,J,bi,bj) - uIceC(I,J-1,bi,bj) )
     &        * _recip_dyU(I,J,bi,bj)
     &        )
C     free slip conditions (sig12=0) are taken care of by masking sig12
     &        *HEFFM(I  ,J  ,bi,bj)*HEFFM(I-1,J  ,bi,bj)
     &        *HEFFM(I  ,J-1,bi,bj)*HEFFM(I-1,J-1,bi,bj)
C     no slip boundary conditions (u(j-1)=-u(j))
C     u(j)+u(j-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with u(j)-u(j-1)
     &        - zetaZloc(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &        * ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &        * hFacM * 2. _d 0
        ENDDO
       ENDDO
      ENDIF

      DO J=jMin,jMax
       DO I=iMin,iMax
C     contribution to the rhs part of grad(sigma)_x
        rhsU(I,J,bi,bj) = rhsU(I,J,bi,bj)
     &       + recip_rAw(I,J,bi,bj) * seaiceMaskU(I,J,bi,bj) *
     &       ( _dyF(I  ,J  ,bi,bj)*sig11(I  ,J  )
     &       - _dyF(I-1,J  ,bi,bj)*sig11(I-1,J  )
     &       + _dxV(I  ,J+1,bi,bj)*sig12(I  ,J+1)
     &       - _dxV(I  ,J  ,bi,bj)*sig12(I  ,J  ) )
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SEAICE_LSR_RHSV
C     !INTERFACE:
      SUBROUTINE SEAICE_LSR_RHSV(
     I     zetaMinusEta, etaPlusZeta, etaZloc, zetaZloc, pressLoc,
     I     uIceC, vIceC,
     U     rhsV,
     I     iMin, iMax, jMin, jMax, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_LSR_RHSV
C     | o Set up stress contribution to rhs of v-momentum eq.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid     :: my Thread Id. number
      INTEGER myThid
      INTEGER iMin, iMax, jMin, jMax, bi, bj
C
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaZloc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaZloc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pressloc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uIceC       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceC       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsV        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j  :: Loop counters
      INTEGER I,J
      INTEGER kSrf
      _RL hFacM
      _RL sig22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      kSrf = 1
      DO J = 1-OLy,sNy+OLy
       DO I = 1-OLx,sNx+OLx
        sig22(I,J) = 0. _d 0
        sig12(I,J) = 0. _d 0
       ENDDO
      ENDDO

C     contribution of sigma22 to rhs
      DO J=jMin-1,jMax
       DO I=iMin,iMax
        sig22(I,J) = zetaMinusEta(I,J,bi,bj)
     &       * ( uIceC(I+1,J,bi,bj) - uIceC(I,J,bi,bj) )
     &       * _recip_dxF(I,J,bi,bj)
     &       + etaPlusZeta(I,J,bi,bj) * k1AtC(I,J,bi,bj)
     &       * 0.5 _d 0 * ( uIceC(I+1,J,bi,bj) + uIceC(I,J,bi,bj) )
     &       - 0.5 _d 0 * pressLoc(I,J,bi,bj)
       ENDDO
      ENDDO
C     contribution of sigma12 to rhs of v-equation
      DO J=jMin,jMax
       DO I=iMin,iMax+1
        hFacM = seaiceMaskU(i,j,bi,bj) - seaiceMaskU(i,j-1,bi,bj)
        sig12(I,J) = etaZloc(I,J,bi,bj) * (
     &       ( uIceC(I,J,bi,bj) - uIceC(I,J-1,bi,bj) )
     &       * _recip_dyU(I,J,bi,bj)
     &       - k2AtZ(I,J,bi,bj)
     &       * 0.5 _d 0 * ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &       )
C     free slip conditions (sig12=0) are taken care of by masking sig12,
     &       *HEFFM(I  ,J  ,bi,bj)*HEFFM(I-1,J  ,bi,bj)
     &       *HEFFM(I  ,J-1,bi,bj)*HEFFM(I-1,J-1,bi,bj)
C     no slip boundary conditions (u(j-1)=-u(j))
C     u(j)+u(j-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with u(j)-u(j-1)
     &       + etaZloc(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &       * ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &         * hFacM * 2. _d 0
       ENDDO
      ENDDO

      IF ( SEAICEuseStrImpCpl ) THEN
C     strictly speaking, this is not a contribution to sig12, but for
C     convenience we add the explicit term -zetaZ*dv/dx here;
C     we do not have to add any metric terms, because they cancel.
       DO J=jMin,jMax
        DO I=iMin,iMax+1
         hFacM = seaiceMaskU(i,j,bi,bj) - seaiceMaskU(i,j-1,bi,bj)
         sig12(I,J) = sig12(I,J) - zetaZloc(I,J,bi,bj) * (
     &        ( vIceC(I,J,bi,bj) - vIceC(I-1,J,bi,bj) )
     &        * _recip_dxV(I,J,bi,bj)
     &        )
C     free slip conditions (sig12=0) are taken care of by masking sig12
     &        *HEFFM(I  ,J  ,bi,bj)*HEFFM(I-1,J  ,bi,bj)
     &        *HEFFM(I  ,J-1,bi,bj)*HEFFM(I-1,J-1,bi,bj)
C     no slip boundary conditions (v(i-1)=-v(i))
C     v(i)+v(i-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with v(i)-v(i-1)
     &        - zetaZloc(I,J,bi,bj) * _recip_dxV(I,J,bi,bj)
     &        * ( vIceC(I,J,bi,bj) + vIceC(I-1,J,bi,bj) )
     &        * hFacM * 2. _d 0
        ENDDO
       ENDDO
      ENDIF

      DO J=jMin,jMax
       DO I=iMin,iMax
C     contribution to the rhs part of grad(sigma)_y
        rhsV(I,J,bi,bj) = rhsV(I,J,bi,bj)
     &       + recip_rAs(I,J,bi,bj) * seaiceMaskV(I,J,bi,bj) *
     &       ( _dyU(I+1,J  ,bi,bj) * sig12(I+1,J  )
     &       - _dyU(I  ,J  ,bi,bj) * sig12(I  ,J  )
     &       + _dxF(I  ,J  ,bi,bj) * sig22(I  ,J  )
     &       - _dxF(I  ,J-1,bi,bj) * sig22(I  ,J-1) )
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SEAICE_LSR_TRIDIAGU
C     !INTERFACE:
      SUBROUTINE SEAICE_LSR_TRIDIAGU(
     I     AU, BU, CU, uRt1, uRt2, rhsU, uTmp, seaiceMaskU, WFAU,
     U     uIce,
     I     iMin, iMax, jMin, jMax, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_LSR_TRIDIAGU
C     | o Solve tridiagonal problem in uIce for LSR solver
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime     :: Simulation time
C     myIter     :: Simulation timestep number
C     myThid     :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER iMin, iMax, jMin, jMax, bi, bj
C     diagonals of coefficient matrices
      _RL AU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, v(j+/-1)
      _RL uRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     right hand side
      _RL rhsU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     velocity of previous iteration step
      _RL uTmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     on entry: first guess and on exit: solution
      _RL uIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     mask
      _RL seaiceMaskU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     relaxation factor
      _RL WFAU
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j  :: Loop counters
      INTEGER I,J,IM
      INTEGER jMinLoc, jStep
#ifdef SEAICE_LSR_ZEBRA
      INTEGER K
      PARAMETER ( jStep = 2 )
#else
      PARAMETER ( jStep = 1 )
#endif /* SEAICE_LSR_ZEBRA */
      _RL AA3, bet
      _RL URT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CUU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     Need to initialize local arrays
      DO J = 1-OLy,sNy+OLy
       DO I = 1-OLx,sNx+OLx
        URT(I,J) = 0. _d 0
        CUU(I,J) = 0. _d 0
       ENDDO
      ENDDO
#ifdef SEAICE_LSR_ZEBRA
      DO K=0,1
      jMinLoc = jMin+K
#else
      jMinLoc = jMin
#endif /* SEAICE_LSR_ZEBRA */
      DO J=jMinLoc,jMax,jStep
       DO I=iMin,iMax
        AA3 = 0. _d 0
        IF (I.EQ.iMin) AA3 = AA3 - AU(I,J,bi,bj)*uIce(I-1,J,bi,bj)
        IF (I.EQ.iMax) AA3 = AA3 - CU(I,J,bi,bj)*uIce(I+1,J,bi,bj)

        URT(I,J)=rhsU(I,J,bi,bj)
     &       + AA3
     &       + uRt1(I,J,bi,bj)*uIce(I,J-1,bi,bj)
     &       + uRt2(I,J,bi,bj)*uIce(I,J+1,bi,bj)
        URT(I,J)=URT(I,J)* seaiceMaskU(I,J,bi,bj)
       ENDDO
C     beginning of tridiagnoal solver
       DO I=iMin,iMax
        CUU(I,J)=CU(I,J,bi,bj)
       ENDDO
       CUU(iMin,J)=CUU(iMin,J)/BU(iMin,J,bi,bj)
       URT(iMin,J)=URT(iMin,J)/BU(iMin,J,bi,bj)
#ifdef SEAICE_VECTORIZE_LSR
      ENDDO
C     start a new loop with reversed order to support automatic vectorization
CADJ loop = sequential
      DO I=iMin+1,iMax
       IM=I-1
       DO J=jMinLoc,jMax,jStep
#else /* do not SEAICE_VECTORIZE_LSR */
CADJ loop = sequential
       DO I=iMin+1,iMax
        IM=I-1
#endif /* SEAICE_VECTORIZE_LSR */
        bet      = BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM,J)
        CUU(I,J) = CUU(I,J)/bet
        URT(I,J) = (URT(I,J)-AU(I,J,bi,bj)*URT(IM,J))/bet
       ENDDO
#ifdef SEAICE_VECTORIZE_LSR
      ENDDO
CADJ loop = sequential
      DO I=iMin,iMax-1
       IM=sNx-I
       DO J=jMinLoc,jMax,jStep
#else
CADJ loop = sequential
       DO I=iMin,iMax-1
        IM=sNx-I
#endif /* SEAICE_VECTORIZE_LSR */
        URT(IM,J)=URT(IM,J)-CUU(IM,J)*URT(IM+1,J)
       ENDDO
#ifdef SEAICE_VECTORIZE_LSR
      ENDDO
#endif /* SEAICE_VECTORIZE_LSR */
C     end of tridiagnoal solver, solution is URT
C     relaxation with WFAU
#ifdef SEAICE_VECTORIZE_LSR
C     go back to original order
      DO J=jMinLoc,jMax,jStep
#endif /* SEAICE_VECTORIZE_LSR */
       DO I=iMin,iMax
        uIce(I,J,bi,bj)=uTmp(I,J,bi,bj)
     &       +WFAU*(URT(I,J)-uTmp(I,J,bi,bj))
       ENDDO
      ENDDO
#ifdef SEAICE_LSR_ZEBRA
      ENDDO
#endif /* SEAICE_LSR_ZEBRA */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SEAICE_LSR_TRIDIAGV
C     !INTERFACE:
      SUBROUTINE SEAICE_LSR_TRIDIAGV(
     I     AV, BV, CV, vRt1, vRt2, rhsV, vTmp, seaiceMaskV, WFAV,
     U     vIce,
     I     iMin, iMax, jMin, jMax, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_LSR_TRIDIAGV
C     | o Solve tridiagonal problem in vIce for LSR solver
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime     :: Simulation time
C     myIter     :: Simulation timestep number
C     myThid     :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER iMin, iMax, jMin, jMax, bi, bj
C     diagonals of coefficient matrices
      _RL AV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, v(j+/-1)
      _RL vRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     right hand side
      _RL rhsV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     velocity of previous iteration step
      _RL vTmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     on entry: first guess and on exit: solution
      _RL vIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     mask
      _RL seaiceMaskV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     relaxation factor
      _RL WFAV
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j  :: Loop counters
      INTEGER I,J,JM
      INTEGER iMinLoc, iStep
#ifdef SEAICE_LSR_ZEBRA
      INTEGER K
      PARAMETER ( iStep = 2 )
#else
      PARAMETER ( iStep = 1 )
#endif /* SEAICE_LSR_ZEBRA */
      _RL AA3, bet
      _RL VRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CVV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     Need to initialize local arrays
      DO J = 1-OLy,sNy+OLy
       DO I = 1-OLx,sNx+OLx
        VRT(I,J) = 0. _d 0
        CVV(I,J) = 0. _d 0
       ENDDO
      ENDDO
#ifdef SEAICE_LSR_ZEBRA
      DO K=0,1
      iMinLoc = iMin+K
#else
      iMinLoc = iMin
#endif /*  SEAICE_LSR_ZEBRA */
#ifdef SEAICE_VECTORIZE_LSR
      DO J=jMin,jMax
       DO I=iMinLoc,iMax,iStep
#else
      DO I=iMinLoc,iMax,iStep
       DO J=jMin,jMax
#endif /* SEAICE_VECTORIZE_LSR */
        AA3 = 0. _d 0
        IF (J.EQ.jMin) AA3 = AA3 - AV(I,J,bi,bj)*vIce(I,J-1,bi,bj)
        IF (J.EQ.jMax) AA3 = AA3 - CV(I,J,bi,bj)*vIce(I,J+1,bi,bj)

        VRT(I,J)=rhsV(I,J,bi,bj)
     &       + AA3
     &       + vRt1(I,J,bi,bj)*vIce(I-1,J,bi,bj)
     &       + vRt2(I,J,bi,bj)*vIce(I+1,J,bi,bj)
        VRT(I,J)=VRT(I,J)* seaiceMaskV(I,J,bi,bj)

C     beginning of tridiagnoal solver
        CVV(I,J)=CV(I,J,bi,bj)
       ENDDO
#ifdef SEAICE_VECTORIZE_LSR
      ENDDO
      DO I=iMinLoc,iMax,iStep
#endif /* SEAICE_VECTORIZE_LSR */
       CVV(I,jMin)=CVV(I,jMin)/BV(I,jMin,bi,bj)
       VRT(I,jMin)=VRT(I,jMin)/BV(I,jMin,bi,bj)
#ifdef SEAICE_VECTORIZE_LSR
      ENDDO
C     start a new loop with reversed order to support automatic vectorization
CADJ loop = sequential
      DO J=jMin+1,jMax
       JM=J-1
       DO I=iMinLoc,iMax,iStep
#else /* do not SEAICE_VECTORIZE_LSR */
CADJ loop = sequential
       DO J=jMin+1,jMax
        JM=J-1
#endif /* SEAICE_VECTORIZE_LSR */
        bet      = BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM)
        CVV(I,J) = CVV(I,J)/bet
        VRT(I,J) = (VRT(I,J)-AV(I,J,bi,bj)*VRT(I,JM))/bet
       ENDDO
#ifdef SEAICE_VECTORIZE_LSR
      ENDDO
C     go back to original order
CADJ loop = sequential
      DO J=jMin,jMax-1
       JM=sNy-J
       DO I=iMinLoc,iMax,iStep
#else
CADJ loop = sequential
       DO J=jMin,jMax-1
        JM=sNy-J
#endif /* SEAICE_VECTORIZE_LSR */
        VRT(I,JM)=VRT(I,JM)-CVV(I,JM)*VRT(I,JM+1)
       ENDDO
#ifdef SEAICE_VECTORIZE_LSR
      ENDDO
C     end of tridiagnoal solver, solution is VRT
C     relaxation with WFAV
      DO J=jMin,jMax
       DO I=iMinLoc,iMax,iStep
#else
       DO J=jMin,jMax
#endif /* SEAICE_VECTORIZE_LSR */
        vIce(I,J,bi,bj)=vTmp(I,J,bi,bj)
     &       +WFAV*(VRT(I,J)-vTmp(I,J,bi,bj))
       ENDDO
      ENDDO
#ifdef SEAICE_LSR_ZEBRA
      ENDDO
#endif /* SEAICE_LSR_ZEBRA */

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */

      RETURN
      END
