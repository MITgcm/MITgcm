C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_lsr.F,v 1.76 2012/10/22 23:59:48 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#else
# define OBCS_UVICE_OLD
#endif

C--  File seaice_lsr.F: seaice LSR dynamical solver S/R:
C--   Contents
C--   o SEAICE_LSR
C--   o SEAICE_RESIDUAL

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

      INTEGER ilcall
      INTEGER i, j, k, m, bi, bj, j1, j2, im, jm
      INTEGER ICOUNT1, ICOUNT2
      INTEGER kSrf
      INTEGER SOLV_MAX_TMP
      LOGICAL doIterate4u, doIterate4v
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _RL WFAU, WFAV, WFAU1, WFAV1, WFAU2, WFAV2
      _RL AA3, S1, S2, S1A, S2A
      _RL hFacM, hFacP
      _RL eplus, eminus

C     coefficients of ice velocities in coefficient matrix
C     for both U and V-equation
C     XX: double derivative in X
C     YY: double derivative in Y
C     XM: metric term with derivative in X
C     YM: metric term with derivative in Y
      _RL UXX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UYY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UXM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UYM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VXX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VYY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VXM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VYM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     diagonals of coefficient matrices
      _RL AU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     RHS
      _RL rhsU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, u(j+/-1)
      _RL uRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, v(i+/-1)
      _RL vRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     abbreviations
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaMeanZ    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     contribution of sigma on righ hand side
      _RL sig11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     auxillary fields
      _RL URT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CUU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VRT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CVV  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER itmpkey, itmpkey2
#endif
      _RL COSWAT
      _RS SINWAT
      _RL UERR
#ifdef SEAICE_ALLOW_CHECK_LSR_CONVERGENCE
      _RL resnorm, EKnorm, counter
#endif /* SEAICE_ALLOW_CHECK_LSR_CONVERGENCE */
      LOGICAL printResidual
      _RL massU, massV
      _RL residUini, residVini, residUend, residVend
#ifdef SEAICE_ALLOW_FREEDRIFT
      _RL uRes (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRes (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL errIni, errFD, errSum
      _RL residU_fd, residV_fd
      _RL residUmix, residVmix
#endif /* SEAICE_ALLOW_FREEDRIFT */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      printResidual = debugLevel.GE.debLevA
     &  .AND. DIFFERENT_MULTIPLE( SEAICE_monFreq, myTime, deltaTClock )
      massU = -1. _d 0
      massV = -1. _d 0

#ifdef ALLOW_AUTODIFF_TAMC
cph break artificial dependencies
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          press(i,j,bi,bj)=0. _d 0
          zeta(i,j,bi,bj)=0. _d 0
          eta(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

#ifdef ALLOW_AUTODIFF_TAMC
        DO ilcall=1,MPSEUDOTIMESTEPS
         itmpkey = (ikey_dynamics-1)*MPSEUDOTIMESTEPS + ilcall
#else
        DO ilcall=1,NPSEUDOTIMESTEPS
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

         IF ( ilcall .LE. NPSEUDOTIMESTEPS ) THEN
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
      WFAU1=0.95 _d 0
      WFAV1=0.95 _d 0
      WFAU2=ZERO
      WFAV2=ZERO

      S1A=0.80 _d 0
      S2A=0.80 _d 0
      WFAU=WFAU1
      WFAV=WFAV1

      S1 = 0.
      S2 = 0.

      k = 1

      IF ( ilcall .EQ. 1 ) THEN
C NOW DO PREDICTOR TIME STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIceNm1(i,j,bi,bj)=uIce(I,J,bi,bj)
           vIceNm1(i,j,bi,bj)=vIce(I,J,bi,bj)
           uIceC(I,J,bi,bj)=uIce(I,J,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE uicec = comlev1_dynsol, kind=isbyte,
cphCADJ &     key = itmpkey
cphCADJ STORE vicec = comlev1_dynsol, kind=isbyte,
cphCADJ &     key = itmpkey
#endif
      ELSE
C NOW DO MODIFIED EULER STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIce(I,J,bi,bj)=HALF*(uIce(I,J,bi,bj)+uIceNm1(i,j,bi,bj))
           vIce(I,J,bi,bj)=HALF*(vIce(I,J,bi,bj)+vIceNm1(i,j,bi,bj))
           uIceC(I,J,bi,bj)=uIce(I,J,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( ilcall .GT. 2 ) THEN
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
cph That is an important one! Note, that
cph * lsr is called twice, thus the icall index
cph * this storing is still outside the iteration loop
CADJ STORE uice = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
CADJ STORE vice = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
#endif /* ALLOW_AUTODIFF_TAMC */

      CALL SEAICE_CALC_STRAINRATES(
     I     uIceC, vIceC,
     O     e11, e22, e12,
     I     ilcall, myTime, myIter, myThid )

      CALL SEAICE_CALC_VISCOSITIES(
     I     e11, e22, e12, zMin, zMax, hEffM, press0,
     O     eta, zeta, press,
     I     ilcall, myTime, myIter, myThid )

      CALL SEAICE_OCEANDRAG_COEFFS(
     I     uIceC, vIceC,
     O     DWATN,
     I     ilcall, myTime, myIter, myThid )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
C     set up anti symmetric drag force and add in current force
C     ( remember to average to correct velocity points )
          FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &         COSWAT * uVel(I,J,kSrf,bi,bj)
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &         ( DWATN(I  ,J,bi,bj) * 0.5 _d 0 *
     &          (vVel(I  ,J  ,kSrf,bi,bj)-vIceC(I  ,J  ,bi,bj)
     &          +vVel(I  ,J+1,kSrf,bi,bj)-vIceC(I  ,J+1,bi,bj))
     &         + DWATN(I-1,J,bi,bj) * 0.5 _d 0 *
     &          (vVel(I-1,J  ,kSrf,bi,bj)-vIceC(I-1,J  ,bi,bj)
     &          +vVel(I-1,J+1,kSrf,bi,bj)-vIceC(I-1,J+1,bi,bj))
     &         )
          FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &         COSWAT * vVel(I,J,kSrf,bi,bj)
     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I,J  ,bi,bj) * 0.5 _d 0 *
     &          (uVel(I  ,J  ,kSrf,bi,bj)-uIceC(I  ,J  ,bi,bj)
     &          +uVel(I+1,J  ,kSrf,bi,bj)-uIceC(I+1,J  ,bi,bj))
     &         + DWATN(I,J-1,bi,bj) * 0.5 _d 0 *
     &          (uVel(I  ,J-1,kSrf,bi,bj)-uIceC(I  ,J-1,bi,bj)
     &          +uVel(I+1,J-1,kSrf,bi,bj)-uIceC(I+1,J-1,bi,bj))
     &         )
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
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
        DO j=1,sNy
         DO i=1,sNx
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         +seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *uIceNm1(i,j,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         +seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *vIceNm1(i,j,bi,bj)
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
         ENDDO
        ENDDO
#ifdef SEAICE_ALLOW_FREEDRIFT
        IF ( LSR_mixIniGuess.GE.1 ) THEN
         DO j=1,sNy
          DO i=1,sNx
          uIce_fd(i,j,bi,bj) = FORCEX(i,j,bi,bj)
     &       / ( 1. _d 0 - seaiceMaskU(i,j,bi,bj)
     &         + seaiceMassU(i,j,bi,bj)/SEAICE_deltaTdyn
     &         + HALF*( DWATN(i,j,bi,bj) + DWATN(i-1,j,bi,bj) ) * COSWAT
     &         )
          vIce_fd(i,j,bi,bj) = FORCEY(i,j,bi,bj)
     &       / ( 1. _d 0 - seaiceMaskV(i,j,bi,bj)
     &         + seaiceMassV(i,j,bi,bj)/SEAICE_deltaTdyn
     &         + HALF*( DWATN(i,j,bi,bj) + DWATN(i,j-1,bi,bj) ) * COSWAT
     &         )
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_ALLOW_FREEDRIFT */
       ENDDO
      ENDDO
C
C     some abbreviations
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=0,sNy
         DO I=0,sNx
          etaPlusZeta (I,J,bi,bj) = ETA (I,J,bi,bj)+ZETA(I,J,bi,bj)
          zetaMinusEta(I,J,bi,bj) = ZETA(I,J,bi,bj)-ETA (I,J,bi,bj)
         ENDDO
        ENDDO
        DO J=1,sNy+1
         DO I=1,sNx+1
          etaMeanZ (I,J,bi,bj) =
     &         (           ETA (I,J  ,bi,bj)  + ETA (I-1,J  ,bi,bj)
     &         +           ETA (I,J-1,bi,bj)  + ETA (I-1,J-1,bi,bj) )
     &         / MAX(1.D0,maskC(I,J,  k,bi,bj)+maskC(I-1,J,  k,bi,bj)
     &         +          maskC(I,J-1,k,bi,bj)+maskC(I-1,J-1,k,bi,bj) )
         ENDDO
        ENDDO
C     free-slip means no lateral stress, which is best achieved masking
C     eta on vorticity(=Z)-points; from now on we only need to worry
C     about the no-slip boundary conditions
        IF (.NOT.SEAICE_no_slip) THEN
         DO J=1,sNy+1
          DO I=1,sNx+1
           etaMeanZ (I,J,bi,bj) = etaMeanZ(I,J,bi,bj)
     &          *maskC(I,J,  k,bi,bj)*maskC(I-1,J,  k,bi,bj)
     &          *maskC(I,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
C     coefficients of uIce(I,J) and vIce(I,J) belonging to ...
        DO J=1,sNy
         DO I=0,sNx
C     ... d/dx (eta+zeta) d/dx u
          UXX(I,J,bi,bj) = _dyF(I,J,bi,bj) * etaPlusZeta(I,J,bi,bj)
     &         * _recip_dxF(I,J,bi,bj)
C     ... d/dx (zeta-eta) k1 u
          UXM(I,J,bi,bj) = _dyF(I,J,bi,bj) * zetaMinusEta(I,J,bi,bj)
     &         * k1AtC(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=1,sNy+1
         DO I=1,sNx
C     ... d/dy eta d/dy u
          UYY(I,J,bi,bj) = _dxV(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
     &         * _recip_dyU(I,J,bi,bj)
C     ... d/dy eta k2 u
          UYM(I,J,bi,bj) = _dxV(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
     &         * k2AtZ(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx+1
C     ... d/dx eta dv/dx
          VXX(I,J,bi,bj) = _dyU(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
     &         * _recip_dxV(I,J,bi,bj)
C     ... d/dx eta k1 v
          VXM(I,J,bi,bj) = _dyU(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
     &         * k1AtZ(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=0,sNy
         DO I=1,sNx
C     ... d/dy eta+zeta dv/dy
          VYY(I,J,bi,bj) = _dxF(I,J,bi,bj) * etaPlusZeta(I,J,bi,bj)
     &         * _recip_dyF(I,J,bi,bj)
C     ... d/dy (zeta-eta) k2 v
          VYM(I,J,bi,bj) = _dxF(I,J,bi,bj) * zetaMinusEta(I,J,bi,bj)
     &         * k2AtC(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Prepare for Solving uIce :

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     assemble coefficient matrix, beware of sign convention: because this
C     is the left hand side we calculate -grad(sigma), but the coefficients
C     of U(I,J+/-1) are counted on the right hand side
        DO J=1,sNy
         DO I=1,sNx
C     coefficients for UICE(I-1,J)
          AU(I,J,bi,bj)= ( - UXX(I-1,J,bi,bj) + UXM(I-1,J,bi,bj) )
     &         * seaiceMaskU(I,J,bi,bj)
C     coefficients for UICE(I+1,J)
          CU(I,J,bi,bj)= ( - UXX(I  ,J,bi,bj) - UXM(I  ,J,bi,bj) )
     &         * seaiceMaskU(I,J,bi,bj)
C     coefficients for UICE(I,J)
          BU(I,J,bi,bj)=(ONE - seaiceMaskU(I,J,bi,bj)) +
     &         ( UXX(I-1,J  ,bi,bj) + UXX(I,J,bi,bj)
     &         + UYY(I  ,J+1,bi,bj) + UYY(I,J,bi,bj)
     &         + UXM(I-1,J  ,bi,bj) - UXM(I,J,bi,bj)
     &         + UYM(I  ,J+1,bi,bj) - UYM(I,J,bi,bj)
     &         ) * seaiceMaskU(I,J,bi,bj)
C     coefficients of uIce(I,J-1)
          uRt1(I,J,bi,bj)= UYY(I,J  ,bi,bj) + UYM(I,J  ,bi,bj)
C     coefficients of uIce(I,J+1)
          uRt2(I,J,bi,bj)= UYY(I,J+1,bi,bj) - UYM(I,J+1,bi,bj)
         ENDDO
        ENDDO

C     apply boundary conditions according to slip factor
C     for no slip, set u on boundary to zero: u(j+/-1)=-u(j)
C     for the free slip case sigma_12 = 0
        DO J=1,sNy
         DO I=1,sNx
          hFacM = seaiceMaskU(I,J-1,bi,bj)
          hFacP = seaiceMaskU(I,J+1,bi,bj)
C     copy contributions to coefficient of U(I,J)
C     beware of sign convection: uRt1/2 have the opposite sign convention
C     than BU, hence the minus sign
          BU(I,J,bi,bj)=BU(I,J,bi,bj) + seaiceMaskU(I,J,bi,bj) *
     &         ( ( 1. _d 0 - hFacM )
     &         * ( UYY(I  ,J  ,bi,bj) + UYM(I  ,J  ,bi,bj) )
     &         + ( 1. _d 0 - hFacP )
     &         * ( UYY(I  ,J+1,bi,bj) - UYM(I  ,J+1,bi,bj) ) )
C     reset coefficients of U(I,J-1) and U(I,J+1)
          uRt1(I,J,bi,bj) = uRt1(I,J,bi,bj) * hFacM
          uRt2(I,J,bi,bj) = uRt2(I,J,bi,bj) * hFacP
         ENDDO
        ENDDO

C     now we need to normalize everything by the grid cell area
        DO J=1,sNy
         DO I=1,sNx
          AU(I,J,bi,bj)    = AU(I,J,bi,bj)    * recip_rAw(I,J,bi,bj)
          CU(I,J,bi,bj)    = CU(I,J,bi,bj)    * recip_rAw(I,J,bi,bj)
C     here we need ad in the contribution from the time derivative
C     and the symmetric drag term; must be done after normalizing
          BU(I,J,bi,bj)    = BU(I,J,bi,bj)    * recip_rAw(I,J,bi,bj)
     &         + seaiceMaskU(I,J,bi,bj) *
     &         ( seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + 0.5 _d 0 * ( DWATN(I,J,bi,bj) + DWATN(I-1,J,bi,bj) )
     &         * COSWAT )
          uRt1(I,J,bi,bj) = uRt1(I,J,bi,bj) * recip_rAw(I,J,bi,bj)
          uRt2(I,J,bi,bj) = uRt2(I,J,bi,bj) * recip_rAw(I,J,bi,bj)
         ENDDO
        ENDDO

C     now set up right-hand side
C     contribution of sigma11 to rhs
        DO J=1,sNy
         DO I=0,sNx
          sig11(I,J) = zetaMinusEta(I,J,bi,bj)
     &         * ( vIceC(I,J+1,bi,bj) - vIceC(I,J,bi,bj) )
     &         * _recip_dyF(I,J,bi,bj)
     &         + etaPlusZeta(I,J,bi,bj) * k2AtC(I,J,bi,bj)
     &         * 0.5 _d 0 * ( vIceC(I,J+1,bi,bj) + vIceC(I,J,bi,bj) )
     &         - 0.5 _d 0 * PRESS(I,J,bi,bj)
         ENDDO
        ENDDO
C     contribution of sigma12 to rhs of u-equation
        DO J=1,sNy+1
         DO I=1,sNx
          hFacM = seaiceMaskV(I,J,bi,bj) - seaiceMaskV(I-1,J,bi,bj)
          sig12(I,J) = etaMeanZ(I,J,bi,bj) * (
     &         ( vIceC(I,J,bi,bj) - vIceC(I-1,J,bi,bj) )
     &         * _recip_dxV(I,J,bi,bj)
     &         - k1AtZ(I,J,bi,bj)
     &         * 0.5 _d 0 * ( vIceC(I,J,bi,bj) + vIceC(I-1,J,bi,bj) )
     &         )
C     free slip conditions (sig12=0) are taken care of by masking sig12
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
C     no slip boundary conditions (v(i-1)=-v(i))
C     v(i)+v(i-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with v(i)-v(i-1)
     &         + etaMeanZ(I,J,bi,bj) * _recip_dxV(I,J,bi,bj)
     &         * ( vIceC(I,J,bi,bj) + vIceC(I-1,J,bi,bj) )
     &         * hFacM * 2. _d 0
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
C     contribution to the rhs part of grad(sigma)_x
          rhsU(I,J,bi,bj) = FORCEX(I,J,bi,bj)
     &         + recip_rAw(I,J,bi,bj) * seaiceMaskU(I,J,bi,bj) *
     &         ( _dyF(I  ,J  ,bi,bj)*sig11(I  ,J  )
     &         - _dyF(I-1,J  ,bi,bj)*sig11(I-1,J  )
     &         + _dxV(I  ,J+1,bi,bj)*sig12(I  ,J+1)
     &         - _dxV(I  ,J  ,bi,bj)*sig12(I  ,J  ) )
         ENDDO
        ENDDO

#ifndef OBCS_UVICE_OLD
C--     prevent tri-diagonal solver to modify OB values:
        DO J=1,sNy
         DO I=1,sNx
          IF ( maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj) .EQ. 0. ) THEN
            AU(I,J,bi,bj) = ZERO
            BU(I,J,bi,bj) = ONE
            CU(I,J,bi,bj) = ZERO
            uRt1(I,J,bi,bj) = ZERO
            uRt2(I,J,bi,bj) = ZERO
            rhsU(I,J,bi,bj) = uIce(I,J,bi,bj)
          ENDIF
         ENDDO
        ENDDO
#endif /* OBCS_UVICE_OLD */

       ENDDO
      ENDDO

C--   Prepare for Solving vIce :

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     assemble coefficient matrix, beware of sign convention: because this
C     is the left hand side we calculate -grad(sigma), but the coefficients
C     of U(I,J+/-1) are counted on the right hand side
        DO J=1,sNy
         DO I=1,sNx
C     coefficients for VICE(I,J-1)
          AV(I,J,bi,bj)=( - VYY(I,J-1,bi,bj) + VYM(I,J-1,bi,bj)
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for VICE(I,J+1)
          CV(I,J,bi,bj)=( - VYY(I,J  ,bi,bj) - VYM(I,J  ,bi,bj)
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for VICE(I,J)
          BV(I,J,bi,bj)= (ONE - seaiceMaskV(I,J,bi,bj)) +
     &         ( VXX(I,J,bi,bj) + VXX(I+1,J  ,bi,bj)
     &         + VYY(I,J,bi,bj) + VYY(I  ,J-1,bi,bj)
     &         - VXM(I,J,bi,bj) + VXM(I+1,J  ,bi,bj)
     &         - VYM(I,J,bi,bj) + VYM(I  ,J-1,bi,bj)
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for V(I-1,J)
          vRt1(I,J,bi,bj) = VXX(I  ,J,bi,bj) + VXM(I  ,J,bi,bj)
C     coefficients for V(I+1,J)
          vRt2(I,J,bi,bj) = VXX(I+1,J,bi,bj) - VXM(I+1,J,bi,bj)
         ENDDO
        ENDDO

C     apply boundary conditions according to slip factor
C     for no slip, set u on boundary to zero: v(i+/-1)=-v(i)
C     for the free slip case sigma_12 = 0
        DO J=1,sNy
         DO I=1,sNx
          hFacM = seaiceMaskV(i-1,j,bi,bj)
          hFacP = seaiceMaskV(i+1,j,bi,bj)
C     copy contributions to coefficient of V(I,J)
C     beware of sign convection: vRt1/2 have the opposite sign convention
C     than BV, hence the minus sign
          BV(I,J,bi,bj)=BV(I,J,bi,bj) + seaiceMaskV(I,J,bi,bj) *
     &         ( ( 1. _d 0 - hFacM )
     &         * ( VXX(I  ,J,bi,bj) + VXM(I  ,J,bi,bj) )
     &         + ( 1. _d 0 - hFacP )
     &         * ( VXX(I+1,J,bi,bj) - VXM(I+1,J,bi,bj) ) )
C     reset coefficients of V(I-1,J) and V(I+1,J)
          vRt1(I,J,bi,bj) = vRt1(I,J,bi,bj) * hFacM
          vRt2(I,J,bi,bj) = vRt2(I,J,bi,bj) * hFacP
         ENDDO
        ENDDO

C     now we need to normalize everything by the grid cell area
        DO J=1,sNy
         DO I=1,sNx
          AV(I,J,bi,bj)    = AV(I,J,bi,bj)    * recip_rAs(I,J,bi,bj)
          CV(I,J,bi,bj)    = CV(I,J,bi,bj)    * recip_rAs(I,J,bi,bj)
C     here we need ad in the contribution from the time derivative
C     and the symmetric drag term; must be done after normalizing
          BV(I,J,bi,bj)    = BV(I,J,bi,bj)    * recip_rAs(I,J,bi,bj)
     &         + seaiceMaskV(I,J,bi,bj) *
     &         ( seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + 0.5 _d 0 * ( DWATN(I,J,bi,bj) + DWATN(I,J-1,bi,bj) )
     &         * COSWAT )
          vRt1(I,J,bi,bj) = vRt1(I,J,bi,bj) * recip_rAs(I,J,bi,bj)
          vRt2(I,J,bi,bj) = vRt2(I,J,bi,bj) * recip_rAs(I,J,bi,bj)
         ENDDO
        ENDDO

C     now set up right-hand-side
C     contribution of sigma22 to rhs
        DO J=0,sNy
         DO I=1,sNx
          sig22(I,J) = zetaMinusEta(I,J,bi,bj)
     &         * ( uIceC(I+1,J,bi,bj) - uIceC(I,J,bi,bj) )
     &         * _recip_dxF(I,J,bi,bj)
     &         + etaPlusZeta(I,J,bi,bj) * k1AtC(I,J,bi,bj)
     &         * 0.5 _d 0 * ( uIceC(I+1,J,bi,bj) + uIceC(I,J,bi,bj) )
     &         - 0.5 _d 0 * PRESS(I,J,bi,bj)
         ENDDO
        ENDDO
C     contribution of sigma12 to rhs of v-equation
        DO J=1,sNy
         DO I=1,sNx+1
          hFacM = seaiceMaskU(i,j,bi,bj) - seaiceMaskU(i,j-1,bi,bj)
          sig12(I,J) = etaMeanZ(I,J,bi,bj) * (
     &         ( uIceC(I,J,bi,bj) - uIceC(I,J-1,bi,bj) )
     &         * _recip_dyU(I,J,bi,bj)
     &         - k2AtZ(I,J,bi,bj)
     &         * 0.5 _d 0 * ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &         )
C     free slip conditions (sig12=0) are taken care of by masking sig12,
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
C     no slip boundary conditions (u(j-1)=-u(j))
C     u(j)+u(j-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with u(j)-u(j-1)
     &         + etaMeanZ(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &         * ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &         * hFacM * 2. _d 0
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
C     contribution to the rhs part of grad(sigma)_y
          rhsV(I,J,bi,bj) = FORCEY(I,J,bi,bj)
     &         + recip_rAs(I,J,bi,bj) * seaiceMaskV(I,J,bi,bj) *
     &         ( _dyU(I+1,J  ,bi,bj) * sig12(I+1,J  )
     &         - _dyU(I  ,J  ,bi,bj) * sig12(I  ,J  )
     &         + _dxF(I  ,J  ,bi,bj) * sig22(I  ,J  )
     &         - _dxF(I  ,J-1,bi,bj) * sig22(I  ,J-1) )
         ENDDO
        ENDDO

#ifndef OBCS_UVICE_OLD
C--     prevent tri-diagonal solver to modify OB values:
        DO J=1,sNy
         DO I=1,sNx
          IF ( maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj) .EQ. 0. ) THEN
            AV(I,J,bi,bj) = ZERO
            BV(I,J,bi,bj) = ONE
            CV(I,J,bi,bj) = ZERO
            vRt1(I,J,bi,bj) = ZERO
            vRt2(I,J,bi,bj) = ZERO
            rhsV(I,J,bi,bj) = vIce(I,J,bi,bj)
          ENDIF
         ENDDO
        ENDDO
#endif /* OBCS_UVICE_OLD */

       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &        'Uice pre  iter (SEAICE_LSR', MOD(ilcall,1000), ')'
        CALL DEBUG_STATS_RL( 1, UICE, msgBuf, myThid )
        WRITE(msgBuf,'(A,I3,A)')
     &        'Vice pre  iter (SEAICE_LSR', MOD(ilcall,1000), ')'
        CALL DEBUG_STATS_RL( 1, VICE, msgBuf, myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

C--   Calculate initial residual of the linearised system
      IF ( printResidual .OR. LSR_mixIniGuess.GE.1 ) THEN
#ifdef SEAICE_ALLOW_FREEDRIFT
        IF ( LSR_mixIniGuess.GE.1 )
     &  CALL EXCH_UV_XY_RL( uIce_fd, vIce_fd, .TRUE., myThid )
        IF ( LSR_mixIniGuess.GE.0 )
     &  CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uIce_fd, vIce_fd,
     U                  massU, massV,
     O                  residU_fd, residV_fd, uRes, vRes,
     I                  printResidual, myIter, myThid )
#endif /* SEAICE_ALLOW_FREEDRIFT */
        CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uIce, vIce,
     U                  massU, massV,
     O                  residUini, residVini, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
      ENDIF
#ifdef SEAICE_ALLOW_FREEDRIFT
      IF ( LSR_mixIniGuess.GE.2 ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice, vice = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
CADJ STORE utmp, vtmp = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
# endif /* ALLOW_AUTODIFF_TAMC */
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
             errIni = uTmp(i,j,bi,bj)*uTmp(i,j,bi,bj)
             errFD  = uRes(i,j,bi,bj)*uRes(i,j,bi,bj)
             IF ( LSR_mixIniGuess.GE.4 ) THEN
               errIni = errIni*errIni
               errFD  = errFD *errFD
             ENDIF
             errSum = ( errIni + errFD )
     &               *maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj)
             IF ( errSum.GT.0. ) THEN
               uIce(i,j,bi,bj) = ( errFD *uIce(i,j,bi,bj)
     &                           + errIni*uIce_fd(i,j,bi,bj)
     &                           )/errSum
             ENDIF
             errIni = vTmp(i,j,bi,bj)*vTmp(i,j,bi,bj)
             errFD  = vRes(i,j,bi,bj)*vRes(i,j,bi,bj)
             IF ( LSR_mixIniGuess.GE.4 ) THEN
               errIni = errIni*errIni
               errFD  = errFD *errFD
             ENDIF
             errSum = ( errIni + errFD )
     &               *maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj)
             IF ( errSum.GT.0. ) THEN
               vIce(i,j,bi,bj) = ( errFD *vIce(i,j,bi,bj)
     &                           + errIni*vIce_fd(i,j,bi,bj)
     &                           )/errSum
             ENDIF
           ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL EXCH_UV_XY_RL( uIce, vIce, .TRUE., myThid )
       IF ( printResidual ) THEN
        CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uIce, vIce,
     U                  massU, massV,
     O                  residUmix, residVmix, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
       ENDIF
      ENDIF
#endif /* SEAICE_ALLOW_FREEDRIFT */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C NOW DO ITERATION

cph--- iteration starts here
cph--- need to kick out goto
      doIterate4u = .TRUE.
      doIterate4v = .TRUE.

C ITERATION START -----------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CADJ LOOP = iteration uice, vice
#endif /* ALLOW_AUTODIFF_TAMC */

#if (defined (ALLOW_AUTODIFF_TAMC) && defined (SEAICE_LSR_ADJOINT_ITER))
      IF ( inAdMode ) THEN
        SOLV_MAX_TMP = SOLV_MAX_FIXED
      ELSE
        SOLV_MAX_TMP = SOLV_MAX_ITERS
      ENDIF
#else
      SOLV_MAX_TMP = SOLV_MAX_ITERS
#endif

      ICOUNT1 = SOLV_MAX_TMP
      ICOUNT2 = SOLV_MAX_TMP

      DO m = 1, SOLV_MAX_TMP

# ifdef ALLOW_AUTODIFF_TAMC
#  ifdef SEAICE_LSR_ADJOINT_ITER
         itmpkey2 = (itmpkey-1)*SOLV_MAX_FIXED +
     &              MIN(m,SOLV_MAX_FIXED)
CADJ STORE utmp, vtmp = comlev1_dyniter, kind=isbyte,
CADJ &     key = itmpkey2
#  else
CADJ STORE utmp, vtmp = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
#  endif
# endif /* ALLOW_AUTODIFF_TAMC */

       IF ( doIterate4u .OR. doIterate4v ) THEN

        IF ( useCubedSphereExchange ) THEN
          doIterate4u = .TRUE.
          doIterate4v = .TRUE.
        ENDIF

# ifdef ALLOW_AUTODIFF_TAMC
#  ifdef SEAICE_LSR_ADJOINT_ITER
CADJ STORE uice, vice = comlev1_dyniter, kind=isbyte,
CADJ &     key = itmpkey2
#  else
CADJ STORE uice, vice = comlev1_dynsol, kind=isbyte,
CADJ &     key = itmpkey
#  endif
# endif /* ALLOW_AUTODIFF_TAMC */

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

C-jmc: get less TAF warnings when always (no if doIterate) saving uIce,vIce:
C     save uIce prior to iteration, NOW SET U(3)=U(1)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            uTmp(I,J,bi,bj)=uIce(I,J,bi,bj)
           ENDDO
          ENDDO
C     save vIce prior to iteration, NOW SET V(3)=V(1)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            vTmp(I,J,bi,bj)=vIce(I,J,bi,bj)
           ENDDO
          ENDDO

          IF ( doIterate4u ) THEN
C Solve for uIce :
           DO J=1,sNy
            DO I=1,sNx
             AA3 = ZERO
             IF (I.EQ.1)   AA3 = AA3 - AU(I,J,bi,bj)*uIce(I-1,J,bi,bj)
             IF (I.EQ.sNx) AA3 = AA3 - CU(I,J,bi,bj)*uIce(I+1,J,bi,bj)

             URT(I,J)=rhsU(I,J,bi,bj)
     &            + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &            + uRt1(I,J,bi,bj)*uTmp(I,J-1,bi,bj)
     &            + uRt2(I,J,bi,bj)*uTmp(I,J+1,bi,bj)
#else
     &            + uRt1(I,J,bi,bj)*uIce(I,J-1,bi,bj)
     &            + uRt2(I,J,bi,bj)*uIce(I,J+1,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
             URT(I,J)=URT(I,J)* seaiceMaskU(I,J,bi,bj)
            ENDDO

            DO I=1,sNx
             CUU(I,J)=CU(I,J,bi,bj)
            ENDDO
            CUU(1,J)=CUU(1,J)/BU(1,J,bi,bj)
            URT(1,J)=URT(1,J)/BU(1,J,bi,bj)
#ifdef SEAICE_VECTORIZE_LSR
           ENDDO
C     start a new loop with reversed order to support automatic vectorization
           DO I=2,sNx
            IM=I-1
            DO J=1,sNy
#else /* do not SEAICE_VECTORIZE_LSR */
            DO I=2,sNx
             IM=I-1
#endif /* SEAICE_VECTORIZE_LSR */
             CUU(I,J)=CUU(I,J)/(BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM,J))
             URT(I,J)=(URT(I,J)-AU(I,J,bi,bj)*URT(IM,J))
     &           /(BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM,J))
            ENDDO
#ifdef SEAICE_VECTORIZE_LSR
           ENDDO
C     go back to original order
           DO J=1,sNy
#endif /* SEAICE_VECTORIZE_LSR */
            DO I=1,sNx-1
             J1=sNx-I
             J2=J1+1
             URT(J1,J)=URT(J1,J)-CUU(J1,J)*URT(J2,J)
            ENDDO
            DO I=1,sNx
             uIce(I,J,bi,bj)=uTmp(I,J,bi,bj)
     &           +WFAU*(URT(I,J)-uTmp(I,J,bi,bj))
            ENDDO
           ENDDO
C--   end doIterate4u
          ENDIF

          IF ( doIterate4v ) THEN
C Solve for vIce
           DO I=1,sNx
            DO J=1,sNy
             AA3 = ZERO
             IF (J.EQ.1)   AA3 = AA3 - AV(I,J,bi,bj)*vIce(I,J-1,bi,bj)
             IF (J.EQ.sNy) AA3 = AA3 - CV(I,J,bi,bj)*vIce(I,J+1,bi,bj)

             VRT(I,J)=rhsV(I,J,bi,bj)
     &            + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &            + vRt1(I,J,bi,bj)*vTmp(I-1,J,bi,bj)
     &            + vRt2(I,J,bi,bj)*vTmp(I+1,J,bi,bj)
#else
     &            + vRt1(I,J,bi,bj)*vIce(I-1,J,bi,bj)
     &            + vRt2(I,J,bi,bj)*vIce(I+1,J,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
             VRT(I,J)=VRT(I,J)* seaiceMaskV(I,J,bi,bj)
            ENDDO

            DO J=1,sNy
             CVV(I,J)=CV(I,J,bi,bj)
            ENDDO
            CVV(I,1)=CVV(I,1)/BV(I,1,bi,bj)
            VRT(I,1)=VRT(I,1)/BV(I,1,bi,bj)
            DO J=2,sNy
             JM=J-1
             CVV(I,J)=CVV(I,J)/(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
             VRT(I,J)=(VRT(I,J)-AV(I,J,bi,bj)*VRT(I,JM))
     &            /(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
            ENDDO
            DO J=1,sNy-1
             J1=sNy-J
             J2=J1+1
             VRT(I,J1)=VRT(I,J1)-CVV(I,J1)*VRT(I,J2)
            ENDDO
            DO J=1,sNy
             vIce(I,J,bi,bj)=vTmp(I,J,bi,bj)
     &           +WFAV*(VRT(I,J)-vTmp(I,J,bi,bj))
            ENDDO
           ENDDO
C--   end doIterate4v
          ENDIF

C     end bi,bj-loops
         ENDDO
        ENDDO

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
c    &   ' U iters,error,WF = ',ilcall,M,S1,S1A,WFAU
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

      IF ( printResidual ) THEN
C--   Calculate final residual of the linearised system
        CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, uIce, vIce,
     U                  massU, massV,
     O                  residUend, residVend, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
        _BEGIN_MASTER( myThid )
        WRITE(standardMessageUnit,'(A,1X,I4,1P3E16.8)')
     &    ' SEAICE_LSR: Residual Initial ipass,Uice,Vice=',
     &        ilcall, residUini, residVini
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
     &    ' SEAICE_LSR (ipass=',ilcall,') iters,dU,Resid=',
     &      ICOUNT1, S1, residUend
        WRITE(standardMessageUnit,'(A,I4,A,I6,1P2E16.8)')
     &    ' SEAICE_LSR (ipass=',ilcall,') iters,dV,Resid=',
     &      ICOUNT2, S2, residVend
        _END_MASTER( myThid )
      ENDIF
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &        'Uice post iter (SEAICE_LSR', MOD(ilcall,1000), ')'
        CALL DEBUG_STATS_RL( 1, UICE, msgBuf, myThid )
        WRITE(msgBuf,'(A,I3,A)')
     &        'Vice post iter (SEAICE_LSR', MOD(ilcall,1000), ')'
        CALL DEBUG_STATS_RL( 1, VICE, msgBuf, myThid )
      ENDIF
#endif /* ALLOW_DEBUG */
      IF ( doIterate4u .OR. doIterate4v ) THEN
        WRITE(msgBuf,'(2A,I10,A,I4,A)') '** WARNING ** SEAICE_LSR ',
     &    '(it=', myIter, ',', ilcall,') did not converge :'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2(A,I6,0PF6.3,1PE14.6))')
     &      ' nIt,wFU,dU=', ICOUNT1, WFAU, S1,
     &    ' ; nIt,wFV,dV=', ICOUNT2, WFAV, S2
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

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
     &      ilcall, sqrt(resnorm), EKnorm
       _END_MASTER( myThid )
      ENDIF
#endif /* SEAICE_ALLOW_CHECK_LSR_CONVERGENCE */

      ENDIF
C     end outer ilcall pseudo-time stepping loop
      ENDDO

      IF ( useHB87StressCoupling ) THEN
C     compute the divergence of stress here to be used later
C
C     compute strain rate from latest velocities
       CALL SEAICE_CALC_STRAINRATES(
     I       uIce, vIce,
     O       e11, e22, e12,
     I       3, myTime, myIter, myThid )
C     compute internal stresses with updated ice velocities
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           sig11(I,J) = 0. _d 0
           sig22(I,J) = 0. _d 0
           sig12(I,J) = 0. _d 0
          ENDDO
         ENDDO

         DO j=0,sNy
          DO i=0,sNx
           eplus = e11(I,J,bi,bj) + e22(I,J,bi,bj)
           eminus= e11(I,J,bi,bj) - e22(I,J,bi,bj)
           sig11(I,J) = zeta(I,J,bi,bj)*eplus + eta(I,J,bi,bj)*eminus
     &          - 0.5 _d 0 * PRESS(I,J,bi,bj)
           sig22(I,J) = zeta(I,J,bi,bj)*eplus - eta(I,J,bi,bj)*eminus
     &          - 0.5 _d 0 * PRESS(I,J,bi,bj)
          ENDDO
         ENDDO

         DO j=1,sNy+1
          DO i=1,sNx+1
           sig12(I,J) = 2. _d 0 * e12(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
          ENDDO
         ENDDO
C     evaluate divergence of stress and apply to forcing
         DO J=1,sNy
          DO I=1,sNx
           stressDivergenceX(I,J,bi,bj) =
     &          ( sig11(I  ,J  ) * _dyF(I  ,J  ,bi,bj)
     &          - sig11(I-1,J  ) * _dyF(I-1,J  ,bi,bj)
     &          + sig12(I  ,J+1) * _dxV(I  ,J+1,bi,bj)
     &          - sig12(I  ,J  ) * _dxV(I  ,J  ,bi,bj)
     &          ) * recip_rAw(I,J,bi,bj)
            stressDivergenceY(I,J,bi,bj) =
     &          ( sig22(I  ,J  ) * _dxF(I  ,J  ,bi,bj)
     &          - sig22(I  ,J-1) * _dxF(I  ,J-1,bi,bj)
     &          + sig12(I+1,J  ) * _dyU(I+1,J  ,bi,bj)
     &          - sig12(I  ,J  ) * _dyU(I  ,J  ,bi,bj)
     &          ) * recip_rAs(I,J,bi,bj)
           ENDDO
          ENDDO
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
     U                  massU, massV,
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
C-    Integrated seaice mass (input-output)
      _RL massU, massV
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
      LOGICAL sumMass

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Calculate residual of the linearised system
#ifdef ALLOW_AUTODIFF_TAMC
      sumMass = calcMeanResid
      massU = 0.
      massV = 0.
#else /* ALLOW_AUTODIFF_TAMC */
      sumMass = calcMeanResid .AND. ( massU.LT.0. .OR. massV.LT.0. )
      IF ( sumMass ) THEN
       massU = 0.
       massV = 0.
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */
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
        IF ( sumMass ) THEN
         DO j=1,sNy
          DO i=1,sNx
            massU  = massU  + seaiceMassU(i,j,bi,bj)
     &                       *rAw(i,j,bi,bj)*maskInW(i,j,bi,bj)
            massV  = massV  + seaiceMassV(i,j,bi,bj)
     &                       *rAs(i,j,bi,bj)*maskInS(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
        IF ( calcMeanResid ) THEN
         DO j=1,sNy
          DO i=1,sNx
            residU = residU + uRes(i,j,bi,bj)*uRes(i,j,bi,bj)
     &                       *seaiceMassU(i,j,bi,bj)
     &                       *rAw(i,j,bi,bj)*maskInW(i,j,bi,bj)
            residV = residV + vRes(i,j,bi,bj)*vRes(i,j,bi,bj)
     &                       *seaiceMassV(i,j,bi,bj)
     &                       *rAs(i,j,bi,bj)*maskInS(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO
      IF ( sumMass ) THEN
        _GLOBAL_SUM_RL( massU, myThid )
        _GLOBAL_SUM_RL( massV, myThid )
      ENDIF
      IF ( calcMeanResid ) THEN
        _GLOBAL_SUM_RL( residU, myThid )
        _GLOBAL_SUM_RL( residV, myThid )
        IF ( massU.GT.0. ) residU = residU / massU
        IF ( massV.GT.0. ) residV = residV / massV
        IF ( residU.GT.0. ) residU = SQRT(residU)
        IF ( residV.GT.0. ) residV = SQRT(residV)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */

      RETURN
      END
