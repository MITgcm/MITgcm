#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_DIAGNOSTICS_STATE
C     !INTERFACE:
      SUBROUTINE SEAICE_DIAGNOSTICS_STATE(
     I                      seqFlag, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  SEAICE_DIAGNOSTICS_STATE
C     | o fill-in diagnostics array for SEAICE state variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: thread number for this instance of the routine.
C     seqFlag :: flag that indicate where this S/R is called from:
C             :: =0 called from the beginning of forward_step
C             :: =1 called from the middle of forward_step
C             :: =2 called from the end of forward_step
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER seqFlag
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     == Local variables ==
C     bi,bj   :: tile indices
      INTEGER i,j,bi,bj
      _RL tIce (1-oLx:sNx+oLx,1-oLy:sNy+oLy,nSx,nSy)
#ifdef SEAICE_CGRID
      _RL sig1 (1:sNx,1:sNy)
      _RL sig2 (1:sNx,1:sNy)
      _RL sig11(1-oLx:sNx+oLx,1-oLy:sNy+oLy)
      _RL sig22(1-oLx:sNx+oLx,1-oLy:sNy+oLy)
      _RL sig12(1-oLx:sNx+oLx,1-oLy:sNy+oLy)
      _RL sigp, sigm, sigTmp, recip_prs
#endif
      INTEGER k
      _RL recip_multDim
      _RL tmp
#ifdef ALLOW_SITRACER
      INTEGER iTracer
      CHARACTER*8   diagName
#endif

      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

      IF ( useDiagnostics .AND. seqFlag .EQ. 0 ) THEN
       CALL DIAGNOSTICS_FILL( AREA, 'SIarea  ', 0, 1, 0, 1,1, myThid )
       CALL DIAGNOSTICS_FILL( HEFF, 'SIheff  ', 0, 1, 0, 1,1, myThid )
       CALL DIAGNOSTICS_FILL( UICE, 'SIuice  ', 0, 1, 0, 1,1, myThid )
       CALL DIAGNOSTICS_FILL( VICE, 'SIvice  ', 0, 1, 0, 1,1, myThid )

       IF ( DIAGNOSTICS_IS_ON('SItices ',myThid) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C     use tIce as a temporary field, as it is done in seaice_growth
          DO j=1,sNy
           DO i=1,sNx
            tIce(I,J,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
C     division by zero is not possible
          recip_multDim = 1. _d 0/SEAICE_multDim
          DO k=1,SEAICE_multDim
           DO j=1,sNy
            DO i=1,sNx
             tmp = 1. _d 0
#ifdef SEAICE_ITD
             IF (AREA(I,J,bi,bj) .GT. ZERO)
     &            tmp=AREAITD(I,J,K,bi,bj)/AREA(I,J,bi,bj)
#endif /* SEAICE_ITD */
             tIce(I,J,bi,bj) = tIce(I,J,bi,bj)
     &            + TICES(I,J,K,bi,bj)*tmp*recip_multDim
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   tIce, AREA, 1. _d 0, 1, 'SItices ',
     I                   0, 1, 0, 1, 1, myThid )
       ENDIF

       CALL DIAGNOSTICS_FILL(HSNOW  ,'SIhsnow ',0,1 ,0,1,1,myThid)

#ifdef SEAICE_ITD
       CALL DIAGNOSTICS_FILL(HEFFITD ,'SIheffN ',0,nITD ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(AREAITD ,'SIareaN ',0,nITD ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(HSNOWITD,'SIhsnowN',0,nITD ,0,1,1,myThid)
#endif

#ifdef ALLOW_SITRACER
       DO iTracer = 1, SItrNumInUse
        WRITE(diagName,'(A4,I2.2,A2)') 'SItr',iTracer,'  '
        IF (SItrMate(iTracer).EQ.'HEFF') THEN
         CALL DIAGNOSTICS_FRACT_FILL(
     I        SItracer(1-OLx,1-OLy,1,1,iTracer), HEFF,
     I        ONE, 1, diagName, 0, 1, 0, 1, 1, myThid )
        ELSE
         CALL DIAGNOSTICS_FRACT_FILL(
     I        SItracer(1-OLx,1-OLy,1,1,iTracer), AREA,
     I        ONE, 1, diagName, 0, 1, 0, 1, 1, myThid )
        ENDIF
       ENDDO
#endif
#ifdef SEAICE_VARIABLE_SALINITY
       CALL DIAGNOSTICS_FILL(HSALT  ,'SIhsalt ',0,1 ,0,1,1,myThid)
#endif
C
       IF ( DIAGNOSTICS_IS_ON('SIuheff ',myThid) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy
           DO i = 1,sNx+1
            tIce(i,j,bi,bj) =
#ifdef SEAICE_CGRID
     &           UICE(i,j,bi,bj)
#else
C     average B-grid velocities to C-grid points
     &           0.5 _d 0*(UICE(i,j,bi,bj)+UICE(i,j+1,bi,bj))
#endif /* SEAICE_CGRID */
     &           *0.5 _d 0*(HEFF(i,j,bi,bj)+HEFF(i-1,j,bi,bj))
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tIce,'SIuheff ',0,1,0,1,1,myThid)
       ENDIF
       IF ( DIAGNOSTICS_IS_ON('SIvheff ',myThid) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy+1
           DO i = 1,sNx
            tIce(i,j,bi,bj) =
#ifdef SEAICE_CGRID
     &           VICE(i,j,bi,bj)
#else
C     average B-grid velocities to C-grid points
     &           0.5 _d 0*(VICE(i,j,bi,bj)+VICE(i+1,j,bi,bj))
#endif /* SEAICE_CGRID */
     &           *0.5 _d 0*(HEFF(i,j,bi,bj)+HEFF(i,j-1,bi,bj))
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tIce,'SIvheff ',0,1,0,1,1,myThid)
       ENDIF
C     allowDiagnostics and seqFlag = 0
      ENDIF
C
C     derived diagnostics require a full state that will only be availabe
C     after seaice_model has been called
C
      IF ( useDiagnostics .AND. seqFlag .EQ. 1 ) THEN
       CALL DIAGNOSTICS_FILL(zeta   ,'SIzeta  ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(eta    ,'SIeta   ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(press  ,'SIpress ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(deltaC ,'SIdelta ',0,1 ,0,1,1,myThid)
#ifdef SEAICE_CGRID
       IF ( DIAGNOSTICS_IS_ON('SItensil',myThid) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C     use tIce as a temporary field
          DO j=1,sNy
           DO i=1,sNx
            IF ( tensileStrFac(I,J,bi,bj) .EQ. 1. _d 0) THEN
C     This special case of tensile strength equal to compressive strength
C     is not very physical and should actually not happen but you never know;
C     in this case, press = P-T = P*(1-k) = 0. and we have to use press0 to
C     get something
             tIce(I,J,bi,bj) = press0(I,J,bi,bj)
            ELSE
C     This is more complicated than you think because press = P-T = P*(1-k),
C     but we are looking for T = k*P = k*press/(1-k)
             tIce(I,J,bi,bj) = tensileStrFac(I,J,bi,bj)
     &            *press(I,J,bi,bj)/(1. _d 0 - tensileStrFac(I,J,bi,bj))
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tIce,'SItensil',0,1 ,0,1,1,myThid)
       ENDIF
       IF ( DIAGNOSTICS_IS_ON('SIsig1  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIsig2  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIshear ',myThid) ) THEN
#ifdef SEAICE_ALLOW_EVP
        IF ( SEAICEuseEVP ) THEN
C     for EVP compute principle stress components from recent
C     stress state and normalize with latest
C     PRESS = PRESS(n-1), n = number of sub-cycling steps
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             sigp = seaice_sigma1(I,J,bi,bj)
             sigm = seaice_sigma2(I,J,bi,bj)
             sig12(I,J) = 0.25 _d 0 *
     &            ( seaice_sigma12(I,  J,  bi,bj)
     &            + seaice_sigma12(I+1,J,  bi,bj)
     &            + seaice_sigma12(I+1,J+1,bi,bj)
     &            + seaice_sigma12(I  ,J+1,bi,bj) )
             sigTmp = SQRT( sigm*sigm + 4. _d 0*sig12(I,J)*sig12(I,J) )
             recip_prs = 0. _d 0
             IF ( press0(I,J,bi,bj) .GT. 1. _d -13 )
     &            recip_prs = 1./press0(I,J,bi,bj)
             sig1(I,J) = 0.5*(sigp + sigTmp)*recip_prs
             sig2(I,J) = 0.5*(sigp - sigTmp)*recip_prs
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(sig1,'SIsig1  ',0,1,3,bi,bj,myThid)
           CALL DIAGNOSTICS_FILL(sig2,'SIsig2  ',0,1,3,bi,bj,myThid)
          ENDDO
         ENDDO
        ELSE
#else
        IF ( .TRUE. ) THEN
#endif /* SEAICE_ALLOW_EVP */
C     recompute strainrates from up-to-date velocities
         CALL SEAICE_CALC_STRAINRATES(
     I        uIce, vIce,
     O        e11, e22, e12,
     I        0, myTime, myIter, myThid )
CML      CALL SEAICE_CALC_VISCOSITIES(
CML     I     e11, e22, e12, zMin, zMax, HEFFM, press0, tensileStrFac,
CML     O     eta, etaZ, zeta, zetaZ, press, deltaC,
CML     I     0, myTime, myIter, myThid )
C     but use old viscosities and pressure for the
C     principle stress components
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           CALL SEAICE_CALC_STRESS(
     I          e11, e22, e12, press, zeta, eta, etaZ,
     O          sig11, sig22, sig12,
     I          bi, bj, myTime, myIter, myThid )
           DO j=1,sNy
            DO i=1,sNx
             sigp   = sig11(I,J) + sig22(I,J)
             sigm   = sig11(I,J) - sig22(I,J)
C     This should be the way of computing sig12 at C-points,
C            sigTmp = 0.25 _d 0 *
C    &            ( sig12(I  ,J  ) + sig12(I+1,J  )
C    &            + sig12(I  ,J+1) + sig12(I+1,J+1) )
C     but sig12 = 2*etaZ*e12, and because of strong gradients in eta,
C     etaZ can be very large for a cell with small eta and the straightforward
C     way of averaging mixes large etaZ with small press0, so we have to do it
C     in different way to get meaningfull sig12C (=sigTmp):
             sigTmp = 2.*eta(I,J,bi,bj) * 0.25 _d 0 *
     &            (e12(I,J,bi,bj) + e12(I+1,J,bi,bj)
     &            +e12(I,J+1,bi,bj)+e12(I+1,J+1,bi,bj))
             sigTmp = SQRT( sigm*sigm + 4.*sigTmp*sigTmp )
             recip_prs = 0. _d 0
             IF ( press0(I,J,bi,bj) .GT. 1. _d -13 )
     &            recip_prs = 1./press0(I,J,bi,bj)
             sig1(I,J) = 0.5*(sigp + sigTmp)*recip_prs
             sig2(I,J) = 0.5*(sigp - sigTmp)*recip_prs
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(sig1,'SIsig1  ',0,1,3,bi,bj,myThid)
           CALL DIAGNOSTICS_FILL(sig2,'SIsig2  ',0,1,3,bi,bj,myThid)
          ENDDO
         ENDDO
C     endif SEAICEuseEVP
        ENDIF
        IF ( DIAGNOSTICS_IS_ON('SIshear ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             sigm = e11(I,J,bi,bj) - e22(I,J,bi,bj)
             sigTmp =
     &            ( e12(I,  J,  bi,bj)**2 + e12(I+1,J,  bi,bj)**2
     &            + e12(I+1,J+1,bi,bj)**2 + e12(I  ,J+1,bi,bj)**2 )
C     shear deformation as sqrt((e11-e22)**2 + 4*e12**2); the 4 pulled into
C     the average
             sig1(I,J) = sqrt(sigm*sigm + sigTmp)
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(sig1,'SIshear ',0,1,3,bi,bj,myThid)
          ENDDO
         ENDDO
        ENDIF
C     endif DIAGNOSTICS_IS_ON(SIsig1/2)
       ENDIF
C
C     Sea ice energy diagnostics
C
       IF ( ( DIAGNOSTICS_IS_ON('SIenpot ',myThid) .OR.
     &        DIAGNOSTICS_IS_ON('SIpRfric',myThid) .OR.
     &        DIAGNOSTICS_IS_ON('SIpSfric',myThid) ) .AND. .NOT.
     &      ( DIAGNOSTICS_IS_ON('SIsig1  ',myThid) .OR.
     &        DIAGNOSTICS_IS_ON('SIsig2  ',myThid) .OR.
     &        DIAGNOSTICS_IS_ON('SIshear ',myThid) )
     &      ) THEN
C     recompute strainrates from up-to-date velocities
C     if we have not yet done so
        CALL SEAICE_CALC_STRAINRATES(
     I       uIce, vIce,
     O       e11, e22, e12,
     I       0, myTime, myIter, myThid )
       ENDIF

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         IF ( DIAGNOSTICS_IS_ON('SIenpi  ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            sig1(I,J)  = 0.5 _d 0 * (
     &            UICE(I,  J,bi,bj)*stressDivergenceX(I,  J,bi,bj)
     &           +UICE(I+1,J,bi,bj)*stressDivergenceX(I+1,J,bi,bj)
     &           +VICE(I,J,  bi,bj)*stressDivergenceY(I,J,  bi,bj)
     &           +VICE(I,J+1,bi,bj)*stressDivergenceY(I,J+1,bi,bj) )
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(sig1,'SIenpi  ',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIenpot ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            sig1(I,J) = 0.5 _d 0 * press0(I,J,bi,bj) *
     &           ( e11(I,J,bi,bj) + e22(I,J,bi,bj) )
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(sig1,'SIenpot ',0,1,3,bi,bj,myThid)
        ENDIF
        IF ( DIAGNOSTICS_IS_ON('SIpRfric',myThid) ) THEN
         DO J=1,sNy
          DO I=1,sNx
           sig1(I,J) = - zeta(I,J,bi,bj)
     &          * ( e11(I,J,bi,bj) + e22(I,J,bi,bj) )
     &          * ( e11(I,J,bi,bj) + e22(I,J,bi,bj) )
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(sig1,'SIpRfric',0,1,3,bi,bj,myThid)
        ENDIF
        IF ( DIAGNOSTICS_IS_ON('SIpSfric',myThid) ) THEN
         DO J=1,sNy
          DO I=1,sNx
           sig1(I,J) = - eta(I,J,bi,bj)
     &          * ( (e11(I,J,bi,bj) - e22(I,J,bi,bj))**2 +
     &            ( e12(I,  J,  bi,bj)**2 + e12(I+1,J,  bi,bj)**2
     &            + e12(I+1,J+1,bi,bj)**2 + e12(I  ,J+1,bi,bj)**2 )
     &          )
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(sig1,'SIpSfric',0,1,3,bi,bj,myThid)
        ENDIF
        ENDDO
       ENDDO
#endif /* SEAICE_CGRID */
C
C     endif useDiagnostics
      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
