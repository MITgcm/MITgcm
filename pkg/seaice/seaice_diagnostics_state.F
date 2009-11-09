C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_diagnostics_state.F,v 1.17 2009/11/09 16:13:44 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_DIAGNOSTICS_STATE
C     !INTERFACE:
      SUBROUTINE SEAICE_DIAGNOSTICS_STATE(
     I                      myTime, myIter, myThid )
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
#include "FFIELDS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     bi,bj   :: tile indices
C     myThid  :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     == Local variables ==
      INTEGER i,j
      INTEGER bi,bj
      _RL sigI (1-oLx:sNx+oLx,1-oLy:sNy+oLy)
      _RL sigII(1-oLx:sNx+oLx,1-oLy:sNy+oLy)
      _RL sig1, sig2, sig12, sigTmp, recip_prs
#ifdef SEAICE_MULTICATEGORY
      INTEGER k
      _RL recip_multdim
#endif 
C
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

      IF ( useDiagnostics ) THEN

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL DIAGNOSTICS_FILL(AREA,
     &        'SIarea  ',0,1 ,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(HEFF,
     &        'SIheff  ',0,1 ,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(UICE,
     &        'SIuice  ',0,1 ,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(VICE,
     &        'SIvice  ',0,1 ,2,bi,bj,myThid)
        ENDDO
       ENDDO
C     
       IF ( DIAGNOSTICS_IS_ON('SItices ',myThid) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef SEAICE_MULTICATEGORY
C     use TICE as a temporary field, as it is done in seaice_growth
          DO j=1,sNy
           DO i=1,sNx
            TICE(I,J,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
C     division by zero is not possible
          recip_multdim = 1. _d 0/MULTDIM
          DO k=1,MULTDIM
           DO j=1,sNy
            DO i=1,sNx
             TICE(I,J,bi,bj) = TICE(I,J,bi,bj) 
     &            + TICES(I,J,K,bi,bj)*recip_multdim
            ENDDO
           ENDDO
          ENDDO
#endif /* SEAICE_MULTICATEGORY */
          CALL DIAGNOSTICS_FRACT_FILL(
     I         TICE, AREA, 1. _d 0,1,'SItices ', 
     I         0,1,1,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDIF
C
       CALL DIAGNOSTICS_FILL(HSNOW  ,'SIhsnow ',0,1 ,0,1,1,myThid)
C
#ifdef SEAICE_SALINITY
       CALL DIAGNOSTICS_FILL(HSALT  ,'SIhsalt ',0,1 ,0,1,1,myThid)
#endif
#ifdef SEAICE_AGE
       CALL DIAGNOSTICS_FILL(IceAge ,'SIage   ',0,1 ,0,1,1,myThid)
#endif
C
       CALL DIAGNOSTICS_FILL(zeta   ,'SIzeta  ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(eta    ,'SIeta   ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(press  ,'SIpress ',0,1 ,0,1,1,myThid)
#ifdef SEAICE_CGRID
       IF ( DIAGNOSTICS_IS_ON('SIsigI  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIsigII ',myThid) ) THEN
#ifdef SEAICE_ALLOW_EVP
        IF ( SEAICEuseEVP ) THEN
C     for EVP compute principle stress components from recent 
C     stress state and normalize with latest 
C     PRESS = PRESS(n-1), n = number of sub-cycling steps
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             sig1 = seaice_sigma1(I,J,bi,bj)
             sig2 = seaice_sigma2(I,J,bi,bj)
             sig12 = 0.25 _d 0 *
     &            ( seaice_sigma12(I,  J,  bi,bj)
     &            + seaice_sigma12(I+1,J,  bi,bj)
     &            + seaice_sigma12(I+1,J+1,bi,bj)
     &            + seaice_sigma12(I  ,J+1,bi,bj) )
             sigTmp = SQRT( sig2*sig2 + 4. _d 0*sig12*sig12 )
             recip_prs = 0. _d 0
             IF ( press(I,J,bi,bj) .GT. 1. _d -13 )
     &            recip_prs = 1./press(I,J,bi,bj)
             sigI (I,J) = 0.5*(sig1 + sigTmp)*recip_prs
             sigII(I,J) = 0.5*(sig1 - sigTmp)*recip_prs
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(sigI ,'SIsigI  ',0,1,2,bi,bj,myThid)
           CALL DIAGNOSTICS_FILL(sigII,'SIsigII ',0,1,2,bi,bj,myThid)
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
C     but use old viscosities and pressure for the
C     principle stress components
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             sig1  = 2.*zeta(I,J,bi,bj)
     &            * (e11(I,J,bi,bj) + e22(I,J,bi,bj))
     &            - press(I,J,bi,bj)
             sig2  = 2.* eta(I,J,bi,bj)
     &            * (e11(I,J,bi,bj) - e22(I,J,bi,bj))
             sig12 = 2.*eta(I,J,bi,bj) * 0.25 _d 0 *
     &            ( e12(I  ,J  ,bi,bj) + e12(I+1,J  ,bi,bj)
     &            + e12(I  ,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
             sigTmp = SQRT( sig2*sig2 + 4. _d 0*sig12*sig12 )
             recip_prs = 0. _d 0
             IF ( press(I,J,bi,bj) .GT. 1. _d -13 )
     &            recip_prs = 1./press(I,J,bi,bj)
             sigI (I,J) = 0.5*(sig1 + sigTmp)*recip_prs
             sigII(I,J) = 0.5*(sig1 - sigTmp)*recip_prs
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(sigI ,'SIsigI  ',0,1,2,bi,bj,myThid)
           CALL DIAGNOSTICS_FILL(sigII,'SIsigII ',0,1,2,bi,bj,myThid)
          ENDDO
         ENDDO
C     endif SEAICEuseEVP
        ENDIF
C     endif DIAGNOSTICS_IS_ON(SIsigI/II)
       ENDIF
#endif /* SEAICE_CGRID */
C     abuse press as a temporary field
        IF ( DIAGNOSTICS_IS_ON('SIuheff ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx+1
             press(i,j,bi,bj) =
#ifdef SEAICE_CGRID
     &            UICE(i,j,bi,bj)
#else
C     average B-grid velocities to C-grid points
     &            0.5 _d 0*(UICE(i,j,bi,bj)+UICE(i,j+1,bi,bj))
#endif /* SEAICE_CGRID */
     &            *0.5 _d 0*(HEFF(i,j,bi,bj)+HEFF(i-1,j,bi,bj))
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(press,'SIuheff ',0,1,0,1,1,myThid)
        ENDIF
        IF ( DIAGNOSTICS_IS_ON('SIvheff ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy+1
            DO i = 1,sNx
             press(i,j,bi,bj) =
#ifdef SEAICE_CGRID
     &            VICE(i,j,bi,bj)
#else
C     average B-grid velocities to C-grid points
     &            0.5 _d 0*(VICE(i,j,bi,bj)+VICE(i+1,j,bi,bj))
#endif /* SEAICE_CGRID */
     &            *0.5 _d 0*(HEFF(i,j,bi,bj)+HEFF(i,j-1,bi,bj))
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(press,'SIvheff ',0,1,0,1,1,myThid)
        ENDIF
C     endif useDiagnostics
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
