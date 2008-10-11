C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_diagnostics_state.F,v 1.7 2008/10/11 20:37:05 heimbach Exp $
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
      _RL e11  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL press(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sig1, sig2, sig12, sigTmp, recip_prs
C
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

      IF ( useDiagnostics ) THEN

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL DIAGNOSTICS_FILL(AREA(1-oLx,1-oLy,1,bi,bj),
     &        'SIarea  ',0,1 ,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(HEFF(1-oLx,1-oLy,1,bi,bj),
     &        'SIheff  ',0,1 ,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(UICE(1-oLx,1-oLy,1,bi,bj),
     &        'SIuice  ',0,1 ,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(VICE(1-oLx,1-oLy,1,bi,bj),
     &        'SIvice  ',0,1 ,2,bi,bj,myThid)
        ENDDO
       ENDDO
C
       CALL DIAGNOSTICS_FILL(HSNOW  ,'SIhsnow ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(frWtrAtm,'SIatmFW ',0,1 ,0,1,1,myThid)
C
#ifdef SEAICE_SALINITY
       CALL DIAGNOSTICS_FILL(HSALT  ,'SIhsalt ',0,1 ,0,1,1,myThid)
#endif
C
#ifdef SEAICE_CGRID
       IF ( DIAGNOSTICS_IS_ON('SIsigI  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIsigII ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIpress ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIzeta  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIeta   ',myThid) ) THEN
C     Unfortunately, for this we have to recompute a few things:
C     compute strainrates, "viscosities" and modified pressure
        CALL SEAICE_CALC_STRAINRATES(
     I       uIce, vIce,
     O       e11, e22, e12,
     I       3, 0, myTime, myIter, myThid )

        CALL SEAICE_CALC_VISCOSITIES(
     I       e11, e22, e12, zMin, zMax, hEffM, press0,
     O       eta, zeta, press,
     I       0, myTime, myIter, myThid )
C
        CALL DIAGNOSTICS_FILL(zeta   ,'SIzeta  ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(eta    ,'SIeta   ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(press  ,'SIpress ',0,1 ,0,1,1,myThid)
       ENDIF

       IF ( DIAGNOSTICS_IS_ON('SIsigI  ',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SIsigII ',myThid) ) THEN
C     principle stress components
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            sig1  = 2.*zeta(I,J,bi,bj)
     &           * (e11(I,J,bi,bj) + e22(I,J,bi,bj))
     &           - press(I,J,bi,bj)
            sig2  = 2.* eta(I,J,bi,bj)
     &           * (e11(I,J,bi,bj) - e22(I,J,bi,bj))
            sig12 = 2.*eta(I,J,bi,bj) * 0.25 _d 0 *
     &           ( e12(I  ,J  ,bi,bj) + e12(I+1,J  ,bi,bj)
     &           + e12(I  ,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
            sigTmp = SQRT( sig2*sig2 + 4. _d 0*sig12*sig12 )
            recip_prs = 0. _d 0
            IF ( press(I,J,  bi,bj) .GT. 1. _d -13 )
     &           recip_prs = 1./press(I,J,bi,bj)
            sigI (I,J) = 0.5*(sig1 + sigTmp)*recip_prs
            sigII(I,J) = 0.5*(sig1 - sigTmp)*recip_prs
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(sigI ,'SIsigI  ',0,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(sigII,'SIsigII ',0,1,2,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDIF
#endif /* SEAICE_CGRID */
C     abuse press as a temporary field
        IF ( DIAGNOSTICS_IS_ON('SIuheff ',myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx+1
             press(i,j,bi,bj) = UICE(i,j,1,bi,bj)*0.5 _d 0
     &            *(HEFF(i,j,1,bi,bj)+HEFF(i-1,j,1,bi,bj))
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
             press(i,j,bi,bj) = VICE(i,j,1,bi,bj)*0.5 _d 0
     &            *(HEFF(i,j,1,bi,bj)+HEFF(i,j-1,1,bi,bj))
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
