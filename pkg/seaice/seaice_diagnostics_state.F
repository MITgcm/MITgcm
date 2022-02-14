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
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     == Local variables ==
C     bi,bj   :: tile indices
      INTEGER i,j,bi,bj
      _RL tIce (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER k
      _RL recip_multDim
      _RL tmp
#ifdef ALLOW_SITRACER
      INTEGER iTracer
      CHARACTER*8   diagName
#endif

      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

      IF ( useDiagnostics ) THEN
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
            tIce(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
C     division by zero is not possible
          recip_multDim = 1. _d 0/SEAICE_multDim
          DO k=1,SEAICE_multDim
           DO j=1,sNy
            DO i=1,sNx
             tmp = 1. _d 0
#ifdef SEAICE_ITD
             IF (AREA(i,j,bi,bj) .GT. ZERO)
     &            tmp=AREAITD(i,j,K,bi,bj)/AREA(i,j,bi,bj)
#endif /* SEAICE_ITD */
             tIce(i,j,bi,bj) = tIce(i,j,bi,bj)
     &            + TICES(i,j,K,bi,bj)*tmp*recip_multDim
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
C
C     endif useDiagnostics
      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
