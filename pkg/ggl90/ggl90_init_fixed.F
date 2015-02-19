C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_init_fixed.F,v 1.5 2015/02/19 15:44:12 mlosch Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: GGL90_INIT_FIXED
C !INTERFACE:
      SUBROUTINE GGL90_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | S/R GGL90_INIT_FIXED
C     | Initialize GGL90 variables that are kept fixed during the run.
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_GGL90_IDEMIX
# include "GRID.h"
#endif /*  ALLOW_GGL90_IDEMIX */
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
#if defined ALLOW_GGL90_SMOOTH || defined ALLOW_GGL90_IDEMIX
      INTEGER i, j, bi, bj
#endif
#ifdef ALLOW_GGL90_IDEMIX
      INTEGER k, km1
#endif
CEOP

C#ifdef ALLOW_MNC
C      IF (useMNC) THEN
C        CALL GGL90_MNC_INIT( myThid )
C      ENDIF
C#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL GGL90_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_GGL90_SMOOTH
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          mskCor(i,j,bi,bj) = 1. _d 0
         ENDDO
        ENDDO
        IF ( useCubedSphereExchange ) THEN
          CALL FILL_CS_CORNER_TR_RL( 0, .FALSE.,
     U                               mskCor(1-OLx,1-OLy,bi,bj),
     I                               bi,bj, myThid )
        ENDIF
       ENDDO
      ENDDO
#endif

#ifdef ALLOW_GGL90_IDEMIX
C
C     hfacs for w (=tke) control volume
C
C     this is likely to move because it may vary with r*-coordinates
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         Km1 = MAX(K-1,1)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           hFacI(i,j,k,bi,bj) = 
     &          MIN(.5 _d 0,_hFacC(i,j,km1,bi,bj) ) +
     &          MIN(.5 _d 0,_hFacC(i,j,k  ,bi,bj) ) 
           recip_hFacI(I,J,K,bi,bj)=0. _d 0
           IF ( hFacI(I,J,K,bi,bj) .NE. 0. _d 0 )
     &        recip_hFacI(I,J,K,bi,bj)=1. _d 0/hFacI(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_GGL90_IDEMIX */

      RETURN
      END
