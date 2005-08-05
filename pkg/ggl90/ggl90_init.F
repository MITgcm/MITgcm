C $Header: /u/gcmpack/MITgcm/pkg/ggl90/Attic/ggl90_init.F,v 1.3 2005/08/05 23:46:09 ce107 Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_INIT( myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GGL90.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of GGL90_INIT
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K, Km1
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec
CEOP

#ifdef ALLOW_GGL90

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,Nr
         Km1 = MAX(K-1,1)
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
CML           GGL90TKE(I,J,K,bi,bj)=0. _d 0
           GGL90TKE(I,J,K,bi,bj)=GGL90TKEmin*maskC(I,J,K,bi,bj)
C
C     hfacs for w (=tke) control volume
C     
         recip_hFacI(I,J,K,bi,bj) = .5*hFacC(I,J,Km1,bi,bj)
     &        + MIN(.5 _d 0,hFacC(I,J,K,bi,bj))
         IF ( recip_hFacI(I,J,K,bi,bj) .NE. 0. _d 0 )
     &        recip_hFacI(I,J,K,bi,bj) = 1./recip_hFacI(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays

       ENDDO
      ENDDO

      IF ( nIter0.NE.0 ) THEN
       CALL GGL90_READ_CHECKPOINT( nIter0, myThid )
      ELSE
C--   Read initial turbulent kinetic energy thickness from file if available.
       IF ( GGL90TKEFile .NE. ' ' ) THEN
        _BEGIN_MASTER( myThid )
        CALL READ_FLD_XYZ_RL( GGL90TKEFile, ' ', GGL90TKE, 0, myThid )
        _END_MASTER(myThid)
        _EXCH_XYZ_R8(GGL90TKE,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             GGL90TKE(I,J,K,bi,bj) = MAX(GGL90TKE(I,J,K,bi,bj),
     &            GGL90TKEmin)*maskC(I,J,K,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
      ENDIF	

#endif /*  ALLOW_GGL90  */

      RETURN
      END
