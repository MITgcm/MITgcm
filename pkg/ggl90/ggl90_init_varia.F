C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_init_varia.F,v 1.6 2010/08/09 20:34:03 gforget Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_INIT_VARIA( myThid )

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
      INTEGER I, J, K
c     INTEGER Km1
CEOP

#ifdef ALLOW_GGL90

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,Nr
c        Km1 = MAX(K-1,1)
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           GGL90viscArU(I,J,K,bi,bj) = 0. _d 0
           GGL90viscArV(I,J,K,bi,bj) = 0. _d 0
           GGL90diffKr(I,J,K,bi,bj)  = 0. _d 0
CML           GGL90TKE(I,J,K,bi,bj)=0. _d 0
           GGL90TKE(I,J,K,bi,bj)=GGL90TKEmin*maskC(I,J,K,bi,bj)
C
C     hfacs for w (=tke) control volume
C
c         recip_hFacI(I,J,K,bi,bj) = .5 _d 0*hFacC(I,J,Km1,bi,bj)
c     &        + MIN(.5 _d 0,hFacC(I,J,K,bi,bj))
c         IF ( recip_hFacI(I,J,K,bi,bj) .NE. 0. _d 0 )
c     &        recip_hFacI(I,J,K,bi,bj)=1. _d 0/recip_hFacI(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays

       ENDDO
      ENDDO

      IF ( nIter0.NE.0 .OR. pickupSuff.NE.' ' ) THEN
       CALL GGL90_READ_PICKUP( nIter0, myThid )
      ELSE
C--   Read initial turbulent kinetic energy thickness from file if available.
       IF ( GGL90TKEFile .NE. ' ' ) THEN
        CALL READ_FLD_XYZ_RL( GGL90TKEFile, ' ', GGL90TKE, 0, myThid )
        _EXCH_XYZ_RL(GGL90TKE,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr
           DO J=1-OLy,sNy+OLy
            DO I=1-OLx,sNx+OLx
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
