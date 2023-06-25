#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE GGL90_INIT_VARIA( myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE GGL90_INIT_VARIA
C     | o initialize variables
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of GGL90_INIT
      INTEGER myThid

#ifdef ALLOW_GGL90
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj,I,J,K - Loop counters
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GGL90viscArU(i,j,k,bi,bj) = 0. _d 0
           GGL90viscArV(i,j,k,bi,bj) = 0. _d 0
           GGL90diffKr(i,j,k,bi,bj)  = 0. _d 0
           IF ( useIDEMIX) THEN
            GGL90TKE(i,j,k,bi,bj)=GGL90eps*maskC(i,j,k,bi,bj)
           ELSE
CML           GGL90TKE(I,J,K,bi,bj)=0. _d 0
            GGL90TKE(i,j,k,bi,bj)=GGL90TKEmin*maskC(i,j,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays

       ENDDO
      ENDDO

#ifdef ALLOW_GGL90_IDEMIX
c-----------------------------------------------------------------------
c  IDEMIX
c-----------------------------------------------------------------------
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IDEMIX_E(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IDEMIX_F_B(i,j,bi,bj) = 0. _d 0
          IDEMIX_F_S(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

       ENDDO
      ENDDO
c-----------------------------------------------------------------------
c      read in forcing fields for IDEMIX
c-----------------------------------------------------------------------
      IF ( useIDEMIX .AND. IDEMIX_tidal_file .NE. ' ' ) THEN
       CALL READ_REC_XY_RL( IDEMIX_tidal_file,IDEMIX_F_b,1,0,myThid )
       _EXCH_XY_RL(IDEMIX_F_b,myThid)

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IDEMIX_F_b(i,j,bi,bj) = -MAX(0. _d 0,
     &          MIN(1. _d 0,IDEMIX_F_b(i,j,bi,bj)))
C     fraction that enters IW field
           IDEMIX_F_b(i,j,bi,bj) = IDEMIX_frac_F_b*
CML     &          IDEMIX_F_b(i,j,bi,bj)*recip_rhoConst
     &          IDEMIX_F_b(i,j,bi,bj)/1024. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      IF ( useIDEMIX .AND. IDEMIX_wind_file .NE. ' ' ) THEN
       CALL READ_REC_XY_RL( IDEMIX_wind_file ,IDEMIX_F_s,1,0,myThid )
       _EXCH_XY_RL(IDEMIX_F_s,myThid)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IDEMIX_F_s(i,j,bi,bj) = MAX(0. _d 0,
     &          MIN(1. _d 0,IDEMIX_F_s(i,j,bi,bj)))
C     fraction that leaves mixed layer
           IDEMIX_F_s(i,j,bi,bj) = IDEMIX_frac_F_s*
CML     &          IDEMIX_F_s(i,j,bi,bj)*recip_rhoConst
     &          IDEMIX_F_s(i,j,bi,bj)/1024.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_GGL90_IDEMIX */

      IF ( nIter0.NE.0 .OR. pickupSuff.NE.' ' ) THEN
       CALL GGL90_READ_PICKUP( nIter0, myThid )
      ELSE
C--   Read initial turbulent kinetic energy thickness from file if available.
       IF ( GGL90TKEFile .NE. ' ' ) THEN
        CALL READ_FLD_XYZ_RL( GGL90TKEFile, ' ', GGL90TKE, 0, myThid )
        _EXCH_XYZ_RL(GGL90TKE,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             GGL90TKE(i,j,k,bi,bj) = MAX(GGL90TKE(i,j,k,bi,bj),
     &            GGL90TKEmin)*maskC(i,j,k,bi,bj)
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
