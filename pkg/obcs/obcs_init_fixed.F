C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_fixed.F,v 1.10 2011/03/04 04:50:19 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE OBCS_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_INIT_FIXED
C     | o Initialise OBCs fixed arrays
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER i, j

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        tileHasOBN(bi,bj) = .FALSE.
        tileHasOBS(bi,bj) = .FALSE.
        tileHasOBE(bi,bj) = .FALSE.
        tileHasOBW(bi,bj) = .FALSE.

C--   Set Interior mask to zero beyond OB
        DO j=1-OLy,sNy+OLy
C-    Eastern boundary
         IF ( OB_Ie(j,bi,bj).NE.0 ) THEN
          tileHasOBE(bi,bj) = .TRUE.
          DO i=OB_Ie(j,bi,bj),sNx+OLx
            maskInC(i,j,bi,bj) = 0.
            maskInW(i,j,bi,bj) = 0.
            maskInS(i,j,bi,bj) = 0.
            IF (j.LT.sNy+OLy) maskInS(i,j+1,bi,bj) = 0.
          ENDDO
         ENDIF
C-    Western boundary
         IF ( OB_Iw(j,bi,bj).NE.0 ) THEN
          tileHasOBW(bi,bj) = .TRUE.
          DO i=1-OLx,OB_Iw(j,bi,bj)
            maskInC(i,j,bi,bj) = 0.
            maskInS(i,j,bi,bj) = 0.
            IF (j.LT.sNy+OLy) maskInS(i,j+1,bi,bj) = 0.
          ENDDO
          DO i=1-OLx,OB_Iw(j,bi,bj)+1
            maskInW(i,j,bi,bj) = 0.
          ENDDO
         ENDIF
        ENDDO
        DO i=1-OLx,sNx+OLx
C-    Northern boundary
         IF ( OB_Jn(i,bi,bj).NE.0 ) THEN
          tileHasOBN(bi,bj) = .TRUE.
          DO j=OB_Jn(i,bi,bj),sNy+OLy
            maskInC(i,j,bi,bj) = 0.
            maskInW(i,j,bi,bj) = 0.
            IF (i.LT.sNx+OLx) maskInW(i+1,j,bi,bj) = 0.
            maskInS(i,j,bi,bj) = 0.
          ENDDO
         ENDIF
C-    Southern boundary
         IF ( OB_Js(i,bi,bj).NE.0 ) THEN
          tileHasOBS(bi,bj) = .TRUE.
          DO j=1-OLy,OB_Js(i,bi,bj)
            maskInC(i,j,bi,bj) = 0.
            maskInW(i,j,bi,bj) = 0.
            IF (i.LT.sNx+OLx) maskInW(i+1,j,bi,bj) = 0.
          ENDDO
          DO j=1-OLy,OB_Js(i,bi,bj)+1
            maskInS(i,j,bi,bj) = 0.
          ENDDO
         ENDIF
        ENDDO

C--   end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_OBCS */
      RETURN
      END
