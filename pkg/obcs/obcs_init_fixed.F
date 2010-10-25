C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_fixed.F,v 1.9 2010/10/25 22:44:09 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_INIT_FIXED( myThid )
C     *==========================================================*
C     | SUBROUTINE OBCS_INIT_FIXED
C     | o Initialise OBCs fixed arrays
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"
#include "GRID.h"
#ifdef NONLIN_FRSURF
#include "SURFACE.h"
#endif

C     == Routine arguments ==
C     myThid :: my Thread Id. number
      INTEGER myThid

#ifdef ALLOW_OBCS
C     == Local variables ==
      INTEGER bi, bj
      INTEGER i, j
#ifdef NONLIN_FRSURF
      INTEGER k
#endif

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

#ifdef NONLIN_FRSURF
C--   Save the initial hFacS at the N & S boundaries :
        DO i=1-OLx,sNx+OLx
          OBNhFac0(i,bi,bj)=0.
          OBShFac0(i,bi,bj)=0.
C-    Northern boundary
          IF ( OB_Jn(i,bi,bj).NE.0 ) THEN
            j = OB_Jn(i,bi,bj)
            k = ksurfS(i,j,bi,bj)
            IF (k.LE.Nr) OBNhFac0(i,bi,bj)=hFacS(i,j,k,bi,bj)
          ENDIF
C-    Southern boundary
          IF ( OB_Js(i,bi,bj).NE.0 ) THEN
            j = OB_Js(i,bi,bj)+1
            k = ksurfS(i,j,bi,bj)
            IF (k.LE.Nr) OBShFac0(i,bi,bj)=hFacS(i,j,k,bi,bj)
          ENDIF
        ENDDO

C--   Save the initial hFacW at the E & W boundaries :
        DO j=1-OLy,sNy+OLy
          OBEhFac0(j,bi,bj)=0.
          OBWhFac0(j,bi,bj)=0.
C-    Eastern boundary
          IF ( OB_Ie(j,bi,bj).NE.0 ) THEN
            i = OB_Ie(j,bi,bj)
            k = ksurfW(i,j,bi,bj)
            IF (k.LE.Nr) OBEhFac0(j,bi,bj)=hFacW(i,j,k,bi,bj)
          ENDIF
C-    Western boundary
          IF ( OB_Iw(j,bi,bj).NE.0 ) THEN
            i = OB_Iw(j,bi,bj)+1
            k = ksurfW(i,j,bi,bj)
            IF (k.LE.Nr) OBWhFac0(j,bi,bj)=hFacW(i,j,k,bi,bj)
          ENDIF
        ENDDO
#endif /* NONLIN_FRSURF */

C--   end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_OBCS */
      RETURN
      END
