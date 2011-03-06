C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_fixed.F,v 1.11 2011/03/06 23:41:57 jmc Exp $
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
#include "GRID.h"
#include "OBCS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
C     !LOCAL VARIABLES:
C     OB_ApplX :: number of grid points (in X dir) overwritten by obcs_apply
C     OB_ApplY :: number of grid points (in Y dir) overwritten by obcs_apply
C     bi,bj    :: tile indices
C     i, j     :: Loop counters
C     tmpFld   :: Temporary array used to load in Interior Mask
      INTEGER OB_ApplX
      INTEGER OB_ApplY
      INTEGER bi, bj
      INTEGER  i, j
      INTEGER iB, jB
      LOGICAL flag
      _RS tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   Set OB active tiles:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         tileHasOBN(bi,bj) = .FALSE.
         tileHasOBS(bi,bj) = .FALSE.
         tileHasOBE(bi,bj) = .FALSE.
         tileHasOBW(bi,bj) = .FALSE.
         DO j=1-OLy,sNy+OLy
          IF ( OB_Ie(j,bi,bj).NE.0 ) tileHasOBE(bi,bj) = .TRUE.
          IF ( OB_Iw(j,bi,bj).NE.0 ) tileHasOBW(bi,bj) = .TRUE.
         ENDDO
         DO i=1-OLx,sNx+OLx
          IF ( OB_Jn(i,bi,bj).NE.0 ) tileHasOBN(bi,bj) = .TRUE.
          IF ( OB_Js(i,bi,bj).NE.0 ) tileHasOBS(bi,bj) = .TRUE.
         ENDDO
       ENDDO
      ENDDO

C==   Set Interior mask at Cell Center:

      IF ( insideOBmaskFile.EQ.' ' ) THEN
C--   If no maskFile is provided, set Interior mask from OB list of indices

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
C-    Eastern boundary
          IF ( OB_Ie(j,bi,bj).NE.0 ) THEN
           flag = .TRUE.
           DO i=OB_Ie(j,bi,bj),sNx
             flag = flag .AND.
     &              ksurfC(i,j,bi,bj).LE.Nr .AND. i.NE.OB_Iw(j,bi,bj)
             IF ( flag ) maskInC(i,j,bi,bj) = 0.
           ENDDO
          ENDIF
C-    Western boundary
          IF ( OB_Iw(j,bi,bj).NE.0 ) THEN
           flag = .TRUE.
           DO i=OB_Iw(j,bi,bj),1,-1
             flag = flag .AND.
     &              ksurfC(i,j,bi,bj).LE.Nr .AND. i.NE.OB_Ie(j,bi,bj)
             IF ( flag ) maskInC(i,j,bi,bj) = 0.
           ENDDO
          ENDIF
         ENDDO
         DO i=1,sNx
C-    Northern boundary
          IF ( OB_Jn(i,bi,bj).NE.0 ) THEN
           flag = .TRUE.
           DO j=OB_Jn(i,bi,bj),sNy
             flag = flag .AND.
     &              ksurfC(i,j,bi,bj).LE.Nr .AND. j.NE.OB_Js(i,bi,bj)
             IF ( flag ) maskInC(i,j,bi,bj) = 0.
           ENDDO
          ENDIF
C-    Southern boundary
          IF ( OB_Js(i,bi,bj).NE.0 ) THEN
           flag = .TRUE.
           DO j=OB_Js(i,bi,bj),1,-1
             flag = flag .AND.
     &              ksurfC(i,j,bi,bj).LE.Nr .AND. j.NE.OB_Jn(i,bi,bj)
             IF ( flag ) maskInC(i,j,bi,bj) = 0.
           ENDDO
          ENDIF
         ENDDO

C--   end bi,bj loops
        ENDDO
       ENDDO

      ELSE
C--   Read in Interior mask from file :

       CALL READ_FLD_XY_RS( insideOBmaskFile, ' ', tmpFld, 0, myThid )

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           IF ( tmpFld(i,j,bi,bj).EQ.0. ) maskInC(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   end computing/reading Interior mask
      ENDIF

C--   Fill in the overlap:
      _EXCH_XY_RS( maskInC, myThid )

C==   Set interior mask at U & V location (grid-cell Wester & Southern edges)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=2-Oly,sNy+Oly
          DO i=2-Olx,sNx+Olx
            maskInW(i,j,bi,bj) = maskInW(i,j,bi,bj)
     &                         * maskInC(i-1,j,bi,bj)*maskInC(i,j,bi,bj)
            maskInS(i,j,bi,bj) = maskInS(i,j,bi,bj)
     &                         * maskInC(i,j-1,bi,bj)*maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C==   After EXCH: reset Interior mask to zero beyond OB: this is necessary
C     when EXCH are not disabled (e.g. with EXCH1) between tile Edges
C     that are closed by OB.
C     Do it over Olx,Oly grid points beyond OB, in agreement with OBCS code
C     (apply_tracer) which overwrites tracer over the same width.
      OB_ApplX = OLx
      OB_ApplY = OLy

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   Set Interior mask to zero beyond OB
        DO j=1-OLy,sNy+OLy
C-    Eastern boundary
         iB = OB_Ie(j,bi,bj)
         IF ( iB.NE.0 ) THEN
          DO i=iB,iB+OB_ApplX-1
            maskInC(i,j,bi,bj) = 0.
            maskInW(i,j,bi,bj) = 0.
            maskInS(i,j,bi,bj) = 0.
            IF (j.LT.sNy+OLy) maskInS(i,j+1,bi,bj) = 0.
          ENDDO
         ENDIF
C-    Western boundary
         iB = OB_Iw(j,bi,bj)
         IF ( iB.NE.0 ) THEN
          DO i=1-OB_ApplX+iB,iB
            maskInC(i,j,bi,bj) = 0.
            maskInS(i,j,bi,bj) = 0.
            IF (j.LT.sNy+OLy) maskInS(i,j+1,bi,bj) = 0.
          ENDDO
          DO i=2-OB_ApplX+iB,iB+1
            maskInW(i,j,bi,bj) = 0.
          ENDDO
         ENDIF
        ENDDO
        DO i=1-OLx,sNx+OLx
C-    Northern boundary
         jB = OB_Jn(i,bi,bj)
         IF ( jB.NE.0 ) THEN
          DO j=jB,jB+OB_ApplY-1
            maskInC(i,j,bi,bj) = 0.
            maskInW(i,j,bi,bj) = 0.
            IF (i.LT.sNx+OLx) maskInW(i+1,j,bi,bj) = 0.
            maskInS(i,j,bi,bj) = 0.
          ENDDO
         ENDIF
C-    Southern boundary
         jB = OB_Js(i,bi,bj)
         IF ( jB.NE.0 ) THEN
          DO j=1-OB_ApplY+jB,jB
            maskInC(i,j,bi,bj) = 0.
            maskInW(i,j,bi,bj) = 0.
            IF (i.LT.sNx+OLx) maskInW(i+1,j,bi,bj) = 0.
          ENDDO
          DO j=2-OB_ApplY+jB,jB+1
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
