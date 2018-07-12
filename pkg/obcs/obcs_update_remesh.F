#include "OBCS_OPTIONS.h"


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_UPDATE_REMESH

C     !INTERFACE:
      SUBROUTINE OBCS_UPDATE_REMESH( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_UPDATE_REMESH
C     | o Initialise maskInW/S/C based on OBCS masks
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
C     OB_ApplX :: number of grid points (in X dir) overwritten by obcs_apply
C     OB_ApplY :: number of grid points (in Y dir) overwritten by obcs_apply
C     bi,bj    :: tile indices
C     i, j     :: Loop counters
      CHARACTER*(MAX_LEN_MBUF) msgBuf, errMsg
      INTEGER OB_ApplX
      INTEGER OB_ApplY
      INTEGER bi, bj
      INTEGER  i, j
      INTEGER im, jm
      INTEGER iB, jB
      LOGICAL flag
      INTEGER ioUnit

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_UPDATE_REMESH',myThid)
#endif

C==   Set Interior mask at Cell Center:



C      IF ( insideOBmaskFile.EQ.' ' ) THEN
C--   If no maskFile is provided, set Interior mask from OB list of indices

C       DO bj = myByLo(myThid), myByHi(myThid)
C        DO bi = myBxLo(myThid), myBxHi(myThid)
C         DO j=1,sNy
C-    Eastern boundary
C          IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
C           flag = .TRUE.
C           DO i=OB_Ie(j,bi,bj),sNx
C             flag = flag .AND.
C     &              kSurfC(i,j,bi,bj).LE.Nr .AND. i.NE.OB_Iw(j,bi,bj)
C             IF ( flag ) OBCS_insideMask(i,j,bi,bj) = 0.
C           ENDDO
C          ENDIF
C-    Western boundary
C          IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
C           flag = .TRUE.
C           DO i=OB_Iw(j,bi,bj),1,-1
C             flag = flag .AND.
C     &              kSurfC(i,j,bi,bj).LE.Nr .AND. i.NE.OB_Ie(j,bi,bj)
C             IF ( flag ) OBCS_insideMask(i,j,bi,bj) = 0.
C           ENDDO
C          ENDIF
C         ENDDO
C         DO i=1,sNx
C-    Northern boundary
C          IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
C           flag = .TRUE.
C           DO j=OB_Jn(i,bi,bj),sNy
C             flag = flag .AND.
C     &              kSurfC(i,j,bi,bj).LE.Nr .AND. j.NE.OB_Js(i,bi,bj)
C             IF ( flag ) OBCS_insideMask(i,j,bi,bj) = 0.
C           ENDDO
C          ENDIF
C-    Southern boundary
C          IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
C           flag = .TRUE.
C           DO j=OB_Js(i,bi,bj),1,-1
C             flag = flag .AND.
C     &              kSurfC(i,j,bi,bj).LE.Nr .AND. j.NE.OB_Jn(i,bi,bj)
C             IF ( flag ) OBCS_insideMask(i,j,bi,bj) = 0.
C           ENDDO
C          ENDIF
C         ENDDO

C--   end bi,bj loops
C        ENDDO
C       ENDDO

C      ELSE
C--   Read in Interior mask from file :

C       CALL READ_FLD_XY_RS( insideOBmaskFile, ' ', OBCS_insideMask,
C     &                      0, myThid )

C       DO bj = myByLo(myThid), myByHi(myThid)
C        DO bi = myBxLo(myThid), myBxHi(myThid)
C         DO j=1,sNy
C          DO i=1,sNx
C           IF ( OBCS_insideMask(i,j,bi,bj).NE.0. )
C     &          OBCS_insideMask(i,j,bi,bj) = 1.
C          ENDDO
C         ENDDO
C        ENDDO
C       ENDDO

C--   end computing/reading Interior mask
C      ENDIF

C--   Fill in the overlap:
C      _EXCH_XY_RS( OBCS_insideMask, myThid )

C==   Set interior mask at U & V location (grid-cell Wester & Southern edges)
C     leave OB edges inside (maskIn=1) (e.g., Eastern OB: maskInW(OB_Ie)=1 )
C     so that velocity normal-component at OB is still in Interior region.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
            maskInW(i,j,bi,bj) = maskInW(i,j,bi,bj)
     &                     *MAX( OBCS_insideMask(i-1,j,bi,bj),
     &                           OBCS_insideMask(i,j,bi,bj) )
            maskInS(i,j,bi,bj) = maskInS(i,j,bi,bj)
     &                     *MAX( OBCS_insideMask(i,j-1,bi,bj),
     &                           OBCS_insideMask(i,j,bi,bj) )
          ENDDO
         ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|




C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C==   After EXCH: reset Interior mask to zero beyond OB: this is necessary
C     when EXCH are not disabled (e.g. with EXCH1) between tile Edges
C     that are closed by OB.
C     Do it over OLx,OLy grid points beyond OB, in agreement with OBCS code
C     (apply_tracer) which overwrites tracer over the same width.
      OB_ApplX = OLx
      OB_ApplY = OLy

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   Set Interior mask to zero beyond OB
C-    Eastern boundary
        DO j=1-OLy,sNy+OLy
         jm = MAX( j-1, 1-OLy )
         iB = OB_Ie(j,bi,bj)
         IF ( iB.NE.OB_indexNone ) THEN
C           DO i=iB,iB+OB_ApplX-1
C             OBCS_insideMask(i,j,bi,bj) = 0.
C           ENDDO
           DO i=iB+1,iB+OB_ApplX-1
             maskInW(i,j,bi,bj) = 0.
           ENDDO
           IF ( OB_Ie(jm,bi,bj).NE.OB_indexNone ) THEN
            iB = MAX( iB, OB_Ie(jm,bi,bj) )
            DO i=iB,iB+OB_ApplX-1
             maskInS(i,j,bi,bj) = 0.
            ENDDO
           ENDIF
         ENDIF
        ENDDO
C-    Western boundary
        DO j=1-OLy,sNy+OLy
         jm = MAX( j-1, 1-OLy )
         iB = OB_Iw(j,bi,bj)
         IF ( iB.NE.OB_indexNone ) THEN
C           DO i=1-OB_ApplX+iB,iB
C             OBCS_insideMask(i,j,bi,bj) = 0.
C           ENDDO
           DO i=2-OB_ApplX+iB,iB
             maskInW(i,j,bi,bj) = 0.
           ENDDO
           IF ( OB_Iw(jm,bi,bj).NE.OB_indexNone ) THEN
            iB = MIN( iB, OB_Iw(jm,bi,bj) )
            DO i=1-OB_ApplX+iB,iB
             maskInS(i,j,bi,bj) = 0.
            ENDDO
           ENDIF
         ENDIF
        ENDDO
C-    Northern boundary
        DO i=1-OLx,sNx+OLx
         im = MAX( i-1, 1-OLx )
         jB = OB_Jn(i,bi,bj)
         IF ( jB.NE.OB_indexNone ) THEN
C           DO j=jB,jB+OB_ApplY-1
C             OBCS_insideMask(i,j,bi,bj) = 0.
C           ENDDO
           DO j=jB+1,jB+OB_ApplY-1
             maskInS(i,j,bi,bj) = 0.
           ENDDO
           IF ( OB_Jn(im,bi,bj).NE.OB_indexNone ) THEN
            jB = MAX( jB, OB_Jn(im,bi,bj) )
            DO j=jB,jB+OB_ApplY-1
             maskInW(i,j,bi,bj) = 0.
            ENDDO
           ENDIF
         ENDIF
        ENDDO
C-    Southern boundary
        DO i=1-OLx,sNx+OLx
         im = MAX( i-1, 1-OLx )
         jB = OB_Js(i,bi,bj)
         IF ( jB.NE.OB_indexNone ) THEN
C           DO j=1-OB_ApplY+jB,jB
C             OBCS_insideMask(i,j,bi,bj) = 0.
C           ENDDO
           DO j=2-OB_ApplY+jB,jB
             maskInS(i,j,bi,bj) = 0.
           ENDDO
           IF ( OB_Js(im,bi,bj).NE.OB_indexNone ) THEN
            jB = MIN( jB, OB_Js(im,bi,bj) )
            DO j=1-OB_ApplY+jB,jB
             maskInW(i,j,bi,bj) = 0.
            ENDDO
           ENDIF
         ENDIF
        ENDDO

C--   Apply mask to maskInC :
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            maskInC(i,j,bi,bj) = maskInC(i,j,bi,bj)
     &                          *OBCS_insideMask(i,j,bi,bj)
          ENDDO
        ENDDO

C--   end bi,bj loops
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Set OB active tiles:


C--   Set domain connected-piece Id for OB grid points:


#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_UPDATE_REMESH',myThid)
#endif

#endif /* ALLOW_OBCS */
      RETURN
      END
