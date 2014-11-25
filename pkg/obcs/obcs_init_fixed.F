C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_fixed.F,v 1.19 2014/11/25 01:08:20 jmc Exp $
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
      IF (debugMode) CALL DEBUG_ENTER('OBCS_INIT_FIXED',myThid)
#endif

C==   Set Interior mask at Cell Center:

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           OBCS_insideMask(i,j,bi,bj) = 1.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( insideOBmaskFile.EQ.' ' ) THEN
C--   If no maskFile is provided, set Interior mask from OB list of indices

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
C-    Eastern boundary
          IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
           flag = .TRUE.
           DO i=OB_Ie(j,bi,bj),sNx
             flag = flag .AND.
     &              kSurfC(i,j,bi,bj).LE.Nr .AND. i.NE.OB_Iw(j,bi,bj)
             IF ( flag ) OBCS_insideMask(i,j,bi,bj) = 0.
           ENDDO
          ENDIF
C-    Western boundary
          IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
           flag = .TRUE.
           DO i=OB_Iw(j,bi,bj),1,-1
             flag = flag .AND.
     &              kSurfC(i,j,bi,bj).LE.Nr .AND. i.NE.OB_Ie(j,bi,bj)
             IF ( flag ) OBCS_insideMask(i,j,bi,bj) = 0.
           ENDDO
          ENDIF
         ENDDO
         DO i=1,sNx
C-    Northern boundary
          IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
           flag = .TRUE.
           DO j=OB_Jn(i,bi,bj),sNy
             flag = flag .AND.
     &              kSurfC(i,j,bi,bj).LE.Nr .AND. j.NE.OB_Js(i,bi,bj)
             IF ( flag ) OBCS_insideMask(i,j,bi,bj) = 0.
           ENDDO
          ENDIF
C-    Southern boundary
          IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
           flag = .TRUE.
           DO j=OB_Js(i,bi,bj),1,-1
             flag = flag .AND.
     &              kSurfC(i,j,bi,bj).LE.Nr .AND. j.NE.OB_Jn(i,bi,bj)
             IF ( flag ) OBCS_insideMask(i,j,bi,bj) = 0.
           ENDDO
          ENDIF
         ENDDO

C--   end bi,bj loops
        ENDDO
       ENDDO

      ELSE
C--   Read in Interior mask from file :

       CALL READ_FLD_XY_RS( insideOBmaskFile, ' ', OBCS_insideMask,
     &                      0, myThid )

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           IF ( OBCS_insideMask(i,j,bi,bj).NE.0. )
     &          OBCS_insideMask(i,j,bi,bj) = 1.
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   end computing/reading Interior mask
      ENDIF

C--   Fill in the overlap:
      _EXCH_XY_RS( OBCS_insideMask, myThid )

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

C--   After exchange, set tiled index arrays OB_Jn/Js/Ie/Iw in overlap region
C     issue a warning if not consistent (similar to OBCS_CHECK but for overlap)

c     IF ( .TRUE. ) THEN
      IF ( OBCS_indexStatus .LT. 2 ) THEN
       ioUnit = standardMessageUnit
       WRITE(msgBuf,'(2A)')
     &      'OBCS_INIT_FIXED: Setting OB indices in Overlap'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
       WRITE(errMsg,'(2A)') 'S/R OBCS_INIT_FIXED: ',
     &              'Inside Mask and OB locations disagree :'
       flag = .TRUE.
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

C-    Eastern boundary
         DO j=1-OLy,sNy+OLy
          DO i=1,sNx+1
           IF ( OBCS_insideMask(i,j,bi,bj).LT.
     &          OBCS_insideMask(i-1,j,bi,bj)
     &          .AND. ( j.LT.1 .OR. j.GT.sNy )
     &          .AND. kSurfW(i,j,bi,bj).LE.Nr ) THEN
            IF ( OB_Ie(j,bi,bj).EQ.OB_indexNone ) THEN
              OB_Ie(j,bi,bj) = i
              WRITE(msgBuf,'(A,I5,2(A,I3),2(A,I5))')
     &        ' Sets OBE(j,bi,bj=',j,',',bi,',',bj,')=', OB_Ie(j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
            ELSEIF ( OB_Ie(j,bi,bj).NE.i ) THEN
              IF ( flag ) CALL PRINT_ERROR( errMsg, myThid )
              flag = .FALSE.
              WRITE(msgBuf,'(A,I5,2(A,I3),2(A,I5))')
     &        ' OBE(j,bi,bj=',j,',',bi,',',bj,')=', OB_Ie(j,bi,bj),
     &        ' but from insideMask expects I=', i
              CALL PRINT_ERROR( msgBuf, myThid )
            ENDIF
           ENDIF
          ENDDO
         ENDDO
C-    Western boundary
         DO j=1-OLy,sNy+OLy
          DO i=0,sNx
           IF ( OBCS_insideMask(i,j,bi,bj).LT.
     &          OBCS_insideMask(i+1,j,bi,bj)
     &          .AND. ( j.LT.1 .OR. j.GT.sNy )
     &          .AND. kSurfW(i+1,j,bi,bj).LE.Nr ) THEN
            IF ( OB_Iw(j,bi,bj).EQ.OB_indexNone ) THEN
              OB_Iw(j,bi,bj) = i
              WRITE(msgBuf,'(A,I5,2(A,I3),2(A,I5))')
     &        ' Sets OBW(j,bi,bj=',j,',',bi,',',bj,')=', OB_Iw(j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
            ELSEIF ( OB_Iw(j,bi,bj).NE.i ) THEN
              IF ( flag ) CALL PRINT_ERROR( errMsg, myThid )
              flag = .FALSE.
              WRITE(msgBuf,'(A,I5,2(A,I3),2(A,I5))')
     &        ' OBW(j,bi,bj=',j,',',bi,',',bj,')=', OB_Iw(j,bi,bj),
     &        ' but from insideMask expects I=', i
              CALL PRINT_ERROR( msgBuf, myThid )
            ENDIF
           ENDIF
          ENDDO
         ENDDO
C-    Northern boundary
         DO j=1,sNy+1
          DO i=1-OLx,sNx+OLx
           IF ( OBCS_insideMask(i,j,bi,bj).LT.
     &          OBCS_insideMask(i,j-1,bi,bj)
     &          .AND. ( i.LT.1 .OR. i.GT.sNx )
     &          .AND. kSurfS(i,j,bi,bj).LE.Nr ) THEN
            IF ( OB_Jn(i,bi,bj).EQ.OB_indexNone ) THEN
              OB_Jn(i,bi,bj) = j
              WRITE(msgBuf,'(A,I5,2(A,I3),2(A,I5))')
     &        ' Sets OBN(i,bi,bj=',i,',',bi,',',bj,')=', OB_Jn(i,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
            ELSEIF ( OB_Jn(i,bi,bj).NE.j ) THEN
              IF ( flag ) CALL PRINT_ERROR( errMsg, myThid )
              flag = .FALSE.
              WRITE(msgBuf,'(A,I5,2(A,I3),2(A,I5))')
     &        ' OBN(i,bi,bj=',i,',',bi,',',bj,')=', OB_Jn(i,bi,bj),
     &        ' but from insideMask expects J=', j
              CALL PRINT_ERROR( msgBuf, myThid )
            ENDIF
           ENDIF
          ENDDO
         ENDDO
C-    Southern boundary
         DO j=0,sNy
          DO i=1-OLx,sNx+OLx
           IF ( OBCS_insideMask(i,j,bi,bj).LT.
     &          OBCS_insideMask(i,j+1,bi,bj)
     &          .AND. ( i.LT.1 .OR. i.GT.sNx )
     &          .AND. kSurfS(i,j+1,bi,bj).LE.Nr ) THEN
            IF ( OB_Js(i,bi,bj).EQ.OB_indexNone ) THEN
              OB_Js(i,bi,bj) = j
              WRITE(msgBuf,'(A,I5,2(A,I3),2(A,I5))')
     &        ' Sets OBS(i,bi,bj=',i,',',bi,',',bj,')=', OB_Js(i,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
            ELSEIF ( OB_Js(i,bi,bj).NE.j ) THEN
              IF ( flag ) CALL PRINT_ERROR( errMsg, myThid )
              flag = .FALSE.
              WRITE(msgBuf,'(A,I5,2(A,I3),2(A,I5))')
     &        ' OBS(i,bi,bj=',i,',',bi,',',bj,')=', OB_Js(i,bi,bj),
     &        ' but from insideMask expects J=', j
              CALL PRINT_ERROR( msgBuf, myThid )
            ENDIF
           ENDIF
          ENDDO
         ENDDO

        ENDDO
       ENDDO
       WRITE(msgBuf,'(2A)')
     &      'OBCS_INIT_FIXED: Setting OB indices in Overlap <= done'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

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
           DO i=iB,iB+OB_ApplX-1
             OBCS_insideMask(i,j,bi,bj) = 0.
           ENDDO
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
           DO i=1-OB_ApplX+iB,iB
             OBCS_insideMask(i,j,bi,bj) = 0.
           ENDDO
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
           DO j=jB,jB+OB_ApplY-1
             OBCS_insideMask(i,j,bi,bj) = 0.
           ENDDO
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
           DO j=1-OB_ApplY+jB,jB
             OBCS_insideMask(i,j,bi,bj) = 0.
           ENDDO
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
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         tileHasOBE(bi,bj) = .FALSE.
         tileHasOBW(bi,bj) = .FALSE.
         tileHasOBN(bi,bj) = .FALSE.
         tileHasOBS(bi,bj) = .FALSE.
         DO j=1-OLy,sNy+OLy
          tileHasOBE(bi,bj) = tileHasOBE(bi,bj) .OR.
     &                      ( OB_Ie(j,bi,bj).NE.OB_indexNone )
          tileHasOBW(bi,bj) = tileHasOBW(bi,bj) .OR.
     &                      ( OB_Iw(j,bi,bj).NE.OB_indexNone )
         ENDDO
         DO i=1-OLx,sNx+OLx
          tileHasOBN(bi,bj) = tileHasOBN(bi,bj) .OR.
     &                      ( OB_Jn(i,bi,bj).NE.OB_indexNone )
          tileHasOBS(bi,bj) = tileHasOBS(bi,bj) .OR.
     &                      ( OB_Js(i,bi,bj).NE.OB_indexNone )
         ENDDO
       ENDDO
      ENDDO

C--   Set domain connected-piece Id for OB grid points:
      CALL OBCS_SET_CONNECT( myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_INIT_FIXED',myThid)
#endif

#endif /* ALLOW_OBCS */
      RETURN
      END
