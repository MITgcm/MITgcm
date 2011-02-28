C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_mon_stats.F,v 1.1 2011/02/28 15:34:28 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C--  File obcs_mon_stats.F: compute statistic of a field at OB section
C--   Contents
C--   o OBCS_MON_STATS_EW_RL
C--   o OBCS_MON_STATS_NS_RL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_MON_STATS_EW_RL

C     !INTERFACE:
      SUBROUTINE OBCS_MON_STATS_EW_RL(
     I     tHasOBE, tHasOBW, iEb, iWb, jNb, jSb,
     I     gPos, kSize, mSize,
     I     arr, arrhFac, arrDy, arrDr,
     O     arrStats,
     I     myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_MON_STATS_EW_RL
C     | o Caclulate field statistics at Eastern & Western OB
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     tHasOBE  :: list of OBE active tiles
C     tHasOBW  :: list of OBW active tiles
C     iEb      :: index of Eastern OB
C     iWb      :: index of Western OB
C     jNb      :: index of Northern OB
C     jSb      :: index of Southern OB
C     gPos     :: field position on C-grid ( 0=center , 1=U , 2=V , 3=Corner)
C     kSize    :: field-array 3rd dimension
C     mSize    :: hFac-array  3rd dimension
C     arr      :: field-array
C     arrhFac  :: hFac factor
C     arrDy    :: grid-cell length along OB
C     arrDr    :: grid-level thickness
C     myThid   :: my Thread Id number
      LOGICAL tHasOBE(nSx,nSy)
      LOGICAL tHasOBW(nSx,nSy)
      INTEGER iEb(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iWb(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER jNb(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER jSb(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER gPos
      INTEGER kSize
      INTEGER mSize
      _RL arr    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,mSize,nSx,nSy)
      _RS arrDy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(kSize)
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     arrStats :: field statistics at Eatern & Western OB
      _RL arrStats(0:4,2)
CEOP

#ifdef ALLOW_OBCS
#ifdef ALLOW_MONITOR

C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     j, k     :: loop indices
C     ii, iB   :: local index of open boundary
      INTEGER bi, bj
      INTEGER j, k, km
      INTEGER ii, iB
      LOGICAL noPnts
      _RL tmpA, tmpV, tmpMask
      _RL theMin, theMax, theArea, theMean, theVar
      _RL tileArea(nSx,nSy)
      _RL tileMean(nSx,nSy)
      _RL tileVar (nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO k=0,4
        arrStats(k,1) = 0. _d 0
      ENDDO
#ifdef ALLOW_OBCS_EAST
      theMin = 0.
      theMax = 0.
      theMean= 0.
      theVar = 0.
      theArea= 0.
      noPnts = .TRUE.
c     IF ( usingEast_OB ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          tileArea(bi,bj) = 0.
          tileMean(bi,bj) = 0.
          tileVar (bi,bj) = 0.
          IF ( tHasOBE(bi,bj) ) THEN
           DO k=1,kSize
            km = MIN(k,mSize)
            DO j=1,sNy
             tmpMask = 0.
             ii = iEb(j,bi,bj)
             IF ( ii.NE.0 ) THEN
              iB = ii
              IF ( j.GT.jSb(ii,bi,bj) .AND.
     &            (j.LT.jNb(ii,bi,bj).OR.jNb(ii,bi,bj).EQ.0)
     &           ) tmpMask = arrhFac(iB,j,km,bi,bj)
             ENDIF
             IF ( tmpMask.GT.0. _d 0 ) THEN
              tmpV = arr(ii,j,k,bi,bj)
              tmpA = arrDy(iB,j,bi,bj)*arrDr(k)*tmpMask
              IF ( noPnts ) THEN
                theMin = tmpV
                theMax = tmpV
                noPnts = .FALSE.
              ENDIF
              theMin = MIN( theMin, tmpV )
              theMax = MAX( theMax, tmpV )
              tileArea(bi,bj) = tileArea(bi,bj) + tmpA
              tileMean(bi,bj) = tileMean(bi,bj) + tmpA*tmpV
              tileVar (bi,bj) = tileVar (bi,bj) + tmpA*tmpV*tmpV
             ENDIF
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( tileArea, theArea, myThid )
c     ENDIF
      IF ( theArea.GT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileMean, theMean, myThid )
        CALL GLOBAL_SUM_TILE_RL( tileVar , theVar , myThid )
        arrStats(0,1) = theArea
        arrStats(1,1) = theMean
        arrStats(2,1) = theVar

        theMean = theMean/theArea
        IF ( noPnts ) theMin = theMean
        theMin = -theMin
        _GLOBAL_MAX_RL(theMin,myThid)
        theMin = -theMin
        IF ( noPnts ) theMax = theMean
        _GLOBAL_MAX_RL(theMax,myThid)
        arrStats(3,1) = theMin
        arrStats(4,1) = theMax

      ENDIF
#endif /* ALLOW_OBCS_EAST */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO k=0,4
        arrStats(k,2) = 0. _d 0
      ENDDO
#ifdef ALLOW_OBCS_WEST
      theMin = 0.
      theMax = 0.
      theMean= 0.
      theVar = 0.
      theArea= 0.
      noPnts = .TRUE.
c     IF ( usingWest_OB ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          tileArea(bi,bj) = 0.
          tileMean(bi,bj) = 0.
          tileVar (bi,bj) = 0.
          IF ( tHasOBW(bi,bj) ) THEN
           DO k=1,kSize
            km = MIN(k,mSize)
            DO j=1,sNy
             tmpMask = 0.
             ii = iWb(j,bi,bj)
             IF ( ii.NE.0 ) THEN
              iB = ii+1
              IF ( j.GT.jSb(ii,bi,bj) .AND.
     &            (j.LT.jNb(ii,bi,bj).OR.jNb(ii,bi,bj).EQ.0)
     &           ) tmpMask = arrhFac(iB,j,km,bi,bj)
             ENDIF
             IF ( tmpMask.GT.0. _d 0 ) THEN
              IF ( gPos.EQ.1 .OR. gPos.EQ.3 ) ii = iB
              tmpV = arr(ii,j,k,bi,bj)
              tmpA = arrDy(iB,j,bi,bj)*arrDr(k)*tmpMask
              IF ( noPnts ) THEN
                theMin = tmpV
                theMax = tmpV
                noPnts = .FALSE.
              ENDIF
              theMin = MIN( theMin, tmpV )
              theMax = MAX( theMax, tmpV )
              tileArea(bi,bj) = tileArea(bi,bj) + tmpA
              tileMean(bi,bj) = tileMean(bi,bj) + tmpA*tmpV
              tileVar (bi,bj) = tileVar (bi,bj) + tmpA*tmpV*tmpV
             ENDIF
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( tileArea, theArea, myThid )
c     ENDIF
      IF ( theArea.GT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileMean, theMean, myThid )
        CALL GLOBAL_SUM_TILE_RL( tileVar , theVar , myThid )
        arrStats(0,2) = theArea
        arrStats(1,2) = theMean
        arrStats(2,2) = theVar

        theMean = theMean/theArea
        IF ( noPnts ) theMin = theMean
        theMin = -theMin
        _GLOBAL_MAX_RL(theMin,myThid)
        theMin = -theMin
        IF ( noPnts ) theMax = theMean
        _GLOBAL_MAX_RL(theMax,myThid)
        arrStats(3,2) = theMin
        arrStats(4,2) = theMax

      ENDIF
#endif /* ALLOW_OBCS_WEST */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_MONITOR */
#endif /* ALLOW_OBCS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_MON_STATS_NS_RL

C     !INTERFACE:
      SUBROUTINE OBCS_MON_STATS_NS_RL(
     I     tHasOBN, tHasOBS, iEb, iWb, jNb, jSb,
     I     gPos, kSize, mSize,
     I     arr, arrhFac, arrDx, arrDr,
     O     arrStats,
     I     myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_MON_STATS_NS_RL
C     | o Caclulate field statistics at Northern & Southern OB
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     tHasOBN  :: list of OBN active tiles
C     tHasOBS  :: list of OBS active tiles
C     iEb      :: index of Eastern OB
C     iWb      :: index of Western OB
C     jNb      :: index of Northern OB
C     jSb      :: index of Southern OB
C     gPos     :: field position on C-grid ( 0=center , 1=U , 2=V , 3=Corner)
C     kSize    :: field-array 3rd dimension
C     mSize    :: hFac & mask 3rd dimension
C     arr      :: field-array
C     arrhFac  :: hFac factor
C     arrDx    :: grid-cell length along OB
C     arrDr    :: grid-level thickness
C     myThid   :: my Thread Id number
      LOGICAL tHasOBN(nSx,nSy)
      LOGICAL tHasOBS(nSx,nSy)
      INTEGER iEb(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iWb(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER jNb(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER jSb(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER gPos
      INTEGER kSize
      INTEGER mSize
      _RL arr    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,mSize,nSx,nSy)
      _RS arrDx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(kSize)
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     arrStats :: field statistics at Northern & Southern OB
      _RL arrStats(0:4,2)
CEOP

#ifdef ALLOW_OBCS
#ifdef ALLOW_MONITOR

C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     i, k     :: loop indices
C     jj, jB   :: local index of open boundary
      INTEGER bi, bj
      INTEGER i, k, km
      INTEGER jj, jB
      LOGICAL noPnts
      _RL tmpA, tmpV, tmpMask
      _RL theMin, theMax, theArea, theMean, theVar
      _RL tileArea(nSx,nSy)
      _RL tileMean(nSx,nSy)
      _RL tileVar (nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO k=0,4
        arrStats(k,1) = 0. _d 0
      ENDDO
#ifdef ALLOW_OBCS_NORTH
      theMin = 0.
      theMax = 0.
      theMean= 0.
      theVar = 0.
      theArea= 0.
      noPnts = .TRUE.
c     IF ( usingNorth_OB ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          tileArea(bi,bj) = 0.
          tileMean(bi,bj) = 0.
          tileVar (bi,bj) = 0.
          IF ( tHasOBN(bi,bj) ) THEN
           DO k=1,kSize
            km = MIN(k,mSize)
            DO i=1,sNx
             tmpMask = 0.
             jj = jNb(i,bi,bj)
             IF ( jj.NE.0 ) THEN
              jB = jj
              IF ( i.GT.iWb(jj,bi,bj) .AND.
     &            (i.LT.iEb(jj,bi,bj).OR.iEb(jj,bi,bj).EQ.0)
     &           ) tmpMask = arrhFac(i,jB,km,bi,bj)
             ENDIF
             IF ( tmpMask.GT.0. _d 0 ) THEN
              tmpV = arr(i,jj,k,bi,bj)
              tmpA = arrDx(i,jB,bi,bj)*arrDr(k)*tmpMask
              IF ( noPnts ) THEN
                theMin = tmpV
                theMax = tmpV
                noPnts = .FALSE.
              ENDIF
              theMin = MIN( theMin, tmpV )
              theMax = MAX( theMax, tmpV )
              tileArea(bi,bj) = tileArea(bi,bj) + tmpA
              tileMean(bi,bj) = tileMean(bi,bj) + tmpA*tmpV
              tileVar (bi,bj) = tileVar (bi,bj) + tmpA*tmpV*tmpV
             ENDIF
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( tileArea, theArea, myThid )
c     ENDIF
      IF ( theArea.GT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileMean, theMean, myThid )
        CALL GLOBAL_SUM_TILE_RL( tileVar , theVar , myThid )
        arrStats(0,1) = theArea
        arrStats(1,1) = theMean
        arrStats(2,1) = theVar

        theMean = theMean/theArea
        IF ( noPnts ) theMin = theMean
        theMin = -theMin
        _GLOBAL_MAX_RL(theMin,myThid)
        theMin = -theMin
        IF ( noPnts ) theMax = theMean
        _GLOBAL_MAX_RL(theMax,myThid)
        arrStats(3,1) = theMin
        arrStats(4,1) = theMax

      ENDIF
#endif /* ALLOW_OBCS_NORTH */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO k=0,4
        arrStats(k,2) = 0. _d 0
      ENDDO
#ifdef ALLOW_OBCS_SOUTH
      theMin = 0.
      theMax = 0.
      theMean= 0.
      theVar = 0.
      theArea= 0.
      noPnts = .TRUE.
c     IF ( usingSouth_OB ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          tileArea(bi,bj) = 0.
          tileMean(bi,bj) = 0.
          tileVar (bi,bj) = 0.
          IF ( tHasOBS(bi,bj) ) THEN
           DO k=1,kSize
            km = MIN(k,mSize)
            DO i=1,sNx
             tmpMask = 0.
             jj = jSb(i,bi,bj)
             IF ( jj.NE.0 ) THEN
              jB = jj+1
              IF ( i.GT.iWb(jj,bi,bj) .AND.
     &            (i.LT.iEb(jj,bi,bj).OR.iEb(jj,bi,bj).EQ.0)
     &           ) tmpMask = arrhFac(i,jB,km,bi,bj)
             ENDIF
             IF ( tmpMask.GT.0. _d 0 ) THEN
              IF ( gPos.EQ.2 .OR. gPos.EQ.3 ) jj = jB
              tmpV = arr(i,jj,k,bi,bj)
              tmpA = arrDx(i,jB,bi,bj)*arrDr(k)*tmpMask
              IF ( noPnts ) THEN
                theMin = tmpV
                theMax = tmpV
                noPnts = .FALSE.
              ENDIF
              theMin = MIN( theMin, tmpV )
              theMax = MAX( theMax, tmpV )
              tileArea(bi,bj) = tileArea(bi,bj) + tmpA
              tileMean(bi,bj) = tileMean(bi,bj) + tmpA*tmpV
              tileVar (bi,bj) = tileVar (bi,bj) + tmpA*tmpV*tmpV
             ENDIF
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( tileArea, theArea, myThid )
c     ENDIF
      IF ( theArea.GT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileMean, theMean, myThid )
        CALL GLOBAL_SUM_TILE_RL( tileVar , theVar , myThid )
        arrStats(0,2) = theArea
        arrStats(1,2) = theMean
        arrStats(2,2) = theVar

        theMean = theMean/theArea
        IF ( noPnts ) theMin = theMean
        theMin = -theMin
        _GLOBAL_MAX_RL(theMin,myThid)
        theMin = -theMin
        IF ( noPnts ) theMax = theMean
        _GLOBAL_MAX_RL(theMax,myThid)
        arrStats(3,2) = theMin
        arrStats(4,2) = theMax

      ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_MONITOR */
#endif /* ALLOW_OBCS */

      RETURN
      END
