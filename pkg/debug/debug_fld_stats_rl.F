#include "DEBUG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DEBUG_FLD_STATS_RL

C     !INTERFACE:
      SUBROUTINE DEBUG_FLD_STATS_RL(
     I                myNr, arr, exclValue,
     O                theMin, theMax, theMean, theSD,
     I                myThid )

C     *==========================================================*
C     | SUBROUTINE DEBUG_FLD_STATS_RL                            |
C     | o Calculate bare statistics of global array "_RL arr"    |
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myNr      :: 3rd  dimension of input field array
C     arr       :: input field array
C     exclValue :: exclusion value
C     theMin    :: field minimum value
C     theMax    :: field maximun value
C     theMean   :: field averaged value
C     theStD    :: field Standard Deviation
C     myThid    :: my Thread Id number
      INTEGER myNr
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RL exclValue
      _RL theMin
      _RL theMax
      _RL theMean
      _RL theSD
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      LOGICAL noPnts
      _RL tmpVal
      _RL nbPnts, rNbPnts
      _RL theVar
      _RL tileMean(nSx,nSy)
      _RL tileVar (nSx,nSy)
      _RL tileSD  (nSx,nSy)
      _RL tileNbPt(nSx,nSy)
CEOP

      theMin = 0.
      theMax = 0.
      theMean= 0.
      theSD  = 0.
      theVar = 0.
      nbPnts = 0.
      noPnts = .TRUE.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileNbPt(bi,bj) = 0.
        tileMean(bi,bj) = 0.
        tileVar (bi,bj) = 0.
        DO k=1,myNr
         DO j=1,sNy
          DO i=1,sNx
           tmpVal = arr(i,j,k,bi,bj)
           IF ( tmpVal.NE.exclValue .AND. noPnts ) THEN
            theMin = tmpVal
            theMax = tmpVal
            noPnts = .FALSE.
           ENDIF
           IF ( tmpVal.NE.exclValue ) THEN
            theMin = MIN( theMin, tmpVal )
            theMax = MAX( theMax, tmpVal )
            tileNbPt(bi,bj) = tileNbPt(bi,bj) + 1. _d 0
            tileMean(bi,bj) = tileMean(bi,bj) + tmpVal
            tileVar (bi,bj) = tileVar (bi,bj) + tmpVal*tmpVal
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( tileNbPt, nbPnts,  myThid )
      CALL GLOBAL_SUM_TILE_RL( tileMean, theMean, myThid )
c     CALL GLOBAL_SUM_TILE_RL( tileVar , theVar,  myThid )

      IF ( nbPnts.GT.zeroRL ) THEN
       rNbPnts = 1. _d 0/nbPnts
       theMean = theMean*rNbPnts
c      theVar  = theVar *rNbPnts

       IF ( noPnts ) theMin = theMean
       theMin = -theMin
       _GLOBAL_MAX_RL( theMin, myThid )
       theMin = -theMin
       IF ( noPnts ) theMax = theMean
       _GLOBAL_MAX_RL( theMax, myThid )

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         tileSD(bi,bj) = 0.
         DO k=1,myNr
          DO j=1,sNy
           DO i=1,sNx
            tmpVal = arr(i,j,k,bi,bj)
            IF ( tmpVal.NE.exclValue ) THEN
             tileSD(bi,bj) = tileSD(bi,bj)
     &                     + (tmpVal-theMean)*(tmpVal-theMean)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( tileSD, theSD, myThid )

       theSD = SQRT( theSD*rNbPnts )
c      theSD = SQRT( theVar - theMean*theMean )
      ENDIF

      RETURN
      END
