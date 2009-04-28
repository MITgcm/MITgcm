C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_stats_latbnd_rl.F,v 1.8 2009/04/28 18:16:53 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_STATS_LATBND_RL

C     !INTERFACE:
      SUBROUTINE MON_STATS_LATBND_RL(
     I     myNr, mskNr, kLoc, nSepBnd, ySepBnd,
     I     arr, arrMask, arrhFac, arrArea, arrY, arrDr,
     O     theMin,theMax,theMean,theVar,theVol,
     I     myThid )

C     !DESCRIPTION:
C     Calculate bare statistics of global array "\_RL arr" on each
C     Latitude band (given by \texttt{ySepBnd}).

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      INTEGER  NLATBND
      EXTERNAL NLATBND

C     !INPUT PARAMETERS:
C     nSepBnd :: Number of latitude bands
C     ySepBnd :: Southern latitude egde (from 2 to nSepBnd, 1 is not used)
      INTEGER myNr, mskNr, kLoc
      INTEGER nSepBnd
      _RS ySepBnd(nSepBnd)
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,mskNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,mskNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      _RL theMin(nSepBnd)
      _RL theMax(nSepBnd)
      _RL theMean(nSepBnd)
      _RL theVar(nSepBnd)
      _RL theVol(nSepBnd)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi,bj,i,j,k,n
      INTEGER km, k1, k2
      LOGICAL noPnts(Ny)
      _RL tmpVal
      _RL tmpVol
      INTEGER nSepDim
      PARAMETER ( nSepDim = 200 )
      _RL tileVol (nSx,nSy,nSepDim)
      _RL tileMean(nSx,nSy,nSepDim)
      _RL tileVar (nSx,nSy,nSepDim)

C-    Check local Dim
      IF ( nSepBnd .GT. nSepDim ) THEN
        WRITE(msgBuf,'(A,I6,A)')
     &  'MON_STATS_LATBND_RL: local array Dim (nSepDim=',nSepDim,
     &  ' ) too small'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A,I6)')
     &  'MON_STATS_LATBND_RL: Need nSepDim to be at least =', nSepBnd
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R MON_STATS_LATBND_RL'
      ENDIF

      IF ( kLoc.EQ.0 ) THEN
        k1 = 1
        k2 = myNr
      ELSE
        k1 = kLoc
        k2 = kLoc
      ENDIF

      DO n=1,nSepBnd
       noPnts(n)=.TRUE.
       theMin(n)=0.
       theMax(n)=0.
       theMean(n)=0.
       theVar(n)=0.
       theVol(n)=0.
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO n=1,nSepBnd
         tileVol (bi,bj,n) = 0.
         tileMean(bi,bj,n) = 0.
         tileVar (bi,bj,n) = 0.
        ENDDO
        DO k=k1,k2
         km = MIN(k,mskNr)
         DO j=1,sNy
          DO i=1,sNx
           n = NLATBND(nSepBnd, ySepBnd, arrY(i,j,bi,bj) )
           tmpVal=arr(i,j,k,bi,bj)
           IF (arrMask(i,j,km,bi,bj).NE.0. .AND. noPnts(n)) THEN
            theMin(n)=tmpVal
            theMax(n)=tmpVal
            noPnts(n)=.FALSE.
           ENDIF
           IF (arrMask(i,j,km,bi,bj).NE.0.) THEN
            theMin(n)=MIN(theMin(n),tmpVal)
            theMax(n)=MAX(theMax(n),tmpVal)
            tmpVol = arrArea(i,j,bi,bj)*arrhFac(i,j,km,bi,bj)*arrDr(k)
     &                                 *arrMask(i,j,km,bi,bj)
            tileVol (bi,bj,n) = tileVol (bi,bj,n) + tmpVol
            tileMean(bi,bj,n) = tileMean(bi,bj,n) + tmpVol*tmpVal
            tileVar (bi,bj,n) = tileVar (bi,bj,n) + tmpVol*tmpVal*tmpVal
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO n=1,nSepBnd
       CALL GLOBAL_SUM_TILE_RL( tileVol(1,1,n) , theVol(n) , myThid )
       CALL GLOBAL_SUM_TILE_RL( tileMean(1,1,n), theMean(n), myThid )
       CALL GLOBAL_SUM_TILE_RL( tileVar(1,1,n) , theVar(n) , myThid )
      ENDDO

      DO n=1,nSepBnd
       IF (theVol(n).GT.0.) THEN
        theMean(n)= theMean(n)/theVol(n)
        theVar(n) = theVar(n) /theVol(n)
        theVar(n) = theVar(n) -theMean(n)*theMean(n)
        IF ( noPnts(n) ) theMin(n) = theMean(n)
        theMin(n) = -theMin(n)
        _GLOBAL_MAX_RL(theMin(n), myThid)
        theMin(n)=-theMin(n)
        IF ( noPnts(n) ) theMax(n) = theMin(n)
        _GLOBAL_MAX_RL(theMax(n), myThid)
       ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: NLATBND

C     !INTERFACE:
      INTEGER FUNCTION NLATBND( nBnd, yBnd, yLoc )
      IMPLICIT NONE

C     !DESCRIPTION:
C     Find the latidude band of yLoc in nSep strip

C     !INPUT PARAMETERS:
C     nBnd :: Number of latitude bands
C     yBnd :: latitude of southern boundary (for each lat. band)
C     yLoc :: current latitude
      INTEGER nBnd
      _RS yBnd(nBnd)
      _RS yLoc
CEOP

C     !LOCAL VARIABLES:
      INTEGER n

      NLATBND = 1
      DO n=2,nBnd
       IF (yLoc .GT. yBnd(n)) NLATBND = n
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
