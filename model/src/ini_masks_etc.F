C $Header: /u/gcmpack/MITgcm/model/src/ini_masks_etc.F,v 1.2 1998/07/02 15:46:21 adcroft Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_MASKS_ETC( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_MASKS_ETC                                 |
C     | o Initialise masks and topography factors                |
C     |==========================================================|
C     | These arrays are used throughout the code and describe   |
C     | the topography of the domain through masks (0s and 1s)   |
C     | and fractional height factors (0<hFac<1). The latter     |
C     | distinguish between the lopped-cell and full-step        |
C     | topographic representations.                             |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CARTESIAN_GRID
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I, J, K

C     Calculate quantities derived from XY depth map
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
C         Inverse of depth
          IF ( h(i,j,bi,bj) .EQ. 0. _d 0 ) THEN
           rH(i,j,bi,bj) = 0. _d 0
          ELSE
           rH(i,j,bi,bj) = 1. _d 0 /  H(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(   rH, myThid )

C     Calculate lopping factor hFacC
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nz
         DO J=1,sNy
          DO I=1,sNx
           IF     ( H(I,J,bi,bj) .LE. zFace(K) ) THEN
C           Below base of domain
            hFacC(I,J,K,bi,bj) = 0.
           ELSEIF ( H(I,J,bi,bj) .GT. zFace(K+1) ) THEN
C           Base of domain is below this cell
            hFacC(I,J,K,bi,bj) = 1.
           ELSE
C           Base of domain is in this cell
C           Set hFac tp the fraction of the cell that is open.
            hFacC(I,J,K,bi,bj) =
     &        (zFace(K)-H(I,J,bi,bj))/(zFace(K)-zFace(K+1))
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XYZ_R4(hFacC , myThid )

C     hFacW and hFacS (at U and V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nz
         DO J=1,sNy
          DO I=1,sNx
           hFacW(I,J,K,bi,bj)=
     &       MIN(hFacC(I,J,K,bi,bj),hFacC(I-1,J,K,bi,bj))
           hFacS(I,J,K,bi,bj)=
     &       MIN(hFacC(I,J,K,bi,bj),hFacC(I,J-1,K,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XYZ_R4(hFacW , myThid )
      _EXCH_XYZ_R4(hFacS , myThid )

C     Masks and reciprocals of hFac[CWS]
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nz
         DO J=1,sNy
          DO I=1,sNx
           IF (HFacC(I,J,K,bi,bj) .NE. 0. D0 ) THEN
            rHFacC(I,J,K,bi,bj) = 1. D0 / HFacC(I,J,K,bi,bj)
           ELSE
            rHFacC(I,J,K,bi,bj) = 0. D0
           ENDIF
           IF (HFacW(I,J,K,bi,bj) .NE. 0. D0 ) THEN
            rHFacW(I,J,K,bi,bj) = 1. D0 / HFacW(I,J,K,bi,bj)
            maskW(I,J,K,bi,bj) = 1. D0
           ELSE
            rHFacW(I,J,K,bi,bj) = 0. D0
            maskW(I,J,K,bi,bj) = 0.0 D0
           ENDIF
           IF (HFacS(I,J,K,bi,bj) .NE. 0. D0 ) THEN
            rHFacS(I,J,K,bi,bj) = 1. D0 / HFacS(I,J,K,bi,bj)
            maskS(I,J,K,bi,bj) = 1. D0
           ELSE
            rHFacS(I,J,K,bi,bj) = 0. D0
            maskS(I,J,K,bi,bj) = 0. D0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XYZ_R4(rHFacC    , myThid )
      _EXCH_XYZ_R4(rHFacW    , myThid )
      _EXCH_XYZ_R4(rHFacS    , myThid )
      _EXCH_XYZ_R4(maskW    , myThid )
      _EXCH_XYZ_R4(maskS    , myThid )

C     Calculate recipricols grid lengths
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          rDxG(I,J,bi,bj)=1.d0/dxG(I,J,bi,bj)
          rDyG(I,J,bi,bj)=1.d0/dyG(I,J,bi,bj)
          rDxC(I,J,bi,bj)=1.d0/dxC(I,J,bi,bj)
          rDyC(I,J,bi,bj)=1.d0/dyC(I,J,bi,bj)
          rDxF(I,J,bi,bj)=1.d0/dxF(I,J,bi,bj)
          rDyF(I,J,bi,bj)=1.d0/dyF(I,J,bi,bj)
          rDxV(I,J,bi,bj)=1.d0/dxV(I,J,bi,bj)
          rDyU(I,J,bi,bj)=1.d0/dyU(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(rDxG, myThid )
      _EXCH_XY_R4(rDyG, myThid )
      _EXCH_XY_R4(rDxC, myThid )
      _EXCH_XY_R4(rDyC, myThid )
      _EXCH_XY_R4(rDxF, myThid )
      _EXCH_XY_R4(rDyF, myThid )
      _EXCH_XY_R4(rDxV, myThid )
      _EXCH_XY_R4(rDyU, myThid )

C
      RETURN
      END
