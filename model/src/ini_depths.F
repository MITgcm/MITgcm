C $Header: /u/gcmpack/MITgcm/model/src/ini_depths.F,v 1.4 1998/06/09 16:48:02 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_DEPTHS( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_DEPTHS                                    |
C     | o Initialise map of model depths                         |
C     |==========================================================|
C     | The depths of the bottom of the model is specified in    |
C     | terms of an XY map with one depth for each column of     |
C     | grid cells. Depths do not have to coincide with the      |
C     | model levels. The model's lopping algorithm makes it     |
C     | possible to represent arbitrary depths.                  |
C     | The mode depths map also influences the models topology  |
C     | By default the model domain wraps around in X and Y.     |
C     | This default doubly periodic topology is "supressed"     |
C     | if a depth map is defined which closes off all wrap      |
C     | around flow.                                             |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     xG, yG - Global coordinate location.
C     zG
C     zUpper - Work arrays for upper and lower 
C     zLower   cell-face heights.
C     phi    - Temporary scalar
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     zUpper - Temporary arrays holding z coordinates of
C     zLower   upper and lower faces.
C     I,J,K
      _RL    xG, yG, zG
      _RL    phi
      _RL    zUpper(Nz), zLower(Nz)
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J, K

      _BARRIER
      IF ( bathyFile .EQ. ' '  ) THEN
C      Set up a flat bottom box with doubly periodic topology.
C      H is the basic variable from which other terms are derived. It
C      is the term that would be set from an external file for a 
C      realistic problem.
       phi = 0. D0
       DO K=1,Nz
        phi = phi+delZ(K)
       ENDDO
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
C          Default depth of full domain
           H(i,j,bi,bj) = phi
C          Test for eastern edge
           IF ( iG .EQ. nX ) H(i,j,bi,bj) = 0.
C          Test for northern edge
           IF ( jG .EQ. nY ) H(i,j,bi,bj) = 0.
C          Island
           IF ( iG .EQ. 1 .AND. 
     &          jG .EQ. 24 ) H(i,j,bi,bj) = 0.75*phi
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XY_RS( bathyFile, ' ', H, 0, myThid )
       _END_MASTER(myThid)
      ENDIF
      _EXCH_XY_R4(    H, myThid )

C--   Cacluate quantities derived from XY depth map
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
C         Inverse of depth
          IF ( h(i,j,bi,bj) .EQ. 0. _d 0 ) THEN
           rH(i,j,bi,bj) = 0. _d 0
          ELSE
           rH(i,j,bi,bj) = 1. _d 0 /  h(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(   rH, myThid )

C--   Now calculate "lopping" factors hFac.
      zG = delZ(1)*0.5 D0
      zFace(1) = 0
      rDzC(1)  = 2. _d 0/delZ(1)
      DO K=1,Nz-1
       saFac(K) = 1. D0
       zC(K)      = zG
       zG         = zG + (delZ(K)+delZ(K+1))*0.5 D0
       zFace(K+1) = zFace(K)+delZ(K)
       rDzC(K+1)  = 2. _d 0/(delZ(K)+delZ(K+1))
      ENDDO
      zC(Nz)      = zG
      zFace(Nz+1) = zFace(Nz)+delZ(Nz)
      DO K=1,Nz
       zUpper(K) = zFace(K)
       zLower(K) = zFace(K+1)
       dzF(K)    = delz(K)
       rdzF(K)   = 1.d0/dzF(K)
      ENDDO
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nz
         DO J=1,sNy
          DO I=1,sNx
           hFacC(I,J,K,bi,bj) = 1.
           IF     ( H(I,J,bi,bj) .LE. zUpper(K) ) THEN
C           Below base of domain
            hFacC(I,J,K,bi,bj) = 0.
           ELSEIF ( H(I,J,bi,bj) .GT. zLower(K) ) THEN
C           Base of domain is below this cell
            hFacC(I,J,K,bi,bj) = 1.
           ELSE
C           Base of domain is in this cell
C           Set hFac tp the fraction of the cell that is open.
            phi = zUpper(K)-H(I,J,bi,bj)
            hFacC(I,J,K,bi,bj) = phi/(zUpper(K)-zLower(K))
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XYZ_R4(hFacC , myThid )

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

C--   Calculate recipricols of hFacC, hFacW and hFacS
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO K=1, Nz
         DO J=1,sNy
          DO I=1,sNx
           rhFacC(I,J,K,bi,bj)=0. D0
           if (hFacC(I,J,K,bi,bj).ne.0.)
     &            rhFacC(I,J,K,bi,bj)=1. D0 /hFacC(I,J,K,bi,bj)
           rhFacW(I,J,K,bi,bj)=0. D0
           if (hFacW(I,J,K,bi,bj).ne.0.)
     &            rhFacW(I,J,K,bi,bj)=1. D0 /hFacW(I,J,K,bi,bj)
           rhFacS(I,J,K,bi,bj)=0. D0
           if (hFacS(I,J,K,bi,bj).ne.0.)
     &            rhFacS(I,J,K,bi,bj)=1. D0 /hFacS(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XYZ_R4(rhFacC , myThid )
      _EXCH_XYZ_R4(rhFacW , myThid )
      _EXCH_XYZ_R4(rhFacS , myThid )
C
      RETURN
      END
