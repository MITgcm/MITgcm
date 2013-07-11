C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_calc_urms.F,v 1.2 2013/07/11 14:33:23 m_bates Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

C     !ROUTINE: EIGENVAL
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_URMS(
     I     iMin, iMax, jMin, jMax,
     I     bi, bj, N2, myThid,
     U     urms)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_URMS
C     | o Calculate the vertical structure of the rms eddy 
C     |   velocity based on baroclinic modal decomposition
C     *==========================================================*
C     \ev

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj    :: tile indices
      INTEGER iMin,iMax,jMin,jMax
      INTEGER bi, bj
      INTEGER myThid
      _RL N2(  1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL urms(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      
#ifdef GM_K3D

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
C     bbs   :: bottom boundary condition (set to zero for now)
C     const :: a constant for each water column
      _RL bbc(   1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL const( 1-Olx:sNx+Olx,1-Oly:sNy+Oly)

C     Constant zero bottom boundary condition
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        bbc(i,j) = zeroRL
       ENDDO
      ENDDO

C     Fit urms to the first baroclinic mode using the SBC and BBC
C     We need at least two cells to do this
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        k = kLowC(i,j,bi,bj)
        IF (k.GT.2) THEN
          const(i,j) = (urms(i,j,k)-urms(i,j,1))
     &         /(modesC(1,i,j,k,bi,bj)-modesC(1,i,j,1,bi,bj))
        ELSE
          const(i,j) = zeroRL
        ENDIF
       ENDDO
      ENDDO      

      DO k=2,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         IF (k.LT.kLowC(i,j,bi,bj)) THEN
           urms(i,j,k) = urms(i,j,1) + 
     &          const(i,j)*(modesC(1,i,j,k,bi,bj)-modesC(1,i,j,1,bi,bj))
         ELSE
           urms(i,j,k)=zeroRL
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     Land, so, we fill with zeros
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        if (kLowC(i,j,bi,bj).EQ.0) urms(i,j,1) = zeroRL
       ENDDO
      ENDDO

      
#endif /* GM_K3D */

      RETURN
      END
