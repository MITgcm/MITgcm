C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_adv_wu.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_ADV_WU(
     I        bi,bj,k,
     I        uFld,wFld,
     O        advectiveFluxWU,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate advective flux in R direction for U eqn
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL advectiveFluxWU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      IF ( k.GT.Nr .OR.
     &    (k.EQ.1.AND.rigidLid) ) THEN
C     Advective flux = 0  at k=Nr+1 ; = 0 at k=1 if rigid-lid

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        advectiveFluxWU(i,j) = 0.
       ENDDO
      ENDDO

      ELSEIF (k.EQ.1) THEN
C     (linear) Free-surface correction at k=1

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        advectiveFluxWU(i,j) =
     &     0.5*(
     &        wFld( i ,j,k,bi,bj)*rA( i ,j,bi,bj)
     &       +wFld(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)
     &         )*uFld(i,j,k,bi,bj)
       ENDDO
      ENDDO

      ELSE

C     Vertical advection - interior ; assume uFld & wFld are masked  
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        advectiveFluxWU(i,j) =
     &    0.25*(
     &        wFld( i ,j,k,bi,bj)*rA( i ,j,bi,bj)
     &       +wFld(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)
     &         )*( uFld(i,j,k,bi,bj)+uFld(i,j,k-1,bi,bj) )
       ENDDO
      ENDDO

      IF (.NOT. rigidLid) THEN
C     (linear) Free-surface correction at k>1
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          advectiveFluxWU(i,j) = advectiveFluxWU(i,j)
     &     +0.25*(
     &          wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)*
     &          (maskC(i,j,k,bi,bj) - maskC(i,j,k-1,bi,bj))
     &         +wFld(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)*
     &          (maskC(i-1,j,k,bi,bj)-maskC(i-1,j,k-1,bi,bj))
     &           )*uFld(i,j,k,bi,bj)
         ENDDO
        ENDDO
C- endif NOT rigidLid
      ENDIF

      ENDIF

      RETURN
      END
