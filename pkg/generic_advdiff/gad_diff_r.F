C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_diff_r.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DIFF_R( 
     I           bi,bj,k,
     I           KappaR,
     I           tracer,
     O           dfr,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL KappaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL dfr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,km1
      LOGICAL TOP_LAYER

      km1=max(1,k-1)

      IF (k.eq.1 .OR. k.gt.Nr) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         dfr(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         dfr(i,j) =
     &    -_rA(i,j,bi,bj)
     &     *KappaR(i,j,k)*recip_drC(k)
     &     *(Tracer(i,j,km1,bi,bj)-Tracer(i,j,k,bi,bj))*rkFac
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
