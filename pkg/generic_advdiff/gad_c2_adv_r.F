C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c2_adv_r.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_C2_ADV_R( 
     I           bi,bj,k,
     I           rTrans,
     I           tracer,
     O           wT,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,km1

      km1=max(1,k-1)

      IF ( (rigidLid .AND. k.EQ.1) .OR. k.GT.Nr) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         wT(i,j) = 0.
        ENDDO
       ENDDO
c?    ELSEIF ( rigidLid ) THEN
c?     DO j=1-Oly,sNy+Oly
c?      DO i=1-Olx,sNx+Olx
c?       wT(i,j) = rTrans(i,j)*
c?   &       (Tracer(i,j,k,bi,bj)+Tracer(i,j,kM1,bi,bj))*0.5 _d 0
c?      ENDDO
c?     ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         wT(i,j) =
     &     rTrans(i,j)
     &      *(maskC(i,j,kM1,bi,bj)*
     &        (Tracer(i,j,k,bi,bj)+Tracer(i,j,kM1,bi,bj))*0.5 _d 0
     &       +(maskC(i,j,k,bi,bj)-maskC(i,j,kM1,bi,bj))*
     &        Tracer(i,j,k,bi,bj)
     &       )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
