C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_del2.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DEL2( 
     I           bi,bj,k,
     I           dTdx,dTdy,
     O           del2,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL dTdx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dTdy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

C     Difference of zonal fluxes ...
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
        del2(i,j)=dTdx(i+1,j)-dTdx(i,j)
       ENDDO
       del2(sNx+Olx,j)=0.
      ENDDO

C     ... add difference of meridional fluxes and divide by volume
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        del2(i,j)=recip_rA(i,j,bi,bj)
     &           *recip_drF(k)
     &           *_recip_hFacC(i,j,k,bi,bj)
     &           *(
     &              del2(i,j)
     &            +( dTdy(i,j+1)-dTdy(i,j) )
     &            )
       ENDDO
      ENDDO

      RETURN
      END
