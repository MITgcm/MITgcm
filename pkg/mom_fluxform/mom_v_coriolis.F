C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_coriolis.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_CORIOLIS( 
     I        bi,bj,k,uFld,
     U        vCoriolisTerm,
     I        myThid)
      IMPLICIT NONE

C     Calculates Coriolis term, f*U, in V equation

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     == Routine arguments ==
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      INTEGER bi,bj,K
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
        vCoriolisTerm(i,j) =
     &    -0.5*(_fCori(i, j ,bi,bj)+_fCori(i,j-1,bi,bj))
     &   *0.25*( uFld(i, j )+uFld(i+1, j )
     &          +uFld(i,j-1)+uFld(i+1,j-1)
     &         )
       ENDDO
      ENDDO

      RETURN
      END
