C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_coriolis.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_CORIOLIS( 
     I        bi,bj,k,vFld,
     U        uCoriolisTerm,
     I        myThid)
      IMPLICIT NONE

C     Calculates Coriolis term, f*V, in U equation

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
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx
        uCoriolisTerm(i,j) =
     &     0.5*( _fCori( i ,j,bi,bj) +
     &           _fCori(i-1,j,bi,bj)  )
     &   *0.25*(
     &     vFld( i ,j)+vFld( i ,j+1)
     &    +vFld(i-1,j)+vFld(i-1,j+1)
     &         )
       ENDDO
      ENDDO

      RETURN
      END
