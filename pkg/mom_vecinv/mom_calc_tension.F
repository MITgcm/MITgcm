C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_calc_tension.F,v 1.1 2001/09/06 14:23:58 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_CALC_TENSION( 
     I        bi,bj,k,
     I        uFld, vFld,
     O        tension,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_CALC_TENSION                                     |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
C     == Routine arguments ==
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO J=1-Oly,sNy+Oly-1
       DO I=1-Olx,sNx+Olx-1

        tension(I,J)= 
     &    ( uFld(i+1, j )*recip_dyg(i+1, j ,bi,bj)
     &     -uFld( i , j )*recip_dyg( i , j ,bi,bj) )
     &   *(dyf(i,j,bi,bj)*recip_dxf(i,j,bi,bj))
     &   -( vFld( i ,j+1)*recip_dxg( i ,j+1,bi,bj)
     &     -vFld( i , j )*recip_dxg( i , j ,bi,bj) )
     &   *(dxf(i,j,bi,bj)*recip_dyf(i,j,bi,bj))
       ENDDO
      ENDDO
        
      RETURN
      END
