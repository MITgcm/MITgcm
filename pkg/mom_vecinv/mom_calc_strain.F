C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_calc_strain.F,v 1.1 2001/09/06 14:23:58 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_CALC_STRAIN( 
     I        bi,bj,k,
     I        uFld, vFld, hFacZ,
     O        strain,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_CALC_STRAIN                                      |
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
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO J=2-Oly,sNy+Oly
       DO I=2-Olx,sNx+Olx

C       Horizontal curl of flow field - ignoring lopping factors
        strain(I,J)=
     &    dyu(I,J,bi,bj)*recip_dxv(I,J,bi,bj)*(
     &              vFld( I , J )*recip_dyc( I , J ,bi,bj)
     &             -vFld(I-1, J )*recip_dyc(I-1, J ,bi,bj) )
     &   +dxv(I,J,bi,bj)*recip_dyu(I,J,bi,bj)*(
     &             +uFld( I , J )*recip_dxc( I , J ,bi,bj)
     &             -uFld( I ,J-1)*recip_dxc( I ,J-1,bi,bj) )

C       Horizontal curl of flow field - including lopping factors
        IF (hFacZ(i,j).EQ.0.) THEN
         strain(I,J)=0.
        ENDIF

       ENDDO
      ENDDO
        
C     Special stuff for Cubed Sphere
      IF (useCubedSphereExchange) THEN
        I=1
        J=1
        strain(I,J)=0.
        I=sNx+1
        J=1
        strain(I,J)=0.
        I=1
        J=sNy+1
        strain(I,J)=0.
        I=sNx+1
        J=sNy+1
        strain(I,J)=0.
      ENDIF

      RETURN
      END
