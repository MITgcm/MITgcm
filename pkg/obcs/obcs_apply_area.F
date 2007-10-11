C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/obcs_apply_area.F,v 1.3 2007/10/11 07:30:05 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_AREA( bi, bj,
     O                          aFld,
     I                          myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_AREA                                      |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER bi,bj
      _RL aFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#if (defined (ALLOW_OBCS) && defined (ALLOW_SEAICE))

C     == Local variables ==
      INTEGER I,J,I_obc,J_obc,K
      _RL obc_mask

      K = 1

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
      DO I=1-Olx,sNx+Olx
C Northern boundary
       J_obc = OB_Jn(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc,K,bi,bj)
        DO J = J_obc, J_obc+Oly
         aFld(I,J)=OBNa(I,bi,bj)*obc_mask
        ENDDO
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_OBCS_SOUTH
      DO I=1-Olx,sNx+Olx
C Southern boundary
       J_obc = OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        DO J = J_obc-Oly, J_obc
         aFld(I,J)=OBSa(I,bi,bj)*obc_mask
        ENDDO
       ENDIF
      ENDDO
#endif

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       I_obc = OB_Ie(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc,J,K,bi,bj)
        DO I = I_obc, I_obc+Olx
         aFld(I,J)=OBEa(J,bi,bj)*obc_mask
        ENDDO
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_OBCS_WEST
      DO J=1-Oly,sNy+Oly
C Western boundary
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        DO I = I_obc-Olx, I_obc
         aFld(I,J)=OBWa(J,bi,bj) * obc_mask
        ENDDO
       ENDIF
      ENDDO
#endif

#endif /* defined (ALLOW_OBCS) && defined (ALLOW_SEAICE) */

      RETURN
      END
