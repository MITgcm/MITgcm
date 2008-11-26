C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_w.F,v 1.5 2008/11/26 15:26:47 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_W( bi, bj, K,
     U                         wFld,
     I                         myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_W                                         |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER bi,bj,K
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER I,J,I_obc,J_obc
#ifdef ALLOW_NONHYDROSTATIC
      _RL obc_mask
#endif

C     Set model variable to zero at the Open Boundaries
      DO I=1-Olx,sNx+Olx
C Northern boundary
        J_obc = OB_Jn(I,bi,bj)
        IF (J_obc.NE.0) wFld(I,J_obc,K,bi,bj) = 0. _d 0
C Southern boundary
        J_obc = OB_Js(I,bi,bj)
        IF (J_obc.NE.0) wFld(I,J_obc,K,bi,bj) = 0. _d 0
      ENDDO
      DO J=1-Oly,sNy+Oly
C Eastern boundary
        I_obc = OB_Ie(J,bi,bj)
        IF (I_obc.NE.0) wFld(I_obc,J,K,bi,bj) = 0. _d 0
C Western boundary
        I_obc = OB_Iw(J,bi,bj)
        IF (I_obc.NE.0) wFld(I_obc,J,K,bi,bj) = 0. _d 0
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
      DO I=1-Olx,sNx+Olx
C Northern boundary
       J_obc=OB_Jn(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc,K,bi,bj)
        wFld(I,J_obc,K,bi,bj)=OBNw(I,K,bi,bj) * obc_mask
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_OBCS_SOUTH
      DO I=1-Olx,sNx+Olx
C Southern boundary
       J_obc=OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        wFld(I,J_obc,K,bi,bj)=OBSw(I,K,bi,bj) * obc_mask
       ENDIF
      ENDDO
#endif

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       I_obc=OB_Ie(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc,J,K,bi,bj)
        wFld(I_obc,J,K,bi,bj)=OBEw(J,K,bi,bj) * obc_mask
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_OBCS_WEST
      DO J=1-Oly,sNy+Oly
C Western boundary
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        wFld(I_obc,J,K,bi,bj)=OBWw(J,K,bi,bj) * obc_mask
       ENDIF
      ENDDO
#endif

#endif /* ALLOW_NONHYDROSTATIC */

#endif /* ALLOW_OBCS */
      RETURN
      END
