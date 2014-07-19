C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/ocn_to_atm_mapxyr8.F,v 1.4 2014/07/19 23:01:48 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE OCN_TO_ATM_MAPXYR8(
     I           oNx, oNy,
     I           fld_ocn,
     I           aNx, aNy,
     O           fld_atm )
C     /==========================================================\
C     | SUBROUTINE OCN_TO_ATM_MAPXYR8                            |
C     | o Maps ocean field to atmos. field for XY 64-bit float   |
C     |   field.                                                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Routine arguments ==
C     fld_atm - Atmos. grid field (ocean field will be mapped to this
C               grid).
C     aNx     - Atmos. grid global extent in X
C     aNy     - Atmos. grid global extent in Y
C     fld_ocn - Ocean grid field
C     oNx     - Ocean grid global extent in X
C     oNy     - Ocean grid global extent in Y
      INTEGER aNx
      INTEGER aNy
      Real*8 fld_atm(aNx, aNy)
      INTEGER oNx
      INTEGER oNy
      Real*8 fld_ocn(oNx, oNy)
CEndOfInterface

C     == Local variables ==
      INTEGER I,J

C     Set to zero everywhere
      DO J=1,aNy
       DO I=1,aNx
        fld_atm(I,J)=0.
       ENDDO
      ENDDO
C     Now copy intersection of _atm and _ocn into
C     _atm.
      DO J=1,MIN(aNy,oNy)
       DO I=1,MIN(aNx,oNx)
        fld_atm(I,J)=fld_ocn(I,J)
       ENDDO
      ENDDO

      RETURN
      END
