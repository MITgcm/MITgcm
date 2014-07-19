C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/atm_to_ocn_mapxyr8.F,v 1.4 2014/07/19 23:01:48 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_TO_OCN_MAPXYR8(
     I           aNx, aNy,
     I           fld_atm,
     I           oNx, oNy,
     O           fld_ocn )
C     /==========================================================\
C     | SUBROUTINE ATM_TO_OCN_MAPXYR8                            |
C     | o Maps atmos. field to ocean field for XY 64-bit float   |
C     |   field.                                                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Routine arguments ==
C     fld_atm - Atmos. grid field
C     aNx     - Atmos. grid global extent in X
C     aNy     - Atmos. grid global extent in Y
C     fld_ocn - Ocean grid field (atmos field will be mapped to this
C                                 field).
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
      DO J=1,oNy
       DO I=1,oNx
        fld_ocn(I,J)=0.
       ENDDO
      ENDDO
C     Now copy intersection of _atm and _ocn into
C     _ocn.
      DO J=1,MIN(aNy,oNy)
       DO I=1,MIN(aNx,oNx)
        fld_ocn(I,J)=fld_atm(I,J)
       ENDDO
      ENDDO

      RETURN
      END
