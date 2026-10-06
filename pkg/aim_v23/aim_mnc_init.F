#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_MNC_INIT

C     !INTERFACE:
      SUBROUTINE AIM_MNC_INIT( myThid )

C     !DESCRIPTION:
C     Initialize MNC variables for AIM.

C     !USES:
      IMPLICIT NONE
#include "AIM_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AIM_PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_SIZE.h"
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC
C     == Local variables ==
      INTEGER i,j, ndim
      INTEGER CW_MAX_LOC
      PARAMETER ( CW_MAX_LOC = 6 )
      CHARACTER*(MNC_MAX_CHAR) dn(CW_MAX_LOC)
      INTEGER dim(CW_MAX_LOC), ib(CW_MAX_LOC), ie(CW_MAX_LOC)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Non-halo-ed variables
      DO i = 1,CW_MAX_LOC
        DO j = 1,MNC_MAX_CHAR
          dn(i)(j:j) = ' '
        ENDDO
        dim(i) = 0
        ib(i) = 0
        ie(i) = 0
      ENDDO

      ndim = 1
      dn(ndim)(1:1) = 'X'
      dim(ndim) = sNx
      ib(ndim)  = 1
      ie(ndim)  = sNx
      ndim = ndim + 1
      dn(ndim)(1:1) = 'Y'
      dim(ndim) = sNy
      ib(ndim)  = 1
      ie(ndim)  = sNy
      ndim = ndim + 1
      dn(ndim)(1:1) = 'T'
      dim(ndim) = -1
      ib(ndim)  = 1
      ie(ndim)  = 1
      CALL MNC_CW_ADD_GNAME('AIM_xyt',  ndim, dim,dn,ib,ie, myThid)
      ndim = 3
      dn(ndim)(1:4) = 'Zaim'
      dim(ndim) = NLEV
      ib(ndim)  = 1
      ie(ndim)  = NLEV
      ndim = ndim + 1
      dn(ndim)(1:1) = 'T'
      dim(ndim) = -1
      ib(ndim)  = 1
      ie(ndim)  = 1
      CALL MNC_CW_ADD_GNAME('AIM_xyzt', ndim, dim,dn,ib,ie, myThid)

      CALL MNC_CW_ADD_VNAME('aim_DRAG0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_DRAG0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SPEED0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SPEED0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_TSR','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_TSR','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_OLR','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_OLR','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SSR0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SSR0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SLR0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SLR0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SHF0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SHF0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_EVAP0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_EVAP0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_PRECNV','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_PRECNV','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_PRECLS','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_PRECLS','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_CLOUDC','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_CLOUDC','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_CLTOP','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_CLTOP','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_CBMF','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_CBMF','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_T0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_T0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_Q0','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_Q0','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_EnPrec','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_EnPrec','units','J/g',myThid)

      CALL MNC_CW_ADD_VNAME('aim_alb10','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_alb10','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_dTsurf1','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_dTsurf1','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_TSWclr','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_TSWclr','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_OLWclr','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_OLWclr','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SSWclr','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SSWclr','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('aim_SLWclr','AIM_xyt',0,0,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('aim_SLWclr','units','---',myThid)

#endif /* ALLOW_MNC */

      RETURN
      END
