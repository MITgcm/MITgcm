C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_get_atmconfig.F,v 1.1 2013/12/02 22:13:23 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AIM
# include "AIM_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: ATM_GET_ATMCONFIG

C !INTERFACE:
      SUBROUTINE ATM_GET_ATMCONFIG(
     O                              landMask,
     I                              myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ATM_GET_ATMCONFIG
C     | o get atmos. config (land/sea mask) from other pkgs
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#ifdef ALLOW_AIM
# include "AIM_SIZE.h"
#else
# include "SIZE.h"
#endif

#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
c#include "ATMIDS.h"
c#include "ATMCPL.h"
#ifdef ALLOW_AIM
# include "AIM_FFIELDS.h"
#endif /* ALLOW_AIM */

C !INPUT/OUTPUT PARAMETERS:
C     landMask :: land / sea mask (=1 : full land; =0 : full ocean grid cell)
C     myThid   :: Thread number for this instance of the routine
      _RL landMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C !LOCAL VARIABLES:
      INTEGER i,j,bi,bj
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   Initialise to zero (no land, full oceanic):
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           landMask(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

#ifdef ALLOW_AIM
        IF ( useAIM ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           landMask(i,j,bi,bj) = aim_landFr(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_AIM */

       ENDDO
      ENDDO

      RETURN
      END
