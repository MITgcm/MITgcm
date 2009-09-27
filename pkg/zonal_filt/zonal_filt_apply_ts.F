C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filt_apply_ts.F,v 1.2 2009/09/27 23:13:58 jmc Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

CBOP 0
C     !ROUTINE: ZONAL_FILT_APPLY_TS

C     !INTERFACE:
      SUBROUTINE ZONAL_FILT_APPLY_TS(
     U                 tFld, sFld,
     I                 myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R ZONAL_FILT_APPLY_TS
C     | o Apply FFT filter to a latitude circle.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: my Thread Id number
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
CEOP

#ifdef ALLOW_ZONAL_FILT

C     !LOCAL VARIABLES:
      INTEGER bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

        IF ( tempStepping ) CALL ZONAL_FILTER(
     U                       tFld( 1-OLx,1-OLy,1,bi,bj),
     I                       hFacC(1-OLx,1-OLy,1,bi,bj),
     I                       1, sNy, Nr, bi, bj, 1, myThid )
        IF ( saltStepping ) CALL ZONAL_FILTER(
     U                       sFld( 1-OLx,1-OLy,1,bi,bj),
     I                       hFacC(1-OLx,1-OLy,1,bi,bj),
     I                       1, sNy, Nr, bi, bj, 1, myThid )


      ENDDO
      ENDDO

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
