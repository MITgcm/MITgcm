C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filt_apply_uv.F,v 1.2 2009/09/27 23:14:44 jmc Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

CBOP 0
C     !ROUTINE: ZONAL_FILT_APPLY_UV

C     !INTERFACE:
      SUBROUTINE ZONAL_FILT_APPLY_UV(
     U                 uFld, vFld,
     I                 myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R ZONAL_FILT_APPLY_UV
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
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
CEOP

#ifdef ALLOW_ZONAL_FILT

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i, j, k
      _RL uFlow(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

       IF ( select_rStar.GT.0 ) THEN

        DO k=1,Nr
C--     Loop on vertical index k
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uFlow(i,j) = uFld(i,j,k,bi,bj)*hFacW(i,j,k,bi,bj)
          ENDDO
         ENDDO
         CALL ZONAL_FILTER(
     U                     uFlow,
     I                     hFacW(1-OLx,1-OLy,k,bi,bj),
     I                     0, sNy+1, 1, bi, bj, 1, myThid )
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uFld(i,j,k,bi,bj) = uFlow(i,j)*recip_hFacW(i,j,k,bi,bj)
          ENDDO
         ENDDO
C--     end of k loop
        ENDDO

       ELSE

        CALL ZONAL_FILTER(
     U                     uFld( 1-OLx,1-OLy,1,bi,bj),
     I                     hFacW(1-OLx,1-OLy,1,bi,bj),
     I                     0, sNy+1, Nr, bi, bj, 1, myThid )

       ENDIF

       CALL ZONAL_FILTER(
     U                     vFld( 1-OLx,1-OLy,1,bi,bj),
     I                     hFacS(1-OLx,1-OLy,1,bi,bj),
     I                     0, sNy+1, Nr, bi, bj, 2, myThid )

      ENDDO
      ENDDO

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
