#include "PACKAGES_CONFIG.h"
! From CPP_EEMACROS.h, need to find a way to include that here,
! because this is not robust
#define _RS Real*8
#define _RL Real*8
#define _GLOBAL_SUM_RL(a,b) CALL GLOBAL_SUM_R8 ( a, b )

!BOP
!     !ROUTINE: COST_TEMP_F90
!     !INTERFACE:
SUBROUTINE COST_TEMP_F90 ( &
     sNx, sNy, nSx, nSy, OLx, OLy, Nr, &
     myByLo, myByHi, myBxLo, myBxHi, &
     maskC, wtheta, cMeanTheta, &
     objf_temp_tut, &
     myThid )
  !     *==========================================================*
  !     | SUBROUTINE COST_TEMP_F90
  !     | o the subroutine computes the sum of the squared errors
  !     |   relatively to the Levitus climatology
  !     *==========================================================*
  !
  !     !USES:
  IMPLICIT NONE

  !   !INPUT/OUTPUT PARAMETERS:
  ! sNx,sNy,nSx,nSy,OLx,OLy,Nr  - array boundaries (set it SIZE.h)
  ! myByLo,myByHi,myBxLo,myBxHi - bi/bj loop boundaries for myThid
  ! myThid        - Thread number for this instance of the routine.
  ! maskC         - 3D mask for C-points (defined in GRID.h)
  ! wtheta        - 1D weights (defined in cost_local.h)
  ! cMeanTheta    - mean temperature over time (defined in cost.h)
  ! objf_temp_tut - contribution to objective function (defined in cost.h)
  INTEGER sNx, sNy, nSx, nSy, OLx, OLy, Nr
  INTEGER myByLo, myByHi, myBxLo, myBxHi
  INTEGER myThid
  _RS     maskC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
  _RL     wtheta                                (Nr,nSx,nSy)
  _RL     cMeanTheta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
  _RL     objf_temp_tut                            (nSx,nSy)
  ! This only works with TAF is TAF_FORTRAN_VERS > F77
  ! INTEGER, INTENT(in) :: sNx,sNy,nSx,nSy,OLx,OLy,Nr
  ! INTEGER, INTENT(in) :: myByLo,myByHi,myBxLo,myBxHi,myThid
  ! _RS, INTENT(in)     :: maskC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
  ! _RL, INTENT(in)     :: wtheta                                (Nr,nSx,nSy)
  ! _RL, INTENT(in)     :: cMeanTheta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
  ! _RL, INTENT(out)    :: objf_temp_tut(nSx,nSy)

#ifdef ALLOW_COST
  !   !LOCAL VARIABLES:
  ! loop indices
  INTEGER :: i, j, k
  INTEGER :: bi, bj
  INTEGER :: Nk
  ! auxilliary variables
  _RL :: locfc,tmp
  ! theta/temperature data read from file
  _RL :: thetalev(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
!EOP

  Nk = 2 ! only loop over top Nk levels
  ! Read annual mean Levitus temperature

  CALL READ_FLD_XYZ_RL('lev_t_an.bin',' ',thetalev,0,myThid)

  ! Total number of wet temperature points
  tmp  = 0. _d 0
  DO bj=myByLo,myByHi
   DO bi=myBxLo,myBxHi
    DO k=1,Nk
     DO j=1,sNy
      DO i=1,sNx
       tmp = tmp + maskC(i,j,k,bi,bj)
      ENDDO
     ENDDO
    ENDDO
   ENDDO
  ENDDO
  _GLOBAL_SUM_RL( tmp , myThid )
  IF ( tmp > 0. ) tmp = 1. _d 0 / tmp

  DO bj=myByLo,myByHi
   DO bi=myBxLo,myBxHi
    locfc = 0.D0
    DO k=1,Nk
     DO j=1,sNy
      DO i=1,sNx
       locfc = locfc + tmp*maskC(i,j,k,bi,bj)* &
            wtheta(k,bi,bj)* &
            ( cMeanTheta(i,j,k,bi,bj) - thetalev(i,j,k,bi,bj) )**2
      ENDDO
     ENDDO
    ENDDO

    objf_temp_tut(bi,bj) = locfc
    !print*,'objf_temp_tut =',locfc,startTime,endTime,tmp

   ENDDO
  ENDDO
#endif /* ALLOW_COST */

  RETURN
  ! This only works with TAF is TAF_FORTRAN_VERS > F77
END ! SUBROUTINE COST_TEMP_F90
