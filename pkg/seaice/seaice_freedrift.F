#include "SEAICE_OPTIONS.h"

CBOP
      SUBROUTINE SEAICE_FREEDRIFT( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE  SEAICE_FREEDRIFT
C     | o Solve ice approximate momentum equation analytically
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_ALLOW_FREEDRIFT
#ifdef SEAICE_CGRID

C     === Local variables ===
      INTEGER i, j, kSrf, bi, bj

      _RL tmpscal1,tmpscal2,tmpscal3,tmpscal4

      _RL taux_onIce_cntr, tauy_onIce_cntr, uvel_cntr, vvel_cntr
      _RL mIceCor, rhs_x, rhs_y, rhs_n, rhs_a, sol_n, sol_a

      _RL uice_cntr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vice_cntr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      IF ( usingPCoords ) THEN
       kSrf = Nr
      ELSE
       kSrf = 1
      ENDIF

C initialize fields:
C ==================

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uice_fd(i,j,bi,bj)=0. _d 0
          vice_fd(i,j,bi,bj)=0. _d 0
          uice_cntr(i,j,bi,bj)=0. _d 0
          Vice_cntr(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

C preliminary computations:
C =========================
C air-ice stress at cell center
          taux_onIce_cntr=HALF*
     &      (FORCEX0(i,j,bi,bj)+FORCEX0(i+1,j,bi,bj))
          tauy_onIce_cntr=HALF*
     &      (FORCEY0(i,j,bi,bj)+FORCEY0(i,j+1,bi,bj))
C mass of ice per unit area (kg/m2) times coriolis f
          mIceCor=SEAICE_rhoIce*HEFF(i,j,bi,bj)*_fCori(i,j,bi,bj)
C ocean velocity at cell center
          uvel_cntr=HALF*(uvel(i,j,kSrf,bi,bj)+uvel(i+1,j,kSrf,bi,bj))
          vvel_cntr=HALF*(vvel(i,j,kSrf,bi,bj)+vvel(i,j+1,kSrf,bi,bj))
C right hand side of free drift equation:
          rhs_x= -taux_onIce_cntr -mIceCor*vvel_cntr
          rhs_y= -tauy_onIce_cntr +mIceCor*uvel_cntr

C norm of angle of rhs
          tmpscal1=rhs_x*rhs_x + rhs_y*rhs_y
          IF ( tmpscal1.GT.ZERO ) THEN
           rhs_n=SQRT( rhs_x*rhs_x + rhs_y*rhs_y )
           rhs_a=ATAN2(rhs_y,rhs_x)
          ELSE
           rhs_n=0. _d 0
           rhs_a=0. _d 0
          ENDIF

C solve for norm:
C ===============
          IF ( YC(i,j,bi,bj) .LT. ZERO ) THEN
           tmpscal1 = 1. _d 0 /rhoConst/SEAICE_waterDrag_south
          ELSE
           tmpscal1 = 1. _d 0 /rhoConst/SEAICE_waterDrag
          ENDIF
C polynomial coefficients
          tmpscal2= tmpscal1*tmpscal1*mIceCor*mIceCor
          tmpscal3= tmpscal1*tmpscal1*rhs_n*rhs_n
C discriminant
          tmpscal4=tmpscal2*tmpscal2+4. _d 0*tmpscal3
          IF ( tmpscal3.GT.ZERO ) THEN
           sol_n=SQRT(HALF*(SQRT(tmpscal4)-tmpscal2))
          ELSE
           sol_n=0. _d 0
          ENDIF

C solve for angle:
C ================
          IF ( YC(i,j,bi,bj) .LT. ZERO ) THEN
           tmpscal1 = SEAICE_waterDrag_south*rhoConst
          ELSE
           tmpscal1 = SEAICE_waterDrag*rhoConst
          ENDIF

          tmpscal2= tmpscal1*sol_n*sol_n
          tmpscal3= mIceCor*sol_n

          tmpscal4=tmpscal2*tmpscal2 + tmpscal3*tmpscal3
          IF ( tmpscal4.GT.ZERO ) THEN
           sol_a=rhs_a-ATAN2(tmpscal3,tmpscal2)
          ELSE
           sol_a=0. _d 0
          ENDIF

C compute uice, vice at cell center:
C ==================================
          uice_cntr(i,j,bi,bj)=uvel_cntr-sol_n*COS(sol_a)
          vice_cntr(i,j,bi,bj)=vvel_cntr-sol_n*SIN(sol_a)

         ENDDO
        ENDDO
       ENDDO
      ENDDO

C interpolated to velocity points:
C ================================

      CALL EXCH_UV_AGRID_3D_RL(uice_cntr,vice_cntr,.TRUE.,1,myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          uice_fd(i,j,bi,bj)=HALF*
     &      (uice_cntr(i-1,j,bi,bj)+uice_cntr(i,j,bi,bj))
          vice_fd(i,j,bi,bj)=HALF*
     &      (vice_cntr(i,j-1,bi,bj)+vice_cntr(i,j,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_UV_XY_RL( uice_fd, vice_fd, .TRUE., myThid )

C     Apply masks (same/similar to seaice_evp.F/seaice_lsr.F)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uIce_fd(i,j,bi,bj)=uIce_fd(i,j,bi,bj)*SIMaskU(i,j,bi,bj)
          vIce_fd(i,j,bi,bj)=vIce_fd(i,j,bi,bj)*SIMaskV(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_CGRID */
#endif /* SEAICE_ALLOW_FREEDRIFT */
      RETURN
      END
