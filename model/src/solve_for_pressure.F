C $Header: /u/gcmpack/MITgcm/model/src/solve_for_pressure.F,v 1.11 1999/05/24 15:42:23 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SOLVE_FOR_PRESSURE( myThid )
C     /==========================================================\
C     | SUBROUTINE SOLVE_FOR_PRESSURE                            |
C     | o Controls inversion of two and/or three-dimensional     |
C     |   elliptic problems for the pressure field.              |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#include "GW.h"
#ifdef ALLOW_OBCS
#include "OBCS.h"
#endif
#include "GRID.h"
#endif

C     == Routine arguments ==
C     myThid - Number of this instance of SOLVE_FOR_PRESSURE
      INTEGER myThid
CEndOfInterface

C     Local variables
      INTEGER i,j,k,bi,bj
#ifdef ALLOW_NONHYDROSTATIC
      _RS uf(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS vf(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
#endif

#ifdef INCLUDE_CD_CODE
C--   Save previous solution.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          cg2d_xNM1(i,j,bi,bj) = cg2d_x(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

C--   Find the surface pressure using a two-dimensional conjugate
C--   gradient solver.
C     see CG2D.h for the interface to this routine.
      CALL CG2D(
     I           cg2d_b,
     U           cg2d_x,
     I           myThid )

      _EXCH_XY_R8(cg2d_x, myThid )

#ifdef ALLOW_NONHYDROSTATIC
      IF ( nonHydrostatic ) THEN

C--   Solve for a three-dimensional pressure term (NH or IGW or both ).
C     see CG3D.h for the interface to this routine.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy+1
          DO i=1,sNx+1
           uf(i,j)=-gBaro*_recip_dxC(i,j,bi,bj)*
     &         (cg2d_x(i,j,bi,bj)-cg2d_x(i-1,j,bi,bj))
           vf(i,j)=-gBaro*_recip_dyC(i,j,bi,bj)*
     &         (cg2d_x(i,j,bi,bj)-cg2d_x(i,j-1,bi,bj))
          ENDDO
         ENDDO

         IF (openBoundaries) THEN
          DO i=1,sNx+1
C Northern boundary
          IF (OB_Jn(I,bi,bj).NE.0) THEN
           uf(I,OB_Jn(I,bi,bj))=0.
           vf(I,OB_Jn(I,bi,bj))=0.
          ENDIF
C Southern boundary
          IF (OB_Js(I,bi,bj).NE.0) THEN
           uf(I,OB_Js(I,bi,bj))=0.
           vf(I,OB_Js(I,bi,bj)+1)=0.
          ENDIF
          ENDDO
          DO j=1,sNy+1
C Eastern boundary
          IF (OB_Ie(J,bi,bj).NE.0) THEN
           uf(OB_Ie(J,bi,bj),J)=0.
           vf(OB_Ie(J,bi,bj),J)=0.
          ENDIF
C Western boundary
          IF (OB_Iw(J,bi,bj).NE.0) THEN
           uf(OB_Iw(J,bi,bj)+1,J)=0.
           vf(OB_Iw(J,bi,bj),J)=0.
          ENDIF
          ENDDO
         ENDIF

         DO K=1,Nr
          DO j=1,sNy
           DO i=1,sNx
c           cg3d_x(i,j,k,bi,bj) = 0.
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +dRF(K)*dYG(i+1,j,bi,bj)*hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -dRF(K)*dYG( i ,j,bi,bj)*hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +dRF(K)*dXG(i,j+1,bi,bj)*hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -dRF(K)*dXG(i, j ,bi,bj)*hFacS(i, j ,k,bi,bj)*vf(i, j )
           ENDDO
          ENDDO
         ENDDO

        ENDDO ! bi
       ENDDO ! bj

       CALL CG3D( myThid )

       _EXCH_XYZ_R8(cg3d_x, myThid )

      ENDIF
#endif

      RETURN
      END
