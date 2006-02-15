C $Header: /u/gcmpack/MITgcm/pkg/seaice/advect.F,v 1.15 2006/02/15 22:26:11 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE advect( UICE,VICE,HEFF,HEFFM,myThid )
C     /==========================================================\
C     | SUBROUTINE advect                                        |
C     | o Calculate ice advection                                |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_PARAMS.h"
CML#include "SEAICE_GRID.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL UICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL VICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      INTEGER K3
      _RL  DELTT

      _RL UI   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy,nSx,nSy)
      _RL VI   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy,nSx,nSy)
      _RL DIFFA(1-OLx:sNx+OLx, 1-OLy:sNy+OLy,nSx,nSy)

C NOW DECIDE IF BACKWARD EULER OR LEAPFROG
      IF(LAD.EQ.1) THEN
C LEAPFROG
         DELTT=SEAICE_deltaTtherm*TWO
         K3=3
      ELSE
C BACKWARD EULER
         DELTT=SEAICE_deltaTtherm
         K3=2
      ENDIF

C NOW REARRANGE HS

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UI(I,J,bi,bj)=UICE(I,J,1,bi,bj)
          VI(I,J,bi,bj)=VICE(I,J,1,bi,bj)
         ENDDO
        ENDDO

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,3,bi,bj)=HEFF(I,J,2,bi,bj)
          HEFF(I,J,2,bi,bj)=HEFF(I,J,1,bi,bj)
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff  = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

C NOW GO THROUGH STANDARD CONSERVATIVE ADVECTION
      IF ( .NOT. SEAICEuseFluxForm ) THEN
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
CML   This formulation gives the same result as the original code on a
CML   lat-lon-grid, but may not be accurate on irregular grids
          HEFF(I,J,1,bi,bj)=HEFF(I,J,K3,bi,bj)
     &         -DELTT*(
     &         (HEFF(I  ,J  ,2,bi,bj)+HEFF(I+1,J  ,2,bi,bj))
     &         *( UI(I+1,J+1,  bi,bj)+  UI(I+1,J  ,  bi,bj))-
     &         (HEFF(I  ,J  ,2,bi,bj)+HEFF(I-1,J  ,2,bi,bj))
     &         *( UI(I  ,J+1,  bi,bj)+  UI(I  ,J  ,  bi,bj)))
     &     *(QUART * _recip_dxF(I,J,bi,bj))
     &         -DELTT*(
     &         (HEFF(I  ,J  ,2,bi,bj)+HEFF(I  ,J+1,2,bi,bj))
     &         *( VI(I  ,J+1,  bi,bj)+  VI(I+1,J+1,  bi,bj))
     &         * _dxG(I  ,J+1,bi,bj) -
     &         (HEFF(I  ,J  ,2,bi,bj)+HEFF(I  ,J-1,2,bi,bj))
     &         *( VI(I  ,J  ,  bi,bj)+  VI(I+1,J  ,  bi,bj))
     &         * _dxG(I,J,bi,bj))
     &     *(QUART * _recip_dyF(I,J,bi,bj) * _recip_dxF(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ELSE
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
C--   Use flux form for MITgcm compliance, unfortunately changes results
          HEFF(I,J,1,bi,bj)=HEFF(I,J,K3,bi,bj)
     &         -DELTT * QUART * (
     &        + _dyG(I+1,J,bi,bj) *
     &         (HEFF(I  ,J  ,2,bi,bj)+HEFF(I+1,J  ,2,bi,bj))
     &         *( UI(I+1,J+1,  bi,bj)+  UI(I+1,J  ,  bi,bj))
     &        - _dyG(I,J,bi,bj) *
     &         (HEFF(I  ,J  ,2,bi,bj)+HEFF(I-1,J  ,2,bi,bj))
     &         *( UI(I  ,J+1,  bi,bj)+  UI(I  ,J  ,  bi,bj))
     &        + _dxG(I  ,J+1,bi,bj) *
     &         (HEFF(I  ,J  ,2,bi,bj)+HEFF(I  ,J+1,2,bi,bj))
     &         *( VI(I  ,J+1,  bi,bj)+  VI(I+1,J+1,  bi,bj))
     &        - _dxG(I  ,J  ,bi,bj)*
     &         (HEFF(I  ,J  ,2,bi,bj)+HEFF(I  ,J-1,2,bi,bj))
     &         *( VI(I  ,J  ,  bi,bj)+  VI(I+1,J  ,  bi,bj))
     &        )*recip_rA(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDIF

      _BARRIER
      CALL SEAICE_EXCH ( HEFF, myThid )
      _BARRIER

      IF (LAD .EQ. 2) THEN

C NOW DO BACKWARD EULER CORRECTION
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             HEFF(I,J,3,bi,bj)=HEFF(I,J,2,bi,bj)
             HEFF(I,J,2,bi,bj)=HALF*(HEFF(I,J,1,bi,bj)
     &                              +HEFF(I,J,2,bi,bj))
            ENDDO
           ENDDO
          ENDDO
         ENDDO

C NOW GO THROUGH STANDARD CONSERVATIVE ADVECTION
         IF ( .NOT. SEAICEuseFluxForm ) THEN
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO J=1,sNy
            DO I=1,sNx
CML   This formulation gives the same result as the original code on a
CML   lat-lon-grid, but may not be accurate on irregular grids
             HEFF(I,J,1,bi,bj)=HEFF(I,J,3,bi,bj)
     &            -DELTT*(
     &            (HEFF(I  ,J  ,2,bi,bj)+HEFF(I+1,J  ,2,bi,bj))
     &            *( UI(I+1,J+1,  bi,bj)+  UI(I+1,J  ,  bi,bj))-
     &            (HEFF(I  ,J  ,2,bi,bj)+HEFF(I-1,J  ,2,bi,bj))
     &            *( UI(I  ,J+1,  bi,bj)+  UI(I  ,J  ,  bi,bj)))
     &     *(QUART * _recip_dxF(I,J,bi,bj))
     &            -DELTT*(
     &            (HEFF(I  ,J  ,2,bi,bj)+HEFF(I  ,J+1,2,bi,bj))
     &            *( VI(I  ,J+1,  bi,bj)+  VI(I+1,J+1,  bi,bj))
     &            * _dxG(I,J+1,bi,bj)
     &            -(HEFF(I  ,J  ,2,bi,bj)+HEFF(I  ,J-1,2,bi,bj))
     &            *(  VI(I  ,J  ,  bi,bj)+  VI(I+1,J  ,  bi,bj))
     &            * _dxG(I,J,bi,bj))
     &     *(QUART * _recip_dyF(I,J,bi,bj) * _recip_dxF(I,J,bi,bj))
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         ELSE
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO J=1,sNy
            DO I=1,sNx
C--   Use flux form for MITgcm compliance, unfortunately changes results
             HEFF(I,J,1,bi,bj)=HEFF(I,J,3,bi,bj)
     &            -DELTT * QUART * (
     &           + _dyG(I+1,J  ,bi,bj) *
     &            (HEFF(I  ,J  ,2,bi,bj)+HEFF(I+1,J  ,2,bi,bj))
     &            *( UI(I+1,J+1,  bi,bj)+  UI(I+1,J  ,  bi,bj))
     &           - _dyG(I  ,J  ,bi,bj) *
     &            (HEFF(I  ,J  ,2,bi,bj)+HEFF(I-1,  J,2,bi,bj))
     &            *( UI(I  ,J+1,  bi,bj)+  UI(I  ,J  ,  bi,bj))
     &           + _dxG(I  ,J+1,bi,bj) *
     &            (HEFF(I  ,J  ,2,bi,bj)+HEFF(I  ,J+1,2,bi,bj))
     &            *( VI(I  ,J+1,  bi,bj)+  VI(I+1,J+1,  bi,bj))
     &           - _dxG(I  ,J  ,bi,bj) *
     &            (HEFF(I  ,J  ,2,bi,bj)+HEFF(I  ,J-1,2,bi,bj))
     &            *( VI(I  ,J  ,  bi,bj)+  VI(I+1,J  ,  bi,bj))
     &            )*recip_rA(I,J,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         ENDIF

         _BARRIER
         CALL SEAICE_EXCH( HEFF, myThid )
         _BARRIER

C     NOW FIX UP H(I,J,2)
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             HEFF(I,J,2,bi,bj)=HEFF(I,J,3,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

      ENDIF

C NOW DO DIFFUSION ON H(I,J,3)
C NOW CALCULATE DIFFUSION COEF ROUGHLY
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DIFFA(I,J,bi,bj)=
     &         DIFF1*MIN( _dxF(I,J,bi,bj), _dyF(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL DIFFUS(HEFF,DIFFA,HEFFM,DELTT, myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,1,bi,bj)=(HEFF(I,J,1,bi,bj)+HEFF(I,J,3,bi,bj))
     &                      *HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C NOW CALCULATE DIFFUSION COEF ROUGHLY
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DIFFA(I,J,bi,bj)=
     &         -(MIN( _dxF(I,J,bi,bj),  _dyF(I,J,bi,bj)))**2/DELTT
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL DIFFUS(HEFF,DIFFA,HEFFM,DELTT, myThid)
 
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,1,bi,bj)=(HEFF(I,J,1,bi,bj)+HEFF(I,J,3,bi,bj))
     &                      *HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
