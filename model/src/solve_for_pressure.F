C $Header: /u/gcmpack/MITgcm/model/src/solve_for_pressure.F,v 1.29 2002/02/10 00:39:22 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: SOLVE_FOR_PRESSURE
C     !INTERFACE:
      SUBROUTINE SOLVE_FOR_PRESSURE(myTime, myIter, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SOLVE_FOR_PRESSURE                             
C     | o Controls inversion of two and/or three-dimensional      
C     |   elliptic problems for the pressure field.               
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "SOLVE_FOR_PRESSURE3D.h"
#include "GW.h"
#endif
#ifdef ALLOW_OBCS
#include "OBCS.h"
#endif
#include "SOLVE_FOR_PRESSURE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of SOLVE_FOR_PRESSURE
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,bi,bj
      _RS uf(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS vf(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL firstResidual,lastResidual
      _RL tmpFac
      INTEGER numIters
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C--   Save previous solution & Initialise Vector solution and source term :
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef INCLUDE_CD_CODE
          etaNm1(i,j,bi,bj) = etaN(i,j,bi,bj)
#endif
          cg2d_x(i,j,bi,bj) = Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
          cg2d_b(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
        IF (useRealFreshWaterFlux) THEN
         tmpFac = freeSurfFac
         IF (exactConserv) tmpFac = freeSurfFac*implicDiv2DFlow
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = 
     &       tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)/deltaTMom
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=Nr,1,-1
         DO j=1,sNy+1
          DO i=1,sNx+1
           uf(i,j) = _dyG(i,j,bi,bj)
     &      *drF(k)*_hFacW(i,j,k,bi,bj)
           vf(i,j) = _dxG(i,j,bi,bj)
     &      *drF(k)*_hFacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
         CALL CALC_DIV_GHAT(
     I       bi,bj,1,sNx,1,sNy,K,
     I       uf,vf,
     U       cg2d_b,
     I       myThid)
        ENDDO
       ENDDO
      ENDDO

C--   Add source term arising from w=d/dt (p_s + p_nh)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTMom
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,1,bi,bj)*horiVertRatio/gravity )
           cg3d_b(i,j,1,bi,bj) = cg3d_b(i,j,1,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTMom
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,1,bi,bj)*horiVertRatio/gravity )
          ENDDO
         ENDDO
        ELSEIF ( exactConserv ) THEN
#else
        IF ( exactConserv ) THEN
#endif
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTMom
     &         * etaH(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTMom
     &         * etaN(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_OBCS
        IF (useOBCS) THEN
         DO i=1,sNx
C Northern boundary
          IF (OB_Jn(I,bi,bj).NE.0) THEN
           cg2d_b(I,OB_Jn(I,bi,bj),bi,bj)=0.
          ENDIF
C Southern boundary
          IF (OB_Js(I,bi,bj).NE.0) THEN
           cg2d_b(I,OB_Js(I,bi,bj),bi,bj)=0.
          ENDIF
         ENDDO
         DO j=1,sNy
C Eastern boundary
          IF (OB_Ie(J,bi,bj).NE.0) THEN
           cg2d_b(OB_Ie(J,bi,bj),J,bi,bj)=0.
          ENDIF
C Western boundary
          IF (OB_Iw(J,bi,bj).NE.0) THEN
           cg2d_b(OB_Iw(J,bi,bj),J,bi,bj)=0.
          ENDIF
         ENDDO
        ENDIF
#endif
       ENDDO
      ENDDO

#ifndef EXCLUDE_DEBUGMODE
      IF (debugMode) THEN
       CALL DEBUG_STATS_RL(1,cg2d_b,'cg2d_b (SOLVE_FOR_PRESSURE)',
     &                        myThid)
      ENDIF
#endif

C--   Find the surface pressure using a two-dimensional conjugate
C--   gradient solver.
C     see CG2D.h for the interface to this routine.
      firstResidual=0.
      lastResidual=0.
      numIters=cg2dMaxIters
      CALL CG2D(
     U           cg2d_b,
     U           cg2d_x,
     O           firstResidual,
     O           lastResidual,
     U           numIters,
     I           myThid )
      _EXCH_XY_R8(cg2d_x, myThid )

#ifndef EXCLUDE_DEBUGMODE
      IF (debugMode) THEN
       CALL DEBUG_STATS_RL(1,cg2d_x,'cg2d_x (SOLVE_FOR_PRESSURE)',
     &                        myThid)
      ENDIF
#endif

      _BEGIN_MASTER( myThid )
      WRITE(msgBuf,'(A34,1PE24.14)') 'cg2d_init_res =',firstResidual
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A34,I6)') 'cg2d_iters =',numIters
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A34,1PE24.14)') 'cg2d_res =',lastResidual
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      _END_MASTER( )

C--   Transfert the 2D-solution to "etaN" :
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          etaN(i,j,bi,bj) = recip_Bo(i,j,bi,bj)*cg2d_x(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
      IF ( nonHydrostatic ) THEN

C--   Solve for a three-dimensional pressure term (NH or IGW or both ).
C     see CG3D.h for the interface to this routine.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy+1
          DO i=1,sNx+1
           uf(i,j)=-_recip_dxC(i,j,bi,bj)*
     &         (cg2d_x(i,j,bi,bj)-cg2d_x(i-1,j,bi,bj))
           vf(i,j)=-_recip_dyC(i,j,bi,bj)*
     &         (cg2d_x(i,j,bi,bj)-cg2d_x(i,j-1,bi,bj))
          ENDDO
         ENDDO

#ifdef ALLOW_OBCS
         IF (useOBCS) THEN
          DO i=1,sNx+1
C Northern boundary
          IF (OB_Jn(I,bi,bj).NE.0) THEN
           vf(I,OB_Jn(I,bi,bj))=0.
          ENDIF
C Southern boundary
          IF (OB_Js(I,bi,bj).NE.0) THEN
           vf(I,OB_Js(I,bi,bj)+1)=0.
          ENDIF
          ENDDO
          DO j=1,sNy+1
C Eastern boundary
          IF (OB_Ie(J,bi,bj).NE.0) THEN
           uf(OB_Ie(J,bi,bj),J)=0.
          ENDIF
C Western boundary
          IF (OB_Iw(J,bi,bj).NE.0) THEN
           uf(OB_Iw(J,bi,bj)+1,J)=0.
          ENDIF
          ENDDO
         ENDIF
#endif

         K=1
         DO j=1,sNy
          DO i=1,sNx
           cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +dRF(K)*dYG(i+1,j,bi,bj)*hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -dRF(K)*dYG( i ,j,bi,bj)*hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +dRF(K)*dXG(i,j+1,bi,bj)*hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -dRF(K)*dXG(i, j ,bi,bj)*hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( freeSurfFac*etaN(i,j,bi,bj)/deltaTMom
     &          -wVel(i,j,k+1,bi,bj)
     &        )*_rA(i,j,bi,bj)/deltaTmom
          ENDDO
         ENDDO
         DO K=2,Nr-1
          DO j=1,sNy
           DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +dRF(K)*dYG(i+1,j,bi,bj)*hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -dRF(K)*dYG( i ,j,bi,bj)*hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +dRF(K)*dXG(i,j+1,bi,bj)*hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -dRF(K)*dXG(i, j ,bi,bj)*hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( wVel(i,j,k  ,bi,bj)
     &         -wVel(i,j,k+1,bi,bj)
     &        )*_rA(i,j,bi,bj)/deltaTmom

           ENDDO
          ENDDO
         ENDDO
         K=Nr
         DO j=1,sNy
          DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +dRF(K)*dYG(i+1,j,bi,bj)*hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -dRF(K)*dYG( i ,j,bi,bj)*hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +dRF(K)*dXG(i,j+1,bi,bj)*hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -dRF(K)*dXG(i, j ,bi,bj)*hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( wVel(i,j,k  ,bi,bj)
     &        )*_rA(i,j,bi,bj)/deltaTmom

          ENDDO
         ENDDO

#ifdef ALLOW_OBCS
         IF (useOBCS) THEN
          DO K=1,Nr
          DO i=1,sNx
C Northern boundary
          IF (OB_Jn(I,bi,bj).NE.0) THEN
           cg3d_b(I,OB_Jn(I,bi,bj),K,bi,bj)=0.
          ENDIF
C Southern boundary
          IF (OB_Js(I,bi,bj).NE.0) THEN
           cg3d_b(I,OB_Js(I,bi,bj),K,bi,bj)=0.
          ENDIF
          ENDDO
          DO j=1,sNy
C Eastern boundary
          IF (OB_Ie(J,bi,bj).NE.0) THEN
           cg3d_b(OB_Ie(J,bi,bj),J,K,bi,bj)=0.
          ENDIF
C Western boundary
          IF (OB_Iw(J,bi,bj).NE.0) THEN
           cg3d_b(OB_Iw(J,bi,bj),J,K,bi,bj)=0.
          ENDIF
          ENDDO
          ENDDO
         ENDIF
#endif

        ENDDO ! bi
       ENDDO ! bj

      firstResidual=0.
      lastResidual=0.
      numIters=cg2dMaxIters
      CALL CG3D(
     U           cg3d_b,
     U           phi_nh,
     O           firstResidual,
     O           lastResidual,
     U           numIters,
     I           myThid )
      _EXCH_XYZ_R8(phi_nh, myThid )

      _BEGIN_MASTER( myThid )
      WRITE(msgBuf,'(A34,1PE24.14)') 'cg3d_init_res =',firstResidual
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A34,I6)') 'cg3d_iters =',numIters
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A34,1PE24.14)') 'cg3d_res =',lastResidual
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      _END_MASTER( )

      ENDIF
#endif

      RETURN
      END
