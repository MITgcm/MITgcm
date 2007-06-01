C $Header: /u/gcmpack/MITgcm/model/src/solve_for_pressure.F,v 1.60 2007/06/01 16:42:16 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
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
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "SOLVE_FOR_PRESSURE.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "SOLVE_FOR_PRESSURE3D.h"
#include "NH_VARS.h"
#endif
#ifdef ALLOW_CD_CODE
#include "CD_CODE_VARS.h"
#endif
#ifdef ALLOW_OBCS
#include "OBCS.h"
#endif

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of SOLVE_FOR_PRESSURE
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
      _RL sumEmP, tileEmP
      LOGICAL putPmEinXvector
      INTEGER numIters, ks
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_NONHYDROSTATIC
      INTEGER kp1
      _RL     wFacKm, wFacKp
      LOGICAL zeroPsNH
#endif
CEOP

#ifdef ALLOW_NONHYDROSTATIC
c       zeroPsNH = .FALSE.
        zeroPsNH = exactConserv
#endif

C deepAtmosphere & useRealFreshWaterFlux: only valid if deepFac2F(ksurf)=1
C anelastic (always Z-coordinate):
C     1) assume that rhoFacF(1)=1 (and ksurf == 1);
C        (this reduces the number of lines of code to modify)
C     2) (a) 2-D continuity eq. compute div. of mass transport (<- add rhoFac)
C        (b) gradient of surf.Press in momentum eq. (<- add 1/rhoFac)
C       => 2 factors cancel in elliptic eq. for Phi_s ,
C       but 1rst factor(a) remains in RHS cg2d_b.

C--   Initialise the Vector solution with etaN + deltaT*Global_mean_PmE
C     instead of simply etaN ; This can speed-up the solver convergence in
C     the case where |Global_mean_PmE| is large.
      putPmEinXvector = .FALSE.
c     putPmEinXvector = useRealFreshWaterFlux

C--   Save previous solution & Initialise Vector solution and source term :
      sumEmP = 0.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef ALLOW_CD_CODE
          etaNm1(i,j,bi,bj) = etaN(i,j,bi,bj)
#endif
          cg2d_x(i,j,bi,bj) = Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
          cg2d_b(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
        IF (useRealFreshWaterFlux) THEN
         tmpFac = freeSurfFac*convertEmP2rUnit
         IF (exactConserv)
     &        tmpFac = freeSurfFac*convertEmP2rUnit*implicDiv2DFlow
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) =
     &       tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)/deltaTMom
          ENDDO
         ENDDO
        ENDIF
        IF ( putPmEinXvector ) THEN
         tileEmP = 0.
         DO j=1,sNy
          DO i=1,sNx
            tileEmP = tileEmP + rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
     &                                       *maskH(i,j,bi,bj)
          ENDDO
         ENDDO
         sumEmP = sumEmP + tileEmP
        ENDIF
       ENDDO
      ENDDO
      IF ( putPmEinXvector ) THEN
        _GLOBAL_SUM_R8( sumEmP, myThid )
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( putPmEinXvector ) THEN
          tmpFac = 0.
          IF (globalArea.GT.0.) tmpFac = freeSurfFac*deltaTfreesurf
     &                          *convertEmP2rUnit*sumEmP/globalArea
          DO j=1,sNy
           DO i=1,sNx
            cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)
     &                        - tmpFac*Bo_surf(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
C- RHS: similar to the divergence of the vertically integrated mass transport:
C       del_i { Sum_k [ rhoFac.(dr.hFac).(dy.deepFac).(u*) ] }  / deltaT
        DO K=Nr,1,-1
         DO j=1,sNy+1
          DO i=1,sNx+1
           uf(i,j) = _dyG(i,j,bi,bj)*deepFacC(k)
     &               *drF(k)*_hFacW(i,j,k,bi,bj)*rhoFacC(k)
           vf(i,j) = _dxG(i,j,bi,bj)*deepFacC(k)
     &               *drF(k)*_hFacS(i,j,k,bi,bj)*rhoFacC(k)
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
        IF ( use3Dsolver .AND. zeroPsNH ) THEN
         DO j=1,sNy
          DO i=1,sNx
           ks = ksurfC(i,j,bi,bj)
           IF ( ks.LE.Nr ) THEN
            cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTfreesurf
     &         * etaH(i,j,bi,bj)
            cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTfreesurf
     &         * etaH(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ELSEIF ( use3Dsolver ) THEN
         DO j=1,sNy
          DO i=1,sNx
           ks = ksurfC(i,j,bi,bj)
           IF ( ks.LE.Nr ) THEN
            cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTfreesurf
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*recip_Bo(i,j,bi,bj) )
            cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTfreesurf
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*recip_Bo(i,j,bi,bj) )
           ENDIF
          ENDDO
         ENDDO
        ELSEIF ( exactConserv ) THEN
#else
        IF ( exactConserv ) THEN
#endif /* ALLOW_NONHYDROSTATIC */
         DO j=1,sNy
          DO i=1,sNx
           ks = ksurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTfreesurf
     &         * etaH(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j=1,sNy
          DO i=1,sNx
           ks = ksurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTfreesurf
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
           cg2d_x(I,OB_Jn(I,bi,bj),bi,bj)=0.
          ENDIF
C Southern boundary
          IF (OB_Js(I,bi,bj).NE.0) THEN
           cg2d_b(I,OB_Js(I,bi,bj),bi,bj)=0.
           cg2d_x(I,OB_Js(I,bi,bj),bi,bj)=0.
          ENDIF
         ENDDO
         DO j=1,sNy
C Eastern boundary
          IF (OB_Ie(J,bi,bj).NE.0) THEN
           cg2d_b(OB_Ie(J,bi,bj),J,bi,bj)=0.
           cg2d_x(OB_Ie(J,bi,bj),J,bi,bj)=0.
          ENDIF
C Western boundary
          IF (OB_Iw(J,bi,bj).NE.0) THEN
           cg2d_b(OB_Iw(J,bi,bj),J,bi,bj)=0.
           cg2d_x(OB_Iw(J,bi,bj),J,bi,bj)=0.
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS */
C-    end bi,bj loops
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
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
c     CALL TIMER_START('CG2D   [SOLVE_FOR_PRESSURE]',myThid)
#ifdef ALLOW_CG2D_NSA
C--   Call the not-self-adjoint version of cg2d
      CALL CG2D_NSA(
     U           cg2d_b,
     U           cg2d_x,
     O           firstResidual,
     O           lastResidual,
     U           numIters,
     I           myThid )
#else /* not ALLOW_CG2D_NSA = default */
      CALL CG2D(
     U           cg2d_b,
     U           cg2d_x,
     O           firstResidual,
     O           lastResidual,
     U           numIters,
     I           myThid )
#endif /* ALLOW_CG2D_NSA */
      _EXCH_XY_R8(cg2d_x, myThid )
c     CALL TIMER_STOP ('CG2D   [SOLVE_FOR_PRESSURE]',myThid)

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
       CALL DEBUG_STATS_RL(1,cg2d_x,'cg2d_x (SOLVE_FOR_PRESSURE)',
     &                        myThid)
      ENDIF
#endif

C- dump CG2D output at monitorFreq (to reduce size of STD-OUTPUT files) :
      IF ( DIFFERENT_MULTIPLE(monitorFreq,myTime,deltaTClock)
     &   ) THEN
       IF ( debugLevel .GE. debLevA ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A34,1PE24.14)') 'cg2d_init_res =',firstResidual
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        WRITE(msgBuf,'(A34,I6)') 'cg2d_iters =',numIters
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        WRITE(msgBuf,'(A34,1PE24.14)') 'cg2d_res =',lastResidual
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        _END_MASTER( myThid )
       ENDIF
      ENDIF

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
      IF ( use3Dsolver ) THEN

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
#endif /* ALLOW_OBCS */

         IF ( usingZCoords ) THEN
C-       Z coordinate: assume surface @ level k=1
           tmpFac = freeSurfFac*deepFac2F(1)
         ELSE
C-       Other than Z coordinate: no assumption on surface level index
           tmpFac = 0.
           DO j=1,sNy
            DO i=1,sNx
              ks = ksurfC(i,j,bi,bj)
              IF ( ks.LE.Nr ) THEN
               cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &              +freeSurfFac*etaN(i,j,bi,bj)/deltaTfreesurf
     &                  *_rA(i,j,bi,bj)*deepFac2F(ks)/deltaTmom
              ENDIF
            ENDDO
           ENDDO
         ENDIF
         K=1
         kp1 = MIN(k+1,Nr)
         wFacKp = deepFac2F(kp1)*rhoFacF(kp1)
         IF (k.GE.Nr) wFacKp = 0.
         DO j=1,sNy
          DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +drF(K)*dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -drF(K)*dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +drF(K)*dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -drF(K)*dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( tmpFac*etaN(i,j,bi,bj)/deltaTfreesurf
     &         -wVel(i,j,kp1,bi,bj)*wFacKp
     &        )*_rA(i,j,bi,bj)/deltaTmom
          ENDDO
         ENDDO
         DO K=2,Nr
          kp1 = MIN(k+1,Nr)
C-       deepFac & rhoFac cancel with the ones in uf[=del_i(Phi)/dx],vf ;
C        both appear in wVel term, but at 2 different levels
          wFacKm = deepFac2F( k )*rhoFacF( k )
          wFacKp = deepFac2F(kp1)*rhoFacF(kp1)
          IF (k.GE.Nr) wFacKp = 0.
          DO j=1,sNy
           DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +drF(K)*dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -drF(K)*dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +drF(K)*dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -drF(K)*dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( wVel(i,j, k ,bi,bj)*wFacKm*maskC(i,j,k-1,bi,bj)
     &         -wVel(i,j,kp1,bi,bj)*wFacKp
     &        )*_rA(i,j,bi,bj)/deltaTmom

           ENDDO
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
#endif /* ALLOW_OBCS */
C-    end bi,bj loops
        ENDDO
       ENDDO

      firstResidual=0.
      lastResidual=0.
      numIters=cg3dMaxIters
      CALL TIMER_START('CG3D   [SOLVE_FOR_PRESSURE]',myThid)
      CALL CG3D(
     U           cg3d_b,
     U           phi_nh,
     O           firstResidual,
     O           lastResidual,
     U           numIters,
     I           myThid )
      _EXCH_XYZ_R8(phi_nh, myThid )
      CALL TIMER_STOP ('CG3D   [SOLVE_FOR_PRESSURE]',myThid)

      IF ( DIFFERENT_MULTIPLE(monitorFreq,myTime,deltaTClock)
     &   ) THEN
       IF ( debugLevel .GE. debLevA ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A34,1PE24.14)') 'cg3d_init_res =',firstResidual
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        WRITE(msgBuf,'(A34,I6)') 'cg3d_iters =',numIters
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        WRITE(msgBuf,'(A34,1PE24.14)') 'cg3d_res =',lastResidual
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
        _END_MASTER( myThid )
       ENDIF
      ENDIF

C--   Update surface pressure (account for NH-p @ surface level) and NH pressure:
      IF ( zeroPsNH ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         IF ( usingZCoords ) THEN
C-       Z coordinate: assume surface @ level k=1
          DO k=2,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             phi_nh(i,j,k,bi,bj) = phi_nh(i,j,k,bi,bj)
     &                           - phi_nh(i,j,1,bi,bj)
            ENDDO
           ENDDO
          ENDDO
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             etaN(i,j,bi,bj) = recip_Bo(i,j,bi,bj)
     &                       *(cg2d_x(i,j,bi,bj) + phi_nh(i,j,1,bi,bj))
             phi_nh(i,j,1,bi,bj) = 0.
           ENDDO
          ENDDO
         ELSE
C-       Other than Z coordinate: no assumption on surface level index
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            ks = ksurfC(i,j,bi,bj)
            IF ( ks.LE.Nr ) THEN
             etaN(i,j,bi,bj) = recip_Bo(i,j,bi,bj)
     &                       *(cg2d_x(i,j,bi,bj) + phi_nh(i,j,ks,bi,bj))
             DO k=Nr,1,-1
              phi_nh(i,j,k,bi,bj) = phi_nh(i,j,k,bi,bj)
     &                            - phi_nh(i,j,ks,bi,bj)
             ENDDO
            ENDIF
           ENDDO
          ENDDO
         ENDIF

        ENDDO
       ENDDO
      ENDIF

      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

#ifdef ALLOW_SHOWFLOPS
      CALL SHOWFLOPS_INSOLVE( myThid)
#endif

      RETURN
      END
