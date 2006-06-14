C $Header: /u/gcmpack/MITgcm/model/src/solve_for_pressure.F,v 1.57 2006/06/14 15:30:14 ce107 Exp $
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
      _RL sumEmP, tileEmP
      LOGICAL putPmEinXvector
      INTEGER numIters
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_NONHYDROSTATIC
      INTEGER ks, kp1
      _RL     maskKp1
      LOGICAL zeroPsNH
#endif
CEOP

#ifdef TIME_PER_TIMESTEP_SFP
CCE107 common block for per timestep timing
C     !TIMING VARIABLES
C     == Timing variables ==
      REAL*8 utnew, utold, stnew, stold, wtnew, wtold
      COMMON /timevars/ utnew, utold, stnew, stold, wtnew, wtold
#endif
#ifdef USE_PAPI_FLOPS_SFP
CCE107 common block for PAPI summary performance
#include <fpapi.h>
      INTEGER*8 flpops, instr
      INTEGER check
      REAL*4 real_time, proc_time, mflops, ipc
      COMMON /papivars/ flpops, instr, real_time, proc_time, mflops, ipc
#else
#ifdef USE_PCL_FLOPS_SFP
CCE107 common block for PCL summary performance
#include <pclh.f>
      INTEGER pcl_counter_list(5), flags, nevents, res, ipcl
      INTEGER*8 i_result(5), descr
      REAL*8 fp_result(5)
      COMMON /pclvars/ i_result, descr, fp_result, pcl_counter_list,
     $     flags, nevents
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 61)
      CHARACTER*22 pcl_counter_name(0:nmaxevents-1)
      COMMON /pclnames/ pcl_counter_name
#endif
#endif

#ifdef ALLOW_NONHYDROSTATIC
c       zeroPsNH = .FALSE.
        zeroPsNH = exactConserv
#endif

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
        IF ( use3Dsolver .AND. zeroPsNH ) THEN
         DO j=1,sNy
          DO i=1,sNx
           ks = ksurfC(i,j,bi,bj)
           IF ( ks.LE.Nr ) THEN
            cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTfreesurf
     &         * etaH(i,j,bi,bj)
            cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTfreesurf
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
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTfreesurf
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*horiVertRatio/gravity )
            cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTfreesurf
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*horiVertRatio/gravity )
           ENDIF
          ENDDO
         ENDDO
        ELSEIF ( exactConserv ) THEN
#else
        IF ( exactConserv ) THEN
#endif /* ALLOW_NONHYDROSTATIC */
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTfreesurf
     &         * etaH(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       -freeSurfFac*_rA(i,j,bi,bj)/deltaTMom/deltaTfreesurf
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
           tmpFac = freeSurfFac
         ELSE
C-       Other than Z coordinate: no assumption on surface level index
           tmpFac = 0. 
           DO j=1,sNy
            DO i=1,sNx
              ks = ksurfC(i,j,bi,bj)
              IF ( ks.LE.Nr ) THEN
               cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &              +freeSurfFac*etaN(i,j,bi,bj)/deltaTfreesurf
     &                          *_rA(i,j,bi,bj)/deltaTmom
              ENDIF
            ENDDO
           ENDDO
         ENDIF
         K=1
         kp1 = MIN(k+1,Nr)
         maskKp1 = 1.
         IF (k.GE.Nr) maskKp1 = 0.
         DO j=1,sNy
          DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +drF(K)*dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -drF(K)*dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +drF(K)*dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -drF(K)*dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( tmpFac*etaN(i,j,bi,bj)/deltaTfreesurf
     &         -wVel(i,j,kp1,bi,bj)*maskKp1
     &        )*_rA(i,j,bi,bj)/deltaTmom
          ENDDO
         ENDDO
         DO K=2,Nr
          kp1 = MIN(k+1,Nr)
          maskKp1 = 1.
          IF (k.GE.Nr) maskKp1 = 0.
          DO j=1,sNy
           DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +drF(K)*dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -drF(K)*dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +drF(K)*dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -drF(K)*dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( wVel(i,j,k  ,bi,bj)*maskC(i,j,k-1,bi,bj)
     &         -wVel(i,j,kp1,bi,bj)*maskKp1
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

#ifdef TIME_PER_TIMESTEP_SFP
CCE107 Time per timestep information
      _BEGIN_MASTER( myThid )
      CALL TIMER_GET_TIME( utnew, stnew, wtnew )
C Only output timing information after the 1st timestep
      IF ( wtold .NE. 0.0D0 ) THEN
        WRITE(msgBuf,'(A34,3F10.6)')
     $        'User, system and wallclock time:', utnew - utold,
     $        stnew - stold, wtnew - wtold
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      ENDIF
      utold = utnew
      stold = stnew
      wtold = wtnew
      _END_MASTER( myThid )
#endif
#ifdef USE_PAPI_FLOPS_SFP
CCE107 PAPI summary performance
      _BEGIN_MASTER( myThid )
#ifdef USE_FLIPS
      call PAPIF_flips(real_time, proc_time, flpops, mflops, check)
#else
      call PAPIF_flops(real_time, proc_time, flpops, mflops, check)
#endif
      WRITE(msgBuf,'(A34,F10.6,A,F10.6)')
     $     'Mflop/s during this timestep:', mflops, ' ', mflops
     $     *proc_time/(real_time + 1E-36)
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#ifdef PAPI_VERSION
      call PAPIF_ipc(real_time, proc_time, instr, ipc, check)
      WRITE(msgBuf,'(A34,F10.6,A,F10.6)')
     $     'IPC during this timestep:', ipc, ' ', ipc*proc_time
     $     /(real_time + 1E-36)
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#endif
      _END_MASTER( myThid )
#else
#ifdef USE_PCL_FLOPS_SFP
CCE107 PCL summary performance
      _BEGIN_MASTER( myThid )
      PCLstop(descr, i_result, fp_result, nevents)
      do ipcl = 1, nevents
         WRITE(msgBuf,'(A22,A26,F10.6)'),
     $        pcl_counter_name(pcl_counter_list(ipcl)),
     $        'during this timestep:', fp_results(ipcl)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      enddo
      PCLstart(descr, pcl_counter_list, nevents, flags)
      _END_MASTER( myThid )
#endif
#endif
      RETURN
      END

#ifdef TIME_PER_TIMESTEP_SFP
CCE107 Initialization of common block for per timestep timing
      BLOCK DATA settimers
C     !TIMING VARIABLES
C     == Timing variables ==
      REAL*8 utnew, utold, stnew, stold, wtnew, wtold
      COMMON /timevars/ utnew, utold, stnew, stold, wtnew, wtold
      DATA utnew, utold, stnew, stold, wtnew, wtold /6*0.0D0/
      END
#endif
#ifdef USE_PAPI_FLOPS_SFP
CCE107 Initialization of common block for PAPI summary performance
      BLOCK DATA setpapis
      INTEGER*8 flpops, instr
      REAL real_time, proc_time, mflops, ipc
      COMMON /papivars/ flpops, instr, real_time, proc_time, mflops, ipc
      DATA flpops, instr, real_time, proc_time, mflops, ipc /2*0,4*0.E0/
      END
#endif
