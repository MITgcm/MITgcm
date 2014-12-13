C $Header: /u/gcmpack/MITgcm/pkg/streamice/adstreamice_cg_solve.F,v 1.7 2014/12/13 14:13:11 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE ADSTREAMICE_CG_SOLVE(
     U                               U_state,    ! velocities - need to be recalc ed
     I                               cg_Bu,      ! adjoint of vel (input)
     U                               V_state,    ! velocities - need to be recalc ed
     I                               cg_Bv,      ! adjoint of vel (input)
     I                               Bu_state,   ! to recalc velocities
     U                               cg_Uin,     ! adjoint of RHS (output)
     I                               Bv_state,   ! to recalc velocities
     U                               cg_Vin,     ! adjoint of RHS (output)
     I                               A_uu,       ! section of matrix that multiplies u and projects on u
     U                               adA_uu,     ! adjoint of matrix coeffs (output)
     I                               A_uv,       ! section of matrix that multiplies v and projects on u
     U                               adA_uv,     ! adjoint of matrix coeffs (output)
     I                               A_vu,       ! section of matrix that multiplies u and projects on v
     U                               adA_vu,     ! adjoint of matrix coeffs (output)
     I                               A_vv,       ! section of matrix that multiplies v and projects on v
     U                               adA_vv,     ! adjoint of matrix coeffs (output)
     I                               tolerance,
     I                               maxiters,
     I                               myThid )
C     *============================================================*
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     *============================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"

C     !INPUT/OUTPUT ARGUMENTS
C     cg_Uin, cg_Vin - input and output velocities
C     cg_Bu, cg_Bv - driving stress
      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL U_state (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL V_state (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Bu_state (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Bv_state (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL
     & A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & adA_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & adA_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & adA_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & adA_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL tolerance
      INTEGER maxiters
      INTEGER myThid

C     !LOCAL VARIABLES
      INTEGER i, j, bi, bj, cg_halo, conv_flag, tmpiter
      INTEGER iter, is, js, ie, je, colx, coly, k
      _RL Utemp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Vtemp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UtempSt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VtempSt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dot_p1, dot_p2, alpha_k, beta_k, resid, resid_0
      _RL dot_p1_tile (nSx,nSy)
      _RL dot_p2_tile (nSx,nSy)
      _RL ad_tolerance
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

!       iters = streamice_max_cg_iter

#ifdef ALLOW_STREAMICE

      WRITE(msgBuf,'(A)') 'CALLING MANUAL CG ADJOINT'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

!      print *, "GOT HERE mythid=", mythid, tolerance

      conv_flag = 0
      ad_tolerance = 1.e-14

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Utemp (i,j,bi,bj) =
     &     cg_Uin (i,j,bi,bj)
          Vtemp (i,j,bi,bj) =
     &     cg_Vin (i,j,bi,bj)
          UtempSt (i,j,bi,bj) =
     &     U_state (i,j,bi,bj)
          VtempSt (i,j,bi,bj) =
     &     V_state (i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

!      print *, "GOT HERE 2 mythid=", mythid, tolerance

#ifndef ALLOW_OPENAD
      CALL STREAMICE_CG_SOLVE(
     &  U_state,
     &  V_state,
     &  Bu_state,
     &  Bv_state,
     &  A_uu,
     &  A_uv,
     &  A_vu,
     &  A_vv,
     &  tolerance,
     &  tmpiter,
     &  maxiters,
     &  myThid )
#endif

!      print *, "GOT HERE 3 mythid=", mythid, tolerance

      tmpiter = 0

      _EXCH_XY_RL( cg_Bu, myThid )
      _EXCH_XY_RL( cg_Bv, myThid )

      CALL STREAMICE_CG_SOLVE(
     &  cg_Uin,
     &  cg_Vin,
     &  cg_Bu,
     &  cg_Bv,
     &  A_uu,
     &  A_uv,
     &  A_vu,
     &  A_vv,
     &  ad_tolerance,
     &  tmpiter,
     &  maxiters,
     &  myThid )

!      print *, "GOT HERE 4 mythid=", mythid, tolerance

      _EXCH_XY_RL( cg_Uin, myThid )
      _EXCH_XY_RL( cg_Vin, myThid )
      _EXCH_XY_RL( U_state, myThid )
      _EXCH_XY_RL( V_state, myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          DO colx=-1,1
           DO coly=-1,1

            if (STREAMICE_umask(i,j,bi,bj).eq.1) then
             if (STREAMICE_umask(i+colx,j+coly,bi,bj).eq.1) then
                adA_uu(i,j,bi,bj,colx,coly) =
     &           adA_uu(i,j,bi,bj,colx,coly) -
     &           cg_Uin(i,j,bi,bj) *
     &           U_state(i+colx,j+coly,bi,bj)

             endif
             if (STREAMICE_vmask(i+colx,j+coly,bi,bj).eq.1) then
                adA_uv(i,j,bi,bj,colx,coly) =
     &           adA_uv(i,j,bi,bj,colx,coly) -
     &           cg_Uin(i,j,bi,bj) *
     &           V_state(i+colx,j+coly,bi,bj)
             endif
            endif

            if (STREAMICE_vmask(i,j,bi,bj).eq.1) then
             if (STREAMICE_umask(i+colx,j+coly,bi,bj).eq.1) then
                adA_vu(i,j,bi,bj,colx,coly) =
     &           adA_vu(i,j,bi,bj,colx,coly) -
     &           cg_Vin(i,j,bi,bj) *
     &           U_state(i+colx,j+coly,bi,bj)
             endif
             if (STREAMICE_vmask(i+colx,j+coly,bi,bj).eq.1) then
                adA_vv(i,j,bi,bj,colx,coly) =
     &           adA_vv(i,j,bi,bj,colx,coly) -
     &           cg_Vin(i,j,bi,bj) *
     &           V_state(i+colx,j+coly,bi,bj)
             endif
            endif

           enddo
          enddo
         enddo
        enddo
       enddo
      enddo

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          if (i.lt.1.or.i.gt.sNx.or.
     &        j.lt.1.or.j.gt.sNy) then
           cg_Uin (i,j,bi,bj) = 0.0
           cg_Vin (i,j,bi,bj) = 0.0

           DO colx=-1,1
            DO coly=-1,1
             ada_uu(i,j,bi,bj,colx,coly)=0.0
             ada_uv(i,j,bi,bj,colx,coly)=0.0
             ada_vu(i,j,bi,bj,colx,coly)=0.0
             ada_vv(i,j,bi,bj,colx,coly)=0.0
            enddo
           enddo

          endif
          cg_Uin (i,j,bi,bj) =
     &     cg_Uin (i,j,bi,bj) +
     &     Utemp (i,j,bi,bj)
          cg_Vin (i,j,bi,bj) =
     &     cg_Vin (i,j,bi,bj) +
     &     Vtemp (i,j,bi,bj)
          cg_bu (i,j,bi,bj) = 0.
          cg_bv (i,j,bi,bj) = 0.
          U_state (i,j,bi,bj) =
     &     UtempSt (i,j,bi,bj)
          V_state (i,j,bi,bj) =
     &     VtempSt (i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      WRITE(msgBuf,'(A,I5,A)') 'DONE WITH MANUAL CG ADJOINT:',tmpiter,
     & 'ITERS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

#endif
      RETURN
      END

