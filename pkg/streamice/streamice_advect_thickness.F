#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_ADVECT_THICKNESS ( myThid,myIter,time_step )

C     *============================================================*
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     *============================================================*
C     |                                                            |
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_ADV.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif

      INTEGER myThid, myIter
      _RL time_step

#ifdef ALLOW_STREAMICE

      INTEGER i, j, bi, bj, Gi, Gj, k
      _RL thick_bd, uflux, vflux, max_icfl, loc_icfl
      _RL time_step_full, time_step_rem
      _RL sec_per_year, time_step_loc, MR, SMB, TMB, irho
      _RL BCVALX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BCVALY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS BCMASKX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS BCMASKY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL utrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vtrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL h_after_uflux_SI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL h_after_vflux_SI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL hflux_x_SI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL hflux_y_SI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      CHARACTER*(MAX_LEN_MBUF) msgBuf

      CALL TIMER_START ('STREAMICE_ADVECT_THICKNESS',myThid)

      sec_per_year = 365.*86400.

      time_step_loc = time_step / sec_per_year
      time_step_full = time_step_loc
      time_step_rem = time_step_loc
      PRINT *, "time_step_loc ", time_step_loc

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE streamice_hmask  = comlev1, key=ikey_dynamics
#endif

#ifdef ALLOW_OPENAD
      DO k=1,streamice_smooth_thick_adjoint
      CALL STREAMICE_SMOOTH_ADJOINT_FIELD (
     O            H_streamice,
     I            myThid)
      ENDDO
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy+1
         DO i=1,sNx+1

          H_streamice_prev(i,j,bi,bj) =
     &     H_streamice(i,j,bi,bj)

          hflux_x_SI (i,j,bi,bj) = 0. _d 0
          hflux_y_SI (i,j,bi,bj) = 0. _d 0

          IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
           h_after_uflux_SI(i,j,bi,bj) = 0. _d 0
           h_after_vflux_SI(i,j,bi,bj) = 0. _d 0
          ELSE
           h_after_uflux_SI(i,j,bi,bj) = H_streamice(i,j,bi,bj)
           h_after_vflux_SI(i,j,bi,bj) = H_streamice(i,j,bi,bj)
          ENDIF

          IF (STREAMICE_ufacemask(i,j,bi,bj).eq.3.0) THEN
           BCMASKX(i,j,bi,bj) = 3.0
           BCVALX(i,j,bi,bj) = h_ubdry_values_SI(i,j,bi,bj)
           IF (STREAMICE_vel_ext_cgrid) THEN
            utrans(i,j,bi,bj) = u_streamice_ext(i,j,bi,bj)
           ELSEIF (STREAMICE_vel_ext) THEN
            utrans(i,j,bi,bj) = .5 * (
     &       u_streamice_ext(i,j,bi,bj)+u_streamice_ext(i,j+1,bi,bj))
           ELSE
            utrans(i,j,bi,bj) = .5 * (
     &       u_streamice(i,j,bi,bj)+u_streamice(i,j+1,bi,bj))
           ENDIF
          ELSEIF (STREAMICE_ufacemask(i,j,bi,bj).eq.4.0) THEN
           IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
            uflux = u_flux_bdry_SI(i,j,bi,bj)
            BCMASKX(i,j,bi,bj) = 3.0
            BCVALX(i,j,bi,bj) = uflux
            utrans(i,j,bi,bj) = 1.0
           ELSEIF (STREAMICE_hmask(i-1,j,bi,bj).eq.1.0) THEN
            uflux = u_flux_bdry_SI(i,j,bi,bj)
            BCMASKX(i,j,bi,bj) = 3.0
            BCVALX(i,j,bi,bj) = uflux
            utrans(i,j,bi,bj) = -1.0
           ENDIF
          ELSEIF (.not.(
     &     STREAMICE_hmask(i,j,bi,bj).eq.1.0.OR.
     &     STREAMICE_hmask(i-1,j,bi,bj).eq.1.0)) THEN
           BCMASKX(i,j,bi,bj) = 0.0
           BCVALX(i,j,bi,bj) = 0. _d 0
           utrans(i,j,bi,bj) = 0. _d 0
          ELSE
           BCMASKX(i,j,bi,bj) = 0.0
           BCVALX(i,j,bi,bj) = 0. _d 0
           IF (STREAMICE_vel_ext_cgrid) THEN
            utrans(i,j,bi,bj) = u_streamice_ext(i,j,bi,bj)
           ELSEIF (STREAMICE_vel_ext) THEN
            utrans(i,j,bi,bj) = .5 * (
     &       u_streamice_ext(i,j,bi,bj)+u_streamice_ext(i,j+1,bi,bj))
           ELSE
            utrans(i,j,bi,bj) = .5 * (
     &       u_streamice(i,j,bi,bj)+u_streamice(i,j+1,bi,bj))
           ENDIF
          ENDIF

          IF (STREAMICE_vfacemask(i,j,bi,bj).eq.3.0) THEN
           BCMASKy(i,j,bi,bj) = 3.0
           BCVALy(i,j,bi,bj) = h_vbdry_values_SI(i,j,bi,bj)
           IF (STREAMICE_vel_ext_cgrid) THEN
            vtrans(i,j,bi,bj) = v_streamice_ext(i,j,bi,bj)
           ELSEIF (STREAMICE_vel_ext) THEN
            vtrans(i,j,bi,bj) = .5 * (
     &       v_streamice_ext(i,j,bi,bj)+v_streamice_ext(i+1,j,bi,bj))
           ELSE
            vtrans(i,j,bi,bj) = .5 * (
     &       v_streamice(i,j,bi,bj)+v_streamice(i+1,j,bi,bj))
           ENDIF
          ELSEIF (STREAMICE_vfacemask(i,j,bi,bj).eq.4.0) THEN
           IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
            vflux = v_flux_bdry_SI(i,j,bi,bj)
            BCMASKY(i,j,bi,bj) = 3.0
            BCVALY(i,j,bi,bj) = vflux
            vtrans(i,j,bi,bj) = 1.0
           ELSEIF (STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) THEN
            vflux = v_flux_bdry_SI(i,j,bi,bj)
            BCMASKY(i,j,bi,bj) = 3.0
            BCVALY(i,j,bi,bj) = vflux
            vtrans(i,j,bi,bj) = -1.0
           ENDIF
          ELSEIF (.not.(
     &     STREAMICE_hmask(i,j,bi,bj).eq.1.0.OR.
     &     STREAMICE_hmask(i,j-1,bi,bj).eq.1.0)) THEN
           BCMASKY(i,j,bi,bj) = 0.0
           BCVALY(i,j,bi,bj) = 0. _d 0
           vtrans(i,j,bi,bj) = 0. _d 0
          ELSE
           BCMASKy(i,j,bi,bj) = 0.0
           BCVALy(i,j,bi,bj) = 0. _d 0
           IF (STREAMICE_vel_ext_cgrid) THEN
            vtrans(i,j,bi,bj) = v_streamice_ext(i,j,bi,bj)
           ELSEIF (STREAMICE_vel_ext) THEN
            vtrans(i,j,bi,bj) = .5 * (
     &       v_streamice_ext(i,j,bi,bj)+v_streamice_ext(i+1,j,bi,bj))
           ELSE
            vtrans(i,j,bi,bj) = .5 * (
     &       v_streamice(i,j,bi,bj)+v_streamice(i+1,j,bi,bj))
           ENDIF
          ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL(utrans,myThid)
      _EXCH_XY_RL(vtrans,myThid)
      _EXCH_XY_RS(BCMASKx,myThid)
      _EXCH_XY_RS(BCMASKy,myThid)
      _EXCH_XY_RL(BCVALX,myThid)
      _EXCH_XY_RL(BCVALY,myThid)
      _EXCH_XY_RL(h_after_uflux_SI,myThid)
      _EXCH_XY_RL(h_after_vflux_SI,myThid)
      _EXCH_XY_RL(hflux_x_SI,myThid)
      _EXCH_XY_RL(hflux_y_SI,myThid)

#ifndef ALLOW_AUTODIFF

      max_icfl = 1.e-20

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF (streamice_hmask(i,j,bi,bj).eq.1.0) THEN
           loc_icfl=max(abs(utrans(i,j,bi,bj)),
     &                  abs(utrans(i+1,j,bi,bj))) / dxF(i,j,bi,bj)
           loc_icfl=max(loc_icfl,max(abs(vtrans(i,j,bi,bj)),
     &                  abs(vtrans(i,j+1,bi,bj))) / dyF(i,j,bi,bj))
           if (loc_icfl.gt.max_icfl) then
            max_icfl = loc_icfl
           ENDIF
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_MAX_R8 (max_icfl, myThid)

#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE streamice_hmask  = comlev1, key=ikey_dynamics
CADJ STORE H_streamice  = comlev1, key=ikey_dynamics
#endif

#ifndef ALLOW_AUTODIFF
      do while (time_step_rem .gt. 1.e-15)
       time_step_loc = min (
     &  streamice_cfl_factor / max_icfl,
     &  time_step_rem )
       if (time_step_loc .lt. time_step_full) then
        PRINT *, "TAKING PARTIAL TIME STEP", time_step_loc
       endif
#endif /* ALLOW_AUTODIFF */

      CALL STREAMICE_ADV_FLUX_FL_X ( myThid ,
     I   utrans ,
     I   H_streamice ,
     I   BCMASKX,
     I   BCVALX,
     O   hflux_x_SI,
     I   time_step_loc )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-3,sNy+3
         DO i=1,sNx
          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
          IF (((Gj .ge. 1) .and. (Gj .le. Ny))
     &       .or.STREAMICE_NS_PERIODIC) THEN

          IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
           h_after_uflux_SI (i,j,bi,bj) = H_streamice(i,j,bi,bj) -
     &      (hflux_x_SI(i+1,j,bi,bj)*dyG(i+1,j,bi,bj) -
     &        hflux_x_SI(i,j,bi,bj)*dyG(i,j,bi,bj))
     &        * recip_rA (i,j,bi,bj) * time_step_loc
           IF ( h_after_uflux_SI (i,j,bi,bj).le.0.0) THEN
            PRINT *, "h neg after x", i,j,hflux_x_SI(i+1,j,bi,bj),
     &        hflux_x_SI(i,j,bi,bj)
           ENDIF
          ENDIF

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE streamice_hmask  = comlev1, key=ikey_dynamics
#endif

!      CALL STREAMICE_ADVECT_THICKNESS_Y ( myThid,
!     O   hflux_y_SI,
!     O   h_after_vflux_SI,
!     I   time_step_loc )

      CALL STREAMICE_ADV_FLUX_FL_Y ( myThid ,
     I   vtrans ,
     I   h_after_uflux_si ,
     I   BCMASKY,
     I   BCVALY,
     O   hflux_y_SI,
     I   time_step_loc )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
          IF (((Gj .ge. 1) .and. (Gj .le. Ny))
     &       .or.STREAMICE_EW_PERIODIC) THEN

          IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
           h_after_vflux_SI (i,j,bi,bj) = h_after_uflux_SI(i,j,bi,bj) -
     &      (hflux_y_SI(i,j+1,bi,bj)*dxG(i,j+1,bi,bj) -
     &        hflux_y_SI(i,j,bi,bj)*dxG(i,j,bi,bj)) *
     &        recip_rA (i,j,bi,bj) * time_step_loc
           IF ( h_after_vflux_SI (i,j,bi,bj).le.0.0) THEN
            PRINT *, "h neg after y", i,j,hflux_y_SI(i,j+1,bi,bj),
     &        hflux_y_SI(i,j,bi,bj)
           ENDIF

          ENDIF
         ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
           H_streamice (i,j,bi,bj) =
     &      h_after_vflux_SI (i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

! NOTE: AT THIS POINT H IS NOT VALID ON OVERLAP!!!

      if (streamice_move_front) then
      CALL STREAMICE_ADV_FRONT (
     &  myThid, time_step_loc,
     &  hflux_x_SI, hflux_y_SI )
      endif

#ifdef ALLOW_STREAMICE_2DTRACER
      CALL STREAMICE_ADVECT_2DTRACER(
     &  myThid,
     &  myIter,
     &  time_step,
     &  uTrans,
     &  vTrans,
     &  BCMASKx,
     &  BCMASKy )
#endif

#ifndef ALLOW_AUTODIFF
      time_step_rem = time_step_rem - time_step_loc
      enddo
#endif /* ALLOW_AUTODIFF */

! NOW WE APPLY MELT RATES !!
! THIS MAY BE MOVED TO A SEPARATE SUBROUTINE
      irho = 1./streamice_density

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
          IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0 .or.
     &       STREAMICE_hmask(i,j,bi,bj).eq.2.0) THEN

           IF (STREAMICE_allow_cpl) THEN
#ifdef ALLOW_SHELFICE
            MR = -1. * (1.-float_frac_streamice(i,j,bi,bj)) *
     &        shelfIceFreshWaterFlux(i,j,bi,bj) * irho *
     &        sec_per_year

#else
            STOP 'SHELFICE IS NOT ENABLED'
#endif
           ELSE
              MR = (1.-float_frac_streamice(i,j,bi,bj)) *
     &             (BDOT_STREAMICE(i,j,bi,bj) +
     &              BDOT_pert(i,j,bi,bj))
           ENDIF

           SMB = ADOT_STREAMICE(i,j,bi,bj)
           TMB = SMB - MR
           IF ((TMB.lt.0.0) .and.
     &         (MR * time_step_loc .gt.
     &          H_streamice (i,j,bi,bj))) THEN
                H_streamice (i,j,bi,bj) = 0. _d 0
                STREAMICE_hmask(i,j,bi,bj) = 0.0
               PRINT *, "GOT HERE melted away! ", i,j
           ELSE
               H_streamice (i,j,bi,bj) =
     &          H_streamice (i,j,bi,bj) + TMB * time_step_loc
           ENDIF

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL(H_streamice,myThid)

      WRITE(msgBuf,'(A)') 'END STREAMICE_ADVECT_THICKNESS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
      CALL TIMER_STOP ('STREAMICE_ADVECT_THICKNESS',myThid)

#endif
      RETURN
      END
