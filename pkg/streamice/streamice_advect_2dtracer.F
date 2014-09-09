C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_advect_2dtracer.F,v 1.3 2014/09/09 23:09:21 jmc Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_ADVECT_2DTRACER (
     &  myThid,
     &  myIter,
     &  time_step,
     &  uTrans,
     &  vTrans,
     &  bcMaskx,
     &  bcMasky)

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

      INTEGER myThid, myIter, Gi, Gj
      _RL time_step
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS bcMaskx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS bcMasky(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL trac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE
#ifdef ALLOW_STREAMICE_2DTRACER

      INTEGER i, j, bi, bj
      _RL thick_bd
      _RL sec_per_year, time_step_loc, MR, SMB, TMB
      _RL BCVALX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BCVALY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL xtracflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ytracflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef STREAMICE_TRACER_AB
      _RL GAD_trac_2dNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      sec_per_year = 365.*86400.

      time_step_loc = time_step / sec_per_year

      PRINT *, "time_step_loc ", time_step_loc

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-3,sNy+3
         DO i=1-3,sNx+3

!          H_streamice_prev(i,j,bi,bj) =
!     &     H_streamice(i,j,bi,bj)

          ytracflux (i,j,bi,bj) = 0. _d 0
          xtracflux (i,j,bi,bj) = 0. _d 0
#ifdef STREAMICE_TRACER_AB
          GAD_trac_2dNm1(i,j,bi,bj)=GAD_trac_2d(i,j,bi,bj)
          GAD_trac_2d (i,j,bi,bj) = 0. _d 0
#endif

          IF (STREAMICE_ufacemask(i,j,bi,bj).eq.3.0) THEN
           BCVALX(i,j,bi,bj) = trac2d_ubdry_values_SI(i,j,bi,bj)
          ELSEIF (STREAMICE_ufacemask(i,j,bi,bj).eq.4.0) THEN
           BCVALX(i,j,bi,bj) = trac2d_ubdry_values_SI(i,j,bi,bj)
          ENDIF

          IF (STREAMICE_vfacemask(i,j,bi,bj).eq.3.0) THEN
           BCVALy(i,j,bi,bj) = trac2d_vbdry_values_SI(i,j,bi,bj)
          ELSEIF (STREAMICE_vfacemask(i,j,bi,bj).eq.4.0) THEN
           BCVALy(i,j,bi,bj) = trac2d_vbdry_values_SI(i,j,bi,bj)
          ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL(BCVALX,myThid)
      _EXCH_XY_RL(BCVALY,myThid)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE trac2d  = comlev1, key=ikey_dynamics
#endif

      CALL STREAMICE_ADV_FLUX_FL_X ( myThid ,
     I   uTrans ,
     I   trac2d ,
     I   BCMASKX,
     I   BCVALX,
     O   xtracflux,
     I   time_step_loc)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-3,sNy+3
         DO i=1,sNx
          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
          IF (((Gj .ge. 1) .and. (Gj .le. Ny))
     &       .or.STREAMICE_NS_PERIODIC) THEN

          IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0 .or.
     &        STREAMICE_hmask(i,j,bi,bj).eq.2.0) THEN

#ifdef STREAMICE_TRACER_AB
           GAD_trac_2d(i,j,bi,bj) = GAD_trac_2d(i,j,bi,bj) -
#else
           trac2d(i,j,bi,bj) = trac2d(i,j,bi,bj) -
#endif
     &      ((xtracflux(i+1,j,bi,bj)*dyG(i+1,j,bi,bj) -
     &       xtracflux(i,j,bi,bj)*dyG(i,j,bi,bj)) *
     &        recip_rA (i,j,bi,bj) -
     &     trac2d(i,j,bi,bj) *
     &      (utrans(i+1,j,bi,bj)*dyG(i+1,j,bi,bj)-
     &       utrans(i,j,bi,bj)*dyG(i,j,bi,bj)) *
     &       recip_rA(i,j,bi,bj))
#ifndef STREAMICE_TRACER_AB
     &      * time_step_loc
#endif
          ENDIF
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_RL(utrans,myThid)

#ifdef ALLOW_AUTODIFF_TAMC
# ifndef STREAMICE_TRACER_AB
CADJ STORE trac2d  = comlev1, key=ikey_dynamics
# endif
#endif

      CALL STREAMICE_ADV_FLUX_FL_Y ( myThid ,
     I   vTrans ,
     I   trac2d ,
     I   BCMASKy,
     I   BCVALy,
     O   ytracflux,
     I   time_step_loc)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j

          IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0 .or.
     &        STREAMICE_hmask(i,j,bi,bj).eq.2.0) THEN
!           IF (Gi.eq.34.and.Gj.eq.94) THEN
!            print *, "GOT HERE YFLUX", ytracflux(i,j,bi,bj),
!     &        ytracflux(i,j+1,bi,bj),trac2d(i,j,bi,bj),
!     &        vtrans(i,j,bi,bj), vtrans(i,j+1,bi,bj),
!     &        bcmasky(i,j,bi,bj)
!           ENDIF

#ifdef STREAMICE_TRACER_AB
           GAD_trac_2d(i,j,bi,bj) = GAD_trac_2d(i,j,bi,bj) -
#else
           trac2d(i,j,bi,bj) = trac2d(i,j,bi,bj) -
#endif
     &      ((ytracflux(i,j+1,bi,bj)*dxG(i,j+1,bi,bj) -
     &       ytracflux(i,j,bi,bj)*dxG(i,j,bi,bj)) *
     &        recip_rA (i,j,bi,bj) -
     &      (vtrans(i,j+1,bi,bj)*dxG(i,j+1,bi,bj) -
     &       vtrans(i,j,bi,bj)*dxG(i,j,bi,bj)) *
     &     trac2d(i,j,bi,bj) *
     &       recip_rA(i,j,bi,bj))
#ifndef STREAMICE_TRACER_AB
     &       * time_step_loc
#endif
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef STREAMICE_TRACER_AB

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

          trac2d(i,j,bi,bj) = trac2d(i,j,bi,bj) + time_step_loc *
     &      GAD_trac_2d(i,j,bi,bj)

          IF (myIter.eq.0) THEN
           trac2d(i,j,bi,bj) = trac2d(i,j,bi,bj) + time_step_loc *
     &      (.5+.01) *
     &      (GAD_trac_2d(i,j,bi,bj) - GAD_trac_2dNm1(i,j,bi,bj))
          ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif

      _EXCH_XY_RL (trac2d, myThid)

      WRITE(msgBuf,'(A)') 'END STREAMICE_ADVECT_THICKNESS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

#endif
#endif
      RETURN
      END
