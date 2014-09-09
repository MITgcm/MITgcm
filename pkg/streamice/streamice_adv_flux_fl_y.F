C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_adv_flux_fl_y.F,v 1.3 2014/09/09 23:01:46 jmc Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE STREAMICE_ADV_FLUX_FL_Y ( myThid ,
     I   VADV ,
     I   TRAC ,
     I   BC_FACEMASK,
     I   BC_YVALUES,
     O   YFLUX,
     I   time_step )

      IMPLICIT NONE

C     O   hflux_x ! flux per unit width across face
C     O   h
C     I   time_step

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
!#include "GAD_FLUX_LIMITER.h"

      INTEGER myThid
      _RL VADV         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL TRAC         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS BC_FACEMASK  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BC_YVALUES   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL YFLUX        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL time_step

#ifdef ALLOW_STREAMICE

C     LOCAL VARIABLES

      INTEGER i, j, bi, bj, Gi, Gj, k
      _RL vface, phi, cfl, Cr, rdenom, d0, d1, psi
      _RL stencil (-1:1)
      LOGICAL H0_valid(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
                        ! there are valid cells to calculate a
                        ! slope-limited 2nd order flux
      _RL SLOPE_LIMITER
!       _RL total_vol_out
      external SLOPE_LIMITER

!       total_vol_out = 0.0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-oly,sNy+oly
         DO i=1-olx,sNx+olx
          H0_valid(i,j,bi,bj)=.false.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy+1
         Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
         DO i=1,sNx
C        THESE ARRAY BOUNDS INSURE THAT AFTER THIS STEP,
C        VALUES WILL BE RELIABLE 1 GRID CELLS OUT IN THE
C        X DIRECTION AND 1 CELLS OUT IN THE Y DIR
          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          IF (((Gi .ge. 1) .and. (Gi .le. Nx)).or.
     &       STREAMICE_EW_PERIODIC ) THEN
           IF ((STREAMICE_hmask(i,j,bi,bj).eq.1.0) .or.
     &         ((STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) .and.
     &          (STREAMICE_hmask(i,j,bi,bj).ne.1.0))) THEN

            vface = VADV(i,j,bi,bj)
            cfl = ABS(vface) * time_step * recip_dyC(i,j,bi,bj)

!            IF (BC_FACEMASK(i,j,bi,bj).eq.3.0) THEN
!              YFLUX (i,j,bi,bj) = BC_YVALUES(i,j,bi,bj) * vface

            IF (BC_FACEMASK(i,j,bi,bj).eq.3.0 .and.
     &          vface.gt.0 .and.
     &          STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
             YFLUX (i,j,bi,bj) = BC_YVALUES(i,j,bi,bj) * vface
            ELSEIF
     &          (BC_FACEMASK(i,j,bi,bj).eq.3.0 .and.
     &          vface.le.0 .and.
     &          STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) THEN
             YFLUX (i,j,bi,bj) = BC_YVALUES(i,j,bi,bj) * vface

            ELSE

             IF (vface .gt. 0. _d 0) THEN
              DO k=-1,1
               stencil (k) = TRAC(i,j+k-1,bi,bj)
              ENDDO
              IF ((STREAMICE_hmask(i,j,bi,bj).eq.1.0) .and.
     &            (STREAMICE_hmask(i,j-2,bi,bj).eq.1.0))
     &             H0_valid(i,j,bi,bj)=.true.

              IF (((Gj.eq.1).and.(STREAMICE_hmask(i,j-1,bi,bj).eq.3.0))
     &             .and.(.not.STREAMICE_NS_PERIODIC))
     &         THEN  ! we are at western bdry and there is a thick. bdry cond

               YFLUX (i,j,bi,bj) = TRAC(i,j-1,bi,bj) * vface

              ELSEIF (H0_valid(i,j,bi,bj)) THEN

               rdenom = (stencil(1)-stencil(0))
               IF (rdenom .ne. 0.) THEN
                Cr = (stencil(0)-stencil(-1))/rdenom
               ELSE
                Cr = 1.E20 *  (stencil(0)-stencil(-1))
               ENDIF

               IF (STREAMICE_ADV_SCHEME.ne.'DST3') THEN
!                phi = SLOPE_LIMITER(stencil(0)-stencil(-1),
!     &                              stencil(1)-stencil(0))
                 phi = SLOPE_LIMITER (Cr)
               ELSE
                d0 = (2.-cfl)*(1.-cfl)/6.0
                d1 = (1.-cfl**2)/6.0
                psi = d0+d1*Cr
                phi = MAX(0. _d 0,MIN(MIN(1. _d 0,psi),
     &                Cr*(1. _d 0 -CFL)/(CFL+1. _d -20) ))
               ENDIF

               IF (STREAMICE_ADV_SCHEME.ne.'DST3') THEN
                YFLUX (i,j,bi,bj) = vface *
     &           (stencil(0) + phi * .5 * (1.0-cfl) *
     &           (stencil(1)-stencil(0)))
               ELSE
                YFLUX (i,j,bi,bj) = vface *
     &           (stencil(0) + phi *
     &           (stencil(1)-stencil(0)))
               ENDIF

              ELSE ! one of the two cells needed for a HO scheme is missing, use FO scheme

               YFLUX (i,j,bi,bj) = vface * stencil(0)

              ENDIF

             ELSE IF (vface .lt. 0.0) THEN ! vface <= 0

              DO k=-1,1
               stencil (k) = TRAC(i,j-k,bi,bj)
              ENDDO
              IF ((STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) .and.
     &            (STREAMICE_hmask(i,j+1,bi,bj).eq.1.0))
     &             H0_valid(i,j,bi,bj)=.true.

              IF (((Gj.eq.Ny).and.(STREAMICE_hmask(i,j+1,bi,bj).eq.3.0))
     &             .and.(.not.STREAMICE_NS_PERIODIC))
     &         THEN  ! we are at western bdry and there is a thick. bdry cond

               YFLUX (i,j,bi,bj) = TRAC(i,j+1,bi,bj) * vface

              ELSEIF (H0_valid(i,j,bi,bj)) THEN

               rdenom = (stencil(1)-stencil(0))
               IF (rdenom .ne. 0.) THEN
                Cr = (stencil(0)-stencil(-1))/rdenom
               ELSE
                Cr = 1.E20 *  (stencil(0)-stencil(-1))
               ENDIF

               IF (STREAMICE_ADV_SCHEME.ne.'DST3') THEN
!                phi = SLOPE_LIMITER(stencil(0)-stencil(-1),
!     &                        stencil(1)-stencil(0))
                 phi = SLOPE_LIMITER (Cr)
               ELSE
                d0 = (2.-cfl)*(1.-cfl)/6.0
                d1 = (1.-cfl**2)/6.0
                psi = d0+d1*Cr
                phi = MAX(0. _d 0,MIN(MIN(1. _d 0,psi),
     &                Cr*(1. _d 0 -CFL)/(CFL+1. _d -20) ))
               ENDIF

               IF (STREAMICE_ADV_SCHEME.ne.'DST3') THEN
                YFLUX (i,j,bi,bj) = vface *
     &           (stencil(0) + phi * .5 * (1.0-cfl) *
     &           (stencil(1)-stencil(0)))
               ELSE
                YFLUX (i,j,bi,bj) = vface *
     &           (stencil(0) + phi *
     &           (stencil(1)-stencil(0)))
               ENDIF

              ELSE ! one of the two cells needed for a HO scheme is missing, use FO scheme

               yflux (i,j,bi,bj) = vface * stencil(0)

              ENDIF

             ELSE

              yflux (i,j,bi,bj) = 0. _d 0

             ENDIF

            ENDIF

           ENDIF
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

!C     X-FLUXES AT CELL BOUNDARIES CALCULATED; NOW TAKE FLUX DIVERGENCE TO INCREMENT THICKNESS
!
!      DO bj=myByLo(myThid),myByHi(myThid)
!       DO bi=myBxLo(myThid),myBxHi(myThid)
!        DO j=1-3,sNy+3
!         Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
!         IF ((Gj .ge. 1) .and. (Gj .le. Ny)) THEN
!          DO i=1-2,sNx+2
!           IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
!            h(i,j,bi,bj) = h(i,j,bi,bj) - time_step *
!     &       (hflux_x(i+1,j,bi,bj)*dyG(i+1,j,bi,bj) -
!     &        hflux_x(i,j,bi,bj)*dyG(i,j,bi,bj)) *
!     &       recip_rA (i,j,bi,bj)
!           ENDIF
!          ENDDO
!         ENDIF
!        ENDDO
!       ENDDO
!      ENDDO

#endif
      RETURN
      END SUBROUTINE STREAMICE_ADV_FLUX_FL_Y
