#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_ADV_FRONT (
     & myThid,
     & time_step,
     & hflux_x_si,
     & hflux_y_si )

C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
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

      INTEGER myThid
      _RL time_step
      _RL hflux_x_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL hflux_y_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE

      INTEGER i, j, bi, bj, k, iter_count, iter_rpt
      INTEGER Gi, Gj
      INTEGER new_partial(4)
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey_front, ikey_1
#endif
      _RL iter_flag
      _RL n_flux_1, n_flux_2
      _RL href, rho, partial_vol, tot_flux, hpot
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL hflux_x_SI2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL hflux_y_SI2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      rho = streamice_density
cph      iter_count = 0
      iter_flag = 1. _d 0
      iter_rpt = 0

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            hflux_x_SI2(i,j,bi,bj) = 0. _d 0
            hflux_y_SI2(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      DO iter_count = 0, 3

#ifdef ALLOW_AUTODIFF_TAMC
         ikey_front = (ikey_dynamics-1)*4 + iter_count + 1
CADJ STORE area_shelf_streamice
CADJ &     = comlev1_stream_front, key = ikey_front
CADJ STORE h_streamice
CADJ &     = comlev1_stream_front, key = ikey_front
CADJ STORE hflux_x_si
CADJ &     = comlev1_stream_front, key = ikey_front
CADJ STORE hflux_x_si2
CADJ &     = comlev1_stream_front, key = ikey_front
CADJ STORE hflux_y_si
CADJ &     = comlev1_stream_front, key = ikey_front
CADJ STORE hflux_y_si2
CADJ &     = comlev1_stream_front, key = ikey_front
CADJ STORE streamice_hmask
CADJ &     = comlev1_stream_front, key = ikey_front
CADJ STORE iter_flag
CADJ &     = comlev1_stream_front, key = ikey_front
#endif

       IF ( iter_flag .GT. 0. ) THEN

       iter_flag = 0. _d 0

       IF (iter_count .gt. 0) then
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            hflux_x_SI(i,j,bi,bj)=hflux_x_SI2(i,j,bi,bj)
            hflux_y_SI(i,j,bi,bj)=hflux_y_SI2(i,j,bi,bj)
            hflux_x_SI2(i,j,bi,bj) = 0. _d 0
            hflux_y_SI2(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

!       iter_count = iter_count + 1
       iter_rpt = iter_rpt + 1

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         DO j=1-1,sNy+1
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
          IF ((Gj .ge. 1) .and. (Gj .le. Ny)) THEN
           DO i=1-1,sNx+1
            Gi = (myXGlobalLo-1)+(bi-1)*sNx+i

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_front - 1
          ikey_1 = i + 1
     &         + (sNx+2)*(j)
     &         + (sNx+2)*(sNy+2)*act1
     &         + (sNx+2)*(sNy+2)*max1*act2
     &         + (sNx+2)*(sNy+2)*max1*max2*act3
     &         + (sNx+2)*(sNy+2)*max1*max2*max3*act4
CADJ STORE area_shelf_streamice(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
CADJ STORE h_streamice(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
CADJ STORE hflux_x_si(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
CADJ STORE hflux_y_si(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
CADJ STORE streamice_hmask(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
#endif

            IF (.not. STREAMICE_calve_to_mask .OR.
     &       STREAMICE_calve_mask (i,j,bi,bj) .eq. 1.0) THEN

            IF ((Gi .ge. 1) .and. (Gi .le. Nx) .and.
     &          (STREAMICE_Hmask(i,j,bi,bj).eq.0.0 .or.
     &           STREAMICE_Hmask(i,j,bi,bj).eq.2.0)) THEN
             n_flux_1 = 0. _d 0
             href = 0. _d 0
             tot_flux = 0. _d 0

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hflux_x_SI(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
#endif
             IF (hflux_x_SI(i,j,bi,bj).gt. 0. _d 0) THEN
              n_flux_1 = n_flux_1 + 1. _d 0
              href = href + H_streamice(i-1,j,bi,bj)
              tot_flux = tot_flux + hflux_x_SI(i,j,bi,bj) *
     &         dxG(i,j,bi,bj) * time_step
              hflux_x_SI(i,j,bi,bj) = 0. _d 0
             ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hflux_x_SI(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
#endif
             IF (hflux_x_SI(i+1,j,bi,bj).lt. 0. _d 0) THEN
              n_flux_1 = n_flux_1 + 1. _d 0
              href = href + H_streamice(i+1,j,bi,bj)
              tot_flux = tot_flux - hflux_x_SI(i+1,j,bi,bj) *
     &         dxG(i+1,j,bi,bj) * time_step
              hflux_x_SI(i+1,j,bi,bj) = 0. _d 0
             ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hflux_y_SI(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
#endif
             IF (hflux_y_SI(i,j,bi,bj).gt. 0. _d 0) THEN
              n_flux_1 = n_flux_1 + 1. _d 0
              href = href + H_streamice(i,j-1,bi,bj)
              tot_flux = tot_flux + hflux_y_SI(i,j,bi,bj) *
     &         dyG(i,j,bi,bj) * time_step
              hflux_y_SI(i,j,bi,bj) = 0. _d 0
             ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hflux_y_SI(i,j,bi,bj)
CADJ &     = comlev1_stream_ij, key = ikey_1
#endif
             IF (hflux_y_SI(i,j+1,bi,bj).lt. 0. _d 0) THEN
              n_flux_1 = n_flux_1 + 1. _d 0
              href = href + H_streamice(i,j+1,bi,bj)
              tot_flux = tot_flux - hflux_y_SI(i,j+1,bi,bj) *
     &         dyG(i,j+1,bi,bj) * time_step
              hflux_y_SI(i,j+1,bi,bj) = 0. _d 0
             ENDIF

             IF (n_flux_1 .gt. 0.) THEN

              href = href / n_flux_1
              partial_vol = H_streamice (i,j,bi,bj) *
     &         area_shelf_streamice (i,j,bi,bj) + tot_flux
              hpot = partial_vol * recip_rA(i,j,bi,bj)

              IF (hpot .eq. href) THEN ! cell is exactly covered, no overflow
                STREAMICE_hmask (i,j,bi,bj) = 1.0
                H_streamice (i,j,bi,bj) = href
                area_shelf_streamice(i,j,bi,bj) =
     &           rA(i,j,bi,bj)
              ELSEIF (hpot .lt. href) THEN ! cell still unfilled

               STREAMICE_hmask (i,j,bi,bj) = 2.0
               area_shelf_streamice (i,j,bi,bj) = partial_vol / href
               H_streamice (i,j,bi,bj) = href
              ELSE ! cell is filled - do overflow

               STREAMICE_hmask (i,j,bi,bj) = 1.0
               area_shelf_streamice(i,j,bi,bj) =
     &           rA(i,j,bi,bj)

               PRINT *, "GOT HERE OVERFLOW ", i,j,
     &          area_shelf_streamice(i,j,bi,bj)
               partial_vol = partial_vol - href * rA(i,j,bi,bj)

               iter_flag  = 1. _d 0

               n_flux_2 = 0. _d 0 ;
               DO k=1,4
                new_partial (:) = 0
               ENDDO

               DO k=1,2
                IF ( (STREAMICE_ufacemask(i-1+k,j,bi,bj).eq.2.0) .or.
     &            (STREAMICE_calve_to_mask .and.
     &             STREAMICE_calve_mask(i+2*k-3,j,bi,bj).ne.1.0)
     &             ) THEN  ! at a permanent calving boundary - no advance allowed
                   n_flux_2 = n_flux_2 + 1. _d 0
                ELSEIF (STREAMICE_hmask(i+2*k-3,j,bi,bj).eq.0 _d 0) THEN ! adjacent cell is completely ice free
                   n_flux_2 = n_flux_2 + 1. _d 0
                   new_partial (k) = 1
                ENDIF
               ENDDO
               DO k=1,2
                IF ( (STREAMICE_vfacemask (i,j-1+k,bi,bj).eq.2.0) .or.
     &            (STREAMICE_calve_to_mask .and.
     &             STREAMICE_calve_mask(i,j+2*k-3,bi,bj).ne.1.0)
     &             ) THEN  ! at a permanent calving boundary - no advance allowed
                    n_flux_2 = n_flux_2 + 1. _d 0
                ELSEIF (STREAMICE_hmask(i,j+2*k-3,bi,bj).eq.0 _d 0) THEN
                    n_flux_2 = n_flux_2 + 1. _d 0
                    new_partial (k+2) = 1
                ENDIF
               ENDDO

               IF (n_flux_2 .eq. 0.) THEN ! there is nowhere to put the extra ice!
                H_streamice(i,j,bi,bj) = href + partial_vol *
     &             recip_rA(i,j,bi,bj)
               ELSE
                H_streamice(i,j,bi,bj) = href

                DO k=1,2
                 IF (new_partial(k) .eq. 1) THEN
                  hflux_x_SI2(i-1+k,j,bi,bj) =
     &             partial_vol/time_step/n_flux_2/
     &               dxG(i-1+k,j,bi,bj)
                 ENDIF
                ENDDO

                DO k=1,2
                 IF (new_partial(k+2) .eq. 1) THEN
                  hflux_y_SI2(i,j-1+k,bi,bj) =
     &             partial_vol/time_step/n_flux_2/
     &               dxG(i,j-1+k,bi,bj)
                 ENDIF
                ENDDO

               ENDIF
              ENDIF
             ENDIF

            ENDIF
            ENDIF
           ENDDO
          ENDIF
         ENDDO
c
        ENDDO
       ENDDO
c
      ENDIF
      ENDDO

      IF (iter_rpt.gt.1) THEN
       WRITE(msgBuf,'(A,I5,A)') 'FRONT ADVANCE: ',iter_rpt,
     &  ' ITERATIONS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
      ENDIF

#endif
      RETURN
      END
