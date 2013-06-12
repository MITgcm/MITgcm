C $Header: /u/gcmpack/MITgcm/pkg/streamice/Attic/streamice_advect_thickness_x.F,v 1.1 2013/06/12 21:30:21 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE STREAMICE_ADVECT_THICKNESS_X ( myThid ,     
     O   hflux_x ,
     O   h , 
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

      INTEGER myThid
      _RL hflux_x          (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL h                (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)  
      _RL time_step
      
#ifdef ALLOW_STREAMICE

C     LOCAL VARIABLES

      INTEGER i, j, bi, bj, Gi, Gj, k
      _RL uface, phi
      _RL stencil (-1:1)
      LOGICAL H0_valid(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
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
        DO j=1-2,sNy+2
         Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
         IF ((Gj .ge. 1) .and. (Gj .le. Ny)) THEN
          DO i=1-1,sNx+2
C        THESE ARRAY BOUNDS INSURE THAT AFTER THIS STEP,
C        VALUES WILL BE RELIABLE 2 GRID CELLS OUT IN THE
C        X DIRECTION AND 3 CELLS OUT IN THE Y DIR
           IF ((STREAMICE_hmask(i,j,bi,bj).eq.1.0) .or.
     &         ((STREAMICE_hmask(i-1,j,bi,bj).eq.1.0) .and.
     &          (STREAMICE_hmask(i,j,bi,bj).ne.1.0))) THEN

            Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
            
            IF (STREAMICE_ufacemask(i,j,bi,bj).eq.4.0) THEN
             hflux_x (i,j,bi,bj) = u_flux_bdry_SI (i,j,bi,bj)
            ELSE
           
             uface = .5 * 
     &        (U_streamice(i,j,bi,bj)+U_streamice(i,j+1,bi,bj))


             IF (uface .gt. 0. _d 0) THEN
              DO k=-1,1
               stencil (k) = h(i+k-1,j,bi,bj)
              ENDDO
              IF ((STREAMICE_hmask(i,j,bi,bj).eq.1.0) .and.
     &            (STREAMICE_hmask(i-2,j,bi,bj).eq.1.0)) 
     &             H0_valid(i,j,bi,bj)=.true. 
             
              IF ((STREAMICE_hmask(i-1,j,bi,bj).eq.3.0)) 
     &         THEN  ! we are at western bdry and there is a thick. bdry cond
               hflux_x (i,j,bi,bj) = h(i-1,j,bi,bj) * uface
!               PRINT *, "BOUNDARY FLUX RIGHT", hflux_x (i,j,bi,bj),
!     &          h(i-1,j,bi,bj),uface,i,j
              ELSEIF (H0_valid(i,j,bi,bj)) THEN
               phi = SLOPE_LIMITER ( 
     &          stencil(0)-stencil(-1),
     &          stencil(1)-stencil(0))
               hflux_x (i,j,bi,bj) = uface *
     &          (stencil(0) - phi * .5 * (stencil(0)-stencil(1)))
              ELSE ! one of the two cells needed for a HO scheme is missing, use FO scheme
               hflux_x (i,j,bi,bj) = uface * stencil(0)
              ENDIF
             
             ELSE ! uface <= 0

              DO k=-1,1
               stencil (k) = h(i-k,j,bi,bj)
              ENDDO
              IF ((STREAMICE_hmask(i-1,j,bi,bj).eq.1.0) .and.
     &            (STREAMICE_hmask(i+1,j,bi,bj).eq.1.0)) 
     &             H0_valid(i,j,bi,bj)=.true. 

              IF ((STREAMICE_hmask(i,j,bi,bj).eq.3.0)) 
     &         THEN  ! we are at western bdry and there is a thick. bdry cond
               hflux_x (i,j,bi,bj) = h(i,j,bi,bj) * uface
!               PRINT *, "BOUNDARY FLUX LEFT", hflux_x(i,j,bi,bj),
!     &          h(i,j,bi,bj),uface
              ELSEIF (H0_valid(i,j,bi,bj)) THEN
               phi = SLOPE_LIMITER ( 
     &          stencil(0)-stencil(-1),
     &          stencil(1)-stencil(0))
               hflux_x (i,j,bi,bj) = uface *
     &          (stencil(0) - phi * .5 * (stencil(0)-stencil(1)))
              ELSE ! one of the two cells needed for a HO scheme is missing, use FO scheme
               hflux_x (i,j,bi,bj) = uface * stencil(0)
              ENDIF

             ENDIF 

            ENDIF 

!             if (streamice_ufacemask(i,j,bi,bj).eq.2.0) THEN
!              total_vol_out = total_vol_out + 
!      &         hflux_x (i,j,bi,bj) * time_step
!             ENDIF

           ENDIF
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     X-FLUXES AT CELL BOUNDARIES CALCULATED; NOW TAKE FLUX DIVERGENCE TO INCREMENT THICKNESS
      
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-3,sNy+3
         Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
         IF ((Gj .ge. 1) .and. (Gj .le. Ny)) THEN
          DO i=1-2,sNx+2
           IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
            h(i,j,bi,bj) = h(i,j,bi,bj) - time_step * 
     &       (hflux_x(i+1,j,bi,bj)*dyG(i+1,j,bi,bj) -
     &        hflux_x(i,j,bi,bj)*dyG(i,j,bi,bj)) *
     &       recip_rA (i,j,bi,bj)
           ENDIF
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDDO


       
#endif
      RETURN
      END SUBROUTINE STREAMICE_ADVECT_THICKNESS_X

