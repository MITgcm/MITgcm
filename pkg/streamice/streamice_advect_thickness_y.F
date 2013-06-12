C $Header: /u/gcmpack/MITgcm/pkg/streamice/Attic/streamice_advect_thickness_y.F,v 1.1 2013/06/12 21:30:21 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


      SUBROUTINE STREAMICE_ADVECT_THICKNESS_Y ( myThid ,     
     O   hflux_y ,
     O   h , 
     I   time_step )

      IMPLICIT NONE

C     O   hflux_y ! flux per unit width across face
C     O   h
C     I   time_step

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"

      INTEGER myThid
      _RL hflux_y          (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL h                (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)  
      _RL time_step
      
#ifdef ALLOW_STREAMICE

C     LOCAL VARIABLES

      INTEGER i, j, bi, bj, Gi, Gj, k
      _RL vface, phi
      _RL stencil (-1:1)
      LOGICAL H0_valid(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
                        ! there are valid cells to calculate a 
                        ! slope-limited 2nd order flux
      _RL SLOPE_LIMITER
      external SLOPE_LIMITER

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
        DO j=1-1,sNy+2
         Gj = (myYGlobalLo-1)+(bj-1)*sNy+j
         DO i=1-2,sNx+2
          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          IF ((Gi.ge.1) .and. (Gi.le.Nx)) THEN
C         THESE ARRAY BOUNDS INSURE THAT AFTER THIS STEP,
C         VALUES WILL BE RELIABLE 1 GRID CELLS OUT IN THE
C         Y DIRECTION 
           IF ((STREAMICE_hmask(i,j,bi,bj).eq.1.0) .or.
     &         ((STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) .and.
     &          (STREAMICE_hmask(i,j,bi,bj).ne.1.0))) THEN

            IF (STREAMICE_vfacemask(i,j,bi,bj).eq.4.0) THEN
             hflux_y (i,j,bi,bj) = v_flux_bdry_SI (i,j,bi,bj)
            ELSE
           
             vface = .5 * 
     &        (V_streamice(i,j,bi,bj)+V_streamice(i+1,j,bi,bj))

             IF (vface .gt. 0. _d 0) THEN
              DO k=-1,1
               stencil (k) = h(i,j+k-1,bi,bj)
              ENDDO
              IF ((STREAMICE_hmask(i,j,bi,bj).eq.1.0) .and.
     &            (STREAMICE_hmask(i,j-2,bi,bj).eq.1.0)) 
     &             H0_valid(i,j,bi,bj)=.true. 
             
              IF ((STREAMICE_hmask(i,j-1,bi,bj).eq.3.0)) 
     &         THEN  ! we are at western bdry and there is a thick. bdry cond
               hflux_y (i,j,bi,bj) = h(i,j-1,bi,bj) * vface
!               PRINT *, "BOUNDARY FLUX UP", hflux_y (i,j,bi,bj),
!     &          h(i,j-1,bi,bj),vface
              ELSEIF (H0_valid(i,j,bi,bj)) THEN
               phi = SLOPE_LIMITER ( 
     &          stencil(0)-stencil(-1),
     &          stencil(1)-stencil(0))
               hflux_y (i,j,bi,bj) = vface *
     &          (stencil(0) - phi * .5 * (stencil(0)-stencil(1)))
              ELSE ! one of the two cells needed for a HO scheme is missing, use FO scheme
               hflux_y (i,j,bi,bj) = vface * stencil(0)
              ENDIF
             ELSE ! uface <= 0
              DO k=-1,1
               stencil (k) = h(i,j-k,bi,bj)
              ENDDO
              IF ((STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) .and.
     &            (STREAMICE_hmask(i,j+1,bi,bj).eq.1.0)) 
     &             H0_valid(i,j,bi,bj)=.true. 

              IF ((STREAMICE_hmask(i,j,bi,bj).eq.3.0)) 
     &         THEN  ! we are at western bdry and there is a thick. bdry cond
               hflux_y (i,j,bi,bj) = h(i,j,bi,bj) * vface
!               PRINT *, "BOUNDARY FLUX DOWN", hflux_y (i,j,bi,bj),
!     &          h(i,j,bi,bj),vface
              ELSEIF (H0_valid(i,j,bi,bj)) THEN
               phi = SLOPE_LIMITER ( 
     &          stencil(0)-stencil(-1),
     &          stencil(1)-stencil(0))
               hflux_y (i,j,bi,bj) = vface *
     &          (stencil(0) - phi * .5 * (stencil(0)-stencil(1)))
              ELSE ! one of the two cells needed for a HO scheme is missing, use FO scheme
               hflux_y (i,j,bi,bj) = vface * stencil(0)
              ENDIF

             ENDIF ! uface  0

            ENDIF 
           ENDIF
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     X-FLUXES AT CELL BOUNDARIES CALCULATED; NOW TAKE FLUX DIVERGENCE TO INCREMENT THICKNESS

      
      
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-1,sNy+1
         DO i=1-2,sNx+2
          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          IF ((Gi .ge. 1) .and. (Gi .le. Nx)) THEN
           IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN
            h(i,j,bi,bj) = h(i,j,bi,bj) - time_step * 
     &       (hflux_y(i,j+1,bi,bj)*dxG(i,j+1,bi,bj) -
     &        hflux_y(i,j,bi,bj)*dxG(i,j,bi,bj)) *
     &       recip_rA (i,j,bi,bj)
           ENDIF
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

!       CALL WRITE_FLD_XY_RL ("h_after_yflux","",
!      & h, 0, myThid)

#endif
      RETURN
      END SUBROUTINE STREAMICE_ADVECT_THICKNESS_Y


