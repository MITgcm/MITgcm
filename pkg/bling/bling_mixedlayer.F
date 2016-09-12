C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_mixedlayer.F,v 1.2 2016/09/12 20:00:28 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_MIXEDLAYER(
     U               sumMLDepth,
     I               bi, bj, imin, imax, jmin, jmax,
     I               myIter, myTime, myThid )
     
C     =================================================================
C     | subroutine bling_mixedlayer
C     | o Calculate mixed layer depth based on density criterion 
C     =================================================================

      implicit none
      
C     === Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
#endif

C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
C     === Output ===
C      sumMLDepth   :: mixed layer depth
      _RL sumMLDepth(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     === Local variables ===
      _RL dens_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dens_z    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL delta_dens(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       integer i,j,k
CEOP

c ---------------------------------------------------------------------
c  Mixed layer depth 

      DO j=jmin,jmax
        DO i=imin,imax
          SumMLDepth(i,j) = drf(1)
        ENDDO
      ENDDO

c  Surface density
      CALL FIND_RHO_2D(
     I     1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I     theta(1-OLx,1-OLy,1,bi,bj), salt(1-OLx,1-OLy,1,bi,bj),
     O     dens_surf,
     I     1, bi, bj, myThid )

      DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
             if (k.eq.1) then
              delta_dens(i,j,1) = 0. _d 0
             else
              delta_dens(i,j,k) = 9999. _d 0
             endif
          ENDDO
        ENDDO
      ENDDO

      DO k = 2,Nr

c  Potential density 
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        dens_z,
     I        k, bi, bj, myThid )

        DO j=jmin,jmax
          DO i=imin,imax
                
c           SumMLDepth(i,j) = 0. _d 0

           IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN
            delta_dens(i,j,k) = dens_z(i,j)-dens_surf(i,j)
            IF (delta_dens(i,j,k) .LT. 0.03 _d 0) THEN
             SumMLDepth(i,j) = SumMLDepth(i,j)+drF(k)
            ENDIF 
           ENDIF
           
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END
      
