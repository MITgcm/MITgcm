C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filter.F,v 1.7 2009/09/27 23:13:58 jmc Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

CBOP 0
C     !ROUTINE: ZONAL_FILTER

C     !INTERFACE:
      SUBROUTINE ZONAL_FILTER(
     U           field,
     I           fieldMask,
     I           jMin, jMax, kSize, bi, bj, gridLoc, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R ZONAL_FILTER
C     | o Apply FFT filter to a latitude circle.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ZONAL_FILT.h"
#include "FFTPACK.h"

C     !INPUT/OUTPUT PARAMETERS:
C     jMin      :: Range of points to filter
C     jMax
C     kSize     :: Number of levels to filter
C     bi, bj    :: tile indices
C     field     :: Field to filter
C     fieldMask :: mask corresponding to field to filter
C     gridLoc   :: Position on the grid (U or V) of field.
C     myThid    :: my Thread Id number
      INTEGER kSize
      _RL     field    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize)
      _RS     fieldMask(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSize)
      INTEGER jMin, jMax, bi, bj
      INTEGER gridLoc
      INTEGER myThid
CEOP

#ifdef ALLOW_ZONAL_FILT

C     !LOCAL VARIABLES:
      Real*8 phi(Nx)
      Real*8 phiMask(Nx)
      Real*8 avPhi
      INTEGER i, j, k

      DO k= 1, kSize
       DO j=jMin, jMax
        IF ( (gridLoc.EQ.1 .AND.ABS(yC(1,j,bi,bj)).GE.zonal_filt_lat)
     &   .OR.(gridLoc.EQ.2 .AND.ABS(yG(2,j,bi,bj)).GE.zonal_filt_lat)
     &   .OR. zonal_filt_mode2dx.EQ.2 ) THEN

C     o Copy zonal line of field into local workspace
        DO i=1,sNx
         phi(i) = field(i,j,k)
         phiMask(i) = fieldMask(i,j,k)
        ENDDO

C Interpolate through land
        CALL ZONAL_FILT_PRESMOOTH( phiMask,phi,avPhi,sNx,myThid )

C     o Forward transform (using specific FFT package)
C       CALL R8FFTF( Nx, phi, FFTPACKWS(1,bj) )
        CALL R8FFTF1( Nx, phi,
     &    FFTPACKWS1(1,bj), FFTPACKWS2(1,bj),FFTPACKWS3(1,bj) )

C     o Apply amplitude filter and normalize
        IF (gridLoc .EQ. 1) THEN
         DO i=1, Nx
          phi(i)=phi(i)*ampFactor(i,j,bi,bj)/float(Nx)
         ENDDO
        ELSEIF (gridLoc .EQ. 2) THEN
         DO i=1, Nx
          phi(i)=phi(i)*ampFactorV(i,j,bi,bj)/float(Nx)
         ENDDO
        ELSE
         WRITE(*,*) 'Error: gridLoc = ',gridLoc
         STOP 'Error: gridLoc has illegal value'
        ENDIF

C     o Backward transform (using specific FFT package)
C       CALL R8FFTB( Nx, phi, FFTPACKWS(1,bj) )
        CALL R8FFTB1( Nx, phi,
     &    FFTPACKWS1(1,bj), FFTPACKWS2(1,bj),FFTPACKWS3(1,bj) )

C De-interpolate through land
        CALL ZONAL_FILT_POSTSMOOTH(phiMask,phi,avPhi,sNx,myThid)

C       o Do periodic wrap around by hand
        DO i=1-OLx,0
         field(i,j,k) = phi(sNx+i)
        ENDDO
        DO i=1,sNx
         field(i,j,k) = phi(i)
        ENDDO
        DO i=sNx+1,sNx+OLx
         field(i,j,k) = phi(i-sNx)
        ENDDO

        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
