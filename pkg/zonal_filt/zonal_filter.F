C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filter.F,v 1.5 2001/12/11 14:50:14 jmc Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

      SUBROUTINE ZONAL_FILTER( 
     U           field, fieldMask,
     I           jMin, jMax, kMin, kMax, bi, bj, gridLoc, myThid )
C     /==========================================================\
C     | S/R ZONAL_FILTER                                         |
C     | o Apply FFT filter to a latitude circle.                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ZONAL_FILT.h"
#include "FFTPACK.h"

C     == Routine arguments ==
C     jMin - Range of points to filter
C     jMax
C     kMin
C     kMax
C     bi
C     bj
C     myThid  - Thread number of this instance of FILTER_LATCIRC_FFT_APPLY
C     field   - Field to filter
C     gridLoc - Orientation (U or V) of field.
      INTEGER myThid
      INTEGER gridLoc
      Real*8 field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 fieldMask(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER jMin, jMax, kMin, kMax, bi, bj

#ifdef ALLOW_ZONAL_FILT

C     == Local data ==
      Real*8 phi(Nx)
      Real*8 phiMask(Nx)
      Real*8 avPhi
      INTEGER I, J, K

      DO k=kMin, kMax
       DO j=jMin, jMax
        IF ( (gridLoc.EQ.1 .AND.ABS(yC(1,j,bi,bj)).GE.zonal_filt_lat)
     &   .OR.(gridLoc.EQ.2 .AND.ABS(yG(2,j,bi,bj)).GE.zonal_filt_lat)
     &   .OR. zonal_filt_mode2dx.EQ.2 ) THEN

C     o Copy zonal line of field into local workspace
        DO i=1,sNx
         phi(I) = field(i,j,k,bi,bj)
         phiMask(I) = fieldMask(i,j,k,bi,bj)
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
         field(i,j,k,bi,bj) = phi(sNx+i)
        ENDDO
        DO i=1,sNx
         field(i,j,k,bi,bj) = phi(I)
        ENDDO
        DO i=sNx+1,sNx+OLx
         field(i,j,k,bi,bj) = phi(i-sNx)
        ENDDO

        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
