C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filt_nofill.F,v 1.4 2001/04/10 22:35:27 heimbach Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

      SUBROUTINE ZONAL_FILT_NOFILL(
     U           field,
     I           jMin, jMax, kMin, kMax, bi, bj, gridLoc, myThid )
C     /==========================================================\
C     | S/R ZONAL_FILT_NOFILL                                    |
C     | o Apply FFT filter to a latitude circle.                 |
C     |   *No fill*                                              |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
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
C     Real*8 field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      INTEGER jMin, jMax, kMin, kMax, bi, bj

#ifdef ALLOW_ZONAL_FILT

C     == Local data ==
      Real*8 phi(Nx)
      INTEGER I, J, K

      DO k=kMin, kMax
       DO j=jMin, jMax

C     o Copy zonal line of field into local workspace
        DO i=1,sNx
         phi(I) = field(i,j,k,bi,bj)
        ENDDO

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

       ENDDO
      ENDDO

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
