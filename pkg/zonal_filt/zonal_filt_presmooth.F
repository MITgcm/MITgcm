C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/zonal_filt_presmooth.F,v 1.4 2001/04/10 22:35:27 heimbach Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

      SUBROUTINE ZONAL_FILT_PRESMOOTH(
     I                           holeMask,
     U                           field,
     O                           avgField,
     I                           lField,
     I                           myThid )
C     /==========================================================\
C     | S/R ZONAL_FILT_PRESMOOTH                                 |
C     | o Smooth data with holes ready for FFT.                  |
C     |==========================================================|
C     | FFT routines act on a series of data points. Having      |
C     | arbitrary values in land introduces unrealistic noise in |
C     | the series. A better approach is to linearly ramp data   |
C     | through the missing points. The mean of the field is also|
C     | removed. The mean is restored in FFT_POSTSMOOTH. This    |
C     | step ensures no bias is introduced by the FFT process -  |
C     | strictly it isnt necessary, but it can help improve      |
C     | numerical conditioning.                                  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     holeMask - Array with 0 for holes and != 0 for valid data.
C     lField   - Length of field to smooth (assumed periodic)
C     field    - Field smoothed.
C     myThid   - Thread number of this instance of FFT_PRESMOOTH_IN_X
      INTEGER lField
      Real*8  holeMask(lField)
      Real*8  field(lField)
      Real*8  avgField
      INTEGER myThid

#ifdef ALLOW_ZONAL_FILT

C     == Local variables ====
C     I         - Loop counter
C     lbuf      - Size of buffer arrays
C     hBase     - Coord for last valid data point.
C     hHead     - Coord for next valid data point.
C     hLo       - Index for last valid data point.
C     hHi       - Index for next valid data point.
C     nValid    - Count of valid data points.
C     dist, len - Temps for interpolating.
C     frac        Interpolation is simple linear ramp
C                 between end-point in grid-point space.
C                 e.g. for a series of points
C          Index    1  2  3  4  5  6  7  8  9 10 11 12
C          Data    M  V  M  M  M  M  V  V  V  V  V  V
C                 where M indicates missing data and V valid
C                 and 1 2 3 .... are indexes in field. Values
C                 for the M points are found by interpolating
C                 between the two V values that bracket a series
C                 of M point. For index=1
C                 dist=1, len=2 and frac=dist/len=1/2 so that
C                 the interpolated value at index=1 is
C                 V(index=12)+frac*( V(index=2)-V(index=12) ).
C                 For index=5 dist=3, len=5 and frac=dist/len=3/5
C                 so interpolated value at index=5 is
C                 V(index=2)+frac*( V(index=7)-V(index=2) ).
C     lastGood  - Temp for tracking of last good data point index.
C     nValid    - Temp for counting number of valid points.
C
      INTEGER lBuf
      PARAMETER ( lBuf = sNx )
      INTEGER hBase(lBuf)
      INTEGER hHead(lBuf)
      INTEGER hLo(lBuf)
      INTEGER hHi(lBuf)
      INTEGER lastGood
      _RL dist
      _RL len
      _RL frac
      INTEGER nValid
      INTEGER I, iLo, iHi


C
      IF ( lField .GT. lBuf ) THEN
       STOP 'S/R FFT_PRESMOOTH_1D: lField .GT. lBuf'
      ENDIF

CcnhDebugStarts
C     WRITE(*,*) 'Before FFT pre-smooth'
C     WRITE(*,'(A,A)') 'Mask ', 'Data'
C     DO I=1,lField
C      WRITE(*,'(A,I4,1X,F3.1,1X,1PE35.25)') 'I= ',I,holeMask(I), field(I)
C     ENDDO
CcnhDebugEnds

C     Count number of valid data points
      nValid   = 0
      avgField = 0.
      DO I=1,lField
       IF ( holeMask(I) .NE. 0. ) THEN
        nValid   = nValid+1
        avgField = avgField+field(I)
       ENDIF
      ENDDO

      IF ( lField .GT. 1 .AND. nValid .GT. 0 ) THEN
C      Get lists of hole starts, ends and extents
C      ( use periodic wrap around ).

C      1. hLo   - Index of valid point at start of run of holes
C         hBase - Coord of hLo (used to get offset when interpolating).
C         Note: The mean is also subtracted from field here.
       lastGood = -1
       avgField = avgField/FLOAT(nValid)
       DO I=1,lField
        IF ( holeMask(I) .EQ. 0. ) THEN
C        A hole
         hLo (I)  = lastGood
         hBase(I) = lastGood
        ELSE
C        Data
         hLo(I)   = 0
         hBase(I) = 0
         lastGood = I
         field(I) = field(I)-avgField
        ENDIF
       ENDDO
       DO I=1,lField
        IF ( hLo(I) .EQ. -1 ) THEN
         hLo(I)   = lastGood
         hBase(I) = lastGood-lField
        ENDIF
       ENDDO

C      2. hHi - Coord of valid point at end of run of holes.
       lastGood = -1
       DO I=lField,1,-1
        IF ( holeMask(I) .EQ. 0. ) THEN
C        A hole
         hHi(I)   = lastGood
         hHead(I) = lastGood
        ELSE
C        Data
         hHi(I)   = 0
         lastGood = I
        ENDIF
       ENDDO
       DO I=lField,1,-1
        IF ( hHi(I) .EQ. -1 ) THEN
         hHi(I)   = lastGood
         hHead(I) = lastGood+lField
        ENDIF
       ENDDO

CcnhDebugStarts
C     WRITE(*,*) 'During FFT pre-smooth'
C     WRITE(*,'(A,A,A,A,A,A)') 'I   ','mask(I)','hHi(I)','hLo(I)','hBase(I)','hHead(I)'
C     DO I=1,lField
C      WRITE(*,'(6(I4,1X))')
C    & I,INT(holeMask(I)),hHi(I),hLo(I),hBase(I),hHead(I)
C     ENDDO
CcnhDebugEnds

C      3. Interpolate
       DO I=1,lField
        IF ( holeMask(I) .EQ. 0. ) THEN
C        A hole
         iLo  = hLo(I)
         iHi  = hHi(I)
         dist = I-hBase(I)
         len  = hHead(I) - hBase(I)
CcnhDebugStarts
C        WRITE(*,'(A,1X,I4,1X,1PE35.25,1X,1PE35.25,)') 'I= ',I,dist, len
C        IF ( dist .LT. 0      ) STOP ' DIST .LT. 0 '
C        IF ( len  .LT. 0      ) STOP ' LEN  .LT. 0 '
C        IF ( dist .GT. lField ) STOP ' dist .GT. lField '
C        IF ( len  .GT. lField ) STOP ' len  .GT. lField '
C        IF ( dist .GT. len    ) STOP ' dist .GT. len    '
CcnhDebugStarts
         frac = dist/len
         field(I) = field(iLo)
     &             +(field(iHi)-field(iLo))*frac
        ENDIF
       ENDDO

CcnhDebugStarts
C     WRITE(*,*) 'After FFT pre-smooth'
C     WRITE(*,'(A,A)') 'Mask ', 'Data'
C     DO I=1,lField
C      WRITE(*,'(A,I4,1X,F3.1,1X,1PE35.25)') 'I= ',I,holeMask(I), field(I)
C     ENDDO
CcnhDebugEnds

      ENDIF
C
#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
