C $Header: /u/gcmpack/MITgcm/model/src/Attic/filter.F,v 1.2 1999/05/18 17:58:23 adcroft Exp $

#include "CPP_OPTIONS.h"

C     Filtering routines for MITgcmUV. Two sorts of filter are
C     present. 
C     1. An FFT based filter.
C        The FFT filter is coded for filtering in a domain that is periodic
C        in J.
C     2. A high order Shapiro style filter.
C        The Shapiro filter can be invoked with either zero flux
C        or zero gradient boundary conditions.
C     The filter code is not that generic and will need customisation for
C     differnt situations. As coded it is configured for use in an atmospheric
C     simulation over the entire globe.
C     FFT filters are applied to gu, gv, gt and gs.
C     Shaprio filters are applied to u, t, s.
C     Overlap is (Shapiro file order)/2
C
C     SUBROUTINE FILTER_FFT_INIT
C     /==========================================================\
C     | S/R FILTER_FFT_INIT                                      |
C     | o Initialise FFT filter for latitude circle.             |
C     |==========================================================|
C     | The details of particular FFT libraries may differ.      |
C     | Changing to a different library may entail modifying the |
C     | code here. However, the broad process is usually the     |
C     | same.                                                    |
C     \==========================================================/
C
C     Code for using FFT in Digital DXML library.
C     CALL FILTER_LATCIRC_FFT_INIT( myThid )
C      CALL FILTER_DXML_FFT_INIT( myThid )
C
C     CALL FILTER_LATCIRC_FFT_APPLY( U , myThid)
C      CALL FILTER_DXML_FFT_APPLY( U , 1, myThid)
C
C     CALL FILTER_LATCIRC_FFT_APPLY( V , myThid)
C      CALL FILTER_DXML_FFT_APPLY( V , 2, myThid)
C
C     CALL FILTER_LATCIRC_FFT_APPLY( T , myThid)
C      CALL FILTER_DXML_FFT_APPLY( T , 1, myThid)
C
C     CALL FILTER_LATCIRC_FFT_APPLY( S , myThid)
C      CALL FILTER_DXML_FFT_APPLY( S , 1, myThid)
C

      SUBROUTINE FILTER_LATCIRC_FFT_INIT(myThid)

C     /==========================================================\
C     | S/R FILTER_LATCIRC_FFT_INIT                              |
C     | o Initialise FFT filter for latitude circle.             |
C     |==========================================================|
C     | The details of particular FFT libraries may differ.      |
C     | Changing to a different library may entail modifying the |
C     | code here. However, the broad process is usually the     |
C     | same.                                                    |
C     | Note - Fourier modes for sNx and sNx+1 are damped in the |
C     |        same way. This is because we have not implemented |
C     |        a scheme that sets the damping factor for the     |
C     |        highest wave number for odd sNx. Instead the      |
C     |        highest wave number for odd sNx. Instead only     |
C     |        wave numbers 1:INT(sNx/2) are partially damped.   |
C     |        Wave number sNx/2 (if it exists) is removed       |
C     |        altogether.                                       |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFTSUPPORT.h"

C     == Routine arguments ==
C     myThid - Thread number of this instance of FILTER_LATCIRC_FFT_INIT
      INTEGER myThid

C     == Local variables ==
C     alpha - Used to evaluate frequency and latitude dependent
C             amplitude damping factor.
C     wvNum - Wave number
C     lat   - Temporary holding latitude
C     nWv   - No. of waves that fit on grid.
      _RL alpha, wvNum, lat
      INTEGER I, J, bi, bj, nPoints, nWv
      _RL one
      PARAMETER( one = 1.0d0 )

C     o Initialise specific library FFT package
#ifdef ALLOW_DXML_FFT
      CALL FILTER_DXML_DFFT_INIT( myThid )
#endif

      bi = 1
      npoints = sNx
      nWv     = sNx/2 + 1
C     o Initialise all points in epsFFTfilt
      DO bj=1,nSy
       DO J=1,sNy
        DO I=1,sNx
         epsFFTFiltG1(I  ,J,bi,bj) = 0.
         epsFFTFiltG2(I  ,J,bi,bj) = 0.
        ENDDO
       ENDDO
      ENDDO
C     o Set latitude circle and frequency damping factors.
C     U, T, S grid point locations
      DO bj=1,nSy
       DO J=1,sNy
        DO I=1,nWv
         lat = yc(I,J,bi,bj)
         IF ( ABS(lat) .LT. latFFTFiltLo ) THEN
C         No damping below latitude "latFFTFiltLo"
          epsFFTFiltG1(I  ,J,bi,bj) = 1.
         ELSE
C         Damp more strongly with increasing latitude and frequency.
          alpha = COS(lat*deg2rad)/cos(latFFTFiltLo*deg2rad)
          wvNum = I-1
          IF ( wvNum .GT. 0. ) THEN
           alpha = alpha/SIN( PI * wvNum / FLOAT(nPoints) )
           alpha = alpha**2
           epsFFTFiltG1(I  ,J,bi,bj) = MIN(one,alpha)
          ELSE
           epsFFTFiltG1(I  ,J,bi,bj) = 1.
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C     V grid point locations
      DO bj=1,nSy
       DO J=1,sNy
        DO I=1,nWv
         lat = yc(I,J,bi,bj)-delY(J)*0.5 _d 0
         IF ( ABS(lat) .LT. latFFTFiltLo ) THEN
C         No damping below latitude "latFFTFiltLo"
          epsFFTFiltG2(I  ,J,bi,bj) = 1.
         ELSE
C         Damp more strongly with increasing latitude and frequency.
          alpha = COS(lat*deg2rad)/cos(latFFTFiltLo*deg2rad)
          wvNum = I-1
          IF ( wvNum .GT. 0. ) THEN
           alpha = alpha/SIN( PI * wvNum / FLOAT(nPoints) )
           alpha = alpha**2
           epsFFTFiltG2(I  ,J,bi,bj) = MIN(one,alpha)
          ELSE
           epsFFTFiltG2(I  ,J,bi,bj) = 1.
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XY_R4(epsFFTFiltG1, myThid )
      _EXCH_XY_R4(epsFFTFiltG2, myThid )
C
      RETURN
      END
      SUBROUTINE FILTER_LATCIRCS_FFT_APPLY( 
     U           field, 
     I           jMin, jMax, kMin, kMax, bi, bj, gridLoc, myThid )
C     /==========================================================\
C     | S/R FILTER_LATCIRCS_FFT_APPLY                            |
C     | o Apply FFT filter to a latitude circle.                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"

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
      Real*8 field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,1,nSy)
      INTEGER jMin, jMax, kMin, kMax, bi, bj


C     == Local data ==
      Real*8 phi(sNx+2)
      INTEGER I, J, K

      DO k=kMin, kMax
       DO j=jMin, jMax
        DO i=1,sNx
         phi(I) = field(i,j,k,bi,bj)
        ENDDO
C     o Apply specific library FFT package
#ifdef ALLOW_DXML_FFT
        CALL FILTER_DXML_DFFT_APPLY( phi, j, bi, bj, gridLoc, myThid)
#endif
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

      RETURN
      END
