C $Header: /u/gcmpack/MITgcm/model/src/Attic/shap_filt.F,v 1.4 2000/09/11 20:52:57 heimbach Exp $

#include "CPP_OPTIONS.h"

C  Shapiro style filter. Used in place of diffusion and
C  viscosity in atmospheric codes.  Current form is a bad
C  strategy for distributed platforms becuase fields are filtered
C  rather than using the filter in a time-stepping framework
C  to create a tendency term.
C  We will try the second approach later.

      SUBROUTINE SHAP_FILT( myCurrentTime, myCurrentIter, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT                                            |
C     | Controls shapiro filtering of prognostic variables       |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myCurrentTime - Current time in simulation
C     myIter        - Current iteration number in simulation
C     myThid        - Thread number for this instance of the routine.
      INTEGER myThid
      _RL myCurrentTime
      INTEGER myCurrentIter

C     == Local variables ==
C     etaSurfX,      - Holds surface elevation gradient in X and Y.
C     etaSurfY
C     iMin, iMax     - Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj, k
      _RL etaSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL etaSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER k

      iMin = 1-OLx+1
      iMax = sNx+OLx
      jMin = 1-OLy+1
      jMax = sNy+OLy

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1, Nr
C--      Calculate gradient of surface pressure
         CALL CALC_GRAD_ETA_SURF(
     I      bi,bj,iMin,iMax,jMin,jMax,
     O      etaSurfX,etaSurfY,
     I      myThid)
C--      Update fields in top level according to tendency terms
         CALL CORRECTION_STEP(
     I      bi,bj,iMin,iMax,jMin,jMax,K,
     I      etaSurfX,etaSurfY,myCurrentTime,myThid)
         CALL SHAP_FILT_U( bi, bj, K, myCurrentTime, myThid )
         CALL SHAP_FILT_V( bi, bj, K, myCurrentTime, myThid )
         IF ( tempStepping ) 
     &    CALL SHAP_FILT_TRACER(theta,bi,bj,K,myCurrentTime,myThid)
         IF ( saltStepping ) 
     &    CALL SHAP_FILT_TRACER( salt,bi,bj,K,myCurrentTime,myThid)
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XYZ_R8( uVel, myThid )
      _EXCH_XYZ_R8( vVel, myThid )
      IF ( tempStepping ) _EXCH_XYZ_R8( theta, myThid )
      IF ( saltStepping ) _EXCH_XYZ_R8( salt , myThid )

      RETURN
      END

      SUBROUTINE SHAP_FILT_U( bi, bj, K, myCurrentTime, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_U                                          |
C     | Applies Shapiro filter to U field over one XY slice      |
C     | of one tile at a time.                                   |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     == Routine arguments
      INTEGER myThid
      _RL     myCurrentTime
      INTEGER bi, bj, K

C     == Local variables ==
      _RL shapXWk1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapXWk2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapYWk1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapYWk2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  coeff
      INTEGER I,J,iShap

      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        shapXWk1(i,j) = uVel(i,j,k,bi,bj)
        shapYWk1(i,j) = uVel(i,j,k,bi,bj)
        shapXWk2(i,j) = uVel(i,j,k,bi,bj)
        shapYWk2(i,j) = uVel(i,j,k,bi,bj)
       ENDDO
      ENDDO
      DO iShap=1,nShap,2
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx+1,sNx+OLx-1
         shapXWk2(i,j) = shapXWk1(i-1,j)
     &                 + shapXWk1(i+1,j)
     &                 - shapXWk1(i,j) * 2. _d 0
         shapXWk2(i,j) = shapXWk2(i,j)
     &    *_maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx+1,sNx+OLx-1
         shapXWk1(i,j) = shapXWk2(i-1,j)
     &                 + shapXWk2(i+1,j)
     &                 - shapXWk2(i,j) * 2. _d 0
         shapXWk1(i,j) = shapXWk1(i,j)
     &    *_maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
C     o ********************************************
C     o *** "No-slip" lateral bc hard coded below **
C     o ********************************************
      DO ishap=1,nShap,2
       DO J=1-OLy+1,sNy+OLy-1
        DO I=1-OLx+1,sNx+OLx
         shapYWk2(i,j) =
     &    ( shapYWk1(i,j+1)
     &      *_maskS(i,j+1,k,bi,bj)
     &      *_maskS(i-1,j+1,k,bi,bj)
     &     -shapYWk1(i,j  )
     &    )
     &   -( shapYWk1(i,j  )
     &     -shapYWk1(i,j-1)
     &      *_maskS(i,j,k,bi,bj)
     &      *_maskS(i-1,j,k,bi,bj)
     &    )
        ENDDO
       ENDDO
       DO J=1-OLy+1,sNy+OLy-1
        DO I=1-OLx+1,sNx+OLx
         shapYWk1(i,j) =
     &    ( shapYWk2(i,j+1)
     &      *_maskS(i,j+1,k,bi,bj)
     &      *_maskS(i-1,j+1,k,bi,bj)
     &     -shapYWk2(i,j  )
     &    )
     &   -( shapYWk2(i,j  )
     &     -shapYWk2(i,j-1)
     &      *_maskS(i,j,k,bi,bj)
     &      *_maskS(i-1,j,k,bi,bj)
     &    )
        ENDDO
       ENDDO
      ENDDO

C     nshap = 1  ==> del**2
C     nshap = 2  ==> del**4
C     nshap = 3  ==> del**6
C     nshap = 4  ==> del**8
      coeff = (-0.25 _d 0)**nshap
      IF ( 2*(nShap/2) .EQ. nShap ) THEN
C      o Even nShap
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx,sNx+OLx
         uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)
     &    -coeff*0.5 _d 0 *( shapXWk1(i,j)+shapYWk1(i,j) )
     &    *_maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSE
C      o Odd nShap
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx,sNx+OLx
         uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)
     &    -coeff*0.5 _d 0 *( shapXWk2(i,j)+shapYWk2(i,j) )
     &    *maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END

      SUBROUTINE SHAP_FILT_V( bi, bj, K, myCurrentTime, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_V                                          |
C     | Applies Shapiro filter to V field over one XY slice      |
C     | of one tile at a time.                                   |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     == Routine arguments
      INTEGER myThid
      _RL     myCurrentTime
      INTEGER bi, bj, K

C     == Local variables ==
      _RL shapXWk1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapXWk2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapYWk1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapYWk2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  coeff
      INTEGER I,J,iShap

      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        shapXWk1(i,j) = vVel(i,j,k,bi,bj)
        shapYWk1(i,j) = vVel(i,j,k,bi,bj)
        shapXWk2(i,j) = vVel(i,j,k,bi,bj)
        shapYWk2(i,j) = vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     o ********************************************
C     o *** "No-slip" lateral bc hard coded below **
C     o ********************************************
      DO iShap=1,nShap,2
       DO J=1-OLy+1,sNy+OLy
        DO I=1-OLx+1,sNx+OLx-1
         shapXWk1(i,j) =
     &    ( shapXWk2(i+1,j)*maskW(i+1,j,k,bi,bj)*maskW(i+1,j-1,k,bi,bj)
     &     -shapXWk2(i,j  )
     &    )
     &   -( shapXWk2(i  ,j)
     &     -shapXWk2(i-1,j)*maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)
     &    )
        ENDDO
       ENDDO
       DO J=1-OLy+1,sNy+OLy
        DO I=1-OLx+1,sNx+OLx-1
         shapXWk2(i,j) =
     &    ( shapXWk1(i+1,j)*maskW(i+1,j,k,bi,bj)*maskW(i+1,j-1,k,bi,bj)
     &     -shapXWk1(i,j  )
     &    )
     &   -( shapXWk1(i  ,j)
     &     -shapXWk1(i-1,j)*maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)
     &    )
        ENDDO
       ENDDO
      ENDDO
      DO iShap=1,nShap,2
       DO J=1-OLy+1,sNy+OLy-1
        DO I=1-OLx,sNx+OLx
         shapYWk1(i,j) = shapYWk2(i,j-1)
     &                   *_maskS(i,j-1,k,bi,bj)
     &                 + shapYWk2(i,j+1)
     &                   *_maskS(i,j+1,k,bi,bj)
     &                 - shapYWk2(i,j) * 2. D 0
     &                   *_maskS(i,j,k,bi,bj)
         shapYWk1(i,j) = shapYWk1(i,j)
     &                   *_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
       DO J=1-OLy+1,sNy+OLy-1
        DO I=1-OLx,sNx+OLx
         shapYWk2(i,j) = shapYWk1(i,j-1)
     &                   *_maskS(i,j-1,k,bi,bj)
     &                 + shapYWk1(i,j+1)
     &                   *_maskS(i,j+1,k,bi,bj)
     &                 - shapYWk1(i,j) * 2. D 0
     &                   *_maskS(i,j,k,bi,bj)
         shapYWk2(i,j) = shapYWk2(i,j)
     &                   *_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C     nshap = 1  ==> del**2
C     nshap = 2  ==> del**4
C     nshap = 3  ==> del**6
C     nshap = 4  ==> del**8
      coeff = (-0.25 _d 0)**nshap
      IF ( 2*(nShap/2) .NE. nShap ) THEN
C      o Even nShap
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx,sNx+OLx
         vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
     &    -coeff*0.5 _d 0 *( shapXWk1(i,j)+shapYWk1(i,j) )
     &    *_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSE
C      o Odd nShap
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx,sNx+OLx
         vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
     &    -coeff*0.5 _d 0 *( shapXWk2(i,j)+shapYWk2(i,j) )
     &    *_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END

      SUBROUTINE SHAP_FILT_TRACER( 
     U           field, 
     I           bi, bj, K, myCurrentTime, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_TRACER                                     |
C     | Applies Shapiro filter to tracer field over one XY slice |
C     | of one tile at a time.                                   |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments
      INTEGER myThid
      _RL     myCurrentTime
      INTEGER bi, bj, K
      _RL field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     == Local variables ==
      _RL shapXWk1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapXWk2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapYWk1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL shapYWk2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  coeff
      INTEGER I,J,iShap

      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        shapXWk1(i,j) = field(i,j,k,bi,bj)
        shapYWk1(i,j) = field(i,j,k,bi,bj)
        shapXWk2(i,j) = field(i,j,k,bi,bj)
        shapYWk2(i,j) = field(i,j,k,bi,bj)
       ENDDO
      ENDDO
      DO iShap=1,nShap,2
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx+1,sNx+OLx-1
         shapXWk2(i,j) = 
     &    ( shapXWk1(i+1,j)-shapXWk1(i  ,j) )
     &    *_maskW(i,j,k,bi,bj)
     &    *_maskW(i+1,j,k,bi,bj)
     &   -( shapXWk1(i  ,j)-shapXWk1(i-1,j) )
     &    *_maskW(i,j,k,bi,bj)
     &    *_maskW(i-1,j,k,bi,bj)
        ENDDO
       ENDDO
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx+1,sNx+OLx-1
         shapXWk1(i,j) = 
     &    ( shapXWk2(i+1,j)-shapXWk2(i  ,j) )
     &    *_maskW(i,j,k,bi,bj)
     &    *_maskW(i+1,j,k,bi,bj)
     &   -( shapXWk2(i  ,j)-shapXWk2(i-1,j) )
     &    *_maskW(i,j,k,bi,bj)
     &    *_maskW(i-1,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      DO iShap=1,nShap,2
       DO J=1-OLy+1,sNy+OLy-1
        DO I=1-OLx,sNx+OLx
         shapYWk2(i,j) =
     &    ( shapYWk1(i,j+1)-shapYWk1(i,j  ) )
     &    *_maskS(i,j+1,k,bi,bj)
     &    *_maskS(i,j,k,bi,bj)
     &   -( shapYWk1(i,j  )-shapYWk1(i,j-1) )
     &    *_maskS(i,j-1,k,bi,bj)
     &    *_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
       DO J=1-OLy+1,sNy+OLy-1
        DO I=1-OLx,sNx+OLx
         shapYWk1(i,j) =
     &    ( shapYWk2(i,j+1)-shapYWk2(i,j  ) )
     &    *_maskS(i,j,k,bi,bj)
     &    *_maskS(i-1,j,k,bi,bj)
     &   -( shapYWk2(i,j  )-shapYWk2(i,j-1) )
     &    *_maskS(i,j,k,bi,bj)
     &    *_maskS(i,j-1,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C     nshap = 1  ==> del**2
C     nshap = 2  ==> del**4
C     nshap = 3  ==> del**6
C     nshap = 4  ==> del**8
      coeff = (-0.25 _d 0)**nshap
      IF ( 2*(nShap/2) .EQ. nShap ) THEN
C      o Even nShap
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx,sNx+OLx
         field(i,j,k,bi,bj) = field(i,j,k,bi,bj)
     &    -coeff*0.5 _d 0 *( shapXWk1(i,j)+shapYWk1(i,j) )
        ENDDO
       ENDDO
      ELSE
C      o Odd nShap
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx,sNx+OLx
         field(i,j,k,bi,bj) = field(i,j,k,bi,bj)
     &    -coeff*0.5 _d 0 *( shapXWk2(i,j)+shapYWk2(i,j) )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
