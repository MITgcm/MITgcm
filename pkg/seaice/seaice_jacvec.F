C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_jacvec.F,v 1.1 2012/10/16 07:00:21 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_JACVEC
C     !INTERFACE:
      SUBROUTINE SEAICE_JACVEC( 
     I     uIceLoc, vIceLoc, uIceRes, vIceRes,
     U     duIce, dvIce,  
     I     newtonIter, krylovIter, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_JACVEC
C     | o For Jacobian-free Newton-Krylov solver compute
C     |   Jacobian times vector by finite difference approximation
C     *==========================================================*
C     | written by Martin Losch, Oct 2012
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     newtonIter :: current iterate of Newton iteration
C     krylovIter :: current iterate of Krylov iteration
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER newtonIter
      INTEGER krylovIter
C     u/vIceLoc :: local copies of the current ice velocity
      _RL uIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vIceRes :: initial residual of this Newton iterate
      _RL uIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     du/vIce   :: correction of ice velocities
      _RL duIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#if ( defined (SEAICE_CGRID) && \
      defined (SEAICE_ALLOW_JFNK) && \
      defined (SEAICE_ALLOW_DYNAMICS) )
C     Local variables:
      _RL utp     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vtp     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vIceResP :: residual computed with u/vtp
      _RL uIceResP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceResP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     i,j,bi,bj :: loop indices
      INTEGER i,j,bi,bj
      _RL epsilon, reps
CEOP
 
C     Initialise
      epsilon = 1. _d -06
      reps    = 1. _d 0/epsilon

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          utp(I,J,bi,bj) = uIce(I,J,bi,bj) + epsilon * duIce(I,J,bi,bj)
          vtp(I,J,bi,bj) = vIce(I,J,bi,bj) + epsilon * dvIce(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Compute new residual F(u)
      CALL SEAICE_CALC_RESIDUAL(
     I     utp, vtp,
     O     uIceResP, vIceResP,
     I     newtonIter, krylovIter, myTime, myIter, myThid )

C     approximate Jacobian times vector by one-sided finite differences
C     and store in du/vIce
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        DO I = 1, sNx
         DO J = 1, sNy
          duIce(I,J,bi,bj) = 
     &         (uIceResP(I,J,bi,bj)-uIceRes(I,J,bi,bj))*reps
          dvIce(I,J,bi,bj) =
     &         (vIceResP(I,J,bi,bj)-vIceRes(I,J,bi,bj))*reps
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_JFNK */

      RETURN
      END
