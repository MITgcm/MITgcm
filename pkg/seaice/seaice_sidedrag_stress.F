#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_SIDEDRAG
C     !INTERFACE:
      SUBROUTINE SEAICE_SIDEDRAG_STRESS(
     I     uIceloc, vIceloc,
     I     NcoastXloc, NcoastYloc, AREALoc,
     O     SideDragXloc,SideDragYloc,
     I     iStep, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_BOTTOMDRAG_COEFFS
C     | o Compute the side drag stress for ice-side drag,
C     |    as a parameterization for island-drag fastice
C     *==========================================================*
C     | written by Yuqing Liu, Jul 2020
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     u/vIceLoc :: local copies of the current ice velocity
C     NcoastX/V :: local copy of normalized coast length
C     SideDraXloc :: side drag in x direction
C     SideDraYloc :: side drag in y direction
C     iStep     :: current sub-time step iterate
C     myTime    :: Simulation time
C     myIter    :: Simulation timestep number
C     myThid    :: my Thread Id. number
      _RL uIceloc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceloc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AREALoc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SideDragXloc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SideDragYloc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL NcoastXloc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL NcoastYloc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef SEAICE_ALLOW_SIDEDRAG
C     === local variables ===
C     i,j,bi,bj,ksrf :: loop indices
      INTEGER i,j,bi,bj
      _RL    tmp, tmpx, tmpy
      _RL    uSpd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     u_0

         u_0 = 0.0001 _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     initialize fields
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SideDragXloc(i,j,bi,bj) = 0. _d 0
          SideDragYloc(i,j,bi,bj) = 0. _d 0
          uSpd(i,j)               = 0. _d 0
         ENDDO
        ENDDO

        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          uSpd(i,j) = 0.5 _d 0 *
     &          SQRT((uIceloc(i,  j,bi,bj)+uIceloc(i+1,j,  bi,bj))**2
     &              +(vIceloc(i,  j,bi,bj)+vIceloc(i,  j+1,bi,bj))**2)
         ENDDO
        ENDDO
C calculate the sidedrag coeff
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF ( AREALoc(i,j,bi,bj) .GT. 0.01 _d 0 ) THEN
           tmpx = 0.5 _d 0 * ( uSpd(i,j) + uSpd(i-1,j) )
           tmpy = 0.5 _d 0 * ( uSpd(i,j) + uSpd(i,j-1) )
c Another way to get |u|, |v|
c        tmpx = sqrt(uIceLoc(i,j,bi,bj)**2+ (0.25 _d 0*
c     &           (vIceLoc(i,  j,  bi,bj)+vIceLoc(i-1,j,bi,bj)+
c     &            vIceLoc(i-1,j+1,bi,bj)+vIceLoc(i,  j+1,bi,bj)))**2)
c        tmpy = sqrt(vIceLoc(i,j,bi,bj)**2+ (0.25 _d 0*
c     &           (uIceLoc(i,  j,  bi,bj)+uIceLoc(i,  j-1,bi,bj)+
c     &            uIceLoc(i+1,j-1,bi,bj)+uIceLoc(i+1,j,bi,bj)))**2)


           SideDragXloc(i,j,bi,bj) = SEAICE_rhoIce * SEAICESideDrag
     &                              * 1. _d 0/(tmpx + u_0)
     &                              * NcoastXloc(i,j,bi,bj)
     &                              * seaiceMassU(i,j,bi,bj)     
           SideDragYloc(i,j,bi,bj) = SEAICE_rhoIce * SEAICESideDrag
     &                              * 1. _d 0/(tmpy + u_0)
     &                              * NcoastYloc(i,j,bi,bj)
     &                              * seaiceMassV(i,j,bi,bj) 
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_SIDEDRAG */

      RETURN
      END
