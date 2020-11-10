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
     I     uIce, vIce, 
     I     SIMaskULoc, SIMaskVLoc, AREALoc,
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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     u/vIceLoc :: local copies of the current ice velocity
C     SIMaskU/V :: local copy of land-sea masks in u/v direction
C     SideDragloc :: side drag in x direction
C     iStep     :: current sub-time step iterate
C     myTime    :: Simulation time
C     myIter    :: Simulation timestep number
C     myThid    :: my Thread Id. number
      _RL uIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIMaskULoc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIMaskVLoc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AREALoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SideDragXloc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SideDragYloc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL coastX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL coastY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      

      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef SEAICE_ALLOW_SIDEDRAG
C     === local variables ===
C     i,j,bi,bj,ksrf :: loop indices
      INTEGER i,j,bi,bj
c      INTEGER kSrf
      _RL    tmp, tmpx, tmpy, mskU, mskV
      _RL    uSpd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      CALL READ_FLD_XY_RS('coast_length_36km_x',
     &                    ' ',coastX,0,myThid) 

      CALL READ_FLD_XY_RS('coast_length_36km_y',
     &                    ' ',coastY,0,myThid) 

c      kSrf = 1
c      IF ( usingPcoords ) kSrf = Nr
C     get the coastline mask
c      DO bj=myByLo(myThid),myByHi(myThid)
c       DO bi=myBxLo(myThid),myBxHi(myThid)
c        DO j=1-OLy+1,sNy+OLy-1
c         DO i=1-OLx+1,sNx+OLx-1
c           coastline(I,J,bi,bj)= 0. _d 0
c           IF (maskC(I,J,kSrf,bi,bj) .EQ. 0. _d 0) THEN
c             tmp=maskC(I-1,J,kSrf,bi,bj) + maskC(I+1,J,kSrf,bi,bj)
c     &         + maskC(I,J-1,kSrf,bi,bj) + maskC(I,J+1,kSrf,bi,bj)  
c             IF (tmp .GT. 0. _d 0) THEN
c                coastline(I,J,bi,bj)=1. _d 0
c             ENDIF
c           ENDIF
c         ENDDO
c       ENDDO
c       ENDDO
c      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SideDragXloc(I,J,bi,bj) = 0. _d 0
          SideDragYloc(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          uSpd(i,j) = 0.5 _d 0 *
     &          sqrt((uIce(i,  j,bi,bj)+uIce(i+1,j,  bi,bj))**2
     &        +      (vIce(i,  j,bi,bj)+vIce(i,  j+1,bi,bj))**2)
         ENDDO
        ENDDO
C calculate the sidedrag coeff  
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF ( AREALoc(I,J,bi,bj) .GT. 0.01 _d 0 ) THEN
           tmpx = 0.5 _d 0 * ( uSpd(i,j) + uSpd(i-1,j) )
           tmpy = 0.5 _d 0 * ( uSpd(i,j) + uSpd(i,j-1) )
c Another way get |u|, |v|
c        tmpx = sqrt(uIceLoc(i,j,bi,bj)**2+ (0.25 _d 0*
c     &           (vIceLoc(i,  j,  bi,bj)+vIceLoc(i-1,j,bi,bj)+
c     &            vIceLoc(i-1,j+1,bi,bj)+vIceLoc(i,  j+1,bi,bj)))**2) 
c        tmpy = sqrt(vIceLoc(i,j,bi,bj)**2+ (0.25 _d 0*
c     &           (uIceLoc(i,  j,  bi,bj)+uIceLoc(i,  j-1,bi,bj)+
c     &            uIceLoc(i+1,j-1,bi,bj)+uIceLoc(i+1,j,bi,bj)))**2) 

         mskU = ( 3. _d 0
     &        - SIMaskULoc(i,j-1,bi,bj)
     &        - SIMaskULoc(i,j,bi,bj)
     &        - SIMaskULoc(i,j+1,bi,bj)) * SIMaskULoc(i,j,bi,bj)
         mskV = ( 3. _d 0
     &        - SIMaskVLoc(i-1,j,bi,bj)
     &        - SIMaskVLoc(i,j,bi,bj)
     &        - SIMaskVLoc(i+1,j,bi,bj) ) * SIMaskVLoc(i,j,bi,bj)


         SideDragXloc(I,J,bi,bj) = SEAICE_rhoIce * SEAICESideDrag 
     &                             * tmpx * mskU
* coastX(I,J,bi,bj)
c * uIceLoc(i,j,bi,bj)
          SideDragYloc(I,J,bi,bj) = SEAICE_rhoIce * SEAICESideDrag  
     &                             * tmpy * mskV
c  coastY(I,J,bi,bj)
c * vIceLoc(i,j,bi,bj)

c          tmp = sqrt(coastX(i,j,bi,bj)**2 + coastY(i,j,bi,bj)**2)
c         SideDragloc(i,j,bi,bj) = SEAICE_rhoIce * tmp * SEAICESideDrag 
c     &                            * sqrt(tmpx**2 + tmpy**2)  
         ENDIF 
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_SIDEDRAG */

      RETURN
      END
