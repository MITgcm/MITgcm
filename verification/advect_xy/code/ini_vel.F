C $Header: /u/gcmpack/MITgcm/verification/advect_xy/code/ini_vel.F,v 1.1 2001/09/13 20:06:19 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE INI_VEL( myThid )
      IMPLICIT NONE

C     Initialize 3D flow field (either to zero or from input files)

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myThid

C     == Local variables ==
C     bi,bj,i,j,k - Loop counters
      INTEGER bi,bj,i,j,k

C--   Initialise velocity fields to zero
C
C     If you want to specify an analytic initial state for the flow
C     field then customize the following section of code.
C     It is, however, often easier to generate initial conditions
C     off-line and read them from input files...
C
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           uVel(i,j,k,bi,bj)=1.
           vVel(i,j,k,bi,bj)=1.
           wVel(i,j,k,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Read an initial state for each component if required
      IF (uVelInitFile .NE. ' ') THEN
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RL( uVelInitFile, ' ', uVel, 0, myThid )
       _END_MASTER(myThid)
c      _EXCH_XYZ_R8(uVel  , myThid )
      ENDIF

      IF (vVelInitFile .NE. ' ') THEN
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RL( vVelInitFile, ' ', vVel, 0, myThid )
       _END_MASTER(myThid)
c      _EXCH_XYZ_R8(vVel  , myThid )
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           uVel(i,j,k,bi,bj)=uVel(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
           vVel(i,j,k,bi,bj)=vVel(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
          ENDDO
         ENDDO
         CALL INTEGRATE_FOR_W(
     I                        bi, bj, k, uVel, vVel,
     O                        wVel,
     I                        myThid )
        ENDDO
       ENDDO
      ENDDO

c     IF (uVelInitFile .NE. ' ' .OR. vVelInitFile .NE. ' ') THEN
       CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
       _EXCH_XYZ_R8(wVel,myThid)
c     ENDIF

      RETURN
      END
