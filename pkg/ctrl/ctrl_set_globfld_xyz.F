#include "CTRL_OPTIONS.h"

      SUBROUTINE CTRL_SET_GLOBFLD_XYZ(
     I                fname, nRecArg, filePrec, myThid )

C     ==================================================================
C     SUBROUTINE ctrl_set_globfld_xyz
C     ==================================================================
C
C     o initialise field
C
C     started: heimbach@mit.edu, 16-Aug-2001
C
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
c#include "PARAMS.h"
c#include "CTRL_SIZE.h"
c#include "CTRL.h"
#include "OPTIMCYCLE.h"

C     == routine arguments ==
      CHARACTER*(MAX_LEN_FNAM)   fname
      INTEGER nRecArg
      INTEGER filePrec
      INTEGER myThid

C     == local variables ==
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER irec
      _RL globfld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     == end of interface ==

C     Initialise temporary file
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k = 1,Nr
            DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
                globfld3d(i,j,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      DO irec = 1, nRecArg
        CALL WRITE_REC_3D_RL( fname, filePrec, Nr,
     &                        globfld3d,
     &                        irec, optimcycle, myThid )
      ENDDO

      RETURN
      END
