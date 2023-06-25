#include "CTRL_OPTIONS.h"

      SUBROUTINE ctrl_set_globfld_xz(
     I     fname, ivartype, myThid )

C     ==================================================================
C     SUBROUTINE ctrl_set_globfld_xz
C     ==================================================================
C
C     o initialise field
C
C     started: heimbach@mit.edu, 16-Aug-2001
C
C     changed: heimbach@mit.edu 17-Jun-2003
C              merged changes from Armin to replace write of
C              nr * globfld2d by 1 * globfld3d
C              (ad hoc fix to speed up global I/O)
C
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"
#include "optim.h"

C     == routine arguments ==
      CHARACTER*( 80) fname
      INTEGER ivartype
      INTEGER myThid

C     == local variables ==
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER irec, nrec_nl
c     _RS dummyRS(1)
      _RL globfld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL globfldxz(1-OLx:sNx+OLx,Nr,nSx,nSy)

C     == end of interface ==

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
C     Initialise temporary file
          DO k = 1,Nr
             DO i = 1-OLx,sNx+OLx
                globfldxz(i,k,bi,bj) = 0. _d 0
             ENDDO
          ENDDO
C     Initialise temporary file
          DO k = 1,Nr
            DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
                globfld3d(i,j,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      IF ( useSingleCPUio ) THEN
C     MDSWRITEFIELDXZ does not know about useSingleCPUio, so the faster
C     method that works for .not.useSingleCPUio cannot be used
        nrec_nl = 0
      ELSE
        nrec_nl = INT(ncvarrecs(ivartype)/Ny)
      ENDIF

      DO irec = 1, nrec_nl
        CALL WRITE_REC_3D_RL( fname, ctrlprec, Nr,
     &                        globfld3d,
     &                        irec, optimcycle, myThid )
      ENDDO

      DO irec = nrec_nl*Ny+1, ncvarrecs(ivartype)
C--   Call directly MDS_WRITE_SEC_XZ (instead of indirectly via MDSWRITEFIELDXZ)
C     --> expect zero difference.
C     In this call, value "False" is passed to 3rd arg "globalFile"
C             and   value "False" is passed to 4th arg "useCurrentDir,"
c       CALL MDS_WRITE_SEC_XZ( fname, ctrlprec, .FALSE., .FALSE.,
c    I                         'RL', Nr, globfldxz, dummyRS,
c    I                         irec, optimcycle, myThid )

C--   The only difference here (vs initial MDSWRITEFIELDXZ call) is that,
C     in WRITE_REC_XZ_RL, argument "globalFile" is not set to false but
C     instead is taken from global setting "globalFiles" ;
C     But this might be more consistent with call to WRITE_REC_3D_RL above
        CALL WRITE_REC_XZ_RL( fname, ctrlprec,
     I                        Nr, globfldxz,
     I                        irec, optimcycle, myThid )
      ENDDO

      RETURN
      END
