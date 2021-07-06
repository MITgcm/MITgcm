C
#include "CTRL_OPTIONS.h"

      subroutine ctrl_set_globfld_xyz(
     I     fname, ivartype, myThid )

c     ==================================================================
c     SUBROUTINE ctrl_set_globfld_xyz
c     ==================================================================
c
c     o initialise field
c
c     started: heimbach@mit.edu, 16-Aug-2001
c
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "ctrl.h"
#include "optim.h"

c     == routine arguments ==

      character*( 80)   fname
      integer ivartype
      integer myThid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer irec

      _RL globfld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)

c     == external ==

c     == end of interface ==

c     Initialise temporary file
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k = 1,nr
            DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
                globfld3d(i,j,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      DO irec = 1, ncvarrecs(ivartype)
        CALL WRITE_REC_3D_RL( fname, ctrlprec, Nr,
     &                        globfld3d,
     &                        irec, optimcycle, myThid )
      ENDDO

      RETURN
      END
