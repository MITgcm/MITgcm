C
#include "CTRL_OPTIONS.h"

      subroutine ctrl_set_globfld_xy(
     I     fname, ivartype, myThid )

c     ==================================================================
c     SUBROUTINE ctrl_set_globfld_xy
c     ==================================================================
c
c     o initialise field
c
c     started: heimbach@mit.edu, 16-Aug-2001
c
c     changed: heimbach@mit.edu 17-Jun-2003
c              merged changes from Armin to replace write of
c              nr * globfld2d by 1 * globfld3d
c              (ad hoc fix to speed up global I/O)
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
      integer itlo,ithi
      integer jtlo,jthi
      integer irec,nrec_nl

      _RL globfld2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL globfld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)

c     == external ==

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)

c     Initialise temporary file
      do bj = jtlo,jthi
        do bi = itlo,ithi
           do j = 1-OLy,sNy+OLy
             do i = 1-OLx,sNx+OLx
                globfld2d(i,j,bi,bj) = 0. _d 0
             enddo
           enddo
        enddo
      enddo
c     Initialise temporary file
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = 1-OLy,sNy+OLy
              do i = 1-OLx,sNx+OLx
                globfld3d(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

      nrec_nl = int(ncvarrecs(ivartype)/Nr)
      do irec = 1,  nrec_nl
        CALL WRITE_REC_3D_RL( fname, ctrlprec, Nr,
     &                        globfld3d,
     &                        irec, optimcycle, myThid )
      enddo

      do irec = nrec_nl*Nr+1, ncvarrecs(ivartype)
        CALL WRITE_REC_3D_RL( fname, ctrlprec, 1,
     &                        globfld2d,
     &                        irec, optimcycle, myThid )
      enddo

      RETURN
      END
