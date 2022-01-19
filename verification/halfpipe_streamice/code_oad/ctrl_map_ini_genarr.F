#include "CTRL_OPTIONS.h"
#include "STREAMICE_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_MAP_INI_GENARR
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_INI_GENARR( myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_INI_GENARR
C     | Add the generic arrays of the
C     | control vector to the model state and update the tile halos.
C     | The control vector is defined in the header file "ctrl.h".
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
c#include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
#endif
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNk
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == local variables ==
      integer bi,bj
      integer i,j,k
      integer jmin,jmax
      integer imin,imax
      integer il
      integer iarr

      logical doglobalread
      logical ladinit
      character*(MAX_LEN_FNAM) fnamebase
      character*( 80)   fnamegeneric
      character*(MAX_LEN_MBUF) msgBuf
      _RL     fac
#ifdef ALLOW_GENARR2D_CONTROL
      _RL     tmpfld2d(1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      _RL     tmpfld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif
CEOP

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      doglobalread = .false.
      ladinit      = .false.
      fac = 1. _d 0

#ifdef ALLOW_GENARR2D_CONTROL
C--   An example of connecting specific fields
C--   to 3 generic 2D control arrays

C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlArr2D

       fnamebase = xx_genarr2d_file(iarr)
       il=ILNBLNK( fnamebase )
       write(fnamegeneric(1:80),'(2a,i10.10)')
     &     fnamebase(1:il),'.',optimcycle
       CALL ACTIVE_READ_XY ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      myThid, xx_genarr2d_dummy(iarr) )
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do j = jmin,jmax
            do i = imin,imax

                if ( iarr .eq. 2 ) then
                  H_streamice(i,j,bi,bj) =
     &                  H_streamice(i,j,bi,bj)
     &                  + tmpfld2d(i,j,bi,bj)
#ifdef ALLOW_OPENAD
cph     &                  + xx_genarr2d(i,j,bi,bj,iarr)
#endif

                elseif (iarr.eq.1) then
                  B_glen(i,j,bi,bj) =
     &                  B_glen(i,j,bi,bj)
     &                  + tmpfld2d(i,j,bi,bj)
#ifdef ALLOW_OPENAD
cph     &                  + xx_genarr2d(i,j,bi,bj,iarr)
#endif

!                elseif (iarr.eq.5) then
!                  BDOT_streamice(i,j,bi,bj) =
!     &                  BDOT_streamice(i,j,bi,bj)
!     &                  + tmpfld2d(i,j,bi,bj)

                endif

            enddo
          enddo
        ENDDO
       ENDDO
C--   end iarr loop
      ENDDO

       _EXCH_XY_RL( H_streamice, myThid )
       _EXCH_XY_RL( R_low_si, myThid )
       _EXCH_XY_RL( C_basal_friction, myThid )

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
C--   An example of connecting specific fields
C--   to 3 generic 3D control arrays
--->>>
--->>> COMPILE FAILURE IS DELIBERATE
--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
--->>>
C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlArr3D

       fnamebase = xx_genarr3d_file(iarr)
       il=ILNBLNK( fnamebase )
       write(fnamegeneric(1:80),'(2a,i10.10)')
     &     fnamebase(1:il),'.',optimcycle
       CALL ACTIVE_READ_XYZ( fnamegeneric, tmpfld3d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_genarr3d_dummy(iarr) )
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do k = 1,Nr
           do j = jmin,jmax
            do i = imin,imax
              if ( iarr .eq. 1 ) then
                theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj)
     &                             + fac*tmpfld3d(i,j,k,bi,bj)
              elseif ( iarr .eq. 2 ) then
                salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj)
     &                            + fac*tmpfld3d(i,j,k,bi,bj)
              elseif ( iarr .eq. 3 ) then
#ifdef ALLOW_DIFFKR_CONTROL
                diffkr(i,j,k,bi,bj) = diffkr(i,j,k,bi,bj)
     &                              + tmpfld3d(i,j,k,bi,bj)
#endif
              endif
            enddo
           enddo
          enddo
        ENDDO
       ENDDO
C--   end iarr loop
      ENDDO
       _EXCH_XYZ_RL( theta, myThid )
       _EXCH_XYZ_RL( salt, myThid )
#ifdef ALLOW_DIFFKR_CONTROL
       _EXCH_XYZ_RL( diffkr, myThid )
#endif

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END
