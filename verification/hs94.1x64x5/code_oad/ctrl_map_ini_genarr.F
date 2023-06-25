#include "CTRL_OPTIONS.h"

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
C     | local version for OpenAD exp. hs94.1x64x5
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

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNk
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == local variables ==
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER jmin,jmax
      INTEGER imin,imax
      INTEGER il
      INTEGER iarr

      LOGICAL doglobalread
      LOGICAL ladinit
      CHARACTER*(80) fnamebase
      CHARACTER*(80) fnamegeneric
      _RL     fac
#ifdef ALLOW_GENARR2D_CONTROL
      _RL     tmpfld2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      _RL     tmpfld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
CEOP

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      doglobalread = .FALSE.
      ladinit      = .FALSE.
      fac = 1. _d 0

#ifdef ALLOW_GENARR2D_CONTROL
C--   An example of connecting specific fields
C--   to 3 generic 2D control arrays
cc--->>>
cc--->>> COMPILE FAILURE IS DELIBERATE
cc--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
cc--->>>
C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlArr2D

       fnamebase = xx_genarr2d_file(iarr)
       il=ILNBLNK( fnamebase )
       WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &     fnamebase(1:il),'.',optimcycle
       CALL ACTIVE_READ_XY ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      myThid, xx_genarr2d_dummy(iarr) )
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = jmin,jmax
            DO i = imin,imax
              IF ( iarr .EQ. 1 ) THEN
                theta(i,j,1,bi,bj) = theta(i,j,1,bi,bj)
     &                             + tmpfld2d(i,j,bi,bj)
              ELSEIF ( iarr .EQ. 2 ) THEN
                salt(i,j,1,bi,bj) = salt(i,j,1,bi,bj)
     &                            + tmpfld2d(i,j,bi,bj)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
       ENDDO
C--   end iarr loop
      ENDDO

       _EXCH_XYZ_RL( theta, myThid )
       _EXCH_XYZ_RL( salt, myThid )

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
C--   An example of connecting specific fields
C--   to 3 generic 3D control arrays
cc--->>>
cc--->>> COMPILE FAILURE IS DELIBERATE
cc--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
cc--->>>
C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlArr3D

       fnamebase = xx_genarr3d_file(iarr)
       il=ILNBLNK( fnamebase )
       WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &     fnamebase(1:il),'.',optimcycle
       CALL ACTIVE_READ_XYZ( fnamegeneric, tmpfld3d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_genarr3d_dummy(iarr) )
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO k = 1,Nr
           DO j = jmin,jmax
            DO i = imin,imax
cc              if ( iarr .EQ. 1 ) then
cc                theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj)
cc     &                             + fac*tmpfld3d(i,j,k,bi,bj)
cc              ELSEIF ( iarr .EQ. 2 ) then
cc                salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj)
cc     &                            + fac*tmpfld3d(i,j,k,bi,bj)
cc              endif
            ENDDO
           ENDDO
          ENDDO
        ENDDO
       ENDDO
C--   end iarr loop
      ENDDO

cc       _EXCH_XYZ_RL( theta, myThid )
cc       _EXCH_XYZ_RL( salt, myThid )

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END
