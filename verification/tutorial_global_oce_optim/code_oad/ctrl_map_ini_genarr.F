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
C     | local version for OpenAD exp. tutorial_global_oce_optim
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
CML#include "DYNVARS.h"
#include "FFIELDS.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"
#include "optim.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNk
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == local variables ==
      INTEGER bi,bj
      INTEGER i,j
      INTEGER il
      INTEGER iarr

      LOGICAL doglobalread
      LOGICAL ladinit
      CHARACTER*(MAX_LEN_FNAM) fnamebase
      CHARACTER*(80) fnamegeneric
      _RL     fac
#ifdef ALLOW_GENARR2D_CONTROL
      _RL     tmpfld2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      INTEGER k
      _RL     tmpfld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
CEOP

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

C     The control variables in this experiment is the mean heat flux. As
C     is it constant in time , we can specify it here and add it to qnet
C     at the beginning of the integration.
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           tmpfld2d(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       fnamebase = xx_genarr2d_file(iarr)
       il=ILNBLNK( fnamebase )
       WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &     fnamebase(1:il),'.',optimcycle
       CALL ACTIVE_READ_XY ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      myThid, xx_genarr2d_dummy(iarr) )
       IF ( iarr .EQ. 1 ) THEN
        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy
           DO i = 1,sNx
            Qnetm(i,j,bi,bj) = Qnetm(i,j,bi,bj)
     &           + fac*tmpfld2d(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
C--   end iarr loop
      ENDDO
      _EXCH_XY_RS( Qnetm, myThid )

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
C--   An example of connecting specific fields
C--   to 3 generic 3D control arrays
cc--->>>
cc--->>> COMPILE FAILURE IS DELIBERATE
cc--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
cc--->>>
cc

C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlArr3D

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO k = 1,Nr
          DO j = 1,sNy
           DO i = 1,sNx
            tmpfld3d(i,j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       fnamebase = xx_genarr3d_file(iarr)
       il=ILNBLNK( fnamebase )
       WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &     fnamebase(1:il),'.',optimcycle
       CALL ACTIVE_READ_XYZ( fnamegeneric, tmpfld3d, 1,
     &                       doglobalread, ladinit, optimcycle,
     &                       myThid, xx_genarr3d_dummy(iarr) )
       IF ( iarr .EQ. 1 ) THEN
CML        DO bj=myByLo(myThid), myByHi(myThid)
CML         DO bi=myBxLo(myThid), myBxHi(myThid)
CML          DO k = 1,Nr
CML           DO j = 1,sNy
CML            DO i = 1,sNx
CML             theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj)
CML     &         + fac*tmpfld3d(i,j,k,bi,bj)
CML            ENDDO
CML           ENDDO
CML          ENDDO
CML         ENDDO
CML        ENDDO
CML        CALL EXCH_3D_RL( theta, Nr, myThid )
      ENDIF

C--   end iarr loop
      ENDDO

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END
