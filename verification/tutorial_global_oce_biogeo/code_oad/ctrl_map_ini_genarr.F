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
cc--->>>
cc--->>> COMPILE FAILURE IS DELIBERATE
cc--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
cc--->>>
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
              if ( iarr .eq. 1 ) then
CC
              elseif ( iarr. eq. 2 ) then
CC
              endif
            enddo
          enddo
        ENDDO
       ENDDO
C--   end iarr loop
      ENDDO
CC       _EXCH_XYZ_RL( theta, myThid )
CC       _EXCH_XYZ_RL( salt, myThid )

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
C--   An example of connecting specific fields
C--   to 3 generic 3D control arrays
cc--->>>
cc--->>> COMPILE FAILURE IS DELIBERATE
cc--->>> BE SURE WHAT YOU ARE DOING AND CUSTOMIZE <<<---
cc--->>>
cc
cc A CONSISTENCY CHECK: FOR THIS CUSTOMIZATION
cc WE ASSUME maxCtrlArr3D <= PTRACERS_num
cc
      IF ( maxCtrlArr3D .GT. PTRACERS_num ) THEN
        WRITE(msgBuf,'(A)')
     &   'CTRL_MAP_INI_GENARR: need maxCtrlArr3D <= PTRACERS_num'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CTRL_MAP_INI_GENARR'
      ENDIF

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
cc#ifdef ALLOW_OPENAD
cc              pTracer(i,j,k,bi,bj,iarr) = pTracer(i,j,k,bi,bj,iarr)
cc     &         + fac*xx_genarr3d(i,j,k,bi,bj,iarr)
cc     &         + fac*tmpfld3d(i,j,k,bi,bj)
cc#else
              pTracer(i,j,k,bi,bj,iarr) = pTracer(i,j,k,bi,bj,iarr)
     &         + fac*tmpfld3d(i,j,k,bi,bj)
cc#endif
            enddo
           enddo
          enddo
        ENDDO
       ENDDO

       CALL EXCH_3D_RL( pTracer(1-OLx,1-OLy,1,1,1,iarr),
     &                  Nr, myThid )
C--   end iarr loop
      ENDDO

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END
