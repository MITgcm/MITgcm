C $Header: /u/gcmpack/MITgcm/verification/global_ocean.90x40x15/code_ad/Attic/ctrl_map_ini_genarr.F,v 1.1 2014/04/28 20:47:57 heimbach Exp $
C $Name:  $

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
#include "CTRL_FIELDS.h"
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
#ifdef ALLOW_BOTTOMDRAG_CONTROL
# ifndef ALLOW_OPENAD
                bottomDragFld(i,j,bi,bj) = bottomDragFld(i,j,bi,bj)
     &                                   + tmpfld2d(i,j,bi,bj)
# else
                bottomDragFld(i,j,bi,bj) = bottomDragFld(i,j,bi,bj)
     &           + xx_genarr2d(i,j,bi,bj,iarr)
     &           + tmpfld2d(i,j,bi,bj)
# endif /* ALLOW_OPENAD */
#endif /* ALLOW_BOTTOMDRAG_CONTROL */
              endif
            enddo
          enddo
        ENDDO
       ENDDO
C--   end iarr loop
      ENDDO
#ifdef ALLOW_BOTTOMDRAG_CONTROL
       _EXCH_XY_RL( bottomDragFld, myThid )
#endif

#endif /* ALLOW_GENARR2D_CONTROL */

      RETURN
      END
