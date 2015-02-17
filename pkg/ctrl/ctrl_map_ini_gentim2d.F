C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini_gentim2d.F,v 1.5 2015/02/17 13:54:21 heimbach Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CTRL_MAP_INI_GENTIM2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_INI_GENTIM2D( myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_INI_GENTIM2D
C     | Dimensionalize and preprocess time variable controls.
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
#include "optim.h"
#include "ctrl_dummy.h"
#include "CTRL_GENARR.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_FIELDS.h"
#endif
#ifdef ALLOW_AUTODIFF
#include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

#ifdef ALLOW_GENTIM2D_CONTROL
C     !LOCAL VARIABLES:
C     == local variables ==
      integer iarr
      integer smoothOpNb
      character*(80) fnamegenIn
      character*(80) fnamegenOut
      character*(80) fnamebase
      character*(80) fnamegeneric
      integer startrec
      integer endrec
      integer diffrec
      integer irec, jrec, krec
      integer replicated_nrec
      integer replicated_ntimes
      logical doglobalread
      logical ladinit
      _RL     xx_gen(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer bi,bj
      integer i,j,k2
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      integer ilgen
CEOP

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

C--   generic 2D control variables
      DO iarr = 1, maxCtrlTim2D

       diffrec=0
       startrec=0
       endrec=0

#ifndef ALLOW_OPENAD
       if (xx_gentim2d_weight(iarr).NE.' ') then
#endif

        fnamebase = xx_gentim2d_file(iarr)
        call ctrl_init_rec ( fnamebase,
     I       xx_gentim2d_startdate1(iarr),
     I       xx_gentim2d_startdate2(iarr),
     I       xx_gentim2d_period(iarr),
     I       1,
     O       xx_gentim2d_startdate(1,iarr),
     O       diffrec, startrec, endrec,
     I       myThid )

        fnamebase = xx_gentim2d_file(iarr)
        ilgen=ilnblnk( fnamebase )
        write(fnamegenIn(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.',optimcycle
        write(fnamegenOut(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.effective.',optimcycle

        smoothOpNb=1
        do k2 = 1, maxCtrlProc
          if (xx_gentim2d_preproc(k2,iarr).EQ.'smooth') then
             if (xx_gentim2d_preproc_i(k2,iarr).NE.0) 
     &           smoothOpNb=xx_gentim2d_preproc_i(k2,iarr)
          endif
        enddo

        replicated_nrec=endrec
        replicated_ntimes=0
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'replicate') then
           if (xx_gentim2d_preproc_i(k2,iarr).NE.0) then
            replicated_nrec=min(endrec,xx_gentim2d_preproc_i(k2,iarr))
            replicated_ntimes=
     &       int(float(endrec)/float(replicated_nrec))
            if (replicated_ntimes*replicated_nrec.LT.endrec)
     &      replicated_ntimes=replicated_ntimes+1
            if (replicated_ntimes*replicated_nrec.GT.endrec)
     &      replicated_ntimes=replicated_ntimes-1
           endif
         endif
        enddo

        DO irec = 1, replicated_nrec
#ifdef ALLOW_AUTODIFF
CADJ STORE xx_gentim2d_dummy = ctrltape, key = 1 , kind = isbyte
#endif

         call active_read_xy( fnamegenIn, xx_gen, irec,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gentim2d_dummy(iarr) )

#ifndef ALLOW_OPENAD
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'variaweight')
     &     call mdsreadfield( xx_gentim2d_weight(iarr), ctrlprec, 'RL',
     &     1, wgentim2d(1-Olx,1-Oly,1,1,iarr), irec, myThid )
        enddo
#endif

#ifdef ALLOW_SMOOTH
         IF ( ctrlSmoothCorrel2D ) THEN
          IF ( useSMOOTH ) THEN
           call smooth_correl2D(xx_gen,maskC,smoothOpNb,mythid)

           DO bj=myByLo(myThid), myByHi(myThid)
            DO bi=myBxLo(myThid), myBxHi(myThid)
             DO j = 1,sNy
              DO i = 1,sNx
               if ((maskC(i,j,1,bi,bj).NE.0.).AND.
     &           (wgentim2d(i,j,bi,bj,iarr).GT.0.)) then
                xx_gen(i,j,bi,bj)=xx_gen(i,j,bi,bj)
     &                  /sqrt(wgentim2d(i,j,bi,bj,iarr))
               else
                xx_gen(i,j,bi,bj)=0. _d 0
               endif
              ENDDO
             ENDDO
            ENDDO
           ENDDO

           CALL EXCH_XY_RL ( xx_gen , myThid )
          ENDIF
         ENDIF
#endif /* ALLOW_SMOOTH */

         call active_write_xy( fnamegenOut, xx_gen, irec, optimcycle,
     &       mythid, xx_gentim2d_dummy(iarr) )

c-- end irec loop
        ENDDO

        DO jrec = 1, replicated_ntimes
        DO irec = 1, replicated_nrec
#ifdef ALLOW_AUTODIFF
CADJ STORE xx_gentim2d_dummy = ctrltape, key = 1 , kind = isbyte
#endif
        krec=replicated_nrec*(jrec-1)+irec
        IF (krec.LE.endrec) THEN
         call active_read_xy( fnamegenOut, xx_gen, irec,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gentim2d_dummy(iarr) )
         call active_write_xy( fnamegenOut, xx_gen, krec, optimcycle,
     &       mythid, xx_gentim2d_dummy(iarr) )
        ENDIF
        ENDDO
        ENDDO

#ifndef ALLOW_OPENAD
       endif
#endif

c-- end iarr loop
      ENDDO

#endif /* ALLOW_GENTIM2D_CONTROL */

      RETURN
      END

