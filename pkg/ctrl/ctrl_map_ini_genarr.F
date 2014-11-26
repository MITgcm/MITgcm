C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini_genarr.F,v 1.15 2014/11/26 03:21:22 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

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
#include "optim.h"
#include "ctrl_dummy.h"
#include "CTRL_FIELDS.h"
#include "CTRL_GENARR.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == local variables ==
      integer iarr
      integer igen_etan0,igen_bdrag,igen_geoth
      integer igen_theta0, igen_salt0
      integer igen_kapgm, igen_kapredi, igen_diffkr
      integer smoothOpNb
#ifdef ALLOW_GENTIM2D_CONTROL
      character*(80) fnamegenIn
      character*(80) fnamegenOut
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
#endif /* ALLOW_GENTIM2D_CONTROL */
CEOP

#ifdef ALLOW_GENTIM2D_CONTROL

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

C--   generic 2D control variables
      DO iarr = 1, maxCtrlTim2D

       if (xx_gentim2d_weight(iarr).NE.' ') then

        call ctrl_init_rec ( xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
     I       xx_gentim2d_startdate1(iarr),
     I       xx_gentim2d_startdate2(iarr),
     I       xx_gentim2d_period(iarr),
     I       1,
     O       xx_gentim2d_startdate(1,iarr),
     O       diffrec, startrec, endrec,
     I       myThid )

        ilgen=ilnblnk( xx_gentim2d_file(iarr) )
        write(fnamegenIn(1:80),'(2a,i10.10)')
     &       xx_gentim2d_file(iarr)(1:ilgen), '.', optimcycle
        write(fnamegenOut(1:80),'(2a,i10.10)')
     &       xx_gentim2d_file(iarr)(1:ilgen),'.effective.',optimcycle

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

         call active_read_xy( fnamegenIn, xx_gen, irec,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gentim2d_dummy(iarr) )

        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'variaweight')
     &     call mdsreadfield( xx_gentim2d_weight(iarr), ctrlprec, 'RL',
     &     1, wgentim2d(1-Olx,1-Oly,1,1,iarr), irec, myThid )
        enddo

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

           _EXCH_XY_RL ( xx_gen , myThid )
          ENDIF
         ENDIF
#endif /* ALLOW_SMOOTH */

         call active_write_xy( fnamegenOut, xx_gen, irec, optimcycle,
     &       mythid, xx_gentim2d_dummy(iarr) )

c-- end irec loop
        ENDDO

        DO jrec = 1, replicated_ntimes
        DO irec = 1, replicated_nrec
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

       endif

c-- end iarr loop
      ENDDO
#endif /* ALLOW_GENTIM2D_CONTROL */

#ifdef ALLOW_GENARR2D_CONTROL

C--   generic 2D control variables

      igen_etan0=0
      igen_bdrag=0
      igen_geoth=0
      DO iarr = 1, maxCtrlArr2D
      if (xx_genarr2d_file(iarr).EQ.'xxg_etan0') igen_etan0=iarr
      if (xx_genarr2d_file(iarr).EQ.'xxg_bottomdrag') igen_bdrag=iarr
      if (xx_genarr2d_file(iarr).EQ.'xxg_geothermal') igen_geoth=iarr
      ENDDO

      call ctrl_map_genarr2d(etaN,igen_etan0,myThid)
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      call ctrl_map_genarr2d(bottomDragFld,igen_bdrag,myThid)
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
      call ctrl_map_genarr2d(geothermalFlux,igen_geoth,myThid)
#endif

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL

C--   generic 3D control variables

      igen_theta0=0
      igen_salt0=0
      igen_kapgm=0
      igen_kapredi=0
      igen_diffkr=0
      DO iarr = 1, maxCtrlArr3D
      if (xx_genarr3d_file(iarr).EQ.'xx_theta') igen_theta0=iarr
      if (xx_genarr3d_file(iarr).EQ.'xx_salt') igen_salt0=iarr
      if (xx_genarr3d_file(iarr).EQ.'xx_kapgm') igen_kapgm=iarr
      if (xx_genarr3d_file(iarr).EQ.'xx_kapredi') igen_kapredi=iarr
      if (xx_genarr3d_file(iarr).EQ.'xx_diffkr') igen_diffkr=iarr
      ENDDO

      call ctrl_map_genarr3d(theta,igen_theta0,myThid)
      call ctrl_map_genarr3d(salt,igen_salt0,myThid)
#ifdef ALLOW_KAPGM_CONTROL
      call ctrl_map_genarr3d(kapgm,igen_kapgm,myThid)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
      call ctrl_map_genarr3d(kapredi,igen_kapredi,myThid)
#endif
#ifdef ALLOW_3D_DIFFKR
      call ctrl_map_genarr3d(diffkr,igen_diffkr,myThid)
#endif

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END

C---------------------------

C     !ROUTINE: CTRL_MAP_GENARR2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENARR2D( fld, iarr, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_GENARR2D
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

#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      _RL fld   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      INTEGER iarr
      INTEGER myThid

#ifdef ALLOW_GENARR2D_CONTROL

C     !LOCAL VARIABLES:
C     == local variables ==
      integer bi,bj
      integer i,j
      integer jmin,jmax
      integer imin,imax
      integer smoothOpNb, k2
      _RL fldTmp  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
CEOP

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = jmin,jmax
            DO i = imin,imax
                fldTmp(i,j,bi,bj) = fld(i,j,bi,bj)
            ENDDO
          ENDDO
        ENDDO
       ENDDO

       smoothOpNb=1
       do k2 = 1, maxCtrlProc
        if (xx_genarr2d_preproc(k2,iarr).EQ.'smooth') then
           if (xx_genarr2d_preproc_i(k2,iarr).NE.0)
     &         smoothOpNb=xx_genarr2d_preproc_i(k2,iarr)
        endif
       enddo

       if ( (xx_genarr2d_weight(iarr).NE.' ').AND.(iarr.GT.0) )
     &  call ctrl_map_ini_gen2D(xx_genarr2d_file(iarr),
     &       xx_genarr2d_weight(iarr), xx_genarr2d_dummy(iarr),
     &       xx_genarr2d_bounds(1,iarr), fldTmp, maskc,
     &       smoothOpNb, mythid )

       _EXCH_XY_RL( fldTmp, mythid)

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx,sNy+Olx
                fld(i,j,bi,bj) = fldTmp(i,j,bi,bj)
            ENDDO
          ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_GENARR2D_CONTROL */

      RETURN
      END

C---------------------------

C     !ROUTINE: CTRL_MAP_GENARR3D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENARR3D( fld, iarr, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_GENARR3D
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

#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      _RL fld   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      INTEGER iarr
      INTEGER myThid

#ifdef ALLOW_GENARR3D_CONTROL

C     !LOCAL VARIABLES:
C     == local variables ==
      integer bi,bj
      integer i,j,k
      integer jmin,jmax
      integer imin,imax
      integer smoothOpNb,k2
      _RL fldTmp  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
CEOP

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = jmin,jmax
            DO i = imin,imax
              DO k = 1,nr
                fldTmp(i,j,k,bi,bj) = fld(i,j,k,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO

       smoothOpNb=1
       do k2 = 1, maxCtrlProc
        if (xx_genarr3d_preproc(k2,iarr).EQ.'smooth') then
           if (xx_genarr3d_preproc_i(k2,iarr).NE.0)
     &         smoothOpNb=xx_genarr3d_preproc_i(k2,iarr)
        endif
       enddo

       if ( (xx_genarr3d_weight(iarr).NE.' ').AND.(iarr.GT.0) )
     &  call ctrl_map_ini_gen3D(xx_genarr3d_file(iarr),
     &       xx_genarr3d_weight(iarr), xx_genarr3d_dummy(iarr),
     &       xx_genarr3d_bounds(1,iarr), fldTmp, maskc,
     &       smoothOpNb, mythid )

       _EXCH_XYZ_RL( fldTmp, mythid)

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx,sNy+Olx
              DO k = 1,nr
                fld(i,j,k,bi,bj) = fldTmp(i,j,k,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END


