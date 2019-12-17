#include "CTRL_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
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
# ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# endif
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == local variables ==
#if (defined (ALLOW_GENARR2D_CONTROL) || defined(ALLOW_GENARR3D_CONTROL))
      integer iarr
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      integer igen_etan,igen_bdrag,igen_geoth
#endif /* ALLOW_GENARR2D_CONTROL */
#ifdef ALLOW_GENARR3D_CONTROL
      integer igen_theta0, igen_salt0
      integer igen_kapgm, igen_kapredi, igen_diffkr
#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      integer igen_uvel0, igen_vvel0
#endif
# ifdef ALLOW_PTRACERS
      integer igen_ptr1, igen_ptr2, igen_ptr3
      integer igen_ptr4, igen_ptr5, igen_ptr6
# endif
#endif /* ALLOW_GENARR3D_CONTROL */
CEOP

#ifdef ALLOW_GENARR2D_CONTROL

C--   generic 2D control variables

      igen_etan=0
      igen_bdrag=0
      igen_geoth=0
      DO iarr = 1, maxCtrlArr2D
      if (xx_genarr2d_weight(iarr).NE.' ') then
        if (xx_genarr2d_file(iarr)(1:7).EQ.'xx_etan')
     &     igen_etan=iarr
        if (xx_genarr2d_file(iarr)(1:13).EQ.'xx_bottomdrag')
     &     igen_bdrag=iarr
        if (xx_genarr2d_file(iarr)(1:13).EQ.'xx_geothermal')
     &     igen_geoth=iarr
      endif
      ENDDO

      if (igen_etan.GT.0) then
            call ctrl_map_genarr2d(etaN,igen_etan,myThid)
      endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      if (igen_bdrag.GT.0)
     &  call ctrl_map_genarr2d(bottomDragFld,igen_bdrag,myThid)
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
      if (igen_geoth.GT.0)
     &  call ctrl_map_genarr2d(geothermalFlux,igen_geoth,myThid)
#endif

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL

C--   generic 3D control variables

      igen_theta0=0
      igen_salt0=0
      igen_kapgm=0
      igen_kapredi=0
      igen_diffkr=0
#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      igen_uvel0=0
      igen_vvel0=0
#endif
# ifdef ALLOW_PTRACERS
      igen_ptr1=0
      igen_ptr2=0
      igen_ptr3=0
      igen_ptr4=0
      igen_ptr5=0
      igen_ptr6=0
# endif
      DO iarr = 1, maxCtrlArr3D
      if (xx_genarr3d_weight(iarr).NE.' ') then
        if (xx_genarr3d_file(iarr)(1:8).EQ.'xx_theta')
     &     igen_theta0=iarr
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_salt')
     &     igen_salt0=iarr
        if (xx_genarr3d_file(iarr)(1:8).EQ.'xx_kapgm')
     &     igen_kapgm=iarr
        if (xx_genarr3d_file(iarr)(1:10).EQ.'xx_kapredi')
     &     igen_kapredi=iarr
        if (xx_genarr3d_file(iarr)(1:9).EQ.'xx_diffkr')
     &     igen_diffkr=iarr
#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_uvel')
     &     igen_uvel0=iarr
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_vvel')
     &     igen_vvel0=iarr
#endif
# ifdef ALLOW_PTRACERS
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_ptr1')
     &     igen_ptr1=iarr
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_ptr2')
     &     igen_ptr2=iarr
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_ptr3')
     &     igen_ptr3=iarr
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_ptr4')
     &     igen_ptr4=iarr
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_ptr5')
     &     igen_ptr5=iarr
        if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_ptr6')
     &     igen_ptr6=iarr
# endif
      endif
      ENDDO

      if (igen_theta0.GT.0)
     &   call ctrl_map_genarr3d(theta,igen_theta0,myThid)
      if (igen_salt0.GT.0)
     &   call ctrl_map_genarr3d(salt,igen_salt0,myThid)
#ifdef ALLOW_KAPGM_CONTROL
      if (igen_kapgm.GT.0)
     &   call ctrl_map_genarr3d(kapgm,igen_kapgm,myThid)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
      if (igen_kapredi.GT.0)
     &   call ctrl_map_genarr3d(kapredi,igen_kapredi,myThid)
#endif
#ifdef ALLOW_3D_DIFFKR
      if (igen_diffkr.GT.0)
     &   call ctrl_map_genarr3d(diffkr,igen_diffkr,myThid)
#endif

#ifdef ALLOW_PTRACERS
CADJ STORE pTracer=comlev1,key=ikey_dynamics,kind=isbyte
      if (igen_ptr1.GT.0) then
        call ctrl_map_genarr3d(pTracer(1-OLx,1-OLy,1,1,1,1),
     &                          igen_ptr1,myThid)
      endif
      if (igen_ptr2.GT.0) then
        call ctrl_map_genarr3d(pTracer(1-OLx,1-OLy,1,1,1,2),
     &                          igen_ptr2,myThid)
      endif
      if (igen_ptr3.GT.0) then
        call ctrl_map_genarr3d(pTracer(1-OLx,1-OLy,1,1,1,3),
     &                          igen_ptr3,myThid)
      endif
      if (igen_ptr4.GT.0) then
        call ctrl_map_genarr3d(pTracer(1-OLx,1-OLy,1,1,1,4),
     &                          igen_ptr4,myThid)
      endif
      if (igen_ptr5.GT.0) then
        call ctrl_map_genarr3d(pTracer(1-OLx,1-OLy,1,1,1,5),
     &                          igen_ptr5,myThid)
      endif
      if (igen_ptr6.GT.0) then
        call ctrl_map_genarr3d(pTracer(1-OLx,1-OLy,1,1,1,6),
     &                          igen_ptr6,myThid)
      endif
#endif

#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      if (igen_uvel0.GT.0 .and. igen_vvel0.GT.0) then
         call ctrl_map_genarr3d(uvel,igen_uvel0,myThid)
         call ctrl_map_genarr3d(vvel,igen_vvel0,myThid)
         CALL EXCH_UV_XYZ_RL(uvel,vvel,.TRUE.,myThid)
      endif
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
#include "optim.h"
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
      integer numsmo, k2
      logical dowc01
      logical dosmooth
      logical doscaling
      _RL xx_gen  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RS dummyRS(1)
      character*(80) fnamegenIn
      character*(80) fnamegenOut
      character*(80) fnamebase
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      integer ilgen
      logical doglobalread
      logical ladinit
CEOP

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          xx_gen(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      dosmooth=.false.
      dowc01  = .false.
      doscaling=.true.

      numsmo=1
      do k2 = 1, maxCtrlProc
          if (xx_genarr2d_preproc(k2,iarr).EQ.'WC01') then
             dowc01=.TRUE.
             if (xx_genarr2d_preproc_i(k2,iarr).NE.0)
     &           numsmo=xx_genarr2d_preproc_i(k2,iarr)
          endif
          if ((.NOT.dowc01).AND.
     &        (xx_genarr2d_preproc(k2,iarr).EQ.'smooth')) then
             dosmooth=.TRUE.
             if (xx_genarr2d_preproc_i(k2,iarr).NE.0)
     &           numsmo=xx_genarr2d_preproc_i(k2,iarr)
          endif
          if (xx_genarr2d_preproc(k2,iarr).EQ.'noscaling') then
             doscaling=.FALSE.
          endif
      enddo

      fnamebase = xx_genarr2d_file(iarr)
      ilgen=ilnblnk( fnamebase )
      write(fnamegenIn(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.',optimcycle
      write(fnamegenOut(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.effective.',optimcycle

      CALL MDS_READ_FIELD(xx_genarr2d_weight(iarr),ctrlprec,.FALSE.,
     & 'RL',1,1,1,wgenarr2d(1-Olx,1-Oly,1,1,iarr),dummyRS,1,mythid)

#ifdef ALLOW_AUTODIFF
      call active_read_xy( fnamegenIn, xx_gen, 1, doglobalread,
     & ladinit, optimcycle, mythid, xx_genarr2d_dummy(iarr) )
#else
      CALL READ_REC_XY_RL( fnamegenIn, xx_gen, 1, 1, myThid)
#endif

#ifdef ALLOW_SMOOTH
      IF (useSMOOTH) THEN
       IF (dowc01) call smooth_correl2d(xx_gen,maskC,numsmo,mythid)
       IF (dosmooth) call smooth2d(xx_gen,maskC,numsmo,mythid)
      ENDIF
#endif

      DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
            DO j = 1,sNy
              DO i = 1,sNx
c scale param adjustment
                if ( (maskC(i,j,1,bi,bj).NE.0.).AND.
     &               (wgenarr2d(i,j,bi,bj,iarr).GT.0.) ) then
                 IF (doscaling) then
                  xx_gen(i,j,bi,bj)=xx_gen(i,j,bi,bj)
     &                     /sqrt( wgenarr2d(i,j,bi,bj,iarr) )
                 ENDIF
                else
                  xx_gen(i,j,bi,bj)=0.
                endif
c add to model parameter
                fld(i,j,bi,bj)=fld(i,j,bi,bj)+xx_gen(i,j,bi,bj)
              enddo
            enddo
        enddo
      enddo

c avoid param out of [boundsVec(1) boundsVec(4)]
      CALL CTRL_BOUND_2D(fld,maskC,xx_genarr2d_bounds(1,iarr),myThid)

      CALL EXCH_XY_RL( fld, mythid )

      CALL MDS_WRITE_FIELD(fnamegenOut,ctrlprec,.FALSE.,.FALSE.,
     & 'RL',1,1,1,fld,dummyRS,1,optimcycle,mythid)

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
#include "optim.h"
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
      integer numsmo,k2
      logical dowc01
      logical dosmooth
      logical doscaling
      _RL xx_gen  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RS dummyRS(1)
      character*(80) fnamegenIn
      character*(80) fnamegenOut
      character*(80) fnamebase
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      integer ilgen
      logical doglobalread
      logical ladinit
#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif
CEOP

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k = 1,nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           xx_gen(i,j,k,bi,bj)=0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      dosmooth=.false.
      dowc01  = .false.
      doscaling=.true.

      numsmo=1
      do k2 = 1, maxCtrlProc
          if (xx_genarr3d_preproc(k2,iarr).EQ.'WC01') then
             dowc01=.TRUE.
             if (xx_genarr3d_preproc_i(k2,iarr).NE.0)
     &           numsmo=xx_genarr3d_preproc_i(k2,iarr)
          endif
          if ((.NOT.dowc01).AND.
     &        (xx_genarr3d_preproc(k2,iarr).EQ.'smooth')) then
             dosmooth=.TRUE.
             if (xx_genarr3d_preproc_i(k2,iarr).NE.0)
     &           numsmo=xx_genarr3d_preproc_i(k2,iarr)
          endif
          if (xx_genarr3d_preproc(k2,iarr).EQ.'noscaling') then
             doscaling=.FALSE.
          endif
      enddo

      fnamebase = xx_genarr3d_file(iarr)
      ilgen=ilnblnk( fnamebase )
      write(fnamegenIn(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.',optimcycle
      write(fnamegenOut(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.effective.',optimcycle

      CALL MDS_READ_FIELD(xx_genarr3d_weight(iarr),ctrlprec,.FALSE.,
     & 'RL',nR,1,nR,wgenarr3d(1-Olx,1-Oly,1,1,1,iarr),dummyRS,1,mythid)

#ifdef ALLOW_AUTODIFF
      call active_read_xyz( fnamegenIn, xx_gen, 1, doglobalread,
     & ladinit, optimcycle, mythid, xx_genarr3d_dummy(iarr) )
#else
      CALL READ_REC_XYZ_RL( fnamegenIn, xx_gen, 1, 1, myThid)
#endif

#ifdef ALLOW_SMOOTH
      IF (useSMOOTH) THEN
       IF (dowc01) call smooth_correl3d(xx_gen,numsmo,mythid)
       IF (dosmooth) call smooth3d(xx_gen,numsmo,mythid)
      ENDIF
#endif

#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
c--   set local mask
      call ecco_zero(localmask,Nr,zeroRL,myThid)
      if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_uvel') then
        call ecco_cprsrl(maskW,nr,localmask,nr,myThid)
      else if (xx_genarr3d_file(iarr)(1:7).EQ.'xx_vvel') then
        call ecco_cprsrl(maskS,nr,localmask,nr,myThid)
      else
        call ecco_cprsrl(maskC,nr,localmask,nr,myThid)
      endif
#endif

      DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do k = 1,nr
            DO j = 1,sNy
              DO i = 1,sNx
c scale param adjustment
#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
                if ( (localmask(i,j,k,bi,bj).NE.0.).AND.
#else
                if ( (maskC(i,j,k,bi,bj).NE.0.).AND.
#endif
     &               (wgenarr3d(i,j,k,bi,bj,iarr).GT.0.) ) then
                 IF (doscaling) then
                  xx_gen(i,j,k,bi,bj)=xx_gen(i,j,k,bi,bj)
     &                     /sqrt( wgenarr3d(i,j,k,bi,bj,iarr) )
                 ENDIF
                else
                  xx_gen(i,j,k,bi,bj)=0.
                endif
c add to model parameter
                fld(i,j,k,bi,bj)=fld(i,j,k,bi,bj)+xx_gen(i,j,k,bi,bj)
              enddo
            enddo
          enddo
        enddo
      enddo

c avoid param out of [boundsVec(1) boundsVec(4)]
#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      CALL CTRL_BOUND_3D(fld,localmask,
     &     xx_genarr3d_bounds(1,iarr),myThid)
#else
      CALL CTRL_BOUND_3D(fld,maskC,xx_genarr3d_bounds(1,iarr),myThid)
#endif

C The tile exchange for xx_uvel and xx_vvel will be
C  done in CTRL_MAP_INI_GENARR.F when both
C  xx_uvel and xx_vvel are read in.
      if (xx_genarr3d_file(iarr)(1:7).NE.'xx_uvel'.AND.
     &    xx_genarr3d_file(iarr)(1:7).NE.'xx_vvel')
     &    CALL EXCH_XYZ_RL( fld, mythid )

      CALL MDS_WRITE_FIELD(fnamegenOut,ctrlprec,.FALSE.,.FALSE.,
     & 'RL',nr,1,nr,fld,dummyRS,1,optimcycle,mythid)

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END
