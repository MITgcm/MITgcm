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
      integer numsmo
      character*(80) fnamegenIn
      character*(80) fnamegenOut
      character*(80) fnamegenTmp
      character*(80) fnamebase
      integer startrec
      integer endrec
      integer diffrec
      integer irec, jrec, krec
      integer replicated_nrec
      integer replicated_ntimes
      logical doglobalread
      logical ladinit
      logical dowc01
      logical dosmooth
      logical doscaling
      _RL     xx_gen(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     mask2D(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RS dummyRS(1)
#ifdef ALLOW_ECCO
      _RL     xx_gen_tmp(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer nyearsINT
      _RL     nyearsRL
#endif
      integer bi,bj
      integer i,j,k2
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      integer ilgen
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

        dosmooth=.false.
        dowc01  = .false.
        doscaling=.true.

        numsmo=1
        do k2 = 1, maxCtrlProc
          if (xx_gentim2d_preproc(k2,iarr).EQ.'WC01') then
             dowc01=.TRUE.
             if (xx_gentim2d_preproc_i(k2,iarr).NE.0)
     &           numsmo=xx_gentim2d_preproc_i(k2,iarr)
          endif
          if ((.NOT.dowc01).AND.
     &        (xx_gentim2d_preproc(k2,iarr).EQ.'smooth')) then
             dosmooth=.TRUE.
             if (xx_gentim2d_preproc_i(k2,iarr).NE.0)
     &           numsmo=xx_gentim2d_preproc_i(k2,iarr)
          endif
          if (xx_gentim2d_preproc(k2,iarr).EQ.'noscaling') then
             doscaling=.FALSE.
          endif
        enddo

        fnamebase = xx_gentim2d_file(iarr)
        ilgen=ilnblnk( fnamebase )
        write(fnamegenIn(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.',optimcycle
        write(fnamegenOut(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.effective.',optimcycle
        write(fnamegenTmp(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.tmp.',optimcycle

c-- docycle

        replicated_nrec=diffrec
        replicated_ntimes=0
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'docycle') then
           if (xx_gentim2d_preproc_i(k2,iarr).NE.0) then
            replicated_nrec=min(diffrec,xx_gentim2d_preproc_i(k2,iarr))
            replicated_ntimes=
     &       int(float(diffrec)/float(replicated_nrec))
            if (replicated_ntimes*replicated_nrec.LT.diffrec)
     &      replicated_ntimes=replicated_ntimes+1
            if (replicated_ntimes*replicated_nrec.GT.diffrec)
     &      replicated_ntimes=replicated_ntimes-1
           endif
         endif
        enddo

        DO jrec = 1, replicated_ntimes+1
        DO irec = 1, replicated_nrec
#ifdef ALLOW_AUTODIFF
CADJ STORE xx_gentim2d_dummy = ctrltape, key = 1 , kind = isbyte
#endif
        krec=replicated_nrec*(jrec-1)+irec
        IF (krec.LE.endrec) THEN
#ifdef ALLOW_AUTODIFF
         call active_read_xy( fnamegenIn, xx_gen, irec,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gentim2d_dummy(iarr) )
#else
         CALL READ_REC_XY_RL( fnamegenIn, xx_gen, iRec, 1, myThid )
#endif
#ifdef ALLOW_AUTODIFF
         call active_write_xy( fnamegenOut, xx_gen, krec, optimcycle,
     &       mythid, xx_gentim2d_dummy(iarr) )
#else
         CALL WRITE_REC_XY_RL( fnamegenOut, xx_gen, iRec, 1, myThid )
#endif
        ENDIF
        ENDDO
        ENDDO

c-- rmcycle
#ifdef ALLOW_ECCO
        replicated_nrec=endrec
        replicated_ntimes=0
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'rmcycle') then
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

c       print*,'endrec',endrec,replicated_ntimes,replicated_nrec

        IF (replicated_ntimes.GT.0) THEN

c create cyclic average

        nyearsINT=1+int((endrec-replicated_nrec)/replicated_nrec)
        nyearsRL=float(nyearsINT)

c       print*,'nyearsINT',nyearsINT,nyearsRL

        DO irec = 1, replicated_nrec

         call ecco_zero(xx_gen,1,zeroRL,myThid)

         do jrec=1,nyearsINT
#ifdef ALLOW_AUTODIFF
CADJ STORE xx_gentim2d_dummy = ctrltape, key = 1 , kind = isbyte
#endif
           krec=irec+(jrec-1)*replicated_nrec
#ifdef ALLOW_AUTODIFF
           call active_read_xy( fnamegenOut, xx_gen_tmp, krec,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gentim2d_dummy(iarr) )
#else
           CALL READ_REC_XY_RL( fnamegenOut, xx_gen_tmp, krec,
     &                       1, myThid )
#endif
           call ecco_add(xx_gen_tmp,1,xx_gen,1,myThid)
         enddo

         call ecco_div(xx_gen,1,nyearsRL,myThid)

#ifdef ALLOW_AUTODIFF
CADJ STORE xx_gentim2d_dummy = ctrltape, key = 1 , kind = isbyte
#endif

#ifdef ALLOW_AUTODIFF
         call active_write_xy( fnamegenTmp, xx_gen, iRec, optimcycle,
     &       mythid, xx_gentim2d_dummy(iarr) )
#else
         CALL WRITE_REC_XY_RL( fnamegenTmp, xx_gen, iRec, 1, myThid )
#endif

        ENDDO

c subtract cyclic average
        DO jrec = 1, replicated_ntimes+1
        DO irec = 1, replicated_nrec
#ifdef ALLOW_AUTODIFF
CADJ STORE xx_gentim2d_dummy = ctrltape, key = 1 , kind = isbyte
#endif
        krec=replicated_nrec*(jrec-1)+irec
        IF (krec.LE.endrec) THEN
#ifdef ALLOW_AUTODIFF
         CALL active_read_xy( fnamegenOut, xx_gen, kRec,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gentim2d_dummy(iarr) )
#else
         CALL READ_REC_XY_RL( fnamegenOut, xx_gen, kRec, 1, myThid )
#endif
#ifdef ALLOW_AUTODIFF
         CALL active_read_xy( fnamegenTmp, xx_gen_tmp, iRec,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gentim2d_dummy(iarr) )
#else
         CALL READ_REC_XY_RL( fnamegenTmp, xx_gen_tmp, iRec, 1, myThid )
#endif
         CALL ecco_subtract(xx_gen_tmp,1,xx_gen,1,myThid)
#ifdef ALLOW_AUTODIFF
         CALL active_write_xy( fnamegenOut, xx_gen, kRec, optimcycle,
     &       mythid, xx_gentim2d_dummy(iarr) )
#else
         CALL WRITE_REC_XY_RL( fnamegenOut, xx_gen, kRec, 1, myThid )
#endif
        ENDIF
        ENDDO
        ENDDO

        ENDIF
#endif /* ifdef ALLOW_ECCO */

c-- scaling and smoothing

        DO irec = 1, diffrec
#ifdef ALLOW_AUTODIFF
CADJ STORE xx_gentim2d_dummy = ctrltape, key = 1 , kind = isbyte
#endif

#ifdef ALLOW_AUTODIFF
         call active_read_xy( fnamegenOut, xx_gen, irec,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_gentim2d_dummy(iarr) )
#else
         CALL READ_REC_XY_RL( fnamegenOut, xx_gen, iRec, 1, myThid )
#endif

#ifndef ALLOW_OPENAD
         jrec=1
         do k2 = 1, maxCtrlProc
          if (xx_gentim2d_preproc(k2,iarr).EQ.'variaweight') jrec=irec
         enddo
         CALL MDS_READ_FIELD( xx_gentim2d_weight(iarr),ctrlprec,
     &    .FALSE.,'RL',1,1,1,wgentim2d(1-Olx,1-Oly,1,1,iarr),
     &    dummyRS,jrec,myThid )

C --- Get appropriate mask
      call ctrl_get_mask2D(xx_gentim2d_file(iarr), mask2D, myThid)

#ifdef ALLOW_SMOOTH
         IF (useSMOOTH) THEN
          IF (dowc01) call smooth_correl2d(xx_gen,mask2D,numsmo,mythid)
          IF (dosmooth) call smooth2d(xx_gen,mask2D,numsmo,mythid)
         ENDIF
#endif /* ALLOW_SMOOTH */

         DO bj=myByLo(myThid), myByHi(myThid)
          DO bi=myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
             if ((mask2D(i,j,bi,bj).NE.0.).AND.
     &           (wgentim2d(i,j,bi,bj,iarr).GT.0.)) then
               IF (doscaling) then
                xx_gen(i,j,bi,bj)=xx_gen(i,j,bi,bj)
     &                  /sqrt(wgentim2d(i,j,bi,bj,iarr))
               ENDIF ! IF (doscaling) then
             else
                xx_gen(i,j,bi,bj)=0. _d 0
             endif
            ENDDO
           ENDDO
          ENDDO
         ENDDO
#endif /* ALLOW_OPENAD */

         CALL CTRL_BOUND_2D(xx_gen,mask2D,
     &        xx_gentim2d_bounds(1,iarr),myThid)

         CALL EXCH_XY_RL ( xx_gen , myThid )

#ifdef ALLOW_AUTODIFF
         call active_write_xy( fnamegenOut, xx_gen, irec, optimcycle,
     &       mythid, xx_gentim2d_dummy(iarr) )
#else
         CALL WRITE_REC_XY_RL( fnamegenOut, xx_gen, iRec, 1, myThid )
#endif

c-- end irec loop
        ENDDO

#ifndef ALLOW_OPENAD
       endif
#endif

c-- end iarr loop
      ENDDO

#endif /* ALLOW_GENTIM2D_CONTROL */

      RETURN
      END
