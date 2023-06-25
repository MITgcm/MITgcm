#include "CTRL_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CTRL_MAP_INI_GENTIM2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_INI_GENTIM2D( myThid )

C     !DESCRIPTION: \bv
C     *================================================================
C     | SUBROUTINE CTRL_MAP_INI_GENTIM2D
C     | Dimensionalize and preprocess time variable controls.
C     *================================================================
C     | Older version (from before Tapenade changes, PR 685) for OpenAD,
C     | used in OpenAD test exp. hs94.1x64x5 & halfpipe_streamice
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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

#ifdef ALLOW_GENTIM2D_CONTROL
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == local variables ==
      integer iarr
      integer numsmo
      CHARACTER*(80) fnamegenIn
      CHARACTER*(80) fnamegenOut
      CHARACTER*(80) fnamegenTmp
      CHARACTER*(80) fnamebase
      integer startrec
      integer endrec
      integer diffrec
      integer iRec, jrec, kRec, lRec
      integer replicated_nrec
      integer replicated_ntimes
      logical doglobalread
      logical ladinit
      logical dowc01
      logical dosmooth
      logical doscaling
      _RL     xx_gen(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS     mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_ECCO
      _RL     xx_gen_tmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      integer nyearsINT
      _RL     recip_nyearsRL
#endif
      integer bi,bj
      integer i,j,k2
      integer ilgen
      integer ilDir
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('CTRL_MAP_INI_GENTIM2D',myThid)
#endif /* ALLOW_DEBUG */

C--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

C     Find ctrlDir (w/o trailing blanks) length
      ilDir = ilnblnk(ctrlDir)

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          xx_gen(i,j,bi,bj)=0. _d 0
#ifdef ALLOW_ECCO
          xx_gen_tmp(i,j,bi,bj)=0. _d 0
#endif
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

        ilgen=ilnblnk( xx_gentim2d_file(iarr) )
        fnamebase = xx_gentim2d_file(iarr)(1:ilgen)
        call ctrl_init_rec ( fnamebase,
     I       xx_gentim2d_startdate1(iarr),
     I       xx_gentim2d_startdate2(iarr),
     I       xx_gentim2d_period(iarr),
     I       1,
     O       xx_gentim2d_startdate(1,iarr),
     O       diffrec, startrec, endrec,
     I       myThid )

C From CTRL_INIT_REC one gets [start,end,diff]rec.
C Below, file xx_*.$iter.data is read in from records startrec to endrec,
C followed by a write to file xx_*.effective.$iter.data of record 1 to diffrec
C (see TAF-generated S/R ctrl_map_ini_gentim2dmd).

#ifdef ALLOW_CTRL_DEBUG
C Note: In the "md" (i.e. TAF modified) version of this fwd S/R, DEBUG calls and
C print statements are removed. See "Automatic Differentiation" chap. in the doc
        write(*,*) 'AA: iarr,xx_gentim2d_startdate(1,iarr): ',
     &              iarr,xx_gentim2d_startdate(1,iarr)
#endif
        dosmooth=.false.
        dowc01  = .false.
        doscaling=.true.

        numsmo=1
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'WC01') then
          dowc01=.TRUE.
          if (xx_gentim2d_preproc_i(k2,iarr).NE.0)
     &         numsmo=xx_gentim2d_preproc_i(k2,iarr)
         endif
         if ((.NOT.dowc01).AND.
     &        (xx_gentim2d_preproc(k2,iarr).EQ.'smooth')) then
          dosmooth=.TRUE.
          if (xx_gentim2d_preproc_i(k2,iarr).NE.0)
     &         numsmo=xx_gentim2d_preproc_i(k2,iarr)
         endif
         if (xx_gentim2d_preproc(k2,iarr).EQ.'noscaling') then
          doscaling=.FALSE.
         endif
        enddo

        ilgen=ilnblnk( xx_gentim2d_file(iarr) )
        fnamebase = xx_gentim2d_file(iarr)(1:ilgen)
        write(fnamegenIn(1:80),'(2a,i10.10)')
     &   ctrlDir(1:ilDir)//fnamebase(1:ilgen),'.',optimcycle
        write(fnamegenOut(1:80),'(2a,i10.10)')
     &   ctrlDir(1:ilDir)//fnamebase(1:ilgen),'.effective.',optimcycle
        write(fnamegenTmp(1:80),'(2a,i10.10)')
     &   ctrlDir(1:ilDir)//fnamebase(1:ilgen),'.tmp.',optimcycle

C--   docycle

        replicated_nrec=diffrec
        replicated_ntimes=0
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'docycle') then
          if (xx_gentim2d_preproc_i(k2,iarr).NE.0) then
           replicated_nrec=min(diffrec,xx_gentim2d_preproc_i(k2,iarr))
           replicated_ntimes=
     &          int(float(diffrec)/float(replicated_nrec))
           if (replicated_ntimes*replicated_nrec.LT.diffrec)
     &          replicated_ntimes=replicated_ntimes+1
           if (replicated_ntimes*replicated_nrec.GT.diffrec)
     &          replicated_ntimes=replicated_ntimes-1
          endif
         endif
        enddo

#ifdef ALLOW_CTRL_DEBUG
        write(*,'(A,1x,5I6)')
     &   'AD:[start,end,diff]rec, replicated_[nrec,ntimes]: ',
     &              startrec, endrec, diffrec,
     &              replicated_nrec,replicated_ntimes
#endif
        DO jrec = 1, replicated_ntimes+1
         DO iRec = 1, replicated_nrec
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE xx_gentim2d_dummy = dummytape, key = 1 , kind = isbyte
#endif
          kRec=replicated_nrec*(jrec-1)+iRec
          lRec=startrec+iRec-1
          IF (kRec.LE.endrec) THEN
#ifdef ALLOW_CTRL_DEBUG
           ilgen=ilnblnk( fnamegenIn )
           write(*,*) 'AE: iarr,[i,j,k,l]rec: ',iarr,iRec,jrec,lRec
           write(*,*) 'AF: fnamegenIn: ', fnamegenIn(1:ilgen)
#endif
C fnamegenIn is xx_*.$iter.data, so the required access records starts at
C startrec+iRec-1 instead of iRec in the read call below. In adjoint mode, S/R
C ADACTIVE_READ_XY reads adxx_*.$iter.data in reverse order, with required
C access from endrec back to startrec. This requires creating adxx_*.$iter.data
C file of size endrec, with valid gradients from records startrec to endrec
C but all zeros from 1 to startrec-1. See documentation:
C https://mitgcm.readthedocs.io/en/latest/ocean_state_est/ocean_state_est.html#ctrl-model-parameter-adjustment-capability
#ifdef ALLOW_AUTODIFF
           CALL ACTIVE_READ_XY( fnamegenIn, xx_gen, lRec,
     &          doglobalread, ladinit, optimcycle,
     &          myThid, xx_gentim2d_dummy(iarr) )
#else
           CALL READ_REC_XY_RL( fnamegenIn, xx_gen, lRec, 1, myThid )
#endif
C Here, we write record kRec (from 1 to diffrec) to file xx_*.effective.$iter
#ifdef ALLOW_AUTODIFF
#ifdef ALLOW_CTRL_DEBUG
           ilgen=ilnblnk( fnamegenOut )
           write(*,*) 'AG: iarr,[i,j,k]rec: ',iarr,iRec,jrec,kRec
           write(*,*) 'AH: fnamegenOut: ', fnamegenOut(1:ilgen)
#endif
           CALL ACTIVE_WRITE_XY( fnamegenOut, xx_gen, kRec, optimcycle,
     &          myThid, xx_gentim2d_dummy(iarr) )
#else
           CALL WRITE_REC_XY_RL( fnamegenOut, xx_gen, kRec, 1, myThid )
#endif
          ENDIF
         ENDDO
        ENDDO

C--   rmcycle
#ifdef ALLOW_ECCO
        replicated_nrec=diffrec
        replicated_ntimes=0
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'rmcycle') then
          if (xx_gentim2d_preproc_i(k2,iarr).NE.0) then
           replicated_nrec=min(diffrec,xx_gentim2d_preproc_i(k2,iarr))
           replicated_ntimes=
     &          int(float(diffrec)/float(replicated_nrec))
           if (replicated_ntimes*replicated_nrec.LT.diffrec)
     &          replicated_ntimes=replicated_ntimes+1
           if (replicated_ntimes*replicated_nrec.GT.diffrec)
     &          replicated_ntimes=replicated_ntimes-1
          endif
         endif
        enddo

#ifdef ALLOW_CTRL_DEBUG
        write(*,'(A,1x,5I6)')
     &  'AI:[start,end,diff]rec, replicated_[nrec,ntimes]: ',
     &      startrec,endrec,diffrec,replicated_nrec,replicated_ntimes
#endif

        IF (replicated_ntimes.GT.0) THEN

C     create cyclic average

         nyearsINT=1+int((diffrec-replicated_nrec)/replicated_nrec)
         recip_nyearsRL=1. _d 0/float(nyearsINT)

#ifdef ALLOW_CTRL_DEBUG
         write(*,*) 'AJ: nyears[INT,RL]: ',nyearsINT,recip_nyearsRL
#endif

         DO iRec = 1, replicated_nrec

          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
              xx_gen(i,j,bi,bj) = zeroRL
             ENDDO
            ENDDO
           ENDDO
          ENDDO

          DO jrec=1,nyearsINT
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE xx_gentim2d_dummy = dummytape, key = 1 , kind = isbyte
#endif
           kRec=iRec+(jrec-1)*replicated_nrec
#ifdef ALLOW_CTRL_DEBUG
           write(*,*) 'AK: iarr,[i,j,k]rec: ',iarr,iRec,jrec,kRec
           ilgen=ilnblnk( fnamegenOut )
           write(*,*) 'AL: fnamegenOut: ',fnamegenOut(1:ilgen)
#endif
#ifdef ALLOW_AUTODIFF
           CALL ACTIVE_READ_XY( fnamegenOut, xx_gen_tmp, kRec,
     &          doglobalread, ladinit, optimcycle,
     &          myThid, xx_gentim2d_dummy(iarr) )
#else
           CALL READ_REC_XY_RL( fnamegenOut, xx_gen_tmp, kRec,
     &          1, myThid )
#endif
           DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
             DO j = 1,sNy
              DO i = 1,sNx
               xx_gen(i,j,bi,bj) = xx_gen(i,j,bi,bj)
     &              +xx_gen_tmp(i,j,bi,bj)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
C     end jrec
          ENDDO

          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
            DO j = 1,sNy
             DO i = 1,sNx
              xx_gen(i,j,bi,bj) = xx_gen(i,j,bi,bj) * recip_nyearsRL
             ENDDO
            ENDDO
           ENDDO
          ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE xx_gentim2d_dummy = dummytape, key = 1 , kind = isbyte
#endif

#ifdef ALLOW_AUTODIFF
          CALL ACTIVE_WRITE_XY( fnamegenTmp, xx_gen, iRec, optimcycle,
     &         myThid, xx_gentim2d_dummy(iarr) )
#else
          CALL WRITE_REC_XY_RL( fnamegenTmp, xx_gen, iRec, 1, myThid )
#endif

         ENDDO

C     subtract cyclic average
         DO jrec = 1, replicated_ntimes+1
          DO iRec = 1, replicated_nrec
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE xx_gentim2d_dummy = dummytape, key = 1 , kind = isbyte
#endif
           kRec=replicated_nrec*(jrec-1)+iRec
#ifdef ALLOW_CTRL_DEBUG
           write(*,*) 'AM: iarr,[i,j,k]rec: ',iarr,iRec,jrec,kRec
           ilgen=ilnblnk( fnamegenOut )
           write(*,*) 'AN: fnamegenOut: ',fnamegenOut(1:ilgen)
#endif
           IF (kRec.LE.diffrec) THEN
#ifdef ALLOW_AUTODIFF
            CALL ACTIVE_READ_XY( fnamegenOut, xx_gen, kRec,
     &           doglobalread, ladinit, optimcycle,
     &           myThid, xx_gentim2d_dummy(iarr) )
#else
            CALL READ_REC_XY_RL( fnamegenOut, xx_gen, kRec, 1, myThid )
#endif
#ifdef ALLOW_AUTODIFF
            CALL ACTIVE_READ_XY( fnamegenTmp, xx_gen_tmp, iRec,
     &           doglobalread, ladinit, optimcycle,
     &           myThid, xx_gentim2d_dummy(iarr) )
#else
            CALL READ_REC_XY_RL( fnamegenTmp, xx_gen_tmp, iRec, 1,
     &           myThid )
#endif
            DO bj=myByLo(myThid),myByHi(myThid)
             DO bi=myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i = 1,sNx
                xx_gen(i,j,bi,bj)=xx_gen(i,j,bi,bj)
     &              -xx_gen_tmp(i,j,bi,bj)
               ENDDO
              ENDDO
             ENDDO
            ENDDO
#ifdef ALLOW_AUTODIFF
            CALL ACTIVE_WRITE_XY( fnamegenOut, xx_gen, kRec,
     &           optimcycle, myThid, xx_gentim2d_dummy(iarr) )
#else
            CALL WRITE_REC_XY_RL( fnamegenOut, xx_gen, kRec, 1,
     &           myThid )
#endif
           ENDIF
          ENDDO
         ENDDO

        ENDIF
#endif /* ifdef ALLOW_ECCO */

C--   scaling and smoothing

C The access of records startrec to endrec in  xx_*.*iter was already
C done correctly above. From here, we read in xx_*.effective.$iter.data
C of size diffrec, so no more fix of record is needed from here on out.
        DO iRec = 1, diffrec
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE xx_gentim2d_dummy = dummytape, key = 1 , kind = isbyte
#endif

#ifdef ALLOW_CTRL_DEBUG
         ilgen=ilnblnk( fnamegenOut )
         lRec=startrec+iRec-1
         write(*,'(A,1x,A,1x,2I6)') 'AO: fnamegenOut, iRec,lRec ',
     &       fnamegenOut(1:ilgen),iRec,lRec
#endif
#ifdef ALLOW_AUTODIFF
         CALL ACTIVE_READ_XY( fnamegenOut, xx_gen, iRec,
     &        doglobalread, ladinit, optimcycle,
     &        myThid, xx_gentim2d_dummy(iarr) )
#else
         CALL READ_REC_XY_RL( fnamegenOut, xx_gen, iRec, 1, myThid )
#endif

#ifndef ALLOW_OPENAD
         jrec=1
C Not clear which record to use for timevariable weights ;
C For now, just assumes records from startrec to endrec are available in file
         do k2 = 1, maxCtrlProc
          if (xx_gentim2d_preproc(k2,iarr).EQ.'variaweight') then
            jrec=startrec+iRec-1
          endif
         enddo
         CALL READ_REC_3D_RL( xx_gentim2d_weight(iarr), ctrlprec, 1,
     &             wgentim2d(1-OLx,1-OLy,1,1,iarr), jrec, 1, myThid )

C--   Get appropriate mask
         call ctrl_get_mask2D(xx_gentim2d_file(iarr), mask2D, myThid)

#ifdef ALLOW_SMOOTH
         IF (useSMOOTH) THEN
          IF (dowc01) call smooth_correl2d(xx_gen,mask2D,numsmo,myThid)
          IF (dosmooth) call smooth2d(xx_gen,mask2D,numsmo,myThid)
         ENDIF
#endif /* ALLOW_SMOOTH */

         DO bj=myByLo(myThid), myByHi(myThid)
          DO bi=myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
             if ((mask2D(i,j,bi,bj).NE.0.).AND.
     &            (wgentim2d(i,j,bi,bj,iarr).GT.0.)) then
              IF (doscaling) then
               xx_gen(i,j,bi,bj)=xx_gen(i,j,bi,bj)
     &              /sqrt(wgentim2d(i,j,bi,bj,iarr))
              ENDIF             ! IF (doscaling) then
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

#ifdef ALLOW_CTRL_DEBUG
         ilgen=ilnblnk( fnamegenOut )
         write(*,'(A,1x,I6,1x,A)')
     &      'AQ: iRec,fnamegenOut: ',iRec,fnamegenOut(1:ilgen)
#endif
#ifdef ALLOW_AUTODIFF
         CALL ACTIVE_WRITE_XY( fnamegenOut, xx_gen, iRec, optimcycle,
     &        myThid, xx_gentim2d_dummy(iarr) )
#else
         CALL WRITE_REC_XY_RL( fnamegenOut, xx_gen, iRec, 1, myThid )
#endif
C--   end iRec loop
        ENDDO

#ifndef ALLOW_OPENAD
       endif
#endif

C--   end iarr loop
      ENDDO

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('CTRL_MAP_INI_GENTIM2D',myThid)
#endif /* ALLOW_DEBUG */
#endif /* ALLOW_GENTIM2D_CONTROL */

      RETURN
      END
