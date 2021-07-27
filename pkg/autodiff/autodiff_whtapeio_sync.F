#include "AUTODIFF_OPTIONS.h"
#include "MDSIO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine autodiff_whtapeio_sync( myLev, myStep, myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE autodiff_whtapeio_sync                             |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ctrl.h"
#ifndef ALLOW_OPENAD
# include "AUTODIFF.h"
#endif
#ifdef ALLOW_WHIO_3D
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
# include "MDSIO_BUFF_WH.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myLev
      integer myStep

#ifdef ALLOW_AUTODIFF_WHTAPEIO

      character*(MAX_LEN_FNAM) fName
      integer filePrec, IL, length_of_rec
      LOGICAL iAmDoingIO

C     sNxWh :: x tile size with halo included
C     sNyWh :: y tile size with halo included
C     pocNyWh :: processor sum of sNyWh
C     gloNyWh :: global sum of sNyWh
      INTEGER sNxWh
      INTEGER sNyWh
      INTEGER procNyWh
      INTEGER gloNyWh
      PARAMETER ( sNxWh = sNx+2*Olx )
      PARAMETER ( sNyWh = sNy+2*Oly )
      PARAMETER ( procNyWh = sNyWh*nSy*nSx )
      PARAMETER ( gloNyWh = procNyWh*nPy*nPx )
      logical  exst
#ifdef ALLOW_WHIO_3D
      character*(max_len_mbuf) msgBuf
      integer ioUnit
#endif

c     == functions ==
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

      IF ( .NOT.useAUTODIFF ) THEN
        RETURN
      ENDIF

      IF ( tapeConcatIO ) THEN

      IF ( doSinglePrecTapelev ) THEN
       filePrec = 32
      ELSE
       filePrec = 64
      ENDIF

C Only do I/O if I am the master thread (and mpi process 0 IF tapeSingleCpuIO):
      iAmDoingIO = .FALSE.
      IF ( .NOT.tapeSingleCpuIO .OR. myProcId.EQ.0 ) THEN
        _BEGIN_MASTER( myThid )
        iAmDoingIO = .TRUE.
        _END_MASTER( myThid )
      ENDIF

      IF ( iAmDoingIO ) THEN
        IL = ilnblnk( adTapeDir )
        IF ( .NOT.tapeSingleCpuIO ) THEN
          WRITE(fName,'(2A,I1.1,A,I3.3,A)')
     &         adTapeDir(1:IL),'tapes',myLev,'.',myProcId,'.data'
          length_of_rec = MDS_RECLEN( filePrec,sNxWh*procNyWh,myThid )
        ELSE
          WRITE(fName,'(2A,I1.1,A)')
     &         adTapeDir(1:IL),'tapes',myLev,'.data'
          length_of_rec = MDS_RECLEN( filePrec,sNxWh*gloNyWh,myThid)
        ENDIF
      ENDIF
#ifdef ALLOW_WHIO_3D
      length_of_rec=length_of_rec*nWh
#endif

      tapeFileUnit=tapeFileUnitS(myLev)
      tapeFileCounter=0

      IF ( iAmDoingIO.AND.(myStep.EQ.0).AND.
     &    (myLev.GT.0).AND.(tapeFileUnit.EQ.0) ) THEN
        inquire( file=fName, exist=exst )
#ifdef AUTODIFF_USE_MDSFINDUNITS
        CALL MDSFINDUNIT( tapeFileUnit, myThid )
#else
        CALL AUTODIFF_FINDUNIT( tapeFileUnit, myThid )
#endif
        OPEN( tapeFileUnit, file=fName, status='unknown',
     &         access='direct', recl=length_of_rec )
        tapeFileUnitS(myLev)=tapeFileUnit
#if (defined (ALLOW_INIT_WHTAPEIO) && defined (ALLOW_WHIO_3D))
c exst needs to have been tested before opening the file
        IF (.NOT.exst) then
          iWh=tapeMaxCounter*MAX(nchklev_2,nchklev_3)/nWh+1
          write(msgBuf,'(a,i1,a,i3)') 'whio : create lev ',
     &          myLev,' rec ',iWh
          ioUnit=standardMessageUnit
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          IF ( .NOT.tapeSingleCpuIO ) then
            IF (filePrec .EQ. precFloat32) THEN
              WRITE(tapeFileUnit,rec=iWh) fld3d_procbuff_r4
            ELSE
              WRITE(tapeFileUnit,rec=iWh) fld3d_procbuff_r8
            ENDIF
          ELSE
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
            IF (filePrec .EQ. precFloat32) THEN
              WRITE(tapeFileUnit,rec=iWh) fld3d_globuff_r4
            ELSE
              WRITE(tapeFileUnit,rec=iWh) fld3d_globuff_r8
            ENDIF
#  endif
          ENDIF
          iWh=0
      ENDIF
#endif
      ENDIF

#ifdef ALLOW_WHIO_3D
      _BARRIER
      IF ((myStep.EQ.1).AND.iAmDoingIO.AND.
     &    tapeBufferIO.AND.writeWh) THEN
      if (iWh.LT.1) stop
          write(msgBuf,'(a,i1,a,i3)') 'whio : write lev ',
     &          myLev,' rec ',iWh
          ioUnit=standardMessageUnit
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          IF ( .NOT.tapeSingleCpuIO ) then
            IF (filePrec .EQ. precFloat32) THEN
              WRITE(tapeFileUnit,rec=iWh) fld3d_procbuff_r4
            ELSE
              WRITE(tapeFileUnit,rec=iWh) fld3d_procbuff_r8
            ENDIF
          ELSE
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
            IF (filePrec .EQ. precFloat32) THEN
              WRITE(tapeFileUnit,rec=iWh) fld3d_globuff_r4
            ELSE
              WRITE(tapeFileUnit,rec=iWh) fld3d_globuff_r8
            ENDIF
#  endif
          ENDIF
      ENDIF
      _BARRIER

      IF (myStep.EQ.0) THEN
        tapeBufferIO=.TRUE.
      ELSE
        tapeBufferIO=.FALSE.
        writeWh=.FALSE.
      ENDIF
      iWh=0
      jWh=0
#endif /* ALLOW_WHIO_3D */

      ENDIF !IF ( tapeConcatIO ) THEN

#endif /* ALLOW_AUTODIFF_WHTAPEIO */

      end
