C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_write_whalos.F,v 1.7 2011/06/23 22:11:07 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C     !ROUTINE: mds_write_whalos
C     !INTERFACE:
      subroutine mds_write_whalos(
     I                    fName,
     I                    len,
     I                    filePrec,
     I                    fid,
     I                    n2d,
     I                    fldRL,
     I                    irec,
     I                    locSingleCPUIO,
     I                    locBufferIO,
     I                    mythid
     &                  )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE mds_write_whalos
c     ==================================================================
c     o Write file that includes halos. The main purpose is for
c       adjoint related "tape I/O". The secondary purpose is debugging.
c     ==================================================================
c     SUBROUTINE mds_write_whalos
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_WHIO
# include "MDSIO_BUFF_WH.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     fName     -  extended tape fName.
c     len       -  number of characters in fName.
c     filePrec  -  number of bits per word in file (32 or 64).
c     fid       -  file unit (its use is not implemented yet).
C     n2d       -  size of the fldRL third dimension.
c     fldRL     -  array to read.
c     irec      -  record number to be written.
c     mythid    -  number of the thread or instance of the program.

      integer mythid
      character*(*) fName
      integer len
      integer fid
      integer filePrec
      integer n2d
      integer irec
      _RL     fldRL(1-Olx:sNx+Olx,1-Oly:sNy+Oly,n2d,nSx,nSy)
      logical locSingleCPUIO, locBufferIO
CEOP

#ifdef ALLOW_WHIO
C     !LOCAL VARIABLES:
c     == local variables ==

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

C     !LOCAL VARIABLES:
c     == local variables ==
      character*(MAX_LEN_FNAM) pfName
      character*(MAX_LEN_MBUF) msgBuf
      integer IL
      integer bx,by

      integer lengthBuff, length_of_rec
      integer i2d, i3d
      integer i,j,k,bi,bj,ii
      integer dUnit, irec2d
      LOGICAL iAmDoingIO

      _RL fld2d(1:sNxWh,1:sNyWh,nSx,nSy)

c     == functions ==
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

c     == end of interface ==

#ifdef ALLOW_WHIO_3D
      writeWh=.TRUE.
#endif

      IF ( .NOT.locSingleCpuIO ) then
        lengthBuff=sNxWh*procNyWh
      ELSE
        lengthBuff=sNxWh*gloNyWh
      ENDIF

C Only do I/O if I am the master thread (and mpi process 0 IF locSingleCpuIO):
      iAmDoingIO = .FALSE.
      IF ( .NOT.locSingleCpuIO .OR. myProcId.EQ.0 ) THEN
        _BEGIN_MASTER( myThid )
        iAmDoingIO = .TRUE.
        _END_MASTER( myThid )
      ENDIF

      IF ( iAmDoingIO ) THEN
c get the unit and open file
      IL  = ILNBLNK( fName )
      IF ( .NOT.locSingleCpuIO ) THEN
        WRITE(pfName,'(2A,I3.3,A)') fName(1:IL),'.',myProcId,'.data'
        length_of_rec = MDS_RECLEN( filePrec,sNxWh*procNyWh,myThid )
      ELSE
        WRITE(pfName,'(2A)') fName(1:IL),'.data'
        length_of_rec = MDS_RECLEN( filePrec,sNxWh*gloNyWh,myThid)
      ENDIF
      IF (fid.EQ.0) THEN
        CALL MDSFINDUNIT( dUnit, myThid )
        OPEN( dUnit, file=pfName, status='unknown',
     &         access='direct', recl=length_of_rec )
      ELSE
        dUnit=fid
      ENDIF
      ENDIF


      do i2d=1,n2d

        IF (filePrec .EQ. precFloat32) THEN
          CALL MDS_PASS_R4toRL( fld2d_procbuff_r4, fldRL,
     &             OLx, OLy, 1, i2d, n2d, 0, 0, .FALSE., myThid )
          IF ( locSingleCpuIO ) then
            CALL BAR2( myThid )
#  ifndef EXCLUDE_WHIO_GLOBUFF_2D
            CALL GATHER_2D_WH_R4( fld2d_globuff_r4,
     &                            fld2d_procbuff_r4,myThid)
#  endif
          ENDIF
        ELSE
          CALL MDS_PASS_R8toRL( fld2d_procbuff_r8, fldRL,
     &             OLx, OLy, 1, i2d, n2d, 0, 0, .FALSE., myThid )
          IF ( locSingleCpuIO ) then
            CALL BAR2( myThid )
#  ifndef EXCLUDE_WHIO_GLOBUFF_2D
            CALL GATHER_2D_WH_R8( fld2d_globuff_r8,
     &                            fld2d_procbuff_r8,myThid)
#  endif
          ENDIF
        ENDIF

        _BARRIER
#ifdef ALLOW_WHIO_3D
        IF ( iAmDoingIO.AND.locBufferIO.AND.(fid.NE.0) ) THEN
c reset counter if needed
          IF (jWh.EQ.nWh) jWh=0
c increment counter
          jWh=jWh+1
c determine current file record
          irec2d=i2d+n2d*(irec-1)
          iWh=(irec2d-1)/nWh+1
c copy
          DO i=1,lengthBuff
            j=(jWh-1)*lengthBuff+i
            IF ( .NOT.locSingleCpuIO ) then
              IF (filePrec .EQ. precFloat32) THEN
                fld3d_procbuff_r4(j)=fld2d_procbuff_r4(i)
              ELSE
                fld3d_procbuff_r8(j)=fld2d_procbuff_r8(i)
              ENDIF
            ELSE
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
              IF (filePrec .EQ. precFloat32) THEN
                fld3d_globuff_r4(j)=fld2d_globuff_r4(i)
              ELSE
                fld3d_globuff_r8(j)=fld2d_globuff_r8(i)
              ENDIF
#  endif
            ENDIF
          ENDDO
c write chunk if needed
          IF (jWh.EQ.nWh) THEN
            IF ( .NOT.locSingleCpuIO ) then
              IF (filePrec .EQ. precFloat32) THEN
                WRITE(dUnit,rec=iWh) fld3d_procbuff_r4
              ELSE
                WRITE(dUnit,rec=iWh) fld3d_procbuff_r8
              ENDIF
            ELSE
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
              IF (filePrec .EQ. precFloat32) THEN
                WRITE(dUnit,rec=iWh) fld3d_globuff_r4
              ELSE
                WRITE(dUnit,rec=iWh) fld3d_globuff_r8
              ENDIF
#  endif
            ENDIF
          ENDIF

        ELSEIF ( iAmDoingIO ) THEN
#else
        IF ( iAmDoingIO ) THEN
#endif
          irec2d=i2d+n2d*(irec-1)
          IF ( .NOT.locSingleCpuIO ) then
            IF (filePrec .EQ. precFloat32) THEN
              WRITE(dUnit,rec=irec2d) fld2d_procbuff_r4
            ELSE
              WRITE(dUnit,rec=irec2d) fld2d_procbuff_r8
            ENDIF
          ELSE
#  ifndef EXCLUDE_WHIO_GLOBUFF_2D
            IF (filePrec .EQ. precFloat32) THEN
              WRITE(dUnit,rec=irec2d) fld2d_globuff_r4
            ELSE
              WRITE(dUnit,rec=irec2d) fld2d_globuff_r8
            ENDIF
#  endif
          ENDIF
        ENDIF
        _BARRIER

      enddo

      IF ( iAmDoingIO.AND.(fid.EQ.0) ) THEN
        CLOSE( dUnit )
      ENDIF

#endif

      RETURN
      END
