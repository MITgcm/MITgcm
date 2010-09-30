C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_read_whalos.F,v 1.3 2010/09/30 01:02:22 jmc Exp $
C $fName:  $

#include "MDSIO_OPTIONS.h"

CBOP
C     !ROUTINE: mds_read_whalos
C     !INTERFACE:
      subroutine mds_read_whalos(
     I                   fName,
     I                   len,
     I                   filePrec,
     I                   fid,
     I                   n2d,
     O                   fldRL,
     I                   irec,
     I                   mythid
     &                 )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE mds_read_whalos
c     ==================================================================
c     o Read file that includes halos. The main purpose is for
c       adjoint related "tape I/O". The secondary purpose is debugging.
c     ==================================================================
c     SUBROUTINE mds_read_whalos
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_WHIO
#include "MDSIO_BUFF_WH.h"
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

      character*(MAX_LEN_FNAM) pfName
      character*(MAX_LEN_MBUF) msgBuf
      integer IL,pIL
      integer bx,by

      integer length2d, length3d, length_of_rec
      integer i2d, i3d
      integer i,j,k,bi,bj,ii
      integer dUnit, irec2d
      LOGICAL iAmDoingIO

      _RL fld2d(1:sNxWh,1:sNyWh,nSx,nSy)

c     == functions ==
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      LOGICAL  MASTER_CPU_IO
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN
      EXTERNAL MASTER_CPU_IO

c     == end of interface ==

      length2d=sNxWh*procNyWh
      length3d=length2d*nr

C Only do I/O if I am the master thread (and mpi process 0 IF useSingleCpuIO):
      iAmDoingIO = MASTER_CPU_IO(myThid)
      IF ( iAmDoingIO ) THEN
c get the unit and open file
      CALL MDSFINDUNIT( dUnit, myThid )
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )
      IF ( pIL.EQ.0 ) THEN
        pfName = fName
      ELSE
       WRITE(pfName,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
      ENDIF
      pIL=ILNBLNK( pfName )
      IF ( .NOT.useSingleCpuIO ) THEN
        WRITE(pfName,'(2A,I3.3,A)') pfName(1:pIL),'.',myProcId,'.data'
        length_of_rec = MDS_RECLEN( filePrec,sNxWh*procNyWh, myThid )
      ELSE
        WRITE(pfName,'(2A)') pfName(1:pIL),'.data'
        length_of_rec = MDS_RECLEN( filePrec,sNxWh*gloNyWh,myThid)
      ENDIF
      OPEN( dUnit, file=pfName, status='old',
     &         access='direct', recl=length_of_rec )
      ENDIF


      do i2d=1,n2d

        _BARRIER
        IF ( iAmDoingIO ) THEN
        irec2d=i2d+n2d*(irec-1)
          IF ( .NOT.useSingleCpuIO ) then
            IF (filePrec .EQ. precFloat32) THEN
              READ(dUnit,rec=irec2d) fld2d_procbuff_r4
            ELSE
              READ(dUnit,rec=irec2d) fld2d_procbuff_r8
            ENDIF
          ELSE
            IF (filePrec .EQ. precFloat32) THEN
              READ(dUnit,rec=irec2d) fld2d_globuff_r4
            ELSE
              READ(dUnit,rec=irec2d) fld2d_globuff_r8
            ENDIF
          ENDIF
        ENDIF
        _BARRIER

        IF (filePrec .EQ. precFloat32) THEN
          IF ( useSingleCpuIO ) then
            CALL SCATTER_2D_WH_R4 ( fld2d_globuff_r4,
     &                              fld2d_procbuff_r4,myThid)
            CALL BAR2( myThid )
          ENDIF
          CALL MDS_PASS_WH_R4toRL( fld2d_procbuff_r4, fldRL,
     &                    1, i2d, n2d, 0, 0, .TRUE., myThid )
        ELSE
          IF ( useSingleCpuIO ) then
            CALL SCATTER_2D_WH_R8 ( fld2d_globuff_r8,
     &                              fld2d_procbuff_r8,myThid)
            CALL BAR2( myThid )
          ENDIF
          CALL MDS_PASS_WH_R8toRL( fld2d_procbuff_r8, fldRL,
     &                    1, i2d, n2d, 0, 0, .TRUE., myThid )
        ENDIF

      enddo

       IF ( iAmDoingIO ) THEN
         CLOSE( dUnit )
       ENDIF

#endif

      return
      end
