#include "MDSIO_OPTIONS.h"

CBOP
C     !ROUTINE: MDS_WRITE_WHALOS
C     !INTERFACE:
      SUBROUTINE MDS_WRITE_WHALOS(
     I                    fName,
     I                    len,
     I                    filePrec,
     I                    fid,
     I                    n2d,
     I                    fldRL,
     I                    irec,
     I                    locSingleCPUIO,
     I                    locBufferIO,
     I                    myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE MDS_WRITE_WHALOS
C     o Write file that includes halos. The main purpose is for
C       adjoint related "tape I/O". The secondary purpose is debugging.
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_WHIO
# include "MDSIO_BUFF_WH.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     fName    ::  extended tape fName.
C     len      ::  number of characters in fName.
C     filePrec ::  number of bits per word in file (32 or 64).
C     fid      ::  file unit (its use is not implemented yet).
C     n2d      ::  size of the fldRL third dimension.
C     fldRL    ::  array to read.
C     irec     ::  record number to be written.
C     myThid   ::  my Thread Id number
      CHARACTER*(*) fName
      INTEGER len
      INTEGER filePrec
      INTEGER fid
      INTEGER n2d
      _RL     fldRL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,n2d,nSx,nSy)
      INTEGER irec
      LOGICAL locSingleCPUIO, locBufferIO
      INTEGER myThid

#ifdef ALLOW_WHIO
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

C     !LOCAL VARIABLES:
C     == local parameters ==
C     sNxWh   :: x tile size with halo included
C     sNyWh   :: y tile size with halo included
C     pocNyWh :: processor sum of sNyWh
C     gloNyWh :: global sum of sNyWh
      INTEGER sNxWh
      INTEGER sNyWh
      INTEGER procNyWh
      INTEGER gloNyWh
      PARAMETER ( sNxWh = sNx+2*OLx )
      PARAMETER ( sNyWh = sNy+2*OLy )
      PARAMETER ( procNyWh = sNyWh*nSy*nSx )
      PARAMETER ( gloNyWh = procNyWh*nPy*nPx )
C     == local variables ==
      CHARACTER*(MAX_LEN_FNAM) pfName
      INTEGER IL
      INTEGER lengthBuff, length_of_rec
      INTEGER i, j, i2d
      INTEGER dUnit, irec2d
      LOGICAL iAmDoingIO
#ifdef ALLOW_WHIO_3D
      INTEGER js
#endif
CEOP

#ifdef ALLOW_WHIO_3D
      writeWh=.TRUE.
#endif

      IF ( .NOT.locSingleCpuIO ) THEN
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
C get the unit and open file
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

      DO i2d=1,n2d

        IF (filePrec .EQ. precFloat32) THEN
          CALL MDS_PASS_R4toRL( fld2d_procbuff_r4, fldRL,
     &             OLx, OLy, 1, i2d, n2d, 0, 0, .FALSE., myThid )
          IF ( locSingleCpuIO ) THEN
            CALL BAR2( myThid )
#  ifndef EXCLUDE_WHIO_GLOBUFF_2D
            CALL GATHER_2D_WH_R4( fld2d_globuff_r4,
     &                            fld2d_procbuff_r4,myThid)
#  endif
          ENDIF
        ELSE
          CALL MDS_PASS_R8toRL( fld2d_procbuff_r8, fldRL,
     &             OLx, OLy, 1, i2d, n2d, 0, 0, .FALSE., myThid )
          IF ( locSingleCpuIO ) THEN
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
C reset counter if needed
          IF (jWh.EQ.nWh) jWh=0
C increment counter
          jWh=jWh+1
C determine current file record
          irec2d=i2d+n2d*(irec-1)
          iWh=(irec2d-1)/nWh+1
C copy
          js = (jWh-1)*lengthBuff
          IF ( .NOT.locSingleCpuIO ) THEN
            IF (filePrec .EQ. precFloat32) THEN
              DO i=1,lengthBuff
                j = js + i
                fld3d_procbuff_r4(j) = fld2d_procbuff_r4(i)
              ENDDO
            ELSE
              DO i=1,lengthBuff
                j = js + i
                fld3d_procbuff_r8(j) = fld2d_procbuff_r8(i)
              ENDDO
            ENDIF
          ELSE
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
            IF (filePrec .EQ. precFloat32) THEN
              DO i=1,lengthBuff
                j = js + i
                fld3d_globuff_r4(j) = fld2d_globuff_r4(i)
              ENDDO
            ELSE
              DO i=1,lengthBuff
                j = js + i
                fld3d_globuff_r8(j) = fld2d_globuff_r8(i)
              ENDDO
            ENDIF
#  endif
          ENDIF
C write chunk if needed
          IF (jWh.EQ.nWh) THEN
            IF ( .NOT.locSingleCpuIO ) THEN
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
#else /* ALLOW_WHIO_3D */
        IF ( iAmDoingIO ) THEN
#endif /* ALLOW_WHIO_3D */
          irec2d=i2d+n2d*(irec-1)
          IF ( .NOT.locSingleCpuIO ) THEN
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

      ENDDO

      IF ( iAmDoingIO.AND.(fid.EQ.0) ) THEN
        CLOSE( dUnit )
      ENDIF

#endif /* ALLOW_WHIO */

      RETURN
      END
