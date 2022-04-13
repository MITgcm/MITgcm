#include "EXF_OPTIONS.h"

CBOP
C !ROUTINE: EXF_INTERP_READ
C !INTERFACE:
       SUBROUTINE EXF_INTERP_READ(
     I                infile, filePrec,
     O                arrayin,
     I                irecord, nx_in, ny_in, myThid )

C !DESCRIPTION:

C !USES:
      IMPLICIT NONE

C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_PARAM.h"
#ifdef ALLOW_USE_MPI
# include "EESUPPORT.h"
#endif /* ALLOW_USE_MPI */

C !INPUT/OUTPUT PARAMETERS:
C  infile      (string)  :: name of the binary input file (direct access)
C  filePrec    (integer) :: number of bits per word in file (32 or 64)
C  arrayin     ( _RL )   :: array to read file into
C  irecord     (integer) :: record number to read
C  nx_in,ny_in (integer) :: size in x & y direction of input file to read
C  myThid      (integer) :: My Thread Id number
      CHARACTER*(*) infile
      INTEGER       filePrec, irecord, nx_in, ny_in
       _RL          arrayin( -1:nx_in+2 , -1:ny_in+2 )
      INTEGER       myThid
CEOP

C !FUNCTIONS
      INTEGER  ILNBLNK
      INTEGER MDS_RECLEN
      LOGICAL MASTER_CPU_IO
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN
      EXTERNAL MASTER_CPU_IO

C !LOCAL VARIABLES
      INTEGER  i, j
      INTEGER  ioUnit, length_of_rec, IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL  exst
#ifdef EXF_INTERP_USE_DYNALLOC
#ifdef EXF_IREAD_USE_GLOBAL_POINTER
C     When using threads the address of the local automatic array
C     "buffer" is not visible to the other threads. So we create
C     a pointer to share that address here. This is presently
C     in an ifdef because it won't go through g77 and I'm not
C     currently sure what TAF would do with this.
      COMMON /EXF_IOPTR8/ glPtr8
      REAL*8, POINTER :: glPtr8(:,:)
      COMMON /EXF_IOPTR4/ glPtr4
      REAL*4, POINTER :: glPtr4(:,:)

      Real*8, target ::  buffer_r8(nx_in,ny_in)
      Real*4, target ::  buffer_r4(nx_in,ny_in)
#else  /* ndef EXF_IREAD_USE_GLOBAL_POINTER */
      Real*8   buffer_r8(nx_in,ny_in)
      Real*4   buffer_r4(nx_in,ny_in)
#endif /* ndef EXF_IREAD_USE_GLOBAL_POINTER */
#else  /* ndef EXF_INTERP_USE_DYNALLOC */
      Real*8   buffer_r8(exf_interp_bufferSize)
      Real*4   buffer_r4(exf_interp_bufferSize)
      COMMON /EXF_INTERP_BUFFER/ buffer_r8, buffer_r4
      INTEGER ijs
#endif /* ndef EXF_INTERP_USE_DYNALLOC */
#ifdef ALLOW_USE_MPI
      INTEGER  ierr
#endif

C--   Check for consistency:
#ifdef EXF_INTERP_USE_DYNALLOC
#ifndef EXF_IREAD_USE_GLOBAL_POINTER
C     The CPP symbol EXF_IREAD_USE_GLOBAL_POINTER must be defined for the
C     case of nThreads > 1. Stop IF it isnt.
      IF ( nThreads .GT. 1 ) THEN
      STOP
     &'EXF_INTERP_READ: nThreads > 1 needs EXF_IREAD_USE_GLOBAL_POINTER'
      ENDIF
#endif
#else  /* ndef EXF_INTERP_USE_DYNALLOC */
#ifdef EXF_IREAD_USE_GLOBAL_POINTER
      STOP
     &'EXF_INTERP_READ: USE_GLOBAL_POINTER needs INTERP_USE_DYNALLOC'
#endif
      IF ( nx_in*ny_in .GT. exf_interp_bufferSize ) THEN
        STOP 'EXF_INTERP_READ: exf_interp_bufferSize too small'
      ENDIF
#endif /* ndef EXF_INTERP_USE_DYNALLOC */

C--   before starting to read, wait for everyone to finish
      _BARRIER

C---  read in input data

      IF ( MASTER_CPU_IO(myThid) ) THEN
C--   master thread of process 0, only, opens a global file

        IL  = ILNBLNK( infile )
        INQUIRE( file=infile, exist=exst )
        IF (exst) THEN
         IF ( debugLevel.GE.debLevB ) THEN
          WRITE(msgBuf,'(A,A)')
     &         ' EXF_INTERP_READ: opening file: ',infile(1:IL)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &         SQUEEZE_RIGHT , myThid)
         ENDIF
        ELSE
         WRITE(msgBuf,'(2A)')
     &        ' EXF_INTERP_READ: filename: ', infile(1:IL)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &        ' EXF_INTERP_READ: File does not exist'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R EXF_INTERP_READ'
        ENDIF

        CALL MDSFINDUNIT( ioUnit, myThid )
        length_of_rec=MDS_RECLEN( filePrec, nx_in*ny_in, myThid )
        OPEN( ioUnit, file=infile, status='old', _READONLY_ACTION
     &       access='direct', recl=length_of_rec )
        IF ( filePrec .EQ. 32 ) THEN
#ifdef EXF_INTERP_USE_DYNALLOC
          READ(ioUnit,rec=irecord)  buffer_r4
#else
          READ(ioUnit,rec=irecord) (buffer_r4(i),i=1,nx_in*ny_in)
#endif
#ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR4(nx_in*ny_in,buffer_r4)
#endif /* _BYTESWAPIO */
        ELSE
#ifdef EXF_INTERP_USE_DYNALLOC
          READ(ioUnit,rec=irecord)  buffer_r8
#else
          READ(ioUnit,rec=irecord) (buffer_r8(i),i=1,nx_in*ny_in)
#endif
#ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nx_in*ny_in,buffer_r8)
#endif /* _BYTESWAPIO */
        ENDIF
        CLOSE( ioUnit )
C--   end if MASTER_CPU_IO
      ENDIF

      _BEGIN_MASTER( myThid )
#ifdef ALLOW_USE_MPI
C--   broadcast to all processes
       IF ( useSingleCpuInput ) THEN
         IF ( filePrec .EQ. 32 ) THEN
           CALL MPI_BCAST(buffer_r4,nx_in*ny_in,MPI_REAL,
     &          0,MPI_COMM_MODEL,ierr)
         ELSE
           CALL MPI_BCAST(buffer_r8,nx_in*ny_in,MPI_DOUBLE_PRECISION,
     &          0,MPI_COMM_MODEL,ierr)
         ENDIF
       ENDIF
#endif /* ALLOW_USE_MPI */

#ifdef EXF_IREAD_USE_GLOBAL_POINTER
       IF ( filePrec .EQ. 32 ) THEN
         glPtr4 => buffer_r4
       ELSE
         glPtr8 => buffer_r8
       ENDIF
#endif
      _END_MASTER( myThid )
      _BARRIER

C---  Transfer buffer to "arrayin" array:
#ifdef EXF_INTERP_USE_DYNALLOC
#ifdef EXF_IREAD_USE_GLOBAL_POINTER
      IF ( filePrec .EQ. 32 ) THEN
        DO j=1,ny_in
          DO i=1,nx_in
            arrayin(i,j)=glPtr4(i,j)
          ENDDO
        ENDDO
      ELSE
        DO j=1,ny_in
          DO i=1,nx_in
            arrayin(i,j)=glPtr8(i,j)
          ENDDO
        ENDDO
      ENDIF
#else /* ndef EXF_IREAD_USE_GLOBAL_POINTER */
      IF ( filePrec .EQ. 32 ) THEN
        DO j=1,ny_in
          DO i=1,nx_in
            arrayin(i,j)=buffer_r4(i,j)
          ENDDO
        ENDDO
      ELSE
        DO j=1,ny_in
          DO i=1,nx_in
            arrayin(i,j)=buffer_r8(i,j)
          ENDDO
        ENDDO
      ENDIF
#endif /* ndef EXF_IREAD_USE_GLOBAL_POINTER */
#else  /* ndef EXF_INTERP_USE_DYNALLOC */
      IF ( filePrec .EQ. 32 ) THEN
        DO j=1,ny_in
          ijs = (j-1)*nx_in
          DO i=1,nx_in
            arrayin(i,j)=buffer_r4(i+ijs)
          ENDDO
        ENDDO
      ELSE
        DO j=1,ny_in
          ijs = (j-1)*nx_in
          DO i=1,nx_in
            arrayin(i,j)=buffer_r8(i+ijs)
          ENDDO
        ENDDO
      ENDIF
#endif /* ndef EXF_INTERP_USE_DYNALLOC */

      RETURN
      END
