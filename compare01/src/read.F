C $Id: read.F,v 1.2 1998/06/12 19:33:32 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C ==========
C | read.F |
C ==========
C Contents
C o READ_DATA
C o READ_DUMP_2D
C o READ_DUMP_3D
C o READ_TOPOGRAPHY
C================================================================================
C     Procedure name: READ_DATA                                                 |
C           Function: Read standard model data file.                            |
C           Comments:                                                           |
C================================================================================
CStartOfinterface
      SUBROUTINE READ_DATA
      IMPLICIT NONE
CEndOfinterface
C     /-------------------------------------------------------------------------\
C     | Read the topography mask                                                |
C     \-------------------------------------------------------------------------/
      CALL READ_TOPOGRAPHY
C
      RETURN
      END
C================================================================================
C     Procedure name: READ_DUMP_2D                                              |
C           Function: Read two-dimensional model dump.                          |
C           Comments:                                                           |
C================================================================================
CStartOfinterface
      SUBROUTINE READ_DUMP_2D(Arr, N1, N2, pref, suff, iErr )
      IMPLICIT NONE
#include "SIZE.h"
#include "PARAMS.h"
#include "EXTERNAL.h"
      INTEGER N1, N2
      REAL Arr(N1,N2)
      CHARACTER*(*) pref
      CHARACTER*(*) suff
      INTEGER iErr
CEndOfinterface
      CHARACTER*(MAXFN) fName
      INTEGER      sLen, s1, s2
      INTEGER      loc
      loc = 1 
      fName = ' '
      s1   = IFNBLNK(pref)
      s2   = ILNBLNK(pref)
      sLen = s2-s1+1
      IF ( loc+sLen-1 .LE. MAXFN ) fName(loc:loc+sLen-1) = pref(s1:s2)
      loc  = loc+sLen
      s1   = IFNBLNK(suff)
      s2   = ILNBLNK(suff)
      sLen = s2-s1+1
      IF ( loc+sLen-1 .LE. MAXFN ) fName(loc:loc+sLen-1) = suff(s1:s2)
      IF ( loc+sLen-1 .GT. MAXFN ) GOTO 999
      CALL IO_R2D(
     I            N1, N2, fName, 'R', scrUnitError,
     O            Arr,
     U            iErr )
 
 1000 CONTINUE
      RETURN
 
  999 CONTINUE
       WRITE(scrUnitError,*)
     &' S/R READ_DUMP_2D: Size of file name buffer "fName" is too small.'
       WRITE(scrUnitError,*)
     &'  Set buffer size (variable MAXFN) to at least ',loc+sLen-1,'.'
       iErr = 1
      GOTO 1000
      END
C================================================================================
C     Procedure name: READ_DUMP_3D                                              |
C           Function: Read three-dimensional model dump.                        |
C           Comments:                                                           |
C================================================================================
CStartOfinterface
      SUBROUTINE READ_DUMP_3D(Arr, N1, N2, N3, pref, suff, iErr )
      IMPLICIT NONE
#include "SIZE.h"
#include "PARAMS.h"
#include "EXTERNAL.h"
      INTEGER N1, N2, N3
      REAL Arr(_I3(N3,N1,N2))
      CHARACTER*(*) pref
      CHARACTER*(*) suff
      INTEGER iErr
CEndOfinterface
      CHARACTER*(MAXFN) fName
      INTEGER      sLen, s1, s2
      INTEGER      loc
      loc = 1
      fName = ' '
      s1   = IFNBLNK(pref)
      s2   = ILNBLNK(pref)
      sLen = s2-s1+1
      IF ( loc+sLen-1 .LE. MAXFN ) fName(loc:loc+sLen-1) = pref(s1:s2)
      loc  = loc+sLen
      s1   = IFNBLNK(suff)
      s2   = ILNBLNK(suff)
      sLen = s2-s1+1
      IF ( loc+sLen-1 .LE. MAXFN ) fName(loc:loc+sLen-1) = suff(s1:s2)
      IF ( loc+sLen-1 .GT. MAXFN ) GOTO 999
      CALL IO_R3D(
     I            N1, N2, N3, fName, 'R', scrUnitError,
     O            Arr,
     U            iErr )

 
 1000 CONTINUE
      RETURN
 
  999 CONTINUE
       WRITE(scrUnitError,*)
     &' S/R READ_DUMP_3D: Size of file name buffer "fName" is too small.'
       WRITE(scrUnitError,*)
     &'  Set buffer size (variable MAXFN) to at least ',loc+sLen-1,'.'
       iErr = 1
      GOTO 1000
      END
C================================================================================
C     Procedure name: READ_TOPOGRAPHY
C           Function: Sets up arrays describing domain topography.
C           Comments: This routine reads a file called "topog". "topog" 
C                     contains the land-water flag for each P volume in
C                     the model domain. This flag has the value 1 for 
C                     every P volume which is water and 0 for every P 
C                     volume which is land. The "topog" file should 
C                     contain 1 record for each west-east line in the 
C                     model domain. The first record of the file holds 
C                     the land-water flag for the northern most west-east
C                     line at the top layer of the model. The next record 
C                     is for the west-east line immeadiatly to the south 
C                     of the line described by the first record. The record 
C                     describing the southern most west-east line for the 
C                     domains top layer is followed by the record for 
C                     northern most west-east line for the layer below in 
C                     the domain. This cycle is repeats until the southern 
C                     most west-east line at the bottom of the model is
C                     reached. Records with the character '#' in column 1 
C                     are treated as comments and ignored.
C================================================================================
      SUBROUTINE READ_TOPOGRAPHY
      IMPLICIT NONE
C     =============== Global data ==========================================
#include "SIZE.h"
#include "MASKS.h"
#include "PARAMS.h"
#include "STRINGS.h"
C     ========= Local variables =========================
C     Topography comes from external file
      CHARACTER*8 TOPOG_FILE
      DATA TOPOG_FILE /'topog'/
      CHARACTER*(*) BTOPOG_FILE
      PARAMETER ( BTOPOG_FILE = 'pmask.sun.b')
C     Unit used to read file
      INTEGER IOUNIT
      DATA IOUNIT /41/
C     Error flag
      INTEGER IERR
C     Temporary CM array
      Real*4 TMPARRCM( IM, JM, KM )
C     Temporary array for making depth map
      REAL DMAP(Nx,Ny)
C     Loop counters
      INTEGER I1, I
      INTEGER I2, J
      INTEGER I3, K
      INTEGER NREC
C     I/O Buffer
      CHARACTER*1024 RECORD

C     /========================================================================\
C     | Read topography/bathymetry map from binary file.                       |
C     \========================================================================/
      IF ( bathySource .EQ. binaryBathymetry ) THEN
       OPEN(FILE=BTOPOG_FILE,UNIT=IOUNIT,FORM='UNFORMATTED',
     &      STATUS='OLD')
       READ (IOUNIT)TMPARRCM
       CLOSE(IOUNIT)
       DO K = 1, Nz
       DO I = 1, Nx
       DO J = 1, Ny
         PMASK(_I3(K,I,J)) = TMPARRCM(I,J,K)
       ENDDO
       ENDDO
       ENDDO
      ENDIF

C     /========================================================================\
C     | Topography/bathymetry map set from in-line definition                  |
C     \========================================================================/
      IF ( bathySource .EQ. inlineBathymetry ) THEN
       WRITE (0,*) 'Inline bathymetry definition used'
       PMASK         = WATER ! Set domain to water everywhere.
       PMASK(_I3(Nz,:,:)) = LAND  ! Make bottom solid prevent Z periodicity.
       PMASK(_I3(:,:,Ny)) = LAND  ! Make north solid prevent Y periodicity.
       PMASK(_I3(:,Nx,:)) = LAND  ! Make east solid prevent X periodicity.
      ENDIF

C     /========================================================================\
C     | Read topography/bathymetry map from textual 0 and 1 file               |
C     \========================================================================/
      IF ( bathySource .EQ. textBathymetry ) THEN
       OPEN (FILE=TOPOG_FILE,UNIT=IOUNIT,STATUS='OLD')
        I1   = 1
        I3   = JM+1
        NREC = 0
  200   CONTINUE
          READ ( IOUNIT, '(A1024)', END = 201 ) RECORD
          NREC = NREC+1
C         Skip commented lines and blank
          IF (RECORD(1:1) .EQ. '#' ) GOTO 200
          IF (RECORD      .EQ. ' ' ) GOTO 200
          I3 = I3 - 1
          IF ( I3 .EQ. 0 ) THEN
            I3 = JM
            I1 = I1 + 1
          ENDIF
          IF ( I1 .GT. KM ) GOTO 999
          READ (RECORD,'(1024F1.0)',ERR=997) (TMPARRCM(I2,I3,I1),I2=1,IM)
          GOTO 200
  201   CONTINUE
        IF ( I1 .NE. KM .OR.
     &       I3 .NE. 1 ) GOTO 998
        CLOSE(IOUNIT)
C       Topography loaded O.K.
        DO I3 = 1, JM
        DO I2 = 1, IM
        DO I1 = 1, KM
          PMASK(_I3(I1,I2,I3)) = TMPARRCM(I2,I3,I1)
        ENDDO
        ENDDO
        ENDDO
      ENDIF

C     /========================================================================\
C     | Print character contour map of bathymetry.                             |
C     \========================================================================/
      DMAP = 0.
      DO K = 1, Nz
       DO J=1,Ny
        DO I=1, Nx
         DMAP(I,J) = DMAP(I,J)+PMASK(_I3(K,I,J))
        ENDDO
       ENDDO
      ENDDO
      CALL PLOT_FIELD(DMAP,Nx,Ny)

      RETURN
  999 CONTINUE
      PRINT *, ' ERROR READING TOPOGRAPHY FROM FILE "',TOPOG_FILE,'"'
      PRINT *,'   THE NUMBER OF RECORDS IN THE THE TOPOGRAPHY FILE "',
     &         TOPOG_FILE,'" EXCEEDS THE MODEL DOMAIN SIZE.'
      STOP
  998 CONTINUE
      PRINT *, ' ERROR READING TOPOGRAPHY FROM FILE "',TOPOG_FILE,'"'
      PRINT *,'   THE NUMBER OF RECORDS IN THE THE TOPOGRAPHY FILE "',
     &         TOPOG_FILE,'" IS LESS THAN THE MODEL DOMAIN SIZE.'
      STOP
  997 CONTINUE
      PRINT *, ' ERROR READING TOPOGRAPHY FROM FILE "',TOPOG_FILE,'"',
     &         ' ON RECORD NUMBER ',NREC,'.'
      PRINT *, '   RECORD: ',RECORD
      STOP
      END
