C $Header: /u/gcmpack/MITgcm/eesupp/inc/Attic/DFILE.h,v 1.5 2001/02/04 14:38:41 cnh Exp $
C $Name:  $

#ifdef USE_DFILE

C     /==========================================================\
C     | DFILE.h                                                  |
C     |==========================================================|
C     | Header file for binary dump file I/O structures.         |
C     \==========================================================/
C     Compile time constants
C     ioUnitsPerThread - Number of ioUnits allowed
C     busyUnit         - Status flag. Indicates whether unit
C     freeUnit           is open
C     accessModeRO     - Read-only v. read-write flag
C     accessModeRW
C     errorModeSTOP    - Indicates action on I/O error
C     errorModeCONT    
      INTEGER ioUnitsPerThread
      PARAMETER ( ioUnitsPerThread = 1 )
      INTEGER busyUnit
      PARAMETER ( busyUnit = 0 )
      INTEGER freeUnit
      PARAMETER ( freeUnit = 1 )
      INTEGER accessModeRO
      PARAMETER ( accessModeRO = 0 )
      INTEGER accessModeRW
      PARAMETER ( accessModeRW = 1 )
      INTEGER errorModeSTOP
      PARAMETER ( errorModeSTOP = 0 )
      INTEGER errorModeCONT
      PARAMETER ( errorModeCONT = 1 )
      INTEGER metaDataNotWritten
      PARAMETER ( metaDataNotWritten = 0 )
      INTEGER metaDataWritten
      PARAMETER ( metaDataWritten = 1 )

C     mUnit - Fortran unit number for metafile
C     dUnit - Fortran unit number for data
C     unitStatus - Busy/Free status flag
C     nameOfMFile - Name of metadata file used in I/O
C     nameOfDFile - Name of data file used in I/O
C     accessMode  - Access mode that was set when this unit was opened.
C                   Access mode is either read-only or
C                   read-write.
C     errorMode   - Error mode that was set when this unit was opened.
C                   Error mode is either STOP on error which means the
C                   program halts or continue or error. For continue
C                   error the program usually tries to write an error
C                   message to stderr. Howeever, the most common cause for
C                   an IO error is a full disk in which case that
C                   error message may be lost!
C     theAcessMode - The current acces mode. This will be the mode
C                    associated with a file that is opened.
C     theErrorMode - The current error mode. This will be the mode
C                    associated with a file that is opened.
      COMMON /DFILE_I/ mUnitNumber, dUnitNumber, unitStatus,
     &                 accessMode, errorMode,
     &                 theAccessMode, theErrorMode, metaDataStatus
      INTEGER mUnitNumber(ioUnitsPerThread,MAX_NO_THREADS)
      INTEGER dUnitNumber(ioUnitsPerThread,MAX_NO_THREADS)
      INTEGER unitStatus (ioUnitsPerThread,MAX_NO_THREADS)
      INTEGER accessMode (ioUnitsPerThread,MAX_NO_THREADS)
      INTEGER errorMode (ioUnitsPerThread,MAX_NO_THREADS)
      INTEGER metaDataStatus(ioUnitsPerThread,MAX_NO_THREADS)
      INTEGER theAccessMode
      INTEGER theErrorMode

C     nameOfMFile - Name of metadata file used in I/O
C     nameOfDFile - Name of data file used in I/O
      COMMON /DFILE_C/ nameOfMfile, nameOfDfile
      CHARACTER*(MAX_LEN_FNAM) 
     &         nameOfMfile(ioUnitsPerThread,MAX_NO_THREADS)
      CHARACTER*(MAX_LEN_FNAM) 
     &         nameOfDfile(ioUnitsPerThread,MAX_NO_THREADS)

C     ioBuf_R4 - IO buffer for 32-bit floating point IO.
C                Sized to fit anything up to a global domain 3d field.
C                ( but without overlaps )
C     ioBuf_R8 - IO buffer for 64-bit floating point IO.
C                Sized to fit anything up to a global domain 3d field.
C                ( but without overlaps )
      COMMON /DFILE_R/
     & ioBuf_R8, ioBuf_R4
      Real*8 ioBuf_R8( sNx*nSx*nPx * sNy*nSy*nPy * Nr )
      Real*4 ioBuf_R4( sNx*nSx*nPx * sNy*nSy*nPy * Nr )

#endif /* USE_DFILE */
