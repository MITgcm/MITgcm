C $Id: write.F,v 1.1 1998/05/25 20:21:06 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C ===========
C | write.F |
C ===========
C Contents
C o WRITE_DUMP_2D
C o WRITE_DUMP_3D
C o WRITE_PICKUP_FILES
C================================================================================
C     Procedure name: WRITE_DUMP_2D                                             |
C           Function: Write two-dimensional model dump.                         |
C           Comments:                                                           |
C================================================================================
CStartOfinterface
      SUBROUTINE WRITE_DUMP_2D ( Arr, N1, N2, pref, suff, iErr )
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
     I            N1, N2, fName, 'W', scrUnitError, 
     O            Arr, 
     U            iErr )

 1000 CONTINUE
      RETURN

  999 CONTINUE 
       WRITE(scrUnitError,*) 
     &' S/R WRITE_DUMP_2D: Size of file name buffer "fName" is too small.'
       WRITE(scrUnitError,*) 
     &'  Set buffer size (variable MAXFN) to at least ',loc+sLen-1,'.'
       iErr = 1
      GOTO 1000
      END
C================================================================================
C     Procedure name: WRITE_DUMP_3D                                             |
C           Function: Write three-dimensional model dump.                       |
C           Comments:                                                           |
C================================================================================
CStartOfinterface
      SUBROUTINE WRITE_DUMP_3D( Arr, N1, N2, N3, pref, suff, iErr )
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
     I            N1, N2, N3, fName, 'W', scrUnitError, 
     O            Arr, 
     U            iErr )

 1000 CONTINUE
      RETURN

  999 CONTINUE 
       WRITE(scrUnitError,*) 
     &' S/R WRITE_DUMP_3D: Size of file name buffer "fName" is too small.'
       WRITE(scrUnitError,*) 
     &'  Set buffer size (variable MAXFN) to at least ',loc+sLen-1,'.'
       iErr = 1
      GOTO 1000
      END
C================================================================================
C     Procedure name: WRITE_PICKUP_FILES                                        |
C           Function: Write model pickup datasets.                              |
C           Comments:                                                           |
C================================================================================
CStartOfinterface
      SUBROUTINE WRITE_PICKUP_FILES(
     I          U, V, W, T, S, PH, PS )
      IMPLICIT NONE
C     /-------------------------------------------------------------------------\
C     | Global variable declarations                                            |
C     \-------------------------------------------------------------------------/
#include "SIZE.h"
#include "OLDG.h"
#include "PARAMS.h"
#include "AJAINF.h"
C     /-------------------------------------------------------------------------\
C     |  Routine argument declarations                                          |
C     |=========================================================================|
C     | U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                            |
C     | T       - Potential temperature (oC).                                   |
C     | S       - Salinity (ppt).                                               |
C     | PH      - Hydrostatic pressure perturbation (m).                        |
C     | PS      - Surface pressure (m).                                         |
C     \-------------------------------------------------------------------------/
      REAL U (_I3(nz,nx,ny))
      REAL V (_I3(nz,nx,ny))
      REAL W (_I3(nz,nx,ny))
      REAL T (_I3(nz,nx,ny))
      REAL S (_I3(nz,nx,ny))
      REAL PS(       nx,ny )
      REAL PH(_I3(nz,nx,ny))
CEndOfinterface
C     /-------------------------------------------------------------------------\
C     |  Local variable declarations                                            |
C     |=========================================================================|
C     | iErr - Error flag                                                       |
C     | suff - Dump file suffix                                                 |
C     \-------------------------------------------------------------------------/
      INTEGER iErr
      CHARACTER*10   suffix
      CHARACTER*1024 record
      iErr = 0
      WRITE(suffix,'(I10.10)') nIter
C
      CALL WRITE_DUMP_3D(U    ,Nx,Ny,Nz,'U.'     ,suffix,iErr)
      CALL WRITE_DUMP_3D(V    ,Nx,Ny,Nz,'V.'     ,suffix,iErr)
      CALL WRITE_DUMP_3D(UAJA ,Nx,Ny,Nz,'UAJA.'  ,suffix,iErr)
      CALL WRITE_DUMP_3D(VAJA ,Nx,Ny,Nz,'VAJA.'  ,suffix,iErr)
      CALL WRITE_DUMP_3D(W    ,Nx,Ny,Nz,'W.'     ,suffix,iErr)
      CALL WRITE_DUMP_3D(T    ,Nx,Ny,Nz,'T.'     ,suffix,iErr)
      CALL WRITE_DUMP_3D(S    ,Nx,Ny,Nz,'S.'     ,suffix,iErr)
      CALL WRITE_DUMP_2D(PS   ,Nx,Ny,   'PS.'    ,suffix,iErr)
      CALL WRITE_DUMP_3D(PH   ,Nx,Ny,Nz,'PH.'    ,suffix,iErr)
      CALL WRITE_DUMP_3D(UNM1 ,Nx,Ny,Nz,'UNM1.'  ,suffix,iErr)
      CALL WRITE_DUMP_3D(VNM1 ,Nx,Ny,Nz,'VNM1.'  ,suffix,iErr)
      CALL WRITE_DUMP_2D(PSNM1,Nx,Ny,   'PSNM1.' ,suffix,iErr)
      CALL WRITE_DUMP_3D(PHNM1,Nx,Ny,Nz,'PHNM1.' ,suffix,iErr)
      CALL WRITE_DUMP_3D(GUNM1,Nx,Ny,Nz,'GUNM1.' ,suffix,iErr)
      CALL WRITE_DUMP_3D(GVNM1,Nx,Ny,Nz,'GVNM1.' ,suffix,iErr)
      CALL WRITE_DUMP_3D(GTNM1,Nx,Ny,Nz,'GTNM1.' ,suffix,iErr)
      CALL WRITE_DUMP_3D(GSNM1,Nx,Ny,Nz,'GSNM1.' ,suffix,iErr)
C
      IF ( iErr .NE. 0 ) THEN
        WRITE(record,*) 
     &  'Error(s) writing pickup datasets at time t = ', 
     &  currentTime,
     &  'seconds.'
        CALL PRINT_HEADER_START
        CALL PRINT_HEADER_RECORD(record)
        CALL PRINT_HEADER_END
        CALL PRINT_ERRORS
        CALL PRINT_TABLE_END
      ENDIF
C
      RETURN
      END
