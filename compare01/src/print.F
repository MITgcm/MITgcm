C $Id: print.F,v 1.1 1998/05/25 20:21:05 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C ===========
C | print.F |
C ===========
C Contents
C o PRINT_DATAFILE
C o PRINT_ERRORS
C o PRINT_HEADER_END
C o PRINT_HEADER_RECORD
C o PRINT_HEADER_START
C o PRINT_HEADING
C o PRINT_PARAMETERS
C o PRINT_SUMMARY    
C o PRINT_TABLE_END
C o 
C================================================================================
C     Procedure name: PRINT_DATAFILE                                            |
C           Function: Echo input data to output file.                           |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_DATAFILE
CEndofinteface
      IMPLICIT NONE
      RETURN
      END

C================================================================================
C     Procedure name: PRINT_ERRORS                                              |
C           Function: Write out the error message buffer to the output file.    |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_ERRORS
CEndofinteface
      IMPLICIT NONE
      RETURN
      END

C================================================================================
C     Procedure name: PRINT_HEADER_END                                          |
C           Function: Print end record for a table header.                      |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_HEADER_END
CEndofinteface
      IMPLICIT NONE
      RETURN
      END

C================================================================================
C     Procedure name: PRINT_HEADER_RECORD                                       |
C           Function: Print table header record.                                |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_HEADER_RECORD
CEndofinteface
      IMPLICIT NONE
      RETURN
      END

C================================================================================
C     Procedure name: PRINT_HEADER_START                                        |
C           Function: Print header for a table.                                 |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_HEADER_START
CEndofinteface
      IMPLICIT NONE
      RETURN
      END

C================================================================================
C     Procedure name: PRINT_HEADING                                             |
C           Function: Print heading information identifying this run.           |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_HEADING  
CEndofinteface
      IMPLICIT NONE
      RETURN
      END

C================================================================================
C     Procedure name: PRINT_PARAMETERS                                          |
C           Function: Print model control parameters.                           |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_PARAMETERS
CEndofinteface
      IMPLICIT NONE
      RETURN
      END

C================================================================================
C     Procedure name: PRINT_SUMMARY                                             |
C           Function: Print tabulated summary of model fields.                  |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_SUMMARY (
     O                         U, V, W, T, S, PH, PS )
      IMPLICIT NONE
C     /-------------------------------------------------------------------------\
C     | Global variable declarations                                            |
C     \-------------------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "STRINGS.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "OLDG.h"
#include "MASKS.h"
#include "CG2DA.h"
#include "CG2DZ.h"
#include "AJAINF.h"
#include "FORCING.h"
C     /-------------------------------------------------------------------------\
C     |  Routine argument declarations                                          |
C     |=========================================================================|
C     | U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                            |
C     | T       - Potential temperature (oC).                                   |
C     | S       - Salinity (ppt).                                               |
C     | PH      - Hydrostatic pressure perturbation (m).                        |
C     | PS      - Surface pressure (m).                                         |
C     \-------------------------------------------------------------------------/
      REAL    U (_I3(nz,nx,ny))
      REAL    V (_I3(nz,nx,ny))
      REAL    W (_I3(nz,nx,ny))
      REAL    T (_I3(nz,nx,ny))
      REAL    S (_I3(nz,nx,ny))
      REAL    PH(_I3(nz,nx,ny))
      REAL    PS(nx, ny       )
CEndofinteface
      RETURN
      END

C================================================================================
C     Procedure name: PRINT_TABLE_END                                           |
C           Function: Print end of table record.                                |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE PRINT_TABLE_END 
CEndofinteface
      IMPLICIT NONE
      RETURN
      END

