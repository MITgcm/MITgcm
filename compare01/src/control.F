C $Id: control.F,v 1.1 1998/05/25 20:21:04 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C================================================================================
C     Procedure name: CONTROL                                                   |
C           Function: Controlling routine for MIT Ocean-Atmosphere model.       |
C           Comments:                                                           |
C================================================================================

CStartofinterface
      SUBROUTINE CONTROL
      IMPLICIT NONE
C     /-------------------------------------------------------------------------\
C     | Global variable declarations                                            |
C     \-------------------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
CEndofinterface

C     /-------------------------------------------------------------------------\
C     |  Local variable declarations                                            |
C     |=========================================================================|
C     | U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                            |
C     | T       - Potential temperature (oC).                                   |
C     | S       - Salinity (ppt).                                               |
C     | PH      - Hydrostatic pressure perturbation (m).                        |
C     | PS      - Surface pressure (m).                                         |
C     \-------------------------------------------------------------------------/
      REAL    U (_I3(Nz,Nx,Ny))
      REAL    V (_I3(Nz,Nx,Ny))
      REAL    W (_I3(Nz,Nx,Ny))
      REAL    T (_I3(Nz,Nx,Ny))
      REAL    S (_I3(Nz,Nx,Ny))
      REAL    PH(_I3(Nz,Nx,Ny))
      REAL    PS(Nx, Ny       )

C     /-------------------------------------------------------------------------\
C     | Initialisation                                                          |
C     |=========================================================================|
C     | S/R SET_DEFAULTS     - Set model parameters to their default value(s).  |
C     | S/R PRINT_HEADING    - Write out run identifying information.           |
C     | S/R READ_DATA        - Read input parameters and basin topography.      |
C     | S/R INITIALISE       - Initialise state variables and finite difference |
C     |                        operators.                                       |
C     | S/R PRINT_PARAMETERS - Print out summary of model options.              |
C     | S/R PRINT_DATAFILE   - Echo input data file.                            |
C     | S/R PRINT_SUMMARY    - Print summary of starting state.                 |
C     \-------------------------------------------------------------------------/
      CALL SET_DEFAULTS(
     O                 U, V, W, T, S, pH, pS )
      CALL PRINT_HEADING
      CALL READ_DATA
      CALL INITIALISE (
     U                 U, V, W, T, S, pH, pS )
      CALL PRINT_PARAMETERS
      CALL PRINT_DATAFILE
      CALL PRINT_SUMMARY (
     I                 U, V, W, T, S, pH, pS )

      IF ( initialisationError ) THEN
       STOP 
     & 'ABNORMAL END: GCMPACK PE-MODEL. ERROR DURING INITIALISATION PHASE'
      ENDIF

C     /-------------------------------------------------------------------------\
C     | Prognostic integration                                                  |
C     |=========================================================================|
C     | S/R MODEL - Step model variables foward by iteration with a time step   |
C     |             delt from time t=startTime to time t>=endTime.              |
C     \-------------------------------------------------------------------------/
      CALL MODEL (
     U            U, V, W, T, S, PH, PS )

C     /-------------------------------------------------------------------------\
C     | Finish                                                                  |
C     |=========================================================================|
C     | S/R PRINT_SUMMARY - Print summary of ending state.                      |
C     \-------------------------------------------------------------------------/
      CALL PRINT_SUMMARY (
     I                 U, V, W, T, S, pH, pS )

      IF ( integrationError ) THEN
       STOP 
     & 'ABNORMAL END: GCMPACK PE-MODEL. ERROR DURING INTEGRATION PHASE'
      ENDIF

      RETURN
      END
