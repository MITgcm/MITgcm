C     /---------------------------------------------------------------\
C     |+| MIT General Circulation Modeling Package (GCMPACK)        |+|
C     |+|                                                           |+|
C     |+|     Copyright (c) 1993, 1994, 1995, 1996, 1997            |+|
C     |+|                                                           |+|
C     |+|     All rights reserved                                   |+|
C     |+|                                                           |+|
C     |+|  This software is provided with absolutely NO WARRANTY.   |+|
C     |+|                                                           |+|
C     |+|  Permission is given to use this software for any         |+|
C     |+|  non-commercial purpose provided that                     |+|
C     |+|   o Publications acknowledge any use of GCMPACK.          |+|
C     |+|   o Alterations to the software are made freely and       |+|
C     |+|     unconditionally available to all and any of GCMPACK   |+|
C     |+|     authors without prejudice.                            |+|
C     |+|                                                           |+|
C     |+|  All other uses, including redistribution in whole or     |+|
C     |+|   in part, are forbidden.                                 |+|
C     |+|                                                           |+|
C     |+|  Chris Hill cnh@plume.mit.edu                             |+|
C     \---------------------------------------------------------------/
C
C     /---------------------------------------------------------------\
C     |||     **************************************************    |||
C     |||     * General Circulation Modeling Package (GCMPACK) *    |||
C     |||     **************************************************    |||
C     |||                                                           |||
C     |||     MIT Ocean-Atmosphere Circulation Model 1.0 Alpha      |||
C     |||     ================================================      |||
C     |||                                                           |||
C     |||     Multithreaded Implementation of the MIT               |||
C     |||     Ocean-Atmosphere Circulation Model.                   |||
C     |||     This model is a general purpose numerical engine that |||
C     |||     can be used in the study of a wide range of oceaninc  |||
C     |||     and atmospheric phenomena. The model uses the pressure|||
C     |||     method to simulate non-hydrostatic (NH),              |||
C     |||     quasi-hydrostatic (QH) and hydrostatic (HY) fluid     |||
C     |||     flows in a rotating frame of reference using a        |||
C     |||     grid-point finite volume discretization. The algorithm|||
C     |||     is described in                                       |||
C     |||     "..."                                                 |||
C     |||     "..."                                                 |||
C     |||     The implmentation here is based on the algorithm laid |||
C     |||     out in the above references. This implementation      |||
C     |||     employs a regular grid and can be applied directly to |||
C     |||     either cartesian or spherical polar frameworks.       |||
C     \---------------------------------------------------------------/
C $Id: main.F,v 1.1 1998/05/25 20:21:05 cnh Exp $
C
C     Baseline Program Structure
C     ==========================
C     MAIN
C      |----CONTROL
C             |---- SET_DEFAULTS          Set default values for model parameters.
C             |       |-- DATE            Record date and time of run.
C             |       |-- PLTFRM          Record machine identfying string.
C             |
C             |---- READ_DATA             Read data file specifying model parameters.
C             |       |-- READ_TOPOGRAPHY Read land/water map.
C             |
C             |---- INITIALISE
C             |       |-- INITIALISE_MASKS      Set land/water mask arrays.
C             |       |-- INITIALISE_DENSITY   Calculate reference densities.
C             |       |      |-- RHO_LOCAL
C             |       |
C             |       |-- INITIALISE_GRIDDING   Set up arrays expressing geometry.
C             |       |      |-- INITIALISE_CARTESIAN_GRID
C             |       |      |-- INITIALISE_SPEHERICAL_POLAR_GRID
C             |       |
C             |       |-- INITIALISE_OPERATORS  Set up differencing operators.
C             |       |      |-- CG2DAI         Define 2d laplacian "A matrix" operator.
C             |       |      |-- CG2DPI         Define 2d "A matrix" preconditioner.
C             |       |
C             |       |-- INITIALISE_CORIOLIS   Set Coriolis term operator.
C             |       |-- INITIALISE_FIELDS     Set initial values of model variables.
C             |              |-- INITIALISE_U
C             |              |-- INITIALISE_V
C             |              |-- INITIALISE_S
C             |              |-- INITIALISE_T
C             |              |-- DUMP_READ2D        Read XY field values from files.
C             |              |     |-- DUMP_IO2D
C             |              |
C             |              |-- DUMP_READ          Read 3d field values from files.
C             |                    |-- DUMP_IO3D
C             |
C             |---- PRINT_HEADING     Print out run data summary page.
C             |
C             |---- MODEL             Perform integration.
C                    |-- RHO_CALC     Evaluate initial density.
C MAIN LOOP ------>  |
C                    |-- LOAD_FORCING Read in external forcing data.
C                    |-- GVEL_CALC    Calculate explicit tendencies for u, v and w.
C                    |-- PFIND        Diagnose pressure field.
C                    |     |-- CG2D   2d preconditioned conjgrad solver
C                    |                (used to solve for surface pressure).
C                    |-- UPDATE_UV    Step forward lateral velocity.
C                    |-- UPDATE_W     Step forward (NH or QH) w, or diagnose from continuity
C                    |                (NH, QH or HY).
C                    |-- GT_CALC      Calculate tendency for temperature.
C                    |-- UPDATE_T     Step forward temperature.
C                    |-- GS_CALC      Calculate tendency for salinity.
C                    |-- UPDATE_S     Step forward salinity.
C                    |-- CADJUST      Mix unstable water column ( coarse resolution ).
C                    |-- UPDATE_RHO   Diagnose new density field. 
C TO MAIN LOOP <---- |-- OUT          Dump out model fields.
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
      IMPLICIT NONE
C     To avoid problems on certain platforms always declare *ALL* COMMON blocks in 
C     main.
C      ======== Global data ============================
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"                                    
#include "PARAMS.h"                                 
#include "OLDG.h"
#include "MASKS.h"
#include "CG2DA.h"
#include "CG2DZ.h"
#include "AJAINF.h"
#include "FORCING.h"
#include "POLYEOS.h"
C     ========== Special code for getting Sun compatible binary data
C     ========== on a CRAY
      INTEGER ier
#ifdef CRAY_SUNBIN
      CALL ASSIGN('assign -O -F f77 -N ieee g:su',ier)
      CALL ASSIGN('assign -O -F f77 -N ieee g:du',ier)
#endif
      CALL control
      STOP 'NORMAL END: GCMPACK PE-MODEL.'
      END
