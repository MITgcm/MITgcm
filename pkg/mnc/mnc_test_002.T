C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_test_002.T,v 1.3 2004/01/31 04:13:09 edhill Exp $
C $Name:  $

C     Simple test program for MNC

      PROGRAM mnc_test_002

      implicit none
#include "MNC_OPTIONS.h"
#include "EEPARAMS.h"

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i

      print*, '=== Starting Program:  mnc_test_002 ==='

      CALL MNC_INIT()
      CALL MNC_CW_INIT( 0,
     &     20,16, 4,4, 1,1, 1,1, 13 )

      CALL MNC_CW_ADD_VNAME(0, 'v_vel', 'V_xy_Hn__C__t')
      CALL MNC_CW_ADD_VATTR_TEXT(0, 'v_vel', 1, 
     &     'long_name', 'meridional_velocity')
      CALL MNC_CW_ADD_VATTR_TEXT(0, 'v_vel', 1, 
     &     'units', 'cm/s')
      CALL MNC_CW_ADD_VATTR_INT(0, 'v_vel', 1, 
     &     'grid_type', -1 )
      CALL MNC_CW_ADD_VATTR_DBL(0, 'v_vel', 1, 
     &     'MITgcm_grid_ver', 0.1D0 )

      CALL MNC_CW_DUMP()

      print*, '=== Ending Program:    mnc_test_002 ==='

      END

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
