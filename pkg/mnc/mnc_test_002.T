C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_test_002.T,v 1.5 2004/03/19 03:28:37 edhill Exp $
C $Name:  $

C     Simple test program for MNC

      PROGRAM mnc_test_002

      implicit none
#include "MNC_OPTIONS.h"
#include "EEPARAMS.h"

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, myThid

      myThid = 0
      print*, '=== Starting Program:  mnc_test_002 ==='

      CALL MNC_INIT(myThid)
      CALL MNC_CW_INIT(20,16, 4,4, 1,1, 1,1, 13, myThid)

      CALL MNC_CW_ADD_VNAME('v_vel', 'V_xy_Hn__C__t', 4,5, myThid )
      CALL MNC_CW_ADD_VATTR_TEXT('v_vel', 1, 
     &     'long_name', 'meridional_velocity', myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('v_vel', 1, 
     &     'units', 'cm/s', myThid)
      CALL MNC_CW_ADD_VATTR_INT('v_vel', 1, 
     &     'grid_type', -1, myThid )
      CALL MNC_CW_ADD_VATTR_DBL('v_vel', 1, 
     &     'MITgcm_grid_ver', 0.1D0, myThid )

      CALL MNC_CW_DUMP(myThid)

      print*, '=== Ending Program:    mnc_test_002 ==='

      END

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
