C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_test_002.T,v 1.2 2004/01/29 05:30:37 edhill Exp $
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

      CALL MNC_CW_DUMP()

      print*, '=== Ending Program:    mnc_test_002 ==='

      END

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
