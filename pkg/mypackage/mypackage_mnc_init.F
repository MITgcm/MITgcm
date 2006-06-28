C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_mnc_init.F,v 1.1 2006/06/28 21:26:21 heimbach Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MYPACKAGE_MNC_INIT

C     !INTERFACE:
      SUBROUTINE MYPACKAGE_MNC_INIT( myThid )

C     !DESCRIPTION:
C     Initialize MNC lookup tables for Mypackage.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MYPACKAGE
#ifdef ALLOW_MNC

      IF (useMNC) THEN

        CALL MNC_CW_ADD_VNAME('mypaStatScal1',
     &      'Cen_xy_Hn__L__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaStatScal1',
     &      'units','[unit]', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaStatScal1','description',
     &      'scalar state variable mypaStatScal1',myThid)

        CALL MNC_CW_ADD_VNAME('mypaStatScal2',
     &      'Cen_xy_Hn__L__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaStatScal2',
     &      'units','[unit]', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaStatScal2','description',
     &      'scalar state variable mypaStatScal2',myThid)

        CALL MNC_CW_ADD_VNAME('mypaStatUvel',
     &      'U_xy_Hn__C__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaStatUvel',
     &      'units','[unit]', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaStatUvel','description',
     &       'uVel-type state variable mypaStatUvel',myThid)

        CALL MNC_CW_ADD_VNAME('mypaStatVvel',
     &      'V_xy_Hn__C__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaStatVvel',
     &      'units','[unit]', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaStatVvel','description',
     &       'vVel-type state variable mypaStatVvel',myThid)

        CALL MNC_CW_ADD_VNAME('mypaSurf1',
     &      'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaSurf1',
     &      'units','[unit]', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaSurf1','description',
     &       'Surface scalar field mypaSurf1',myThid)

        CALL MNC_CW_ADD_VNAME('mypaSurf2',
     &      'Cen_xy_Hn__-__t', 3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaSurf2',
     &      'units','[unit]', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('mypaSurf2','description',
     &       'Surface scalar field mypaSurf2',myThid)

      ENDIF

#endif /* ALLOW_MNC */
#endif /* ALLOW_MYPACKAGE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
