#include "GMREDI_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: GMREDI_MNC_INIT

C     !INTERFACE:
      SUBROUTINE GMREDI_MNC_INIT( myThid )

C     !DESCRIPTION:
C     Initialize MNC lookup tables for GMRedi.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
#ifdef ALLOW_MNC

      IF (useMNC) THEN

        CALL MNC_CW_ADD_VNAME('Kux','U_xy_Hn__C__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kux','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kux','description',
     &       'GM Tensor Kux is K_11 element, X direction at U point',
     &       myThid)
C        CALL MNC_CW_ADD_VATTR_TEXT('Kux',
C    &       'coordinates','XC YC RC iter', myThid)

        CALL MNC_CW_ADD_VNAME('Kvy','V_xy_Hn__C__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kvy','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kvy','description',
     &       'GM Tensor Kvy is K_22 element, Y direction at V point',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('Kuz','U_xy_Hn__C__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kuz','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kuz','description',
     &       'GM Tensor Kuz is K_13 element, Z direction at U point',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('Kvz','V_xy_Hn__C__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kvz','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kvz','description',
     &       'GM Tensor Kvz is K_23 element, Z direction at V point',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('Kwx','Cen_xy_Hn__L__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kwx','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kwx','description',
     &       'GM Tensor Kwx is K_31 element, X direction at W point',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('Kwy','Cen_xy_Hn__L__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kwy','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kwy','description',
     &       'GM Tensor Kwy is K_32 element, Y direction at W point',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('Kwz','Cen_xy_Hn__L__t', 4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kwz','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('Kwz','description',
     &       'GM Tensor Kwz is K_33 element, Z direction at W point',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('VisbK','Cen_xy_Hn__-__t',3,4,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('VisbK','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('VisbK','description',
     &       'GM mixing/stirring coef after Visbeck et al.',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('PsiX','Cen_xy_Hn__C__t',4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('PsiX','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('PsiX','description',
     &       'GM bolus velocity streamfunction in X',
     &       myThid)

        CALL MNC_CW_ADD_VNAME('PsiY','Cen_xy_Hn__C__t',4,5,myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('PsiY','units','m^2/s', myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('PsiY','description',
     &       'GM bolus velocity streamfunction in Y', 
     &       myThid)

      ENDIF

#endif /* ALLOW_MNC */
#endif /* ALLOW_GMREDI */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
