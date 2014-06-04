C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_diagnostics_init.F,v 1.6 2014/06/04 14:48:32 rpa Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: LAYERS_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE LAYERS_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics for LAYERS package

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_LAYERS
# include "LAYERS_SIZE.h"
# include "LAYERS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid

#ifdef ALLOW_LAYERS
#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      INTEGER  DIAGS_GET_PARMS_I
      EXTERNAL DIAGS_GET_PARMS_I
      CHARACTER*(16) DIAGS_MK_UNITS
      EXTERNAL DIAGS_MK_UNITS

C     !LOCAL VARIABLES:
      INTEGER        iLa
      INTEGER        diagNum
      INTEGER        diagMate
      CHARACTER*8    diagName
      CHARACTER*16   diagCode
      CHARACTER*16   diagUnits
      CHARACTER*(80) diagTitle
      CHARACTER*2    rUnit2c
CEOP

c     IF ( useDiagnostics ) THEN

      IF ( usingPCoords ) THEN
        rUnit2c= 'Pa'
      ELSE
        rUnit2c= 'm '
      ENDIF
      diagNum = DIAGS_GET_PARMS_I( 'LAST_DIAG_ID', myThid )

      DO iLa=1,layers_maxNum
       IF ( layers_num(iLa).NE.0 ) THEN

#if (defined LAYERS_UFLUX) || (defined LAYERS_VFLUX)

C --- UH, VH
        WRITE(diagName,'(A4,I1,A3)') 'LaUH',iLa,layers_name(iLa)
        diagTitle = 'Layer Integrated  zonal Transport (UH, m^2/s)'
        diagUnits = DIAGS_MK_UNITS( rUnit2c//'.m/s', myThid )
        diagCode  = 'UU      MX      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaVH',iLa,layers_name(iLa)
        diagTitle = 'Layer Integrated merid. Transport (VH, m^2/s)'
        diagUnits = DIAGS_MK_UNITS( rUnit2c//'.m/s', myThid )
        diagCode  = 'VV      MX      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

#ifdef LAYERS_THICKNESS

C --- Thickness
        WRITE(diagName,'(A4,I1,A3)') 'LaHw',iLa,layers_name(iLa)
        diagTitle = 'Layer Thickness at U points (m)'
        diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
        diagCode  = 'SU      MX      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaHs',iLa,layers_name(iLa)
        diagTitle = 'Layer Thickness at V points (m)'
        diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
        diagCode  = 'SV      MX      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

C --- PI
        WRITE(diagName,'(A4,I1,A3)') 'LaPw',iLa,layers_name(iLa)
        diagTitle = 'Layer Probability at U points [-]'
        diagUnits = '1               '
        diagCode  = 'SU      MX      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaPs',iLa,layers_name(iLa)
        diagTitle = 'Layer Probability at V points [-]'
        diagUnits = '1               '
        diagCode  = 'SV      MX      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

C --- U, V
        WRITE(diagName,'(A4,I1,A3)') 'LaUa',iLa,layers_name(iLa)
        diagTitle = 'Layer-averaged U velocity (non-weighted) (m/s)'
        diagUnits = 'm/s             '
        diagCode  = 'UU      MX      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaVa',iLa,layers_name(iLa)
        diagTitle = 'Layer-averaged V velocity (non-weighted) (m/s)'
        diagUnits = 'm/s             '
        diagCode  = 'VV      MX      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

#endif /* LAYERS_THICKNESS */

#endif /* LAYERS_UFLUX or LAYERS_VFLUX */

#ifdef LAYERS_THERMODYNAMICS

        WRITE(diagName,'(A4,I1,A3)') 'LaTs',iLa,layers_name(iLa)
        diagTitle = 'Layer THETA transformation from surf. forc.'
        diagUnits = 'm deg./s        '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaTh',iLa,layers_name(iLa)
        diagTitle = 'Layer THETA transformation from horiz. diff.'
        diagUnits = 'm deg./s        '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaTr',iLa,layers_name(iLa)
        diagTitle = 'Layer THETA transformation from vert. diff.'
        diagUnits = 'm deg./s        '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaSs',iLa,layers_name(iLa)
        diagTitle = 'Layer SALT transformation from surf. forc.'
        diagUnits = 'm PSU/s        '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaSh',iLa,layers_name(iLa)
        diagTitle = 'Layer SALT transformation from horiz. diff.'
        diagUnits = 'm PSU/s        '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaSr',iLa,layers_name(iLa)
        diagTitle = 'Layer SALT transformation from vert. diff.'
        diagUnits = 'm PSU/s        '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaHc',iLa,layers_name(iLa)
        diagTitle = 'Layer Thickness at tracer points (m)'
        diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

C --- PI
        WRITE(diagName,'(A4,I1,A3)') 'LaPc',iLa,layers_name(iLa)
        diagTitle = 'Layer Probability at tracer points [-]'
        diagUnits = '1               '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )
#endif /* LAYERS_THERMODYNAMICS */

       ENDIF
      ENDDO

c     ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_LAYERS */

      RETURN
      END
