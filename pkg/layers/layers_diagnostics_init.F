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
#include "GRID.h"
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
#ifdef LAYERS_PRHO_REF
      INTEGER kLoc
      _RL     rLoc
#endif
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

#ifdef LAYERS_PRHO_REF
       IF ( layers_num(iLa).EQ.3 ) THEN
        WRITE(diagName,'(A4,I1,A3)') 'LaTr',iLa,layers_name(iLa)
        rLoc = 0.
        kLoc = layers_krho(iLa)
        IF ( kLoc.GE.1 .AND. kLoc.LE.Nr ) THEN
          rLoc = rC(kLoc)
        ELSE
          kLoc = 0
        ENDIF
        WRITE(diagTitle,'(2A,F9.2,A,I4)') 'Pot. density (minus 1000)',
     &    ' for layer averaging, ref_z=', rLoc, ',', kLoc
        diagUnits = 'kg/m^3          '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       ENDIF
#endif

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

        WRITE(diagName,'(A4,I1,A3)') 'LaHc',iLa,layers_name(iLa)
        diagTitle = 'Layer Thickness at tracer points (m)'
        diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaTs',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to THETA surf. forc.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaTh',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to THETA horiz. diff.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

C ------- This diagnostic used to be called 'LaTr', but that name
C         conflicts with JMC s prho diagnostics.
        WRITE(diagName,'(A4,I1,A3)') 'LaTz',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to THETA vert. diff.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LTha',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to THETA horiz. adv.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LTza',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to THETA vert. adv.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LTto',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to all THETA terms.'
        diagUnits = 'm/day           '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaSs',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to SALT surf. forc.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaSh',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to SALT horiz. diff.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LaSz',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to SALT vert. diff.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LSha',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to SALT horiz. adv.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LSza',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to SALT vert. adv.'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

        WRITE(diagName,'(A4,I1,A3)') 'LSto',iLa,layers_name(iLa)
        diagTitle = 
     &   'Layer thickness tendency due to all SALT terms.'
        diagUnits = 'm/day           '
        diagCode  = 'SM      MX      '
        diagMate  = 0
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        CALL DIAGNOSTICS_SETKLEV( diagName, Nlayers-1 , myThid )

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
