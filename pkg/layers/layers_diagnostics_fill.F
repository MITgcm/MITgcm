C $Header: /u/gcmpack/MITgcm/pkg/layers/Attic/layers_diagnostics_fill.F,v 1.5 2012/10/17 23:10:07 jmc Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: LAYERS_DIAGNOSTICS_FILL

C !INTERFACE:
      SUBROUTINE LAYERS_DIAGNOSTICS_FILL( myThid )

C     !DESCRIPTION:
C     Fill-in diagnostics array for LAYERS package

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
C     !LOCAL VARIABLES:
      CHARACTER*8    diagName
      INTEGER iLa
CEOP

c     IF ( useDiagnostics ) THEN

      DO iLa=1,layers_maxNum
       IF ( layers_num(iLa).NE.0 ) THEN

#ifdef LAYERS_UFLUX
         WRITE(diagName,'(A4,I1,A3)') 'LaUH',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_UH(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# ifdef LAYERS_THICKNESS
         WRITE(diagName,'(A4,I1,A3)') 'LaHw',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_Hw(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaPw',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_PIw(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaUa',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_U(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# endif
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
         WRITE(diagName,'(A4,I1,A3)') 'LaVH',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_VH(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# ifdef LAYERS_THICKNESS
         WRITE(diagName,'(A4,I1,A3)') 'LaHs',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_Hs(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaPs',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_PIs(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
         WRITE(diagName,'(A4,I1,A3)') 'LaVa',iLa,layers_name(iLa)
         CALL DIAGNOSTICS_FILL( layers_V(1-OLx,1-OLy,1,1,1,iLa),
     &                          diagName,0,Nlayers, 0, 1, 1, myThid )
# endif
#endif /* LAYERS_VFLUX */

       ENDIF
      ENDDO

c     ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_LAYERS */

      RETURN
      END
