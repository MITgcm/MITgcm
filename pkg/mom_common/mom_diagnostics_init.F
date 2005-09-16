C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_diagnostics_init.F,v 1.4 2005/09/16 19:32:20 baylor Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE MOM_DIAGNOSTICS_INIT( myThid )
C     !DESCRIPTION:
C     Routine to initialize Momentum diagnostics
C      (from mom_common, mom_fluxform & mom_vecinv packages)

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instance of GAD_INIT
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
#ifdef ALLOW_MOM_COMMON
C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  Add diagnostics to the (long) list of available diagnostics:

C--   add diagnostics for viscosity coefficients:
      diagName  = 'VISCAHD '
      diagTitle = 'Harmonic Viscosity Coefficient (m2/s) (Div Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VISCA4D '
      diagTitle = 'Biharmonic Viscosity Coefficient (m4/s) (Div Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VISCAHZ '
      diagTitle = 'Harmonic Viscosity Coefficient (m2/s) (Zeta Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VISCA4Z '
      diagTitle = 'Biharmonic Viscosity Coefficient (m4/s) (Zeta Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

C--    add diagnostics of simple derivative quantities:
       diagName  = 'momKE   '
       diagTitle = 'Kinetic Energy (in momentum Eq.)'
       diagCode  = 'SM      MR      '
       diagUnits = 'm^2/s^2         '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

#ifdef ALLOW_MOM_VECINV
       diagName  = 'momHDiv '
       diagTitle = 'Horizontal Divergence (in momentum Eq.)'
       diagCode  = 'SM      MR      '
       diagUnits = 's^-1            '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

       diagName  = 'momVort3'
       diagTitle = '3rd component (vertical) of Vorticity'
       diagCode  = 'SZ      MR      '
       diagUnits = 's^-1            '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
#endif /* ALLOW_MOM_VECINV */

C--    add diagnostics of tendencies from each terms
       diagUnits = 'm/s^2           '

       diagName  = 'Um_Diss '
       diagTitle = 'U momentum tendency from Dissipation'
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'Vm_Diss '
       diagTitle = 'V momentum tendency from Dissipation'
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

       diagName  = 'Um_Advec'
       diagTitle = 'U momentum tendency from Advection terms'
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'Vm_Advec'
       diagTitle = 'V momentum tendency from Advection terms'
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

       diagName  = 'Um_Cori '
       diagTitle = 'U momentum tendency from Coriolis term'
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'Vm_Cori '
       diagTitle = 'V momentum tendency from Coriolis term'
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

#ifdef ALLOW_MOM_VECINV
       diagName  = 'Um_AdvZ3'
       diagTitle = 'U momentum tendency from Vorticity Advection'
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'Vm_AdvZ3'
       diagTitle = 'V momentum tendency from Vorticity Advection'
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

       diagName  = 'Um_AdvRe'
       diagTitle = 'U momentum tendency from vertical Advection'
     &           //' (Explicit part)'
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'Vm_AdvRe'
       diagTitle = 'V momentum tendency from vertical Advection'
     &           //' (Explicit part)'
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
#endif /* ALLOW_MOM_VECINV */

C--    add diagnostics of advective & viscous flux :
       IF ( usingPCoords ) THEN
         diagUnits = 'Pa.m^3/s^2      '
       ELSE
         diagUnits = 'm^4/s^2         '
       ENDIF

C-     Advective flux of Zonal momentum:
#ifdef ALLOW_MOM_FLUXFORM
       diagName  = 'ADVx_Um '
       diagTitle = 'Zonal      Advective Flux of U momentum'
       WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'ADVy_Um '
       diagTitle = 'Meridional Advective Flux of U momentum'
       WRITE(diagCode,'(A,I3.3,A)') 'VZ   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'ADVrE_Um'
       diagTitle = 'Vertical   Advective Flux of U momentum'
     &           //' (Explicit part)'
       diagCode  = 'WU      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
#endif /* ALLOW_MOM_FLUXFORM */
c      diagName  = 'ADVrI_Um'
c      diagTitle = 'Vertical   Advective Flux of U momentum'
c    &           //' (Implicit part)'
c      diagCode  = 'WU      LR      '
c      CALL DIAGNOSTICS_ADD2LIST( diagNum,
c    I              diagName, diagCode, diagUnits, diagTitle, myThid )

C-     Advective flux of Meridional momentum:
#ifdef ALLOW_MOM_FLUXFORM
       diagName  = 'ADVx_Vm '
       diagTitle = 'Zonal      Advective Flux of V momentum'
       WRITE(diagCode,'(A,I3.3,A)') 'UZ   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'ADVy_Vm '
       diagTitle = 'Meridional Advective Flux of V momentum'
       WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'ADVrE_Vm'
       diagTitle = 'Vertical   Advective Flux of V momentum'
     &           //' (Explicit part)'
       diagCode  = 'WV      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
#endif /* ALLOW_MOM_FLUXFORM */
c      diagName  = 'ADVrI_Vm'
c      diagTitle = 'Vertical   Advective Flux of V momentum'
c    &           //' (Implicit part)'
c      diagCode  = 'WV      LR      '
c      CALL DIAGNOSTICS_ADD2LIST( diagNum,
c    I              diagName, diagCode, diagUnits, diagTitle, myThid )

c-     Viscous flux of Zonal momentum :
#ifdef ALLOW_MOM_FLUXFORM
       diagName  = 'VISCx_Um'
       diagTitle = 'Zonal      Viscous Flux of U momentum'
       WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'VISCy_Um'
       diagTitle = 'Meridional Viscous Flux of U momentum'
       WRITE(diagCode,'(A,I3.3,A)') 'VZ   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'VISrE_Um'
       diagTitle = 'Vertical   Viscous Flux of U momentum'
     &           //' (Explicit part)'
       diagCode  = 'WU      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
#endif /* ALLOW_MOM_FLUXFORM */
       diagName  = 'VISrI_Um'
       diagTitle = 'Vertical   Viscous Flux of U momentum'
     &           //' (Implicit part)'
       diagCode  = 'WU      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

c-     Viscous flux of Meridional momentum :
#ifdef ALLOW_MOM_FLUXFORM
       diagName  = 'VISCx_Vm'
       diagTitle = 'Zonal      Viscous Flux of V momentum'
       WRITE(diagCode,'(A,I3.3,A)') 'UZ   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'VISCy_Vm'
       diagTitle = 'Meridional Viscous Flux of V momentum'
       WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
       diagName  = 'VISrE_Vm'
       diagTitle = 'Vertical   Viscous Flux of V momentum'
     &           //' (Explicit part)'
       diagCode  = 'WV      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
#endif /* ALLOW_MOM_FLUXFORM */
       diagName  = 'VISrI_Vm'
       diagTitle = 'Vertical   Viscous Flux of V momentum'
     &           //' (Implicit part)'
       diagCode  = 'WV      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_MOM_COMMON */
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
