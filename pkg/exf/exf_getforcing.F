c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getforcing.F,v 1.17 2005/06/28 22:05:49 heimbach Exp $

#include "EXF_OPTIONS.h"

CBOI
C
C !TITLE: EXTERNAL FORCING
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: External forcing package
c \bv
c * The external forcing package, in conjunction with the
c   calendar package, enables the handling of realistic forcing
c   fields of differing temporal forcing patterns.
c * It comprises climatological restoring and relaxation
c * Bulk formulae are implemented to convert atmospheric fields
c   to surface fluxes.
c
C     !CALLING SEQUENCE:
c ...
c  exf_getforcing
c  |
c  |-- exf_getclim (get climatological fields used e.g. for relax.)
c  |
c  |-- exf_getffields <- this one does almost everything
c  |   |   1. reads in fields, either flux or atmos. state,
c  |   |      depending on CPP options
c  |   |   2. If forcing and control is flux, we're already done here
c  |   |   3. If forcing is atmos. state, then
c  |   |      (a) if control is atmos. state, then the control variable
c  |   |          anomalies are read here
c  |   |          * ctrl_getatemp
c  |   |          * ctrl_getaqh
c  |   |          * ctrl_getuwind
c  |   |          * ctrl_getvwind
c  |   |   4. flux fields are interpolated on current time
c  |   |      (b) next the flux fields are computed via bulk formulae
c  |   |
c  |   |-- exf_obcs
c  |       If open boundaries are prescribed externally,
c  |       OB values for current time step are read here
c  |       1. for each boundary (north, south, west, east)
c  |          sliced fields are read for T,S,U,V
c  |       2. if OB's are part of control vector,
c  |          control variable anomalies are added to OB values
c  |          * ctrl_getobcsn
c  |          * ctrl_getobcss
c  |          * ctrl_getobcsw
c  |          * ctrl_getobcse
c  |
c  |-- exf_bulkformulae
c  |   If ALLOW_BULKFORMULAE, compute fluxes via bulkformulae
c  |
c  |-- exf_getsurfacefluxes
c  |   If forcing and control is flux, then the
c  |   control vector anomalies are read here
c  |      * ctrl_getheatflux
c  |      * ctrl_getsaltflux
c  |      * ctrl_getzonstress
c  |      * call ctrl_getmerstress
c  |
c  |-- exf_mapfields
c  |   Forcing fields from exf package are mapped onto
c  |   mitgcm forcing arrays.
c  |   Mapping enables a runtime rescaling of fields
c
c \ev
CEOI

CBOP
C     !ROUTINE: exf_getforcing
C     !INTERFACE:
      subroutine exf_getforcing( mytime, myiter, mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE exf_getforcing
c     *=================================================================
c     o Get the forcing fields for the current time step. The switches
c       for the inclusion of the individual forcing components have to
c       be set in EXF_OPTIONS.h (or ECCO_CPPOPTIONS.h).
c       A note on surface fluxes:
c       The MITgcmUV's vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c       started: eckert@mit.edu, heimbach@mit.edu, ralf@ocean.mit.edu
c       mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c     *=================================================================
c     | SUBROUTINE exf_getforcing
c     *=================================================================
C     \ev

C     !USES:
      implicit none

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_fields.h"
#include "exf_constants.h"

c     == global variables ==

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid
      integer myiter
      _RL     mytime

C     !LOCAL VARIABLES:
c     == local variables ==

      integer bi,bj
      integer i,j,k
      character*(max_len_mbuf) msgbuf

c     == end of interface ==
CEOP

#ifdef ALLOW_ECCO_OPTIMIZATION
      write(standardmessageunit,'(A,I10)')
     &   'exf: myiter = ', myiter
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

c     Get values of climatological fields.
      call exf_getclim( mytime, myiter, mythid )

c     Get the surface forcing fields.
      call exf_getffields( mytime, myiter, mythid )

      if ( useExfCheckRange .AND.
     &     ( myiter.EQ.niter0 .OR. debugLevel.GE.debLevB ) ) then
         call exf_check_range( mytime, myiter, mythid )
      endif

c     Compute bulk formulae
#ifdef ALLOW_BULKFORMULAE
      call exf_bulkformulae( mytime, myiter, mythid )
#endif

c     Apply runoff, masks and exchanges
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          k = 1
          do j = 1,sny
            do i = 1,snx
#ifdef ALLOW_RUNOFF
              sflux(i,j,bi,bj) = sflux(i,j,bi,bj) - runoff(i,j,bi,bj)
#endif

              hflux(i,j,bi,bj) = hflux(i,j,bi,bj)*maskc(i,j,1,bi,bj)
              sflux(i,j,bi,bj) = sflux(i,j,bi,bj)*maskc(i,j,1,bi,bj)
            enddo
          enddo
        enddo
      enddo

c     Update the tile edges.
      _EXCH_XY_R8(hflux,   mythid)
      _EXCH_XY_R8(sflux,   mythid)
       CALL EXCH_UV_XY_RL(ustress, vstress, .TRUE., myThid)
#ifdef SHORTWAVE_HEATING
      _EXCH_XY_R8(swflux, mythid)
#endif
#ifdef ATMOSPHERIC_LOADING
      _EXCH_XY_R8(apressure, mythid)
#endif

c     Get values of the surface flux anomalies.
      call exf_getsurfacefluxes( mytime, myiter, mythid )

c     Diagnostics output
      call exf_diagnostics_fill( mytime, myiter, mythid )

c     Map the forcing fields onto the corresponding model fields.
      call exf_mapfields( mythid )

      end

