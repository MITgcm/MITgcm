c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getforcing.F,v 1.2 2001/09/28 15:55:25 heimbach Exp $

#include "CPP_OPTIONS.h"

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
c  |-- exf_getsurfacefluxes
c  |   |   
c  |   |-- exf_getffields <- this one does almost everything
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
c  |   |   5. if forcing and control is flux, then the control vector 
c  |   |      anomalies are read here
c  |   |      * ctrl_getheatflux
c  |   |      * ctrl_getsaltflux
c  |   |      * ctrl_getzonstress
c  |   |      * call ctrl_getmerstress
c  |   |
c  |   |-- exf_obcs
c  |   |   If open boundaries are prescribed externally,
c  |   |   OB values for current time step are read here
c  |   |   1. for each boundary (north, south, west, east)
c  |   |      sliced fields are read for T,S,U,V
c  |   |   2. if OB's are part of control vector,
c  |   |      control variable anomalies are added to OB values
c  |   |      * ctrl_getobcsn
c  |   |      * ctrl_getobcss
c  |   |      * ctrl_getobcsw
c  |   |      * ctrl_getobcse
c  |   |
c  |   |-- exf_mapfields
c  |   |   Forcing fields from exf package are mapped onto
c  |   |   mitgcm forcing arrays.
c  |   |   Mapping enables a runtime rescaling of fields
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
c       be set in EXF_CPPOPTIONS.h (or ECCO_CPPOPTIONS.h).
c       A note on surface fluxes:
c       The MITgcmUV's vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c       started: eckert@mit.edu, heimbach@mit.edu, ralf@ocean.mit.edu
c     *=================================================================
c     | SUBROUTINE exf_getforcing
c     *=================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid
      integer myiter
      _RL     mytime

C     !LOCAL VARIABLES:
c     == local variables ==

c     == end of interface ==
CEOP

c     Get values of climatological fields.
      call exf_getclim(
     I                     mytime, myiter, mythid
     &                   )

c     Get values of the surface fluxes.
      call exf_getsurfacefluxes(
     I                           mytime, myiter, mythid
     &                         )

#ifdef ALLOW_OBCS
      call exf_getobcs(
     I                           mytime, myiter, mythid
     &                         )
#endif

c     Map the forcing fields onto the corresponding model fields.
      call exf_mapfields(
     I                    mythid
     &                  )

      end

