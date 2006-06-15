C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/accept_component_registrations.F,v 1.3 2006/06/15 23:29:17 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ACCEPT_COMPONENT_REGISTRATIONS
C     /==========================================================\
C     | SUBROUTINE ACCEPT_COMPONENT_REGISTRATIONS                |
C     | o Routine to coordinate registration of component other  |
C     |   component processes.                                   |
C     |==========================================================|
C     | During registration each component process sends its     |
C     | attributes. Currently attributes are the layout of tiles |
C     | that the process owns. The 'Coupler' component (i.e. this|
C     | process) stores these attributes. Subsequent transfers   |
C     | to/from the 'Coupler' process implicitly assume these    |
C     | gridding attributes. Only one set of attributes is       |
C     | allowed per process. The dimension arguments in the      |
C     | 'Coupler' process calls to the registration routine are  |
C     | used to check consistency. The tile interior extents     |
C     | registered by a component have to lie within the global  |
C     | extents set in the 'Coupler' process for that component. |
C     \==========================================================/
      IMPLICIT NONE
C     === GLobal variables ==
#include "CPLIDS.h"
#include "ATMSIZE.h"
#include "OCNSIZE.h"
#include "ATMIDS.h"
#include "OCNIDS.h"

CEndOfInterface

C     === Local variables ===

C     During registration the coupler process collects attributes from each 
C     of the "component" participants. It collects the attributes from each 
C     participating component in turn. Individual components comprise of one 
C     or more processes. Each component process does its own registration call 
C     to report its attributes. The attributes that are collected
C     include information on the grid region that a particular process "owns"
C     and sends out information from.

C     Get attributes for atmosphere component.
      CALL CPL_REGISTER_ATM

C     Get attributes for ocean component.
      CALL CPL_REGISTER_OCN


      RETURN
      END
