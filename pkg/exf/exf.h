c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf.h,v 1.2 2002/12/28 10:11:11 dimitri Exp $
c
c
c     ==================================================================
c     HEADER EXF
c     ==================================================================
c
c     o Header for the external forcing package
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c
c              - restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c     ==================================================================
c     HEADER EXF
c     ==================================================================

      character*(5) externalforcingversion
      character*(5) usescalendarversion

      parameter(    externalforcingversion = '0.2.1' )
      parameter(    usescalendarversion    = '0.2.0' )
