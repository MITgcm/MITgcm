
#include "CPP_OPTIONS.h"

      subroutine adjoint_model(
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE adjoint_model
c     ==================================================================
c
c     o This is just a place holder for the TAMC generated adjoint code
c       of the MITgcmuv and related routines. Below you find a short
c       description of some topics pertaining to the adjoint model.
c
c       This routine will be overwritten as soon as TAMC will have
c       generated the adjoint model. If you want to keep it copy it
c       to another place.
c
c
c     Documentation:
c     ==============
c
c     This is a brief description of the MITgcmuv adjoint model.
c
c     Contents:
c
c      1.0 Basics
c      2.0 The Tangent Linear and Adjoint Model Compiler
c          2.1. TAMC Directives
c          2.2. Hand-coded MITgcmUV Routines
c          2.3. Postprocessing of the TAMC Generated Adjoint Code
c
c      3.0 Data Management
c          3.1. Active Variables on File
c          3.2. Storing the Reference Trajectory
c
c      4.0 Checkpointing
c          4.1. Checkpointing the Adjoint Code
c          4.2. Checkpointing the Model; Doing Restarts
c
c
c      For a documentation of the MITgcmuv see:
c
c        Hydrostatic, quasi-hydrostatic, and nonhydrostatic ocean
c        modeling.
c        J. Marshall, Ch. Hill, L. Perelman, and A. Adcroft
c        JGR, Vol. 102, No. C3, pp. 5733-5752
c
c        A finite-volume, incompressible Navier Stokes model for
c        studies of the ocean on parallel computers.
c        J. Marshall, A. Adcroft, Ch. Hill, L. Perelman, and
c        C. Heisey
c        JGR, Vol. 102, No. C3, pp. 5753-5766
c
c        Representation of Topography by Shaved Cells in a Height
c        Coordinate Ocean Model.
c        A. Adcroft, Ch. Hill, and J. Marshall
c        Mon. Wea. Rev., 1997, Vol. 125, pp. 2293-2315
c
c
c      For a documentation of the Tangent Linear and Adjoint
c      Model Compiler see:
c
c        http://puddle.mit.edu/~ralf/tamc/tamc.html
c        and the references therein.
c
c
c      1.0 Basics
c
c
c      2.0 The Tangent Linear and Adjoint Model Compiler
c
c          2.1. TAMC Directives
c
c          2.2. Hand-coded MITgcmUV Routines
c
c
c      3.0 Data Management
c
c          3.1. Active Variables on File
c
c          3.2. Storing the Reference Trajectory
c
c
c      4.0 Checkpointing
c
c          4.1. Checkpointing the Adjoint Code
c
c          4.2. Checkpointing the Model; Doing Restarts
c
c
c     started: Christian Eckert eckert@mit.edu 21-Jul-1999
c
c     changed: Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE adjoint_model
c     ==================================================================

      implicit none

c     == routine arguments ==

      integer mythid

c     == end of interface ==

      return
      end
