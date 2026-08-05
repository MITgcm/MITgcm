#include "AUTODIFF_OPTIONS.h"

c     ==================================================================
c
c     adopen_adclose.F: Routines to handle the I/O of the TAF generated
c                       code. All files are direct access files.
c     Routines:
c
c     o  adopen  - Open file  (here a dummy routine).
c     o  adclose - Close file (here a dummy routine).
c
c
c     The following input veriables are used throughout in the argument
c     lists:
c
c     name   -  character
c                 On entry, name is the extended tape name.
c     len    -  integer
c                 On entry, len is the number of characters in name.
c     tid    -  integer
c                 On entry, tid identifies the tape.
c     vid    -  integer
c                 On entry, vid identifies the variable to be stored on
c                 the tape.
c     var    -  real array of dimension length
c                 On entry, var contains the values to be stored.
c                           var must not be changed.
c     size   -  integer
c                 On entry, size is the size in bytes of the type of
c                           variable var.
c     length -  integer
c                 On entry, length is the dimension of the variable
c                           stored on the tape.
c     irec   -  integer
c                 On entry, irec is the record number to be written.
c     mythid -  integer
c                 On entry, mythid is the number of the thread or
c                           instance of the program.
c     myiter -  integer
c                 On entry, myiter is the current iteration step during
c                           the integration.
c
c     TAF does not provide the two leading arguments mythid and myiter
c     when compiling the MITgcmUV code. Instead the is a sed script avail-
c     able that does change the TAF-generated adjoint code.
c
c     Only the master thread is allowed to write data and only gobal
c     model arrays are allowed to be written be the subsequent routines.
c     Tiled data are to be stored in common blocks. This implies that at
c     least a two level checkpointing for the adjoint code has to be
c     available.
c
c     ==================================================================


CBOP
C     !ROUTINE: adopen
C     !INTERFACE:
      subroutine adopen(
     I                   mythid,
cph(
cph     I                   myiter,
cph)
     I                   name,
     I                   len,
     I                   tid,
     I                   vid,
     I                   size,
     I                   length
     &                 )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adopen
c     ==================================================================
c     o Dummy routine expected to be available by TAF I/O.
c     This routine is simply a dummy routine expected to be available by
c     the Transformation of Algorithm in Fortran (TAF). Files are opened
c     and closed by the routines that are called by *adread* and
c     *adwrite*.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
c     SUBROUTINE adopen
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     name   -  extended tape name.
c     len    -  number of characters in name.
c     tid    -  tape identifier.
c     vid    -  identifies the variable to be stored on tape.
c     size   -  size in bytes of the type of variable var.
c     length -  dimension of the variable stored on the tape.
c     mythid -  number of the thread or instance of the program.

      integer mythid
cph(
cph      integer myiter
cph)
      character*(*) name
      integer len
      integer tid
      integer vid
      integer size
      integer length

C     !LOCAL VARIABLES:
c     == local variables ==

c     == end of interface ==
CEOP

      return
      end


CBOP
C     !ROUTINE: adclose
C     !INTERFACE:
      subroutine adclose(
     I                    mythid,
cph(
cph     I                    myiter,
cph)
     I                    name,
     I                    len,
     I                    tid,
     I                    vid,
     I                    size,
     I                    length
     &                  )


C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adclose
c     ==================================================================
c     o Dummy routine expected to be available by TAF I/O.
c     This routine is simply a dummy routine expected to be available by
c     the Transformation of Algorithm in Fortran (TAF). Files are opened
c     and closed by the routines that are called by *adread* and
c     *adwrite*.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
c     SUBROUTINE adclose
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     name   -  extended tape name.
c     len    -  number of characters in name.
c     tid    -  tape identifier.
c     vid    -  identifies the variable to be stored on tape.
c     size   -  size in bytes of the type of variable var.
c     length -  dimension of the variable stored on the tape.
c     mythid -  number of the thread or instance of the program.

      integer mythid
cph(
cph      integer myiter
cph)
      character*(*) name
      integer len
      integer tid
      integer vid
      integer size
      integer length

C     !LOCAL VARIABLES:
c     == local variables ==

c     == end of interface ==
CEOP

      return
      end
