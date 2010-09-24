C $Header: /u/gcmpack/MITgcm/pkg/autodiff/adread_adwrite.F,v 1.11 2010/09/24 23:22:20 gforget Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "AUTODIFF_OPTIONS.h"

c     ==================================================================
c
c     ad_read_write.F: routines to handle the I/O of the TAMC generated
c                      code. All files are direct access files.
c     Routines:
c
c     o  adread  - Read  data from file.
c     o  adwrite - Write data to   file.
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
c     For further details on this see the TAMC Users Manual, Appendix B,
c     User defined Storage Subroutines.
c
c     TAMC does not provide the two leading arguments mythid and myiter
c     when compiling the MITgcmUV code. Instead the is a sed script avail-
c     able that does change the TAMC-generated adjoint code.
c
c     Only the master thread is allowed to write data and only gobal
c     model arrays are allowed to be written be the subsequent routines.
c     Tiled data are to be stored in common blocks. This implies that at
c     least a two level checkpointing for the adjoint code has to be
c     available.
c
c     ==================================================================


CBOP
C     !ROUTINE: adread
C     !INTERFACE:
      subroutine adread(
     I                   mythid,
     I                   name,
     I                   len,
     I                   tid,
     I                   vid,
     O                   var,
     I                   size,
     I                   length,
     I                   irec
     &                 )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adread
c     ==================================================================
c     o Read direct access file.
c     A call to this routine implies an open-read-close sequence
c     since it uses the MITgcmUV i/o routine MDSREADVECTOR. Only
c     the master thread reads the data. Otherwise each thread would
c     read from file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
c     SUBROUTINE adread
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"
#include "optim.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     name   -  extended tape name.
c     len    -  number of characters in name.
c     tid    -  tape identifier.
c     vid    -  identifies the variable to be stored on tape.
c     var    -  values to be stored.
c     size   -  size in bytes of the type of variable var.
c     length -  dimension of the variable stored on the tape.
c     mythid -  number of the thread or instance of the program.
c     irec   -  record number to be written.

      integer mythid
      character*(*) name
      integer len
      integer tid
      integer vid
      integer size, sizetmp
      integer length
      integer irec
      _RL     var(length)

C     !LOCAL VARIABLES:
c     == local variables ==
      character*(7) itername
      character*(MAX_LEN_FNAM) fname
      character*(MAX_LEN_MBUF) msgBuf
      integer il
      integer bx,by
      integer n2d,length2d

c     == functions ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_ENTER('ADREAD',myThid)
#endif

      IF ( doSinglePrecTapelev ) THEN
       sizetmp = 32
      ELSE
       sizetmp = 64
      ENDIF

#ifdef ALLOW_AUTODIFF_WHTAPEIO

c    determine number of 2d fields
      length2d=(sNx+2*Olx)*(sNy+2*Oly)*nSy*nSy
      n2d=int(length/length2d)
c
      if (n2d*length2d.EQ.length) then
      call mds_read_whalos(name,len,sizetmp,
     &               0,n2d,var,irec,mythid)
      else
c   The other case actually does not (and should not) occur within the main loop, 
c   where we only store global arrays (i.e. with bi,bj indices) to disk. 
c     At init and final time it is always be possible to recompute or store in
c   memory without much trouble or computational cost.
c
c    Presently there are three instances where non-global arrays are stored to disk:
c   (a) the one instance when onetape is used, to store mytime, which is of no effect. 
c   In the_main_loop, we switch onetape to memory tape if ALLOW_AUTODIFF_WHTAPEIO
c   (b) the two instances when tapelev_ini_bibj_k is used (in convective
c   _adjustment_ini.F and cost_drifter.F) are disabled at compile time if
c   ALLOW_AUTODIFF_WHTAPEIO. So is the definition of tapelev_ini_bibj_k,
c   which is not supported with ALLOW_AUTODIFF_WHTAPEIO (side-note:
c   tapelev_ini_bibj_k is likely unsafe with mdsreadvector/mdsreadvector) 
c
c   The issue could be revisited if needed.
         WRITE(msgBuf,'(3A)')
     &        'ADWRITE: ',name,'was not saved to tape.'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
      endif
      
#else

      write(fname(1:MAX_LEN_FNAM),'(a)') ' '
      write(itername,'(a,i4.4)') '.it',optimcycle

      il = ilnblnk( name )

      write(fname(1:il+7),'(a,a)') name(1:il),itername

      _BEGIN_MASTER( mythid )
        by = myByLo(myThid)
        bx = myBxLo(myThid)
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
         WRITE(msgBuf,'(A,I3,A)')
     &     'ADREAD: call mdsreadvector for size, fname = ',
     &      sizetmp, fname(1:il+7)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
      ENDIF
#endif
        call mdsreadvector( fname, sizetmp, 'RL',
     &                      length, var, bx, by, irec, mythid )
      _END_MASTER( mythid )

#endif /* ALLOW_AUTODIFF_WHTAPEIO */

c     Everyone must wait for the read operation to be completed.
      _BARRIER

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_LEAVE('ADREAD',myThid)
#endif

      return
      end


CBOP
C     !ROUTINE: adwrite
C     !INTERFACE:
      subroutine adwrite(
     I                    mythid,
     I                    name,
     I                    len,
     I                    tid,
     I                    vid,
     I                    var,
     I                    size,
     I                    length,
     I                    irec
     &                  )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adwrite
c     ==================================================================
c     o Write to direct access file.
c     A call to this routine implies an open-read-close sequence
c     since it uses the MITgcmUV i/o routine MDSREADVECTOR. Only
c     the master thread writes the data. Otherwise each thread would
c     write to file. This would result in an excessive waste of
c     disk space.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
c     SUBROUTINE adwrite
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"
#include "optim.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     name   -  extended tape name.
c     len    -  number of characters in name.
c     tid    -  tape identifier.
c     vid    -  identifies the variable to be stored on tape.
c     var    -  values to be stored.
c     size   -  size in bytes of the type of variable var.
c     length -  dimension of the variable stored on the tape.
c     mythid -  number of the thread or instance of the program.
c     irec   -  record number to be written.

      integer mythid
      character*(*) name
      integer len
      integer tid
      integer vid
      integer size, sizetmp
      integer length
      integer irec
      _RL     var(length)

C     !LOCAL VARIABLES:
c     == local variables ==
      character*(7) itername
      character*(MAX_LEN_FNAM) fname
      character*(MAX_LEN_MBUF) msgBuf
      integer il
      integer bx,by
      logical globalfile
      integer n2d,length2d

c     == functions ==
      integer ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_ENTER('ADWRITE',myThid)
#endif

      IF ( doSinglePrecTapelev ) THEN
       sizetmp = 32
      ELSE
       sizetmp = 64
      ENDIF

#ifdef ALLOW_AUTODIFF_WHTAPEIO
c    determine number of 2d fields
      length2d=(sNx+2*Olx)*(sNy+2*Oly)*nSy*nSy
      n2d=int(length/length2d)
c
      if (n2d*length2d.EQ.length) then
      call mds_write_whalos(name,len,sizetmp,
     &               0,n2d,var,irec,mythid)
      else
         WRITE(msgBuf,'(3A)') 
     &        'ADREAD: ',name,'was not read from tape.'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
      endif

#else

      globalfile = .false.
      il         = ilnblnk( name )
      write(fname(1:MAX_LEN_FNAM),'(a)') ' '

      write(itername,'(a,i4.4)') '.it',optimcycle

      write(fname(1:il+7),'(a,a)') name(1:il),itername

      _BEGIN_MASTER( mythid )
        by = myByLo(myThid)
        bx = myBxLo(myThid)
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
         WRITE(msgBuf,'(A,I3,A)')
     &     'ADWRITE: call mdswritevector for size, fname = ',
     &     sizetmp, fname(1:il+7)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(A,I12,4I6)')
     &     'ADWRITE: ph', length, bx, by, irec
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
      ENDIF
#endif
        call mdswritevector( fname, sizetmp, globalfile, 'RL',
     &                       length, var, bx, by, irec, 0, mythid )
      _END_MASTER( mythid )

#endif /* ALLOW_AUTODIFF_WHTAPEIO */

c     Everyone must wait for the write operation to be completed.
      _BARRIER

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_LEAVE('ADWRITE',myThid)
#endif

      return
      end
