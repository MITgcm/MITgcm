C $Header: /u/gcmpack/MITgcm/pkg/ctrl/optim_readparms.F,v 1.4 2012/08/10 19:38:58 jmc Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"


      subroutine optim_readparms(
     I                            mythid
     &                          )

c     ==================================================================
c     SUBROUTINE optim_readparms
c     ==================================================================
c
c     o Initialise the optimization part of the ECCO release.
c
c     started: Christian Eckert eckert@mit.edu 03-Mar-2000
c
c     changed:
c
c     ==================================================================
c     SUBROUTINE optim_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"

#include "optim.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer errio
      integer il
      integer iUnit

      character*(max_len_prec) record
      character*(max_len_mbuf) msgbuf

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Optimization parameters.
      namelist /optim/
     &                 optimcycle, nvars,
     &                 nondimcontrol,
     &                 numiter, nfunc, fmin, iprint,
     &                 epsf, epsx, epsg,
     &                 nupdate, eps

      _BEGIN_MASTER( mythid )

c--     Set default values.
        optimcycle    =   0
        nvars         =  10
        nondimcontrol =  .false.
        numiter       =   0
        nfunc         =   1
        fmin          =   0.0 _d 0
        iprint        =  10
        epsx          =   1.e-6
        epsg          =   1.e-6
        eps           =  -1.e-6
        nupdate       =   0

        WRITE(msgBuf,'(A)') 'OPTIM_READPARMS: opening data.optim'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.optim', 'OPTIM_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(unit = iUnit, nml = optim)

        WRITE(msgBuf,'(A)')
     &       'OPTIM_READPARMS: finished reading data.optim'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

        CLOSE( iUnit )

      _END_MASTER( mythid )

      _BARRIER

      return
      end

