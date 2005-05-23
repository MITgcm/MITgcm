C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readgeoid.F,v 1.2 2005/05/23 16:11:38 heimbach Exp $

#include "COST_CPPOPTIONS.h"

      subroutine cost_readgeoid(
     I                     geoid_covariancefile, reclength, jrec,
     O                     pinv,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_readgeoid
c     ==================================================================
c
c     o Evaluate the cost function of the geoid contribution.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     changed: Ralf Giering Ralf.Giering@FastOpt.de 12-Jun-2001
c              - totally rewrite for parallel processing
c
c              heimbach@mit.edu 05-May-2005
c              - debugged and restructuted
c
c     ==================================================================
c     SUBROUTINE cost_readgeoid
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_SPHERE
# include "sphere.h"
#endif

c     == routine arguments ==

      character*(MAX_LEN_FNAM) geoid_covariancefile
      integer reclength
      integer jrec
#ifndef ALLOW_SPHERE
c--   dummy declaration
      integer ncshc
      parameter ( ncshc = 1 )
#endif
      Real*8  pinv( ncshc )
      integer mythid

#ifdef ALLOW_EGM96_ERROR_COV

c     == local variables ==

      integer ilo,ihi
      integer ireads
      integer egmunit
      character*(max_len_mbuf) msgbuf

c     == external functions ==
      integer  ilnblnk
      external ilnblnk
      integer  ifnblnk
      external ifnblnk

c     == end of interface ==

#ifndef ALLOW_SPHERE
      print *, 'WARNING: this routine assumes package sphere'
      print *, 'enable sphere in packages.conf when using '
      print *, 'CPP option ALLOW_EGM96_ERROR_COV'
      STOP 'in S/R cost_readgeoid'
#endif

c--   Only the master thread is doing I/O
      _BEGIN_MASTER( mythid )

c--   Open the geoid error covariance data file.
      call mdsfindunit( egmunit, mythid )

      ilo = ifnblnk(geoid_covariancefile)
      ihi = ilnblnk(geoid_covariancefile)

      write(msgbuf,'(2a)')
     &     'opening geoid_covariancefile = ',
     &     geoid_covariancefile(ilo:ihi)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      open(egmunit, file   = geoid_covariancefile(ilo:ihi),
     &              form   = 'unformatted',
     &              access = 'direct',
     &              recl   = reclength,
     &              status = 'old' )

      read(egmunit,rec=jrec) pinv

c--   close geoid error covariance file
      close(egmunit)

      _END_MASTER( mythid )

#endif /* ALLOW_EGM96_ERROR_COV */

      end


