C $Header: /u/gcmpack/MITgcm/verification/global_with_exf/code/DIAGNOSTICS_SIZE.h,v 1.1 2005/05/05 20:25:16 dimitri Exp $
C $Name:  $


C     Diagnostics Array Dimension
C     ---------------------------
C     ndiagMax   :: maximum total number of available diagnostics
C     numlists   :: maximum number of diagnostics list (in data.diagnostics)
C     numperlist :: maximum number of active diagnostics per list (data.diagnostics)
C     numLevels  :: maximum number of levels to write    (data.diagnostics)
C     numdiags   :: maximum size of the storage array for active diagnostics
      INTEGER    ndiagMax
      INTEGER    numlists
      INTEGER    numperlist
      INTEGER    numLevels
      INTEGER    numdiags
      PARAMETER( ndiagMax = 500 )
      PARAMETER( numlists = 7, numperlist = 1, numLevels=Nr )
      PARAMETER( numdiags = 4*Nr+3 )


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
