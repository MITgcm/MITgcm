
#ifndef BLAS1_H
#define BLAS1_H

#ifdef IS_DOUBLE
# define SDOT    ddot
# define SNRM2   dnrm2
# define SSUM    dsum
# define SSCAL   dscal
#else
# define SDOT    sdot
# define SNRM2   snrm2
# define SSUM    ssum
# define SSCAL   sscal
#endif

#endif
