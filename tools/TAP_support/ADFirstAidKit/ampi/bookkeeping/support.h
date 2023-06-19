/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_BOOKKEEPING_SUPPORT_H_
#define _AMPI_BOOKKEEPING_SUPPORT_H_

#include "ampi/userIF/request.h"
#include "ampi/userIF/window.h"

/**
 * \file 
 * methods needed for internal request bookkeeping
 */ 

/**
 * \param ampiRequest is added (by deep copy) to the internal bookkeeping using the already set valueu of member plainRequest as key
 */
void BK_AMPI_put_AMPI_Request(struct AMPI_Request_S  *ampiRequest);

/**
 * \param request is used as key to look up the associated AMPI_Request_S instance which then is deep copied 
 * \param ampiRequest pointer to the structure into which the values are copied 
 * \param traced if non-zero indicates one should match tracedReques instead of plainRequest 
 * the information is removed from the internal bookkeeping data
 */
void BK_AMPI_get_AMPI_Request(MPI_Request *request, struct AMPI_Request_S  *ampiRequest, int traced);

/**
 * \param request is used as key to look up the associated AMPI_Request_S instance which then is deep copied 
 * \param ampiRequest pointer to the structure into which the values are copied 
 * \param traced if non-zero indicates one should match tracedReques instead of plainRequest 
 * the information is retained in the internal bookkeeping data
 */
void BK_AMPI_read_AMPI_Request(MPI_Request *request, struct AMPI_Request_S  *ampiRequest, int traced);

/**
 * \file 
 * methods needed for internal window bookkeeping
 */ 

/**
 * \param ampiWin is added (by deep copy) to the internal bookkeeping using the already set valueu of member plainRequest as key
 */
void BK_AMPI_put_AMPI_Win(AMPI_Win  *ampiWin);

/**
 * \param win is used as key to look up the associated AMPI_Win instance which then is deep copied 
 * \param ampiWin pointer to the structure into which the values are copied 
 * the information is removed from the internal bookkeeping data
 */
void BK_AMPI_get_AMPI_Win(MPI_Win *win, AMPI_Win  *ampiWin);

/**
 * \param win is used as key to look up the associated AMPI_Win instance which then is deep copied 
 * \param ampiWin pointer to the structure into which the values are copied 
 * the information is retained in the internal bookkeeping data
 */
void BK_AMPI_read_AMPI_Win(MPI_Win *win, AMPI_Win  *ampiWin);

#endif

