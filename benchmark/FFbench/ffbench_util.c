/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "ffbench_util.h"


/*-------------------------------------------------------------------------
 *  Function:      FFbench_timer_start() 
 *
 *  Purpose :      Start timer 
 *
 *  Return:        SUCCESS : FFB_SUCCESS
 *                 FAILURE : FFB_FAIL
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */

int FFbench_timer_start(timer *rettimer){
  
  int ret_value = FFB_SUCCESS;
  timer starttime;
  
  if (FFB_SUCCESS != gettimeofday(&starttime, NULL))
    ret_value = FFB_FAIL;

  *rettimer = starttime;
  return ret_value;
}


/*-------------------------------------------------------------------------
 *  Function:      FFbench_timer_end();
 *
 *  Purpose :      End timer 
 *
 *  Return:        SUCCESS : FFB_SUCCESS
 *                 FAILURE : FFB_FAIL
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */

int FFbench_timer_end(timer *rettimer){
  
  int ret_value = FFB_SUCCESS;
  timer endtime;

  if (FFB_SUCCESS !=  gettimeofday(&endtime, NULL)){
    ret_value =  FFB_FAIL;
  }

  *rettimer = endtime;
  return ret_value;
}


/*-------------------------------------------------------------------------
 *  Function:      FFbench_timer_gettime 
 *
 *  Purpose :      Calculate the total_time consumed
 *
 *  Return:        SUCCESS : Positive
 *                 FAILURE : Negative
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */

double FFbench_timer_gettime(timer start, 
			     timer end){
  
  double elapsedTime;

  elapsedTime = (end.tv_sec - start.tv_sec) * 1000.0;
  elapsedTime += (end.tv_usec - start.tv_usec) / 1000.0;

  return elapsedTime;
  
}

/*-------------------------------------------------------------------------
 *  Function:      FFbench_getBandwidth 
 *
 *  Purpose :      Get the I/O bandwidth from the totalbytes written
 *                 + totat time consumed in MB/s. The input is expected in 
 *                 bytes
 *
 *  Return:        SUCCESS : Positive
 *                 FAILURE : Negative
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */

double FFbench_getBandwidth(length_t totallength,
			    double totaltime){
  
  /*Convert to MB*/
  totallength /= 1024;
  totallength /= 1024; 
  
  return (double) (totallength/totaltime);
  
}
