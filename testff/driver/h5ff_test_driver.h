/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

#ifndef H5FF_TEST_DRIVER_H
#define H5FF_TEST_DRIVER_H

#include <string>
#include <vector>

#include <h5ff_sys/Process.h>

class H5FFTestDriver
{
public:
  int Main(int argc, char* argv[]);
  H5FFTestDriver();
  ~H5FFTestDriver();

protected:
  void SeparateArguments(const char* str, 
                         std::vector<std::string>& flags);
  
  void ReportCommand(const char* const* command, const char* name);
  int ReportStatus(h5ff_sysProcess* process, const char* name);
  int ProcessCommandLine(int argc, char* argv[]);
  void CollectConfiguredOptions();
  void CreateCommandLine(std::vector<const char*>& commandLine,
                         const char* paraView,
                         const char* numProc,
                         int argStart=0,
                         int argCount=0,
                         char* argv[]=0);
  
  int StartServer(h5ff_sysProcess* server, const char* name,
                  std::vector<char>& out, std::vector<char>& err);
  int StartClient(h5ff_sysProcess* client, const char* name);
  void Stop(h5ff_sysProcess* p, const char* name);
  int OutputStringHasError(const char* pname, std::string& output);

  int WaitForLine(h5ff_sysProcess* process, std::string& line, double timeout,
                  std::vector<char>& out, std::vector<char>& err);
  void PrintLine(const char* pname, const char* line);
  int WaitForAndPrintLine(const char* pname, h5ff_sysProcess* process,
                          std::string& line, double timeout,
                          std::vector<char>& out, std::vector<char>& err,
                          int* foundWaiting);

  std::string GetDirectory(std::string location);

private:
  std::string ClientExecutable;  // fullpath to paraview executable
  std::string ServerExecutable;  // fullpath to paraview server executable
  std::string MPIRun;  // fullpath to mpirun executable

  // This specify the preflags and post flags that can be set using:
  // VTK_MPI_PRENUMPROC_FLAGS VTK_MPI_PREFLAGS / VTK_MPI_POSTFLAGS at config time
  std::vector<std::string> MPIPreNumProcFlags;
  std::vector<std::string> MPIPreFlags;
  std::vector<std::string> MPIPostFlags;
  
  // Specify the number of process flag, this can be set using: VTK_MPI_NUMPROC_FLAG. 
  // This is then split into : 
  // MPIServerNumProcessFlag & MPIRenderServerNumProcessFlag
  std::string MPINumProcessFlag;
  std::string MPIServerNumProcessFlag;
  std::string MPIClientNumProcessFlag;

  std::string CurrentPrintLineName;

  double TimeOut;
  double ServerExitTimeOut; // time to wait for servers to finish.
  int TestServer;

  int ArgStart;
  int AllowErrorInOutput;
};

#endif //H5FF_TEST_DRIVER_H

