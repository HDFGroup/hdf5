/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5_API_TEST_DRIVER_H
#define H5_API_TEST_DRIVER_H

#include <string>
#include <vector>

#include <h5_api_test_sys/Process.h>

class H5APITestDriver {
public:
    int Main(int argc, char *argv[]);
    H5APITestDriver();
    ~H5APITestDriver();

protected:
    void SeparateArguments(const char* str, std::vector<std::string> &flags);

    void ReportCommand(const char * const *command, const char *name);
    int  ReportStatus(h5_api_test_sysProcess *process, const char *name);
    int  ProcessCommandLine(int argc, char *argv[]);
    void CollectConfiguredOptions();
    void CreateCommandLine(std::vector<const char *> &commandLine,
        const char *cmd, int isServer, int isHelper, const char *numProc,
        int argStart = 0, int argCount = 0, char *argv[] = 0);

    int StartServer(h5_api_test_sysProcess *server, const char *name,
        std::vector<char> &out, std::vector<char> &err);
    int StartClientHelper(h5_api_test_sysProcess *client, const char *name,
        std::vector<char> &out, std::vector<char> &err);
    int StartClientInit(h5_api_test_sysProcess *client, const char *name,
        std::vector<char> &out, std::vector<char> &err);
    int StartClient(h5_api_test_sysProcess *client, const char *name);
    void Stop(h5_api_test_sysProcess *p, const char *name);
    int OutputStringHasError(const char *pname, std::string &output);
    int OutputStringHasToken(const char *pname, const char *regex,
        std::string &output, std::string &token);

    int WaitForLine(h5_api_test_sysProcess *process, std::string &line,
        double timeout, std::vector<char> &out, std::vector<char> &err);
    void PrintLine(const char *pname, const char *line);
    int WaitForAndPrintLine(const char *pname, h5_api_test_sysProcess *process,
        std::string &line, double timeout, std::vector<char> &out,
        std::vector<char> &err, const char *waitMsg, int *foundWaiting);

    std::string GetDirectory(std::string location);

private:
    std::string ClientExecutable;       // fullpath to client executable
    std::string ClientHelperExecutable; // fullpath to client helper executable
    std::string ClientInitExecutable;   // fullpath to client init executable
    std::string ServerExecutable;       // fullpath to server executable
    std::string MPIRun;                 // fullpath to mpirun executable

    // This specify the preflags and post flags that can be set using:
    // VTK_MPI_PRENUMPROC_FLAGS VTK_MPI_PREFLAGS / VTK_MPI_POSTFLAGS at config time
    // std::vector<std::string> MPIPreNumProcFlags;
    std::vector<std::string> ClientEnvVars;
    std::vector<std::string> MPIClientPreFlags;
    std::vector<std::string> MPIClientPostFlags;
    std::vector<std::string> MPIServerPreFlags;
    std::vector<std::string> MPIServerPostFlags;

    // Specify the number of process flag, this can be set using: VTK_MPI_NUMPROC_FLAG.
    // This is then split into :
    // MPIServerNumProcessFlag & MPIRenderServerNumProcessFlag
    std::string MPINumProcessFlag;
    std::string MPIServerNumProcessFlag;
    std::string MPIClientNumProcessFlag;

    std::string ClientTokenVar;  // use token to launch client if requested

    std::string CurrentPrintLineName;

    double TimeOut;
    double ServerExitTimeOut;   // time to wait for servers to finish.
    bool ClientHelper;
    bool ClientInit;
    bool TestServer;

    int ClientArgStart;
    int ClientArgCount;
    int ClientHelperArgStart;
    int ClientHelperArgCount;
    int ClientInitArgStart;
    int ClientInitArgCount;
    int ServerArgStart;
    int ServerArgCount;
    bool AllowErrorInOutput;
    bool TestSerial;
    bool IgnoreServerResult;
};

#endif //H5_API_TEST_DRIVER_H
