#include <driver/h5vl_test_driver.hxx>

#include "h5vl_test_config.h"

#include <cstdio>
#include <sstream>
#include <iostream>
#include <cstring>
#include <cstdlib>

#if !defined(_WIN32) || defined(__CYGWIN__)
# include <unistd.h>
# include <sys/wait.h>
#endif

#include <h5vl_test_sys/RegularExpression.hxx>
#include <h5vl_test_sys/SystemTools.hxx>

using std::vector;
using std::string;
using std::cerr;

// The main function as this class should only be used by this program
int
main(int argc, char *argv[])
{
    H5VLTestDriver d;
    return d.Main(argc, argv);
}

//----------------------------------------------------------------------------
H5VLTestDriver::H5VLTestDriver()
{
    this->ClientArgStart = 0;
    this->ClientArgCount = 0;
    this->ClientHelperArgStart = 0;
    this->ClientHelperArgCount = 0;
    this->ClientInitArgStart = 0;
    this->ClientInitArgCount = 0;
    this->ServerArgStart = 0;
    this->ServerArgCount = 0;
    this->AllowErrorInOutput = false;
    // try to make sure that this times out before dart so it can kill all the processes
    this->TimeOut = DART_TESTING_TIMEOUT - 10.0;
    this->ServerExitTimeOut = 2; /* 2 seconds timeout for server to exit */
    this->ClientHelper = false;
    this->ClientInit = false;
    this->TestServer = false;
    this->TestSerial = false;
    this->IgnoreServerResult = false;
}

//----------------------------------------------------------------------------
H5VLTestDriver::~H5VLTestDriver()
{
}

//----------------------------------------------------------------------------
void
H5VLTestDriver::SeparateArguments(const char *str, vector<string> &flags)
{
    string arg = str;
    string::size_type pos1 = 0;
    string::size_type pos2 = arg.find_first_of(" ;");
    if (pos2 == arg.npos) {
        flags.push_back(str);
        return;
    }
    while (pos2 != arg.npos) {
        flags.push_back(arg.substr(pos1, pos2 - pos1));
        pos1 = pos2 + 1;
        pos2 = arg.find_first_of(" ;", pos1 + 1);
    }
    flags.push_back(arg.substr(pos1, pos2 - pos1));
}

//----------------------------------------------------------------------------
void
H5VLTestDriver::CollectConfiguredOptions()
{
    if (this->TimeOut < 0)
        this->TimeOut = 1500;

#ifdef H5VL_TEST_ENV_VARS
    this->SeparateArguments(H5VL_TEST_ENV_VARS, this->ClientEnvVars);
#endif

    // now find all the mpi information if mpi run is set
#ifdef MPIEXEC_EXECUTABLE
    this->MPIRun = MPIEXEC_EXECUTABLE;
#else
    return;
#endif
    int maxNumProc = 1;

# ifdef MPIEXEC_MAX_NUMPROCS
    if (!this->TestSerial)
        maxNumProc = MPIEXEC_MAX_NUMPROCS;
# endif
# ifdef MPIEXEC_NUMPROC_FLAG
    this->MPINumProcessFlag = MPIEXEC_NUMPROC_FLAG;
# endif
# ifdef MPIEXEC_PREFLAGS
    this->SeparateArguments(MPIEXEC_PREFLAGS, this->MPIClientPreFlags);
# endif
# ifdef MPIEXEC_POSTFLAGS
    this->SeparateArguments(MPIEXEC_POSTFLAGS, this->MPIClientPostFlags);
# endif
# ifdef MPIEXEC_SERVER_PREFLAGS
    this->SeparateArguments(MPIEXEC_SERVER_PREFLAGS, this->MPIServerPreFlags);
#else
    this->MPIServerPreFlags = this->MPIClientPreFlags;
# endif
# ifdef MPIEXEC_SERVER_POSTFLAGS
    this->SeparateArguments(MPIEXEC_SERVER_POSTFLAGS, this->MPIServerPostFlags);
#else
    this->MPIServerPostFlags = this->MPIClientPostFlags;
# endif
    std::stringstream ss;
    ss << maxNumProc;
    this->MPIServerNumProcessFlag = "1";
    this->MPIClientNumProcessFlag = ss.str();
}

//----------------------------------------------------------------------------
/// This adds the debug/build configuration crap for the executable on windows.
static string
FixExecutablePath(const string &path)
{
#ifdef  CMAKE_INTDIR
    string parent_dir =
    h5vl_test_sys::SystemTools::GetFilenamePath(path.c_str());

    string filename =
    h5vl_test_sys::SystemTools::GetFilenameName(path);

    if (!h5vl_test_sys::SystemTools::StringEndsWith(parent_dir.c_str(), CMAKE_INTDIR)) {
        parent_dir += "/" CMAKE_INTDIR;
    }
    return parent_dir + "/" + filename;
#endif

    return path;
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::ProcessCommandLine(int argc, char *argv[])
{
    int *ArgCountP = NULL;
    int i;
    for (i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "--client") == 0) {
            this->ClientExecutable = ::FixExecutablePath(argv[i + 1]);
            ++i; /* Skip executable */
            this->ClientArgStart = i + 1;
            this->ClientArgCount = this->ClientArgStart;
            ArgCountP = &this->ClientArgCount;
            continue;
        }
        if (strcmp(argv[i], "--client-helper") == 0) {
            std::cerr << "Client Helper" << std::endl;
            this->ClientHelper = true;
            this->ClientHelperExecutable = ::FixExecutablePath(argv[i + 1]);
            ++i; /* Skip executable */
            this->ClientHelperArgStart = i + 1;
            this->ClientHelperArgCount = this->ClientHelperArgStart;
            ArgCountP = &this->ClientHelperArgCount;
            continue;
        }
        if (strcmp(argv[i], "--client-init") == 0) {
            std::cerr << "Client Init" << std::endl;
            this->ClientInit = true;
            this->ClientInitExecutable = ::FixExecutablePath(argv[i + 1]);
            ++i; /* Skip executable */
            this->ClientInitArgStart = i + 1;
            this->ClientInitArgCount = this->ClientInitArgStart;
            ArgCountP = &this->ClientInitArgCount;
            continue;
        }
        if (strcmp(argv[i], "--server") == 0) {
            std::cerr << "Test Server" << std::endl;
            this->TestServer = true;
            this->ServerExecutable = ::FixExecutablePath(argv[i + 1]);
            ++i; /* Skip executable */
            this->ServerArgStart = i + 1;
            this->ServerArgCount = this->ServerArgStart;
            ArgCountP = &this->ServerArgCount;
            continue;
        }
        if (strcmp(argv[i], "--timeout") == 0) {
            this->TimeOut = atoi(argv[i + 1]);
            std::cerr << "The timeout was set to " << this->TimeOut << std::endl;
            ArgCountP = NULL;
            continue;
        }
        if (strncmp(argv[i], "--allow-errors", strlen("--allow-errors")) == 0) {
            this->AllowErrorInOutput = true;
            std::cerr << "The allow errors in output flag was set to " <<
                this->AllowErrorInOutput << std::endl;
            ArgCountP = NULL;
            continue;
        }
        if (strncmp(argv[i], "--allow-server-errors", strlen("--allow-server-errors")) == 0) {
            this->IgnoreServerResult = true;
            std::cerr << "The allow server errors in output flag was set to " <<
                this->IgnoreServerResult << std::endl;
            ArgCountP = NULL;
            continue;
        }
        if (strcmp(argv[i], "--serial") == 0) {
            this->TestSerial = true;
            std::cerr << "This is a serial test" << std::endl;
            ArgCountP = NULL;
            continue;
        }
        if (ArgCountP)
            (*ArgCountP)++;
    }

    return 1;
}

//----------------------------------------------------------------------------
void
H5VLTestDriver::CreateCommandLine(vector<const char*> &commandLine,
    const char *cmd, int isServer, int isHelper, const char *numProc, int argStart,
    int argCount, char *argv[])
{
    if (!isServer && this->ClientEnvVars.size()) {
        for (unsigned int i = 0; i < this->ClientEnvVars.size(); ++i)
            commandLine.push_back(this->ClientEnvVars[i].c_str());
#ifdef H5VL_TEST_CLIENT_INIT_TOKEN_VAR
        if (this->ClientTokenVar.size())
            commandLine.push_back(this->ClientTokenVar.c_str());
#endif
    }

    if (!isHelper && this->MPIRun.size()) {
        commandLine.push_back(this->MPIRun.c_str());
        commandLine.push_back(this->MPINumProcessFlag.c_str());
        commandLine.push_back(numProc);

        if (isServer)
            for (unsigned int i = 0; i < this->MPIServerPreFlags.size(); ++i)
                commandLine.push_back(this->MPIServerPreFlags[i].c_str());
        else
            for (unsigned int i = 0; i < this->MPIClientPreFlags.size(); ++i)
                commandLine.push_back(this->MPIClientPreFlags[i].c_str());
    }

    commandLine.push_back(cmd);

    if (isServer)
        for (unsigned int i = 0; i < this->MPIServerPostFlags.size(); ++i)
            commandLine.push_back(MPIServerPostFlags[i].c_str());
    else
        for (unsigned int i = 0; i < this->MPIClientPostFlags.size(); ++i)
            commandLine.push_back(MPIClientPostFlags[i].c_str());

    // remaining flags for the test
    for (int ii = argStart; ii < argCount; ++ii) {
        commandLine.push_back(argv[ii]);
    }

    commandLine.push_back(0);
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::StartServer(h5vl_test_sysProcess *server, const char *name,
    vector<char> &out, vector<char> &err)
{
    if (!server)
        return 1;

    cerr << "H5VLTestDriver: starting process " << name << "\n";
    h5vl_test_sysProcess_SetTimeout(server, this->TimeOut);
    h5vl_test_sysProcess_Execute(server);
    int foundWaiting = 0;
    string output;
    while (!foundWaiting) {
        int pipe = this->WaitForAndPrintLine(name, server, output, 100.0, out,
            err, H5VL_TEST_SERVER_START_MSG, &foundWaiting);
        if (pipe == h5vl_test_sysProcess_Pipe_None
            || pipe == h5vl_test_sysProcess_Pipe_Timeout) {
            break;
        }
    }
    if (foundWaiting) {
        cerr << "H5VLTestDriver: " << name << " successfully started.\n";
        return 1;
    } else {
        cerr << "H5VLTestDriver: " << name << " never started.\n";
        h5vl_test_sysProcess_Kill(server);
        return 0;
    }
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::StartClientHelper(h5vl_test_sysProcess *client,
    const char *name, vector<char> &out, vector<char> &err)
{
    if (!client)
        return 1;

    cerr << "H5VLTestDriver: starting process " << name << "\n";
    h5vl_test_sysProcess_SetTimeout(client, this->TimeOut);
    h5vl_test_sysProcess_Execute(client);
    int foundWaiting = 0;
    string output;
    while (!foundWaiting) {
        int pipe = this->WaitForAndPrintLine(name, client, output, 100.0, out,
            err, H5VL_TEST_CLIENT_HELPER_START_MSG, &foundWaiting);
        if (pipe == h5vl_test_sysProcess_Pipe_None
            || pipe == h5vl_test_sysProcess_Pipe_Timeout) {
            break;
        }
    }
    if (foundWaiting) {
        cerr << "H5VLTestDriver: " << name << " successfully started.\n";
        return 1;
    } else {
        cerr << "H5VLTestDriver: " << name << " never started.\n";
        h5vl_test_sysProcess_Kill(client);
        return 0;
    }
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::StartClientInit(h5vl_test_sysProcess *client,
    const char *name, vector<char> &out, vector<char> &err)
{
    if (!client)
        return 1;

    cerr << "H5VLTestDriver: starting process " << name << "\n";
    h5vl_test_sysProcess_SetTimeout(client, this->TimeOut);
    h5vl_test_sysProcess_Execute(client);
    int foundToken = 0;
    string output, token;
    while (!foundToken) {
        int pipe = this->WaitForAndPrintLine(name, client, output, 100.0, out,
            err, NULL, NULL);
        if (pipe == h5vl_test_sysProcess_Pipe_None
            || pipe == h5vl_test_sysProcess_Pipe_Timeout) {
            break;
        }
        if (this->OutputStringHasToken(name, H5VL_TEST_CLIENT_INIT_TOKEN_REGEX, output, token)) {
            foundToken = 1;
            this->ClientTokenVar = std::string(H5VL_TEST_CLIENT_INIT_TOKEN_VAR)
                + std::string("=") + std::string(token);
            break;
        }
    }

    if (foundToken) {
        cerr << "H5VLTestDriver: " << name << " token: " << token << " was found.\n";
        return 1;
    } else {
        cerr << "H5VLTestDriver: " << name << " token was not found.\n";
        return 0;
    }
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::StartClient(h5vl_test_sysProcess *client, const char *name)
{
    if (!client)
        return 1;

    cerr << "H5VLTestDriver: starting process " << name << "\n";
    h5vl_test_sysProcess_SetTimeout(client, this->TimeOut);
    h5vl_test_sysProcess_Execute(client);
    if (h5vl_test_sysProcess_GetState(client)
        == h5vl_test_sysProcess_State_Executing) {
        cerr << "H5VLTestDriver: " << name << " successfully started.\n";
        return 1;
    } else {
        this->ReportStatus(client, name);
        h5vl_test_sysProcess_Kill(client);
        return 0;
    }
}

//----------------------------------------------------------------------------
void
H5VLTestDriver::Stop(h5vl_test_sysProcess *p, const char *name)
{
    if (p) {
        cerr << "H5VLTestDriver: killing process " << name << "\n";
        h5vl_test_sysProcess_Kill(p);
        h5vl_test_sysProcess_WaitForExit(p, 0);
    }
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::OutputStringHasError(const char *pname, string &output)
{
    const char* possibleMPIErrors[] = {"error", "Error", "Missing:",
        "core dumped", "process in local group is dead", "Segmentation fault",
        "erroneous", "ERROR:", "Error:",
        "mpirun can *only* be used with MPI programs", "due to signal",
        "failure", "abnormal termination", "failed", "FAILED", "Failed", 0};

    const char* nonErrors[] = {
        "Memcheck, a memory error detector",  //valgrind
        0};

    if (this->AllowErrorInOutput)
        return 0;

    vector<string> lines;
    vector<string>::iterator it;
    h5vl_test_sys::SystemTools::Split(output.c_str(), lines);

    int i, j;

    for (it = lines.begin(); it != lines.end(); ++it) {
        for (i = 0; possibleMPIErrors[i]; ++i) {
            if (it->find(possibleMPIErrors[i]) != it->npos) {
                int found = 1;
                for (j = 0; nonErrors[j]; ++j) {
                    if (it->find(nonErrors[j]) != it->npos) {
                        found = 0;
                        cerr << "Non error \"" << it->c_str()
                            << "\" suppressed " << std::endl;
                    }
                }
                if (found) {
                    cerr
                        << "H5VLTestDriver: ***** Test will fail, because the string: \""
                        << possibleMPIErrors[i]
                        << "\"\nH5VLTestDriver: ***** was found in the following output from the "
                        << pname << ":\n\"" << it->c_str() << "\"\n";
                    return 1;
                }
            }
        }
    }
    return 0;
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::OutputStringHasToken(const char *pname, const char *regex,
    string &output, string &token)
{
    vector<string> lines;
    vector<string>::iterator it;
    h5vl_test_sys::SystemTools::Split(output.c_str(), lines);
    h5vl_test_sys::RegularExpression re(regex);

    for (it = lines.begin(); it != lines.end(); ++it) {
        if (re.find(*it)) {
            token = re.match(1);
            return 1;
        }
    }

    return 0;
}

//----------------------------------------------------------------------------
#define H5VL_CLEAN_PROCESSES do {      \
  h5vl_test_sysProcess_Delete(client); \
  h5vl_test_sysProcess_Delete(client_helper); \
  h5vl_test_sysProcess_Delete(client_init); \
  h5vl_test_sysProcess_Delete(server); \
} while (0)

#define H5VL_EXECUTE_CMD(cmd) do {                              \
    if (strlen(cmd) > 0) {                                      \
        std::vector<std::string> commands =                     \
            h5vl_test_sys::SystemTools::SplitString(cmd, ';');  \
        for (unsigned int cc = 0; cc < commands.size(); cc++) { \
            std::string command = commands[cc];                 \
            if (command.size() > 0) {                           \
                std::cout << command.c_str() << std::endl;      \
                system(command.c_str());                        \
            }                                                   \
        }                                                       \
    }                                                           \
} while (0)

//----------------------------------------------------------------------------
int
H5VLTestDriver::Main(int argc, char* argv[])
{
#ifdef H5VL_TEST_INIT_COMMAND
    // run user-specified commands before initialization.
    // For example: "killall -9 rsh test;"
    H5VL_EXECUTE_CMD(H5VL_TEST_INIT_COMMAND);
#endif

    if (!this->ProcessCommandLine(argc, argv))
        return 1;
    this->CollectConfiguredOptions();

    // mpi code
    // Allocate process managers.
    h5vl_test_sysProcess *server = 0;
    h5vl_test_sysProcess *client = 0;
    h5vl_test_sysProcess *client_helper = 0;
    h5vl_test_sysProcess *client_init = 0;

    if (this->TestServer) {
        server = h5vl_test_sysProcess_New();
        if (!server) {
            H5VL_CLEAN_PROCESSES;
            cerr << "H5VLTestDriver: Cannot allocate h5vl_test_sysProcess to "
                "run the server.\n";
            return 1;
        }
    }
    if (this->ClientHelper) {
        client_helper = h5vl_test_sysProcess_New();
        if (!client_helper) {
            H5VL_CLEAN_PROCESSES;
            cerr << "H5VLTestDriver: Cannot allocate h5vl_test_sysProcess to "
                "run the client helper.\n";
            return 1;
        }
    }
    if (this->ClientInit) {
        client_init = h5vl_test_sysProcess_New();
        if (!client_init) {
            H5VL_CLEAN_PROCESSES;
            cerr << "H5VLTestDriver: Cannot allocate h5vl_test_sysProcess to "
                "run the client init.\n";
            return 1;
        }
    }
    client = h5vl_test_sysProcess_New();
    if (!client) {
        H5VL_CLEAN_PROCESSES;
        cerr << "H5VLTestDriver: Cannot allocate h5vl_test_sysProcess to "
            "run the client.\n";
        return 1;
    }

    vector<char> ClientStdOut;
    vector<char> ClientStdErr;
    vector<char> ClientHelperStdOut;
    vector<char> ClientHelperStdErr;
    vector<char> ClientInitStdOut;
    vector<char> ClientInitStdErr;
    vector<char> ServerStdOut;
    vector<char> ServerStdErr;

    vector<const char *> serverCommand;
    if (server) {
        const char* serverExe = this->ServerExecutable.c_str();

        this->CreateCommandLine(serverCommand, serverExe, 1, 0,
            this->MPIServerNumProcessFlag.c_str(), this->ServerArgStart,
            this->ServerArgCount, argv);
        this->ReportCommand(&serverCommand[0], "server");
        h5vl_test_sysProcess_SetCommand(server, &serverCommand[0]);
        h5vl_test_sysProcess_SetWorkingDirectory(server,
            this->GetDirectory(serverExe).c_str());
    }

    vector<const char *> clientHelperCommand;
    if (client_helper) {
        // Construct the client helper process command line.
        const char *clientHelperExe = this->ClientHelperExecutable.c_str();
        this->CreateCommandLine(clientHelperCommand, clientHelperExe, 0, 1,
            "1", this->ClientHelperArgStart,
            this->ClientHelperArgCount, argv);
        this->ReportCommand(&clientHelperCommand[0], "client_helper");
        h5vl_test_sysProcess_SetCommand(client_helper, &clientHelperCommand[0]);
        h5vl_test_sysProcess_SetWorkingDirectory(client_helper,
            this->GetDirectory(clientHelperExe).c_str());
    }

    vector<const char *> clientInitCommand;
    if (client_init) {
        // Construct the client helper process command line.
        const char *clientInitExe = this->ClientInitExecutable.c_str();
        this->CreateCommandLine(clientInitCommand, clientInitExe, 0, 1,
            "1", this->ClientInitArgStart, this->ClientInitArgCount, argv);
        this->ReportCommand(&clientInitCommand[0], "client_init");
        h5vl_test_sysProcess_SetCommand(client_init, &clientInitCommand[0]);
        h5vl_test_sysProcess_SetWorkingDirectory(client_init,
            this->GetDirectory(clientInitExe).c_str());
    }

    // Start the server if there is one
    if (!this->StartServer(server, "server", ServerStdOut, ServerStdErr)) {
        cerr << "H5VLTestDriver: Server never started.\n";
        H5VL_CLEAN_PROCESSES;
        return -1;
    }

    // Start the client helper here if there is one
    if (!this->StartClientHelper(client_helper, "client_helper",
        ClientHelperStdOut, ClientHelperStdErr)) {
        cerr << "H5VLTestDriver: Client Helper never started.\n";
        this->Stop(server, "server");
#ifdef H5VL_TEST_SERVER_EXIT_COMMAND
        H5VL_EXECUTE_CMD(H5VL_TEST_SERVER_EXIT_COMMAND);
#endif
        H5VL_CLEAN_PROCESSES;
        return -1;
    }

    // Start the client init here if there is one
    if (!this->StartClientInit(client_init, "client_init",
        ClientInitStdOut, ClientInitStdErr)) {
        cerr << "H5VLTestDriver: Client Init never started.\n";
        this->Stop(server, "server");
#ifdef H5VL_TEST_SERVER_EXIT_COMMAND
        H5VL_EXECUTE_CMD(H5VL_TEST_SERVER_EXIT_COMMAND);
#endif
        this->Stop(client_helper, "client_helper");
#ifdef H5VL_TEST_CLIENT_HELPER_EXIT_COMMAND
        H5VL_EXECUTE_CMD(H5VL_TEST_CLIENT_HELPER_EXIT_COMMAND);
#endif
        H5VL_CLEAN_PROCESSES;
        return -1;
    }

    // Construct the client process command line.
    vector<const char *> clientCommand;
    const char *clientExe = this->ClientExecutable.c_str();
    this->CreateCommandLine(clientCommand, clientExe, 0, 0,
        this->MPIClientNumProcessFlag.c_str(), this->ClientArgStart,
        this->ClientArgCount, argv);
    this->ReportCommand(&clientCommand[0], "client");
    h5vl_test_sysProcess_SetCommand(client, &clientCommand[0]);
    h5vl_test_sysProcess_SetWorkingDirectory(client,
        this->GetDirectory(clientExe).c_str());

    // Now run the client
    if (!this->StartClient(client, "client")) {
        this->Stop(server, "server");
        this->Stop(client_helper, "client_helper");
        this->Stop(client_init, "client_init");
        H5VL_CLEAN_PROCESSES;
        return -1;
    }

    // Report the output of the processes.
    int clientPipe = 1;

    string output;
    int mpiError = 0;
    while (clientPipe) {
        clientPipe = this->WaitForAndPrintLine("client", client, output, 0.1,
            ClientStdOut, ClientStdErr, NULL, NULL);
        if (!mpiError && this->OutputStringHasError("client", output)) {
            mpiError = 1;
        }
        // If client has died, we wait for output from the server processes
        // for this->ServerExitTimeOut, then we'll kill the servers, if needed.
        double timeout = (clientPipe) ? 0 : this->ServerExitTimeOut;
        output = "";
        this->WaitForAndPrintLine("server", server, output, timeout,
            ServerStdOut, ServerStdErr, NULL, NULL);
        if (!mpiError && this->OutputStringHasError("server", output)) {
            mpiError = 1;
        }
        output = "";
    }

    // Wait for the client and server to exit.
    h5vl_test_sysProcess_WaitForExit(client, 0);

    // Once client is finished, the servers
    // must finish quickly. If not, it usually is a sign that
    // the client crashed/exited before it attempted to connect to
    // the server.
    if (server) {
#ifdef H5VL_TEST_SERVER_EXIT_COMMAND
        H5VL_EXECUTE_CMD(H5VL_TEST_SERVER_EXIT_COMMAND);
#endif
        h5vl_test_sysProcess_WaitForExit(server, &this->ServerExitTimeOut);
    }

    if (client_helper) {
#ifdef H5VL_TEST_CLIENT_HELPER_EXIT_COMMAND
        H5VL_EXECUTE_CMD(H5VL_TEST_CLIENT_HELPER_EXIT_COMMAND);
#endif
        h5vl_test_sysProcess_WaitForExit(client_helper, 0);
    }

    // Get the results.
    int clientResult = this->ReportStatus(client, "client");
    int serverResult = 0;
    if (server) {
        serverResult = this->ReportStatus(server, "server");
        h5vl_test_sysProcess_Kill(server);
    }

    // Free process managers.
    H5VL_CLEAN_PROCESSES;

    // Report the server return code if it is nonzero.  Otherwise report
    // the client return code.
    if (serverResult && !this->IgnoreServerResult)
        return serverResult;

    if (mpiError) {
        cerr
            << "H5VLTestDriver: Error string found in output, H5VLTestDriver returning "
            << mpiError << "\n";
        return mpiError;
    }

    // if server is fine return the client result
    return clientResult;
}

//----------------------------------------------------------------------------
void
H5VLTestDriver::ReportCommand(const char * const *command, const char *name)
{
    cerr << "H5VLTestDriver: " << name << " command is:\n";
    for (const char * const *c = command; *c; ++c)
        cerr << " \"" << *c << "\"";
    cerr << "\n";
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::ReportStatus(h5vl_test_sysProcess *process, const char *name)
{
    int result = 1;
    switch (h5vl_test_sysProcess_GetState(process)) {
        case h5vl_test_sysProcess_State_Starting: {
            cerr << "H5VLTestDriver: Never started " << name << " process.\n";
        }
            break;
        case h5vl_test_sysProcess_State_Error: {
            cerr << "H5VLTestDriver: Error executing " << name << " process: "
                << h5vl_test_sysProcess_GetErrorString(process) << "\n";
        }
            break;
        case h5vl_test_sysProcess_State_Exception: {
            cerr << "H5VLTestDriver: " << name
                << " process exited with an exception: ";
            switch (h5vl_test_sysProcess_GetExitException(process)) {
                case h5vl_test_sysProcess_Exception_None: {
                    cerr << "None";
                }
                    break;
                case h5vl_test_sysProcess_Exception_Fault: {
                    cerr << "Segmentation fault";
                }
                    break;
                case h5vl_test_sysProcess_Exception_Illegal: {
                    cerr << "Illegal instruction";
                }
                    break;
                case h5vl_test_sysProcess_Exception_Interrupt: {
                    cerr << "Interrupted by user";
                }
                    break;
                case h5vl_test_sysProcess_Exception_Numerical: {
                    cerr << "Numerical exception";
                }
                    break;
                case h5vl_test_sysProcess_Exception_Other: {
                    cerr << "Unknown";
                }
                    break;
            }
            cerr << "\n";
        }
            break;
        case h5vl_test_sysProcess_State_Executing: {
            cerr << "H5VLTestDriver: Never terminated " << name
                << " process.\n";
        }
            break;
        case h5vl_test_sysProcess_State_Exited: {
            result = h5vl_test_sysProcess_GetExitValue(process);
            cerr << "H5VLTestDriver: " << name << " process exited with code "
                << result << "\n";
        }
            break;
        case h5vl_test_sysProcess_State_Expired: {
            cerr << "H5VLTestDriver: killed " << name
                << " process due to timeout.\n";
        }
            break;
        case h5vl_test_sysProcess_State_Killed: {
            cerr << "H5VLTestDriver: killed " << name << " process.\n";
        }
            break;
    }
    return result;
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::WaitForLine(h5vl_test_sysProcess *process, string &line,
    double timeout, vector<char> &out, vector<char> &err)
{
    line = "";
    vector<char>::iterator outiter = out.begin();
    vector<char>::iterator erriter = err.begin();
    while (1) {
        // Check for a newline in stdout.
        for (; outiter != out.end(); ++outiter) {
            if ((*outiter == '\r') && ((outiter + 1) == out.end())) {
                break;
            } else if (*outiter == '\n' || *outiter == '\0') {
                int length = outiter - out.begin();
                if (length > 1 && *(outiter - 1) == '\r')
                    --length;
                if (length > 0)
                    line.append(&out[0], length);
                out.erase(out.begin(), outiter + 1);
                return h5vl_test_sysProcess_Pipe_STDOUT;
            }
        }

        // Check for a newline in stderr.
        for (; erriter != err.end(); ++erriter) {
            if ((*erriter == '\r') && ((erriter + 1) == err.end())) {
                break;
            } else if (*erriter == '\n' || *erriter == '\0') {
                int length = erriter - err.begin();
                if (length > 1 && *(erriter - 1) == '\r')
                    --length;
                if (length > 0)
                    line.append(&err[0], length);
                err.erase(err.begin(), erriter + 1);
                return h5vl_test_sysProcess_Pipe_STDERR;
            }
        }

        // No newlines found.  Wait for more data from the process.
        int length;
        char *data;
        int pipe = h5vl_test_sysProcess_WaitForData(process, &data, &length,
            &timeout);
        if (pipe == h5vl_test_sysProcess_Pipe_Timeout) {
            // Timeout has been exceeded.
            return pipe;
        } else if (pipe == h5vl_test_sysProcess_Pipe_STDOUT) {
            // Append to the stdout buffer.
            vector<char>::size_type size = out.size();
            out.insert(out.end(), data, data + length);
            outiter = out.begin() + size;
        } else if (pipe == h5vl_test_sysProcess_Pipe_STDERR) {
            // Append to the stderr buffer.
            vector<char>::size_type size = err.size();
            err.insert(err.end(), data, data + length);
            erriter = err.begin() + size;
        } else if (pipe == h5vl_test_sysProcess_Pipe_None) {
            // Both stdout and stderr pipes have broken.  Return leftover data.
            if (!out.empty()) {
                line.append(&out[0], outiter - out.begin());
                out.erase(out.begin(), out.end());
                return h5vl_test_sysProcess_Pipe_STDOUT;
            } else if (!err.empty()) {
                line.append(&err[0], erriter - err.begin());
                err.erase(err.begin(), err.end());
                return h5vl_test_sysProcess_Pipe_STDERR;
            } else {
                return h5vl_test_sysProcess_Pipe_None;
            }
        }
    }
}

//----------------------------------------------------------------------------
void
H5VLTestDriver::PrintLine(const char *pname, const char *line)
{
    // if the name changed then the line is output from a different process
    if (this->CurrentPrintLineName != pname) {
        cerr << "-------------- " << pname << " output --------------\n";
        // save the current pname
        this->CurrentPrintLineName = pname;
    }
    cerr << line << "\n";
    cerr.flush();
}

//----------------------------------------------------------------------------
int
H5VLTestDriver::WaitForAndPrintLine(const char *pname,
    h5vl_test_sysProcess *process, string &line, double timeout,
    vector<char> &out, vector<char> &err, const char *waitMsg,
    int *foundWaiting)
{
    int pipe = this->WaitForLine(process, line, timeout, out, err);
    if (pipe == h5vl_test_sysProcess_Pipe_STDOUT
        || pipe == h5vl_test_sysProcess_Pipe_STDERR) {
        this->PrintLine(pname, line.c_str());
        if (foundWaiting && (line.find(waitMsg) != line.npos))
            *foundWaiting = 1;
    }
    return pipe;
}

//----------------------------------------------------------------------------
string
H5VLTestDriver::GetDirectory(string location)
{
    return h5vl_test_sys::SystemTools::GetParentDirectory(location.c_str());
}
