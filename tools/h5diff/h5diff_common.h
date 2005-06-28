extern unsigned char g_Parallel;
extern int g_nTasks;

void usage(void);
int check_n_input( const char* );
int check_f_input( const char* );
void parse_input(int argc, const char* argv[], const char** fname1, const char** fname2, const char** objname1, const char** objname2, diff_opt_t* options);
void h5diff_exit(int status);
void print_results(hsize_t nfound, diff_opt_t* options);

