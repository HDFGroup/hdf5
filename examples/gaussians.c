#include <assert.h>
#include <curses.h>
#include <err.h>
#include <errno.h>
#include <libgen.h> /* basename(3) */
#include <math.h>   /* expf(3) */
#include <locale.h> /* setlocale(3) */
#include <signal.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>   /* struct timespec, nanosleep(2), time(3),
                     * clock_gettime(2)
                     */
#include <unistd.h> /* getopt, PATH_MAX, ... */

#include <sys/param.h> /* for MIN(a, b) */

#include "hdf5.h"
#include "nbcompat.h"

#define SWMR_TICK_LEN 4 /* in 100 ms */

typedef enum { STANDALONE = 0, READ = 1, WRITE = 2 } personality_t;

typedef enum { TOP = 0, BOTTOM, LEFT, RIGHT, NSIDES } side_t;

typedef struct {
    bool side[NSIDES];
} inside_t;

typedef struct {
    float x, y;
} vec_t;

#define RANK 3
#define ROWS 20
#define COLS 40
static const hsize_t         original_dims[RANK] = {0, ROWS, COLS};
static const hsize_t         max_dims[RANK]      = {H5S_UNLIMITED, ROWS, COLS};
static const hsize_t         frame_dims[RANK]    = {1, ROWS, COLS};
static const hsize_t *       chunk_dims          = frame_dims;
static volatile sig_atomic_t unbroken            = 1;

typedef struct {
    /* main-loop statistics */
    uint64_t        max_elapsed_ns, min_elapsed_ns, total_elapsed_ns;
    uint64_t        total_loops;
    hid_t           dataset, memspace, dcpl, file, group;
    char            output_file[PATH_MAX];
    char            progname[PATH_MAX];
    struct timespec update_interval;
    bool            fuzz;
    bool            constantrate;
    unsigned int    partstep;
} state_t;

#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .total_elapsed_ns = 0, .total_loops = 0, .min_elapsed_ns = UINT64_MAX, .max_elapsed_ns = 0,          \
        .memspace = H5I_INVALID_HID, .file = H5I_INVALID_HID, .constantrate = false, .partstep = 0,          \
        .output_file = "", .update_interval = (struct timespec)                                              \
        {                                                                                                    \
            .tv_sec = 0, .tv_nsec = 1000000000UL / 30 /* 1/30 second */                                      \
        }                                                                                                    \
    }

static void state_init(state_t *, int, char **);

static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: %s [-u milliseconds]\n"
            "\n"
            "-c:	increase the frame number continously (reader mode)\n"
            "-f:	add \"fuzz\" (linear noise) to the data (writer mode)\n"
            "-u ms: milliseconds interval between updates to %s.h5\n"
            "\n",
            progname, progname);
    exit(EXIT_FAILURE);
}

static void
state_init(state_t *s, int argc, char **argv)
{
    int           ch;
    char          tfile[PATH_MAX];
    char *        end;
    unsigned long millis;

    *s = ALL_HID_INITIALIZER;
    strlcpy(tfile, argv[0], sizeof(tfile));
    strlcpy(s->progname, basename(tfile), sizeof(s->progname));

    while ((ch = getopt(argc, argv, "cfu:")) != -1) {
        switch (ch) {
            case 'c':
                s->constantrate = true;
                break;
            case 'f':
                s->fuzz = true;
                break;
            case 'u':
                errno  = 0;
                millis = strtoul(optarg, &end, 0);
                if (millis == ULONG_MAX && errno == ERANGE) {
                    err(EXIT_FAILURE, "option -p argument \"%s\"", optarg);
                }
                else if (*end != '\0') {
                    errx(EXIT_FAILURE, "garbage after -p argument \"%s\"", optarg);
                }
                s->update_interval.tv_sec  = millis / 1000UL;
                s->update_interval.tv_nsec = (millis * 1000000UL) % 1000000000UL;
                warnx("%lu milliseconds between updates", millis);
                break;
            case '?':
            default:
                usage(s->progname);
        }
    }
    argc -= optind;
    argv += optind;

    s->dataset = H5I_INVALID_HID;
    s->group   = H5I_INVALID_HID;

    s->memspace = H5Screate_simple(RANK, frame_dims, NULL);

    if (s->memspace < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
    }

    const int nprinted = snprintf(s->output_file, sizeof(s->output_file), "%s.h5", s->progname);
    if (nprinted < 0 || nprinted >= sizeof(s->output_file)) {
        errx(EXIT_FAILURE, "%s.%d: output filename truncated.", __func__, __LINE__);
    }
}

static void
matrix_read(state_t *s, int *framenop, float mat[ROWS][COLS])
{
    hid_t   ds, filespace;
    herr_t  status;
    hsize_t dims[RANK];

    ds = s->dataset;

    if (H5Drefresh(ds) < 0)
        errx(EXIT_FAILURE, "H5Drefresh failed");

    filespace = H5Dget_space(ds);

    if (H5Sget_simple_extent_dims(filespace, dims, NULL) < 0)
        errx(EXIT_FAILURE, "H5Sget_simple_extent_dims failed");

    if (dims[1] != original_dims[1] || dims[2] != original_dims[2]) {
        errx(EXIT_FAILURE, "Unexpected dimensions N x %ju x %ju", (uintmax_t)dims[1], (uintmax_t)dims[2]);
    }

    const uint64_t update_ms        = timespec2ns(&s->update_interval) / 1000000;
    const uint32_t tick_ms          = SWMR_TICK_LEN * 100;
    const uint64_t updates_per_tick = (tick_ms + update_ms - 1) / update_ms;
    const hssize_t hang_back        = 2 * updates_per_tick;
    const int      frameno          = *framenop;

    const int lead = frameno + hang_back - dims[0];
    if (s->constantrate) {
        *framenop = frameno + 1;
    }
    else if (lead > hang_back * 2) {
        if (++s->partstep % 3 == 0)
            *framenop = frameno + 1;
    }
    else if (lead > 0) {
        if (++s->partstep % 2 == 0)
            *framenop = frameno + 1;
    }
    else if (lead == 0) {
        *framenop = frameno + 1;
    }
    else if (lead < -hang_back * 2) {
        /* We're way behind, so jump close to the front. */
        *framenop = dims[0] - hang_back;
    }
    else /* lead < 0 */ {
        *framenop = frameno + 1;
        if (++s->partstep % 2 == 0)
            *framenop = frameno + 2;
    }

#if 0
	if (!s->constantrate && (lead < -2 || 2 < lead)) {
		int gain = 31250 / 4;
		const struct timespec prior_integral = s->update_integral;
		struct timespec current_interval;
		if (lead > 0)
			gain *= 2;
		struct timespec adjustment = (struct timespec){.tv_sec = 0,
			.tv_nsec = gain * MAX(MIN(4, lead), -4)};
		/* XXX clamp it XXX */
		timespecadd(&s->update_integral,
		    &adjustment, &s->update_integral);
		timespecadd(&s->update_integral,
		    &s->update_interval, &current_interval);
		if (timespeccmp(&current_interval, &s->min_interval, <=))
			s->update_integral = prior_integral;
	}
#endif

    if (frameno >= dims[0]) {
        int i, j;
        for (i = 0; i < ROWS; i++) {
            for (j = 0; j < COLS; j++)
                mat[i][j] = ((i + j) % 2 == 0) ? 0. : 1.;
        }
        return;
    }

    hsize_t offset[RANK] = {frameno, 0, 0};

    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, frame_dims, NULL) < 0)
        errx(EXIT_FAILURE, "H5Sselect_hyperslab failed");

    status = H5Dread(ds, H5T_NATIVE_FLOAT, s->memspace, filespace, H5P_DEFAULT, mat);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Dread failed");

    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");
}

static void
matrix_write(state_t *s, int frameno, float mat[ROWS][COLS])
{
    hid_t   ds, filespace;
    herr_t  status;
    hsize_t size[RANK]   = {frameno + 1, ROWS, COLS};
    hsize_t offset[RANK] = {frameno, 0, 0};

    ds = s->dataset;

    if (H5Dset_extent(ds, size) < 0)
        errx(EXIT_FAILURE, "H5Dset_extent failed");

    filespace = H5Dget_space(ds);

    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, frame_dims, NULL) < 0)
        errx(EXIT_FAILURE, "H5Sselect_hyperslab failed");

    status = H5Dwrite(ds, H5T_NATIVE_FLOAT, s->memspace, filespace, H5P_DEFAULT, mat);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Dwrite failed");

    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");

    if (H5Dflush(ds) < 0)
        errx(EXIT_FAILURE, "H5Dflush failed");
}

static void
open_group(state_t *s)
{
    hid_t       g;
    const char *gname = "/group-0";

    assert(s->group == H5I_INVALID_HID);

    g = H5Gopen(s->file, gname, H5P_DEFAULT);

    if (g < 0)
        errx(EXIT_FAILURE, "H5Gcreate failed");

    fprintf(stderr, "Opened group %s\n", gname);
    s->group = g;
}

static void
create_group(state_t *s)
{
    hid_t       g;
    const char *gname = "/group-0";

    assert(s->group == H5I_INVALID_HID);

    g = H5Gcreate(s->file, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (g < 0)
        errx(EXIT_FAILURE, "H5Gcreate failed");

    s->group = g;
}

static void
open_dataset(state_t *s)
{
    const char *dname = "/group-0/dataset-0";
    hid_t       ds;
    hid_t       filespace;
    hid_t       ty;
    hsize_t     dims[RANK], maxdims[RANK];

    assert(s->dataset == H5I_INVALID_HID);

    ds = H5Dopen(s->file, dname, H5P_DEFAULT);

    if (ds < 0)
        errx(EXIT_FAILURE, "H5Dopen(, \"%s\", ) failed", dname);

    if ((ty = H5Dget_type(ds)) < 0)
        errx(EXIT_FAILURE, "H5Dget_type failed");

    if (H5Tequal(ty, H5T_IEEE_F32BE) <= 0)
        errx(EXIT_FAILURE, "Unexpected data type");

    if ((filespace = H5Dget_space(ds)) < 0)
        errx(EXIT_FAILURE, "H5Dget_space failed");

    if (H5Sget_simple_extent_ndims(filespace) != RANK)
        errx(EXIT_FAILURE, "Unexpected rank");

    if (H5Sget_simple_extent_dims(filespace, dims, maxdims) < 0)
        errx(EXIT_FAILURE, "H5Sget_simple_extent_dims failed");

    if (dims[1] != original_dims[1] || dims[2] != original_dims[2]) {
        errx(EXIT_FAILURE, "Unexpected dimensions ? x %ju x %ju", (uintmax_t)dims[1], (uintmax_t)dims[2]);
    }

    if (maxdims[1] != original_dims[1] || maxdims[2] != original_dims[2]) {
        errx(EXIT_FAILURE, "Unexpected maximum dimensions ? x %ju x %ju", (uintmax_t)dims[1],
             (uintmax_t)dims[2]);
    }

    fprintf(stderr, "Opened dataset %s\n", dname);
    s->dataset = ds;
}

static void
create_dataset(state_t *s)
{
    const char *dname = "/group-0/dataset-0";
    hid_t       ds;
    hid_t       filespace;

    assert(s->dataset == H5I_INVALID_HID);

    filespace = H5Screate_simple(__arraycount(original_dims), original_dims, max_dims);

    if (filespace < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
    }

    if ((s->dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Pcreate failed", __func__, __LINE__);
    }

    if (H5Pset_chunk(s->dcpl, RANK, chunk_dims) < 0)
        errx(EXIT_FAILURE, "H5Pset_chunk failed");

    ds = H5Dcreate2(s->file, dname, H5T_IEEE_F32BE, filespace, H5P_DEFAULT, s->dcpl, H5P_DEFAULT);

    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");

    filespace = H5I_INVALID_HID;

    if (ds < 0)
        errx(EXIT_FAILURE, "H5Dcreate(, \"%s\", ) failed", dname);

    s->dataset = ds;
}

static void
handle_signal(int signo)
{
    unbroken = 0;
}

static void
disestablish_handler(const struct sigaction *osa)
{
    if (sigaction(SIGINT, osa, NULL) == -1)
        err(EXIT_FAILURE, "%s: sigaction", __func__);
}

static void
establish_handler(struct sigaction *osa)
{
    struct sigaction sa;

    memset(&sa, '\0', sizeof(sa));
    sa.sa_handler = handle_signal;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGINT, &sa, osa) == -1)
        err(EXIT_FAILURE, "%s: sigaction", __func__);
}

static void
step(vec_t *center, vec_t *direction, float steplen, bool *recursed)
{
    static const float top = 0., bottom = (float)COLS, left = 0., right = (float)ROWS;
    struct {
        bool top, bottom, left, right;
    } bounce = {false, false, false, false};
    float xback, yback;
    vec_t before = *center;
    vec_t after  = (vec_t){.x = before.x + direction->x * steplen, .y = before.y + direction->y * steplen};

    if (before.x < right && after.x >= right) {
        xback        = (right - before.x) / (after.x - before.x);
        bounce.right = true;
    }
    else if (before.x > left && after.x <= left) {
        xback       = (before.x - left) / (before.x - after.x);
        bounce.left = true;
    }
    else
        xback = 0.;

    if (before.y < bottom && after.y >= bottom) {
        yback         = (bottom - before.y) / (after.y - before.y);
        bounce.bottom = true;
    }
    else if (before.y > top && after.y <= top) {
        yback      = (before.y - top) / (before.y - after.y);
        bounce.top = true;
    }
    else
        yback = 0.;

    /* I shorten the step length until a corner crossing becomes
     * a side crossing.
     */
    if ((bounce.top && bounce.right) || (bounce.right && bounce.bottom) || (bounce.bottom && bounce.left) ||
        (bounce.left && bounce.top)) {

        float newsteplen = steplen * 2 / 3;
        if (recursed != NULL)
            *recursed = true;
        step(center, direction, newsteplen, NULL);
        step(center, direction, steplen - newsteplen, NULL);
    }
    if (bounce.right || bounce.left) {
        after.x      = before.x + direction->x * (2 * xback - 1) * steplen;
        direction->x = -direction->x;
    }
    if (bounce.top || bounce.bottom) {
        after.y      = before.y + direction->y * (2 * yback - 1) * steplen;
        direction->y = -direction->y;
    }
    *center = after;
}

static float
gaussian(float x, float y, float r)
{
    return expf(-(x * x + y * y) / (r * r));
}

int
stepno(float v)
{
    if (v < 1. / 8.)
        return 0;
    if (v < 3. / 8.)
        return 1;
    if (v < 7 / 8.)
        return 2;

    return 3;
}

static void
draw_border(WINDOW *w)
{
    wborder(w, 0, 0, 0, 0, 0, 0, 0, 0);
}

static void
matrix_draw(WINDOW *w, float mat[ROWS][COLS])
{
    int         ch, i, j;
    static char steps[] = " .oO";

    wclear(w);
    draw_border(w);
    for (i = 0; i < ROWS; i++) {
        wmove(w, 1 + i, 1);
        for (j = 0; j < COLS; j++) {
            ch = steps[stepno(mat[i][j])];
            waddch(w, ch);
        }
    }

    wnoutrefresh(w);
}

static void
matrix_compute(vec_t *center, size_t ncenters, float mat[ROWS][COLS])
{
    int   i, j, k;
    float radius = 4;

    for (i = 0; i < ROWS; i++) {
        for (j = 0; j < COLS; j++) {
            mat[i][j] = 0.;
            for (k = 0; k < ncenters; k++) {
                mat[i][j] += gaussian(i - center[k].x, j - center[k].y, radius);
            }
        }
    }
}

static void
move_centers(vec_t *center, vec_t *direction, size_t ncenters)
{
    const float steplen = .01;
    int         k;
    bool        recursed[2] = {false, false};

    for (k = 0; k < ncenters; k++) {
        recursed[k] = false;
        step(&center[k], &direction[k], steplen, &recursed[k]);
    }
}

static void
matrix_open(state_t *s, bool rw)
{
    const char *          func;
    hid_t                 fapl, fcpl;
    H5F_vfd_swmr_config_t config;

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    if (fapl < 0) {
        errx(EXIT_FAILURE, "%s.%d H5Pcreate failed", __func__, __LINE__);
    }

    fcpl = H5Pcreate(H5P_FILE_CREATE);
    if (fcpl < 0) {
        errx(EXIT_FAILURE, "%s.%d H5Pcreate failed", __func__, __LINE__);
    }

    memset(&config, '\0', sizeof(config));

    config.version           = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config.tick_len          = SWMR_TICK_LEN;
    config.max_lag           = 5;
    config.writer            = rw;
    config.md_pages_reserved = 128;

#if 0 /* raw-data flushing is not implemented; default open-tries is ok */
	config.flush_raw_data = true;
	config.md_open_tries = 1;
#endif

    strlcpy(config.md_file_path, "./my_md_file", sizeof(config.md_file_path));

    /* Enable page buffering */
    if (H5Pset_page_buffer_size(fapl, 4096, 100, 0) < 0)
        errx(EXIT_FAILURE, "H5Pset_page_buffer_size failed");

    /* Enable VFD SWMR configuration */
    if (H5Pset_vfd_swmr_config(fapl, &config) < 0)
        errx(EXIT_FAILURE, "H5Pset_vfd_swmr_config failed");

    /* Set file space strategy to paged aggregation in fcpl.
     * Page buffering *requires* this strategy.
     *
     * I set the free-space threshold to 1GB so that deleted
     * datasets are not recycled.
     */
    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1024 * 1024 * 1024) < 0)
        errx(EXIT_FAILURE, "H5Pset_file_space_strategy failed");

    if (rw) {
        s->file = H5Fcreate("gaussians.h5", H5F_ACC_TRUNC, fcpl, fapl);
        func    = "H5Fcreate";
    }
    else {
        s->file = H5Fopen("gaussians.h5", H5F_ACC_RDONLY, fapl);
        func    = "H5Fopen";
    }

    H5Pclose(fapl);

    if (s->file < 0)
        errx(EXIT_FAILURE, "%s failed", func);
}

static void
fuzz(float mat[ROWS][COLS])
{
    int i, j;

    for (i = 0; i < ROWS; i++) {
        for (j = 0; j < COLS; j++) {
            mat[i][j] += (float)random() / RAND_MAX * (9. / 64.);
        }
    }
}

int
main(int argc, char **argv)
{
    char             buf[32];
    float            mat[ROWS][COLS];
    int              frameno;
    vec_t            center[2]    = {{.x = .5, .y = .5}, {.x = ROWS - .5, .y = COLS - .5}};
    vec_t            direction[2] = {{.x = 3, .y = 7}, {.x = 43, .y = 41}};
    struct sigaction osa;
    WINDOW *         topw = NULL, *w = NULL;
    personality_t    personality;
    state_t          s;

    srandom((unsigned int)time(NULL));

    setlocale(LC_ALL, "");

    state_init(&s, argc, argv);

    switch (s.progname[0]) {
        case 'r':
            personality = READ;
            break;
        case 'w':
            personality = WRITE;
            break;
        default:
            personality = STANDALONE;
            break;
    }
    establish_handler(&osa);

    switch (personality) {
        case WRITE:
            matrix_open(&s, true);
            create_group(&s);
            create_dataset(&s);
            break;
        case READ:
            matrix_open(&s, false);
            open_group(&s);
            open_dataset(&s);
            break;
        default:
            break;
    }

    if ((topw = initscr()) == NULL)
        errx(EXIT_FAILURE, "initscr failed");
    else if ((w = subwin(topw, ROWS + 2, COLS + 2, 0, 0)) == NULL)
        errx(EXIT_FAILURE, "subwin failed");

    for (frameno = 0; unbroken;) {
        struct timespec elapsed, start, stop;
        uint64_t        elapsed_ns;
        clock_gettime(CLOCK_MONOTONIC, &start);

        switch (personality) {
            case READ:
                matrix_read(&s, &frameno, mat);
                break;
            case WRITE:
            case STANDALONE:
                matrix_compute(center, __arraycount(center), mat);
                if (s.fuzz)
                    fuzz(mat);
                break;
        }
        switch (personality) {
            case READ:
            case STANDALONE:
                matrix_draw(w, mat);
#if 0
			wmove(topw, ROWS + 3, 0);
			waddstr(topw, "\"Don't cross the streams.\"");
#endif
                break;
            case WRITE:
                matrix_write(&s, frameno, mat);
                break;
        }

        snprintf(buf, sizeof(buf), "Frame %d.", frameno);
        wmove(topw, ROWS + 2, 0);
        waddstr(topw, buf);
        snprintf(buf, sizeof(buf), "Rate %lld/s.", 1000000000ULL / timespec2ns(&s.update_interval));
        wmove(topw, ROWS + 2, COLS + 2 - strlen(buf));
        waddstr(topw, buf);
        wnoutrefresh(topw);
        doupdate();

        nanosleep(&s.update_interval, NULL);

        switch (personality) {
            case STANDALONE:
            case WRITE:
                move_centers(center, direction, __arraycount(center));
                frameno++;
                break;
            case READ:
                break;
        }
        clock_gettime(CLOCK_MONOTONIC, &stop);

        timespecsub(&stop, &start, &elapsed);
        elapsed_ns = timespec2ns(&elapsed);

        if (elapsed_ns < s.min_elapsed_ns)
            s.min_elapsed_ns = elapsed_ns;
        if (elapsed_ns > s.max_elapsed_ns)
            s.max_elapsed_ns = elapsed_ns;
        s.total_elapsed_ns += elapsed_ns;
        s.total_loops++;
    }
    endwin();
    fprintf(stderr, "Iteration stats:\n");
    fprintf(stderr, "min. elapsed %" PRIu64 " ms\n", s.min_elapsed_ns / 1000000);
    fprintf(stderr, "max. elapsed %" PRIu64 " ms\n", s.max_elapsed_ns / 1000000);
    fprintf(stderr, "avg. elapsed %.3f ms\n", (double)s.total_elapsed_ns / s.total_loops / 1000000);
    disestablish_handler(&osa);
    return EXIT_SUCCESS;
}
