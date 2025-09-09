typedef struct H5RT_t H5RT_t;

typedef struct H5RT_leaf_t {
    void *record;
    hsize_t min[H5S_MAX_RANK];
    hsize_t max[H5S_MAX_RANK];
    hsize_t mid[H5S_MAX_RANK];
    struct H5RT_leaf_t *next;
} H5RT_leaf_t;

H5RT_t *H5RT_create(int rank, H5RT_leaf_t *leaves, size_t count);
H5RT_leaf_t *H5RT_search(H5RT_t *rtree, hsize_t min[], hsize_t max[]);
herr_t H5RT_free(H5RT_t *rtree);
