typedef struct H5RT_node_t {
    hsize_t min[H5S_MAX_RANK]; /* Invalid for root node */
    hsize_t max[H5S_MAX_RANK]; /* Invalid for root node */
    union {
        H5RT_node_t *nodes[H5RT_MAX_NODE_SIZE];
        H5RT_leaf_t *leaves[H5RT_MAX_NODE_SIZE];
    } children;
    int nchildren;
    bool children_are_leaves;
}

struct H5RT_t {
    H5RT_node_t root;
    H5RT_leaf_t *leaves;
    int rank;
};
