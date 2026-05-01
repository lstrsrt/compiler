#pragma once

#include "new-ir.hh"

namespace new_ir::dom {

struct SemiNCAState {
    std::vector<std::vector<int>> succ_edges;
    std::vector<std::vector<int>> pred_edges;
    std::vector<int> df_order;
    std::vector<int> rev_df_order;
    std::vector<int> compress;
    std::vector<int> best;
    std::vector<int> sdom;
    std::vector<int> idom;
};

void compute_dominator_tree(SemiNCAState &, const std::vector<BasicBlock *> &);

} // namespace new_ir::dom
