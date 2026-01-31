#include "new-ir.hh"

#include <numeric>

namespace new_ir::dom {

// TODO: worth doing incremental updates?

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

void build_df_tree(SemiNCAState &snca, int pred, int &i)
{
    // Number in DFS order.
    snca.df_order[pred] = i;
    snca.rev_df_order[i++] = pred;

    for (auto succ : snca.succ_edges[pred]) {
        if (snca.df_order[succ] < 0) {
            snca.compress[succ] = pred;
            // Visit the next unnumbered successor.
            build_df_tree(snca, succ, i);
        }
    }
}

// https://github.com/gcc-mirror/gcc/blob/releases/gcc-15/gcc/dominance.cc#L467
int compress_path(SemiNCAState &snca, int v, int last_linked)
{
    if (snca.df_order[v] < last_linked) {
        return v;
    }

    // Apparently it's not worth making this non-recursive (see gcc source).
    auto parent = snca.compress[v];
    auto ancestor = compress_path(snca, parent, last_linked);
    if (snca.df_order[snca.best[parent]] < snca.df_order[snca.best[v]]) {
        snca.best[v] = snca.best[parent];
    }

    snca.compress[v] = ancestor;
    return ancestor;
}

void compute_dominator_tree(const std::vector<BasicBlock *> &blocks)
{
    SemiNCAState state;
    const auto n = static_cast<int>(size(blocks));

    state.succ_edges.resize(n);
    state.pred_edges.resize(n);
    for (auto *bb : blocks) {
        auto u = bb->index_in_fn;
        for (auto *succ : bb->successors) {
            auto v = succ->index_in_fn;
            state.succ_edges[u].push_back(static_cast<int>(v));
            state.pred_edges[v].push_back(static_cast<int>(u));
        }
    }

    state.idom.assign(n, -1);
    state.df_order.assign(n, -1);
    state.rev_df_order.resize(n);
    state.compress.resize(n);
    state.sdom.resize(n);

    {
        int i = 0;
        build_df_tree(state, 0, i);
    }

    // Compute semidominators like Lengauer-Tarjan does.
    state.best.resize(n);
    std::ranges::iota(state.best, 0);
    for (auto i = n - 1; i > 0; --i) {
        auto v = state.rev_df_order[i];
        state.sdom[v] = v;

        auto last_linked = i + 1;
        for (auto pred : state.pred_edges[v]) {
            // FIXME: >= last_linked?
            if (state.df_order[pred] >= 0) {
                compress_path(state, pred, last_linked);
                if (state.df_order[state.best[pred]] < state.df_order[state.sdom[v]]) {
                    state.sdom[v] = state.best[pred];
                }
            }
        }

        state.best[v] = state.sdom[v];
        state.idom[v] = state.compress[v];
    }

    for (int i = 1; i < n; ++i) {
        auto v = state.rev_df_order[i];
        // Walk up to the end of the dominator chain.
        while (state.df_order[state.idom[v]] > state.df_order[state.sdom[v]]) {
            state.idom[v] = state.idom[state.idom[v]];
        }
    }

    for (int i = 0; i < n; ++i) {
        dbgln("{} idom: {}", i, state.idom[i]);
    }
}

} // namespace new_ir::dom
