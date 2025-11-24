#include "range.hh"

bool union_mergeable(const Range &A, const Range &B)
{
    if (A.hi < B.lo) {
        return false;
    }
    if (A.hi == B.lo && !(A.hi_inc || B.lo_inc)) {
        // The ranges would touch if they were inclusive
        return false;
    }
    return true;
}

bool range_contains(const Range &r, Integer i)
{
    if (i < r.lo || i > r.hi) {
        return false;
    }
    if ((i == r.lo && !r.lo_inc) || (i == r.hi && !r.hi_inc)) {
        return false;
    }
    return true;
}

bool non_empty_intersection(const Range &a, const Range &b)
{
    auto lo = Integer::max(a.lo, b.lo);
    auto hi = Integer::min(a.hi, b.hi);

    if (lo > hi) {
        return false;
    }
    if (lo < hi) {
        return true;
    }
    return range_contains(a, lo) && range_contains(b, lo);
}

// non_empty_intersection() must return true for this to make sense
Range intersect_ranges(const Range &a, const Range &b)
{
    Range r{};

    // Lower bound
    if (a.lo > b.lo) {
        r.lo = a.lo;
        r.lo_inc = a.lo_inc;
    } else if (b.lo > a.lo) {
        r.lo = b.lo;
        r.lo_inc = b.lo_inc;
    } else {
        // A.lo == B.lo
        r.lo = a.lo;
        r.lo_inc = (a.lo_inc && b.lo_inc);
    }

    // Upper bound
    if (a.hi < b.hi) {
        r.hi = a.hi;
        r.hi_inc = a.hi_inc;
    } else if (b.hi < a.hi) {
        r.hi = b.hi;
        r.hi_inc = b.hi_inc;
    } else {
        // A.hi == B.hi
        r.hi = a.hi;
        r.hi_inc = (a.hi_inc && b.hi_inc);
    }
    return r;
}

// Does this range have any integer points?
bool range_is_valid(const Range &r)
{
    if (r.lo > r.hi) {
        return false;
    }

    Integer eff_lo{};
    if (r.lo_inc) {
        eff_lo = r.lo;
    } else {
        if (r.lo.value == U64Max) {
            return false;
        }
        eff_lo = r.lo + 1;
    }

    Integer eff_hi{};
    if (r.hi_inc) {
        eff_hi = r.hi;
    } else {
        if (r.hi.as_signed() == S64Min) {
            return false;
        }
        eff_hi = r.hi - 1;
    }
    return eff_lo <= eff_hi;
}

Ranges intersect_ranges(const Ranges &a, const Ranges &b)
{
    Ranges ret;
    size_t a_idx = 0;
    size_t b_idx = 0;

    while (a_idx < size(a) && b_idx < size(b)) {
        const auto &cur_a = a[a_idx];
        const auto &cur_b = b[b_idx];

        if (cur_a.hi < cur_b.lo) {
            ++a_idx;
            continue;
        }
        if (cur_b.hi < cur_a.lo) {
            ++b_idx;
            continue;
        }

        if (non_empty_intersection(cur_a, cur_b)) {
            Range r = intersect_ranges(cur_a, cur_b);
            if (range_is_valid(r)) {
                ret.push_back(r);
            }
        }

        if (cur_a.hi < cur_b.hi) {
            ++a_idx;
        } else if (cur_b.hi < cur_a.hi) {
            ++b_idx;
        } else {
            ++a_idx;
            ++b_idx;
        }
    }
    return ret;
}

Ranges merge_overlapping_ranges(const Ranges &in)
{
    Ranges ret;
    if (in.empty()) {
        return ret;
    }

    ret.push_back(in[0]);

    for (size_t i = 1; i < size(in); ++i) {
        const auto &r = in[i];
        auto &last = ret.back();

        if (union_mergeable(last, r)) {
            if (r.hi > last.hi) {
                last.hi = r.hi;
                last.hi_inc = r.hi_inc;
            } else if (r.hi == last.hi) {
                last.hi_inc = last.hi_inc || r.hi_inc;
            }

            // New lower bound
            // TODO: not normally needed? last.lo <= r.lo
            if (r.lo < last.lo) {
                last.lo = r.lo;
                last.lo_inc = r.lo_inc;
            } else if (r.lo == last.lo) {
                last.lo_inc = last.lo_inc || r.lo_inc;
            }
        } else {
            // disjoint
            ret.push_back(r);
        }
    }
    return ret;
}

Ranges union_(const Ranges &A, const Ranges &B)
{
    Ranges combined;
    size_t i = 0;
    size_t j = 0;

    while (i < size(A) && j < size(B)) {
        if (A[i].lo < B[j].lo || (A[i].lo == B[j].lo && A[i].lo_inc && !B[j].lo_inc)) {
            combined.push_back(A[i++]);
        } else {
            combined.push_back(B[j++]);
        }
    }

    while (i < size(A)) {
        combined.push_back(A[i++]);
    }
    while (j < size(B)) {
        combined.push_back(B[j++]);
    }
    return merge_overlapping_ranges(combined);
}

Ranges create_ranges(Operation op, Integer val, Integer min, Integer max)
{
    Ranges rs;
    if (op == Operation::Less) {
        rs.emplace_back(min, val, true, false);
    } else if (op == Operation::LessEquals) {
        rs.emplace_back(min, val, true, true);
    } else if (op == Operation::Greater) {
        rs.emplace_back(val, max, false, true);
    } else if (op == Operation::GreaterEquals) {
        rs.emplace_back(val, max, true, true);
    } else if (op == Operation::Equals) {
        rs.emplace_back(val, val, true, true);
    } else if (op == Operation::NotEquals) {
        rs.emplace_back(min, val, true, false);
        rs.emplace_back(val, max, false, true);
    }
    return rs;
}

Ranges feasible_ranges_or(const LogicalRange &ranges, Integer min, Integer max)
{
    Ranges feasible{};

    for (const auto &range : ranges.comp) {
        Integer val{};
        val.value = range.constant->u.u64;
        val.is_signed = ranges.type->is_signed();
        Ranges constraint = create_ranges(range.comparison->operation, val, min, max);
        feasible = union_(feasible, constraint);
    }
    return merge_overlapping_ranges(feasible);
}

Ranges feasible_ranges_and(const LogicalRange &ranges, Integer min, Integer max)
{
    Ranges feasible = { { min, max, true, true } };

    for (const auto &range : ranges.comp) {
        Integer val{};
        val.value = range.constant->u.u64;
        val.is_signed = ranges.type->is_signed();
        Ranges rs = create_ranges(range.comparison->operation, val, min, max);
        feasible = intersect_ranges(feasible, rs);
        if (feasible.empty()) {
            return {};
        }
    }

    return feasible;
}
