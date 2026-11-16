#pragma once

#include "parser.hh"

struct Range {
    Integer lo{};
    Integer hi{};
    bool lo_inc = false;
    bool hi_inc = false;
};

using Ranges = std::vector<Range>;

struct Comparison {
    AstBinary *comparison;
    AstLiteral *constant;
    // The comparison as `variable operation constant`, i.e. mirrored if the constant is on the
    // left (`5 < x` is `x > 5`).
    Operation operation;
};

struct LogicalRange {
    std::vector<Comparison> comp;
    Type *type;
};

Ranges feasible_ranges_or(const LogicalRange &ranges, Integer min, Integer max);
Ranges feasible_ranges_and(const LogicalRange &ranges, Integer min, Integer max);
