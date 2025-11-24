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
};

struct LogicalRange {
    std::vector<Comparison> comp;
    Type *type;
};

Ranges feasible_ranges_or(const LogicalRange &ranges, Integer min, Integer max);
Ranges feasible_ranges_and(const LogicalRange &ranges, Integer min, Integer max);
