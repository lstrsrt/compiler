#pragma once

#include <string>

// Get a string with a random color.
// The same string will always return the same color.
// TODO: make color range customizable
std::string colored(const std::string &);
std::string random_color();
