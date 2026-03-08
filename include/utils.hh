#pragma once

#include <random>
#include <string>

template<class T = int>
inline auto random(T min, T max)
{
    static std::random_device rd{};
    static std::mt19937 engine{ rd() };
    return std::uniform_int_distribution<T>{ min, max }(engine);
}

// Get a string with a random color.
// The same string will always return the same color.
// TODO: make color range customizable
std::string colored(const std::string &);
std::string random_color();
