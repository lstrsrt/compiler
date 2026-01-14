#include "base.hh"

#include <algorithm>
#include <array>
#include <random>
#include <unordered_map>

std::array<int, 3> hsv_to_rgb(double h, double s, double v)
{
    h = std::fmod(h, 360.0);
    if (h < 0) {
        h += 360.0;
    }
    s = std::clamp(s, 0.0, 1.0);
    v = std::clamp(v, 0.0, 1.0);

    double r = 0.0;
    double g = 0.0;
    double b = 0.0;

    if (s == 0.0) {
        auto gray = static_cast<int>(round(v * 255.0));
        gray = std::clamp(gray, 0, 255);
        return { gray, gray, gray };
    }

    double hh = h / 60.0;
    int i = static_cast<int>(floor(hh));
    double f = hh - i;
    double p = v * (1.0 - s);
    double q = v * (1.0 - s * f);
    double t = v * (1.0 - s * (1.0 - f));

    switch (i) {
        case 0:
            r = v, g = t, b = p;
            break;
        case 1:
            r = q, g = v, b = p;
            break;
        case 2:
            r = p, g = v, b = t;
            break;
        case 3:
            r = p, g = q, b = v;
            break;
        case 4:
            r = t, g = p, b = v;
            break;
        case 5:
            r = v, g = p, b = q;
            break;
        default:
            r = g = b = 0;
    }

    auto R = static_cast<int>(std::round(r * 255.0));
    auto G = static_cast<int>(std::round(g * 255.0));
    auto B = static_cast<int>(std::round(b * 255.0));
    R = std::clamp(R, 0, 255);
    G = std::clamp(G, 0, 255);
    B = std::clamp(B, 0, 255);
    return { R, G, B };
}

std::string random_color()
{
    static std::mt19937 rng{ std::random_device{}() };
    // if we don't have truecolor:
    // std::uniform_int_distribution<> dist{ 20, 225 };
    // return std::format("\e[38;5;{}m", dist(rng));
    std::uniform_int_distribution<> dist_h{ 0, 360 };
    std::uniform_int_distribution<> dist_sv{ 50, 100 };
    auto h = dist_h(rng);
    auto s = dist_sv(rng) / 100.0;
    auto v = dist_sv(rng) / 100.0;
    auto [r, g, b] = hsv_to_rgb(h, s, v);
    return std::format("\e[38;2;{};{};{}m", r, g, b);
}

std::string colored(const std::string &s)
{
    static std::unordered_map<std::string, std::string> color_map;
    if (auto it = color_map.find(s); it != color_map.end()) {
        return it->second;
    }
    auto v = random_color() + s + colors::Default;
    color_map[s] = v;
    return v;
}
