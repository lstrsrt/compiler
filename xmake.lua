add_rules("mode.debug", "mode.release")

-- necessary for the has_package() calls to work...
add_requires("clang", {optional = true})
add_requires("mold", {optional = true})

add_requires("cpptrace")

target("compiler")
    set_kind("binary")
    set_languages("cxx23")
    set_warnings("allextra")
    add_cxxflags("-Wno-unused-const-variable")
    add_cxxflags("-Wimplicit-fallthrough")
    add_includedirs("include/", "vendor/")
    set_rundir(os.projectdir())
    if not is_mode("fuzz") then
        if (has_package("clang")) then
            set_toolchains("clang")
        end
        if has_package("mold") then
            add_ldflags("-fuse-ld=mold")
        end
    end
    add_packages("cpptrace")
    add_files("src/*.cc")
    if is_mode("debug") then
        add_defines("_DEBUG")
        set_targetdir("build/debug")
        if is_plat("windows") then
            set_runtimes("MDd")
        end
    elseif is_mode("release") then
        add_defines("NDEBUG")
        add_cxxflags("-march=native")
        set_optimize("faster")
        set_symbols("debug")
        set_strip("none")
        set_targetdir("build/release")
    elseif is_mode("fuzz") then
        set_targetdir("build/afl")
        set_optimize("faster")
        set_symbols("debug")
    end

if is_mode("fuzz") then
    add_defines("FUZZING")
    set_optimize("fast")
    set_toolset("cc", "clang@afl-clang-fast")
    set_toolset("cxx", "clang++@afl-clang-fast++")
    set_toolset("ld", "clang++@afl-clang-fast++")
    set_toolset("sh", "clang++@afl-clang-fast++")
end
