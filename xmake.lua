add_rules("mode.debug", "mode.release")

-- necessary for the has_package() calls to work...
add_requires("clang", {optional = true})
add_requires("mold", {optional = true})

target("compiler")
    set_kind("binary")
    set_languages("cxx23")
    set_warnings("allextra")
    add_cxxflags("-Wno-unused-const-variable")
    add_cxxflags("-Wimplicit-fallthrough")
    add_includedirs("include/", "vendor/")
    set_rundir(os.projectdir())
    if (has_package("clang")) then
        set_toolchains("clang")
    end
    if has_package("mold") then
        add_ldflags("-fuse-ld=mold")
    end
    add_files("src/*.cc")
    if is_mode("debug") then
        add_defines("_DEBUG")
        set_targetdir("build/debug")
    else -- release
        add_defines("NDEBUG")
        set_optimize("faster")
        set_targetdir("build/release")
    end
