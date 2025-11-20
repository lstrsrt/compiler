add_rules("mode.debug", "mode.release")

target("compiler")
    set_kind("binary")
    set_languages("cxx23")
    set_warnings("allextra")
    add_cxxflags("-Wno-missing-designated-field-initializers", "-Wno-unused-const-variable")
    add_cxxflags("-Wimplicit-fallthrough")
    add_includedirs("include/", "vendor/")
    set_rundir(os.projectdir())
    set_toolchains("clang")
    add_files("src/*.cc")
    if is_mode("debug") then
        add_defines("_DEBUG")
        -- set_policy("build.sanitizer.leak", true)
        set_targetdir("build/debug")
        if is_plat("windows") then
            set_runtimes("MDd")
        end
    else -- release
        add_defines("NDEBUG")
        set_optimize("faster")
        set_targetdir("build/release")
    end
