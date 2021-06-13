workspace "vlox"
    configurations {"Debug", "Release"}

project "vlox"
    kind "ConsoleApp"
    language "C"
    targetdir "bin/%{cfg.buildcfg}"

    includedirs  "./include/" 
    files { "**.h", "**.c", "**/**/*.h", "**/**/*.c" }

    filter "configurations:Debug"
        defines { "VDEBUG", "VDEBUG_PRINT_CODE" }
        symbols "On"

    filter "configurations:Release"
        defines { "VRELEASE" }
        optimize "On"
