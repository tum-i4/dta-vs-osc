@echo off
set build_type=%1
SETLOCAL EnableDelayedExpansion

where vswhere /q 
if errorlevel 1 (
    echo "vswhere" not in path, trying default installation directory
    set vswhere="%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
    if not exist !vswhere! (
        echo error: vswhere not found
        goto FAILED
    )
) else (
    echo Using vswhere from path
    set vswhere=vswhere
)
for /f "usebackq tokens=1* delims=: " %%i in (`!vswhere! -latest -products *`) do (
    if /i "%%i"=="installationPath" set InstallDir=%%j
)
if "!InstallDir!" == "" (
    echo Visual Studio Installation not found using vswhere
    goto FAILED
)
echo Using visual studio installation at !InstallDir!

set "VSINSTALLDIR=!InstallDir!\"
set DEVPROMPT64="!InstallDir!\VC\Auxiliary\Build\vcvars64.bat"
set DEVPROMPT32="!InstallDir!\VC\Auxiliary\Build\vcvars32.bat"
call !DEVPROMPT64! & cd /D %cd%

SETLOCAL EnableDelayedExpansion
@echo on

if "!build_type!" == "" (
    set build_type=RelWithDebInfo
)

set mypath=%~dp0
set build_path=!mypath!\..\build_tracer_!build_type!\windows
set out_path=!mypath!\..\out_tracer_!build_type!\windows
cd !mypath!

if not exist !build_path! (
    mkdir !build_path!
    cd !build_path!

    set CC=cl.exe
    set CXX=cl.exe

    cmake -G "Ninja" ^
        -D "CMAKE_BUILD_TYPE:STRING=!build_type!" ^
        -D "CMAKE_EXPORT_COMPILE_COMMANDS:BOOL=1" ^
        -D "CMAKE_INSTALL_PREFIX=!out_path!" ^
        -D "DynamoRIO_DIR:PATH=!mypath!/../DynamoRIO-Windows-7.1.0-1/cmake" ^
        ../../tracer

    cd ..
)
cd !build_path!
set "NINJA_STATUS=[%%f/%%t] %%es:  "
REM cmake --build . --target install --config !build_type!
cmake --build . --config !build_type!