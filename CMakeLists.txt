cmake_minimum_required(VERSION 3.24)
project(emu6502)

set(CMAKE_CXX_STANDARD 17)

find_package(X11 REQUIRED)
include_directories(${X11_INCLUDE_DIR})
link_directories(${X11_LIBRARIES})

add_executable(emu6502 main.cpp j6502.cpp j6502.h Bus.cpp Bus.h olcPixelGameEngine.h)
target_link_libraries(emu6502 X11 GL png stdc++fs pthread)