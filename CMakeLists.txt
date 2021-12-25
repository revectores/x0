cmake_minimum_required(VERSION 3.19)
project(x0 C)

set(CMAKE_C_STANDARD 11)

add_executable(x0 x0.c x0.h main.c)
add_executable(x0_test x0.c x0.h tests.c)
