@echo off

rem //---------------------------------------------------------------------------------------------
rem // FGMT-RiscV: Implementation of 32-Bit Risc-V allowing fine grained multiprocessing
rem // Copyright (C) 2025  Bernhard Lang
rem //
rem // This program is free software; you can redistribute it and/or modify
rem // it under the terms of the GNU General Public License as published by
rem // the Free Software Foundation; either version 3 of the License, or
rem // (at your option) any later version.
rem //
rem // This program is distributed in the hope that it will be useful,
rem // but WITHOUT ANY WARRANTY; without even the implied warranty of
rem // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem // GNU General Public License for more details.
rem //
rem // You should have received a copy of the GNU General Public License
rem // along with this program; if not, see http://www.gnu.org/licenses
rem // or write to the Free Software Foundation,Inc., 51 Franklin Street,
rem // Fifth Floor, Boston, MA 02110-1301  USA
rem //---------------------------------------------------------------------------------------------

rem ADAPT TO YOUR NEEDS

rem Make path to “Windows Build Tools” known
PATH L:\tools\xpack-windows-build-tools-4.2.1-2\bin
rem Make path to Risc-V compiler known
PATH L:\tools\xpack-riscv-none-elf-gcc-12.2.0-3\bin;%PATH%

rem Set path to Eclipse executable and to the selected workspace
set ECLIPSE_PATH=L:\tools\eclipse-cpp-2024-12-R-win32-x86_64\eclipse

set WORKSPACE=%CD%
%ECLIPSE_PATH%\eclipse.exe -data %WORKSPACE%
