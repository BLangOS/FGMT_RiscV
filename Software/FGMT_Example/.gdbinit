#-----------------------------------------------------------------------------------------------
#-- FGMT-RiscV: Implementation of 32-Bit Risc-V allowing fine grained multiprocessing
#-- Copyright (C) 2025  Bernhard Lang
#--
#-- This program is free software; you can redistribute it and/or modify
#-- it under the terms of the GNU General Public License as published by
#-- the Free Software Foundation; either version 3 of the License, or
#-- (at your option) any later version.
#--
#-- This program is distributed in the hope that it will be useful,
#-- but WITHOUT ANY WARRANTY; without even the implied warranty of
#-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#-- GNU General Public License for more details.
#--
#-- You should have received a copy of the GNU General Public License
#-- along with this program; if not, see http://www.gnu.org/licenses
#-- or write to the Free Software Foundation,Inc., 51 Franklin Street,
#-- Fifth Floor, Boston, MA 02110-1301  USA
#-----------------------------------------------------------------------------------------------

# switch debugging outputs on/off
set debug remote 1
set verbose on

# Limit the memory area to the existing area
# (otherwise the gdb can access wrong memory areas if the SP has not yet been set)
mem 0x2000 0xffff rw

# Set baudrate
set serial baud 115200

# Select Risc-V 32Bit
set arch riscv:rv32

# Reset System (is ignored here, therefore add to the initialization commands in the debug configuration)
#monitor r
