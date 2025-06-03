//---------------------------------------------------------------------------------------------
// FGMT-RiscV: Implementation of 32-Bit Risc-V allowing fine grained multiprocessing
// Copyright (C) 2025  Bernhard Lang
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, see http://www.gnu.org/licenses
// or write to the Free Software Foundation,Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301  USA
//---------------------------------------------------------------------------------------------

//----------------------------------------------------
// Wishbone Adresses of the Debugging Peripherals
//----------------------------------------------------

#include "io.h"

// Debug Interface
#define DEBUGIF_BASE 0xffffff40
#define DEBUGIF_ID         0x00
#define DEBUGIF_STATUS     0x04
#define DEBUGIF_TF_THBITS  0x08
#define DEBUGIF_REMOVE_WFI 0x0C
#define DEBUGIF_IJ_REGV    0x10
#define DEBUGIF_IJ_ACTIVE  0x14
#define DEBUGIF_IJ_INST    0x18
#define DEBUGIF_DTH_PC     0x20
#define DEBUGIF_DTH_THNO   0x24
#define DEBUGIF_ETH_PC     0x28
#define DEBUGIF_ETH_THNO   0x2C
#define DEBUGIF_HTH_PC     0x30
#define DEBUGIF_HTH_THNO   0x34

#define DEBUGIF_ID_VALUE   0xdb1f0000
#define DEBUGIF_ACTIVE_BIT (1<<31)

// Launcher
#define LAUNCHER_BASE 0xffffffe0
#define LAUNCHER_STATUS 0x00
#define LAUNCHER_THREAD 0x04
#define LAUNCHER_PC     0x08
#define LAUNCHER_THNO   0x0C

//----------------------------------------------------
// Macros for generating injection commands
//----------------------------------------------------
#define LUI(rd,value)       ( 0x00000037 | ((value)&0xfffff000)  | ((rd&0x1f)<<7) )
#define ADDI(rd,rs1,value)  ( 0x00000013 | (((value)&0xfff)<<20) | ((rs1&0x1f)<<15) | ((rd&0x1f)<<7) )
#define LW(rd,rs1,value)    ( 0x00002003 | (((value)&0xfff)<<20) | ((rs1&0x1f)<<15) | ((rd&0x1f)<<7) )
#define LB(rd,rs1,value)    ( 0x00000003 | (((value)&0xfff)<<20) | ((rs1&0x1f)<<15) | ((rd&0x1f)<<7) )
#define SW(rs2,rs1,value)   ( 0x00002023 | (((value)&0xfe0)<<20) | ((rs2&0x1f)<<20) | ((rs1&0x1f)<<15) | ((value&0x1f)<<7) )
#define SB(rs2,rs1,value)   ( 0x00000023 | (((value)&0xfe0)<<20) | ((rs2&0x1f)<<20) | ((rs1&0x1f)<<15) | ((value&0x1f)<<7) )
#define JALR(rd,offset,rs1) ( 0x00000067 | (((offset)&0xfff)<<20) | ((rs1&0x1f)<<15) | ((rd&0x1f)<<7) )

#define ITERATIONS 10

//static unsigned int current_thread = StartThread;

int riscv_Inject(unsigned int Inst, unsigned int thread) {
  unsigned int regval;
  int error=0;
  int i;
  while (1) {
    if (0==in32(DEBUGIF_BASE+DEBUGIF_IJ_ACTIVE)) {
      // Injektion für gewuenschten thread setzen
      out32(DEBUGIF_BASE+DEBUGIF_IJ_ACTIVE,DEBUGIF_ACTIVE_BIT+thread);
      // Ueberpruefen, ob dies gelungen ist
      if (DEBUGIF_ACTIVE_BIT+thread==in32(DEBUGIF_BASE+DEBUGIF_IJ_ACTIVE)) { break; }
    }
  }
  out32(DEBUGIF_BASE+DEBUGIF_IJ_INST,Inst);                    // Instruction setzen
  out32(DEBUGIF_BASE+DEBUGIF_TF_THBITS,1<<thread);             // Threadfilter: thread herausfiltern
  //
  do {
    // Launcher mit eigener ThId belegen (Versuch).
    // Der Wert ist egal, da die Hardware die ThId des aktuellen Threads schreibt und den Datenwert ignoriert
    out32(LAUNCHER_BASE+LAUNCHER_THREAD,0);
    // Wenn Belegen erfolgreich war, dann muss Bit0 im Status-Register zu 1 gesetzt sein
    if (1==(in32(LAUNCHER_BASE+LAUNCHER_STATUS)&1)) { break; } // Thread hat es geschafft den Launcher zu belegen
  } while (1);
  // PC zum Launchen muss nicht gesetzt werden. Der PC-Wert ist egal, da der injizierten Befehl verwendet wird.
  out32(LAUNCHER_BASE+LAUNCHER_THNO,thread);                   // thread setzen und Token launchen
  // Warten bis Thread-ID zurück
  for (i=0; i<ITERATIONS; i++) {
    regval = in32(DEBUGIF_BASE+DEBUGIF_STATUS);
    if (1==(regval&1)) { break; }
  }
  if (ITERATIONS==i) { error=-1; }
  out32(DEBUGIF_BASE+DEBUGIF_DTH_THNO,0);  // Injection Token konsumieren
  out32(DEBUGIF_BASE+DEBUGIF_IJ_ACTIVE,0); // Injektion abschalten
  return error;
}

#if 0
static int fgmt_Inject(unsigned int Inst) {
  unsigned int regval;
  int error=0;
  int i;
  out32(DEBUGIF_BASE+DEBUGIF_IJ_ACTIVE,DEBUGIF_ACTIVE_BIT+current_thread); // Injektion für current_thread setzen
  out32(DEBUGIF_BASE+DEBUGIF_IJ_INST,Inst);                                // Instruction setzen
  out32(DEBUGIF_BASE+DEBUGIF_TF_THBITS,1<<current_thread);                 // Threadfilter: "current_thread" herausfiltern
  //out32(LAUNCHER_BASE+LAUNCHER_THNO,current_thread);                       // Instruktion launchen
  fgmt_Launch();
  // Warten bis Thread-ID zurück
  for (i=0; i<ITERATIONS; i++) {
    regval = in32(DEBUGIF_BASE+DEBUGIF_STATUS);
    if (1==(regval&1)) { break; }
  }
  if (ITERATIONS==i) {
    // Fehler: Thread Token nicht zurückgekommen
    out32(DEBUGIF_BASE+DEBUGIF_TF_THBITS,0);    // Threadfilter abschalten ****
    error = -1;
  } else { 
    out32(DEBUGIF_BASE+DEBUGIF_DTH_THNO,0);     // Injection Token konsumieren
  }
#if ERRCHECK>0
  regval = in32(DEBUGIF_BASE+DEBUGIF_STATUS); // Prüfen ob Injection Token konsumieret ist (kann weg)
  if ((regval&1)!=0) { return -2; }           // Fehler, falls nicht konsumiert (kann weg)
#endif
  out32(DEBUGIF_BASE+DEBUGIF_IJ_ACTIVE,0);    // Injektion abschalten
  return error;
}
#endif

void write_riscv_register(unsigned int regno, unsigned int value, unsigned int thread) {
  int error=0;
  // Injection-Befehl setzen und über Launcher ausfuehren
  // if (0<(value&0x800)) { Inst = LUI(regno,value+0x1000); } else { Inst = LUI(regno,value); }
  error += riscv_Inject((0<(value&0x800)) ? LUI(regno,value+0x1000) : LUI(regno,value),thread); // Instruktion "lui regno, value" injizieren
  error += riscv_Inject(ADDI(regno,regno,value),thread);                                        // Instruktion "addi regno,regno,regno" injizieren
  return;
}

#define SP 2
#if 0
// alter code
void riscv_Launch(void* pc, unsigned int thread, void* stack) {
  write_riscv_register(SP, (unsigned int) stack, thread);
  out32(LAUNCHER_BASE+LAUNCHER_PC,(unsigned int) pc); // "current_pc" zum Launchen setzen
  out32(LAUNCHER_BASE+LAUNCHER_THNO,thread);          // "current_thread" setzen und Token launchen
  return;
}
#else
unsigned int startup_code[7];
void riscv_Launch(void* pc, unsigned int thread, void* stack) {
#if 0
  unsigned int new_sp = ((unsigned int) stack)&0xffffff00;
  unsigned int stack_upper = ((unsigned int) stack)&0xffffff00;
  unsigned int stack_lower = ((unsigned int) stack)&0x000000ff;
  asm (
          "mv sp, %0"
          : /* kein Output */
          : "r"(new_sp) // Input Operanden
      );
  //asm ("lui sp,%0\n\t"   : : (stack_upper));
  //asm ("addi sp, sp, %0" : : (stack_lower));
#elif 1
  unsigned int value; 
  // setup stack pointer
  value = (unsigned int) stack;
  startup_code[0] = (0<(value&0x800)) ? LUI(SP,value+0x1000) : LUI(SP,value);
  startup_code[1] = ADDI(SP,SP,value);
  // load thread start address into register 1
  value = (unsigned int) pc;
  startup_code[2] = (0<(value&0x800)) ? LUI(1,value+0x1000) : LUI(1,value);
  startup_code[3] = ADDI(1,1,value);
  // Call thread
  startup_code[4] = JALR(1,0,1);
  // On thread return jump to 0xfffffffc to terminate thread
  startup_code[5] = JALR(0,0xfffffffc,0);
#else
  write_riscv_register(SP, (unsigned int) stack, thread);
#endif
  do {
    out32(LAUNCHER_BASE+LAUNCHER_THREAD,0);                // Launcher mit eigener ThId belegen (Versuch)
    // Wenn Belegen erfolgreich war, dann muss Bit0 im Status-Register zu 1 gesetzt sein
    if (1==(in32(LAUNCHER_BASE+LAUNCHER_STATUS)&1)) { break; } // Thread hat es geschafft den Launcher zu belegen
  } while (1);
  // out32(LAUNCHER_BASE+LAUNCHER_PC,(unsigned int) pc);  // pc zum Launchen setzen
  out32(LAUNCHER_BASE+LAUNCHER_PC,(unsigned int) startup_code);
  out32(LAUNCHER_BASE+LAUNCHER_THNO,thread);           // thread setzen und Token launchen
  for (volatile int i=0; i<5; i++) { /* wait some time that the thread can launch */ }
}
#endif


