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

#include <stdint.h>
#include <string.h>
#include "io.h"
#include "main.h"

#define IRC_BASE 0xffffffd0
#define IRC_STATUS      0x00
#define IRC_INT_ACTIVE  0x04
#define IRC_INT_PENDING 0x08
#define IRC_INT_ENABLE  0x0C

#define GPIO_BASE 0xfffffec0
#define GPIO_PINS  0x00
#define GPIO_DO    0x04
#define GPIO_DIR   0x08
#define GPIO_IE_FF 0x10
#define GPIO_IP_FF 0x14
#define GPIO_IE_SF 0x18
#define GPIO_IP_SF 0x1C

#define TIMER_BASE 0xfffffee0
#define TIMER_PERIODE      0x00
#define TIMER_SCHWELLE     0x04
#define TIMER_ZAEHLERSTAND 0x08
#define TIMER_KONTROLL     0x0C
#define TIMER_STATUS       0x10

#define PERIODE 10000000

#define UART_BASE 0xfffffeb0

#define SYSTEM_FREQUENCY 50000000 // 50 MHz
#define UART_BAUDRATE  115200
#define UART_DATABITS  8

void riscv_Launch(void* pc,unsigned int thread, void* stack);

unsigned int UP(unsigned int v) {
  return v+1;
}

#define SYSTEM_FREQUENCY 50000000

unsigned int main_val;

//---------------------------------------------------------------------
void ThreadF(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int ThreadF_Stack[0x40]; // Stack fuer ThreadF
//---------------------------------------------------------------------
void ThreadE(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int ThreadE_Stack[0x40]; // Stack fuer ThreadE
//---------------------------------------------------------------------
void ThreadD(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int ThreadD_Stack[0x40]; // Stack fuer ThreadD
//---------------------------------------------------------------------
void ThreadC(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int ThreadC_Stack[0x40]; // Stack fuer ThreadC
//---------------------------------------------------------------------
void ThreadB(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int ThreadB_Stack[0x40]; // Stack fuer ThreadB
//---------------------------------------------------------------------
void ThreadA(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int ThreadA_Stack[0x40]; // Stack fuer ThreadA
//---------------------------------------------------------------------
void Thread9(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int Thread9_Stack[0x40]; // Stack fuer Thread9
//---------------------------------------------------------------------
void Thread8(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int Thread8_Stack[0x40]; // Stack fuer Thread8
//---------------------------------------------------------------------
void Thread7(void) {
    unsigned char val=0;
  while (1) {
    for (int i=0; i<800000; i++) {}
    val = val+1;
    if (val==0x10) { val=0; }
  }
}
unsigned int Thread7_Stack[0x40]; // Stack fuer Thread7
//---------------------------------------------------------------------
void Thread6(void) {
  unsigned char val=0;
  for (int i=0; i<200000; i++) {
    val = val+1;
    if (val==0x10) { val=0; }
  }
  val = val+1;
}
unsigned int Thread6_Stack[0x40]; // Stack fuer Thread6
//---------------------------------------------------------------------
unsigned char th5_val;
void Thread5(void) {
  unsigned char val=0;
  while (1) {
    for (int i=0; i<800000;) {
      i++;
    }
    val = val+1;
    if (val>=0x10) { val=0; }
    th5_val = val;
  }
}
unsigned int Thread5_Stack[0x40]; // Stack fuer Thread5
//---------------------------------------------------------------------
unsigned char th4_val;
void Thread4(void) {
  unsigned char val=0;
  while (1) {
    for (int i=0; i<700000;) {
      i++;
    }
    val = val+1;
    if (val>=0x10) { val=0; }
    th4_val = val;
  }
}
unsigned int Thread4_Stack[0x40]; // Stack fuer Thread4
//---------------------------------------------------------------------
unsigned char th3_val;
void Thread3(void) {
  unsigned char val=0;
  while (1) {
    for (int i=0; i<600000;) {
      i++;
    }
    val = val+1;
    if (val>=0x10) { val=0; }
    th3_val = val;
  }
}
unsigned int Thread3_Stack[0x40]; // Stack fuer Thread3
//---------------------------------------------------------------------
void Thread2(void) { // Interrupt thread
  unsigned int SiebenSeg_val;
  //
  out32(TIMER_BASE+TIMER_KONTROLL,0b10);       // Timer anhalten
  out32(TIMER_BASE+TIMER_PERIODE,PERIODE-1);   // Periode setzen in Taktperioden
  out32(TIMER_BASE+TIMER_SCHWELLE,PERIODE>>1); // Tastverhaeltnis 50%
  in32(TIMER_BASE+TIMER_STATUS);               // Interrupt-FlipFlop ruecksetzen
  out32(TIMER_BASE+TIMER_KONTROLL,0b01);       // Timer-Interrupt freigeben
  //
  out32(IRC_BASE+IRC_INT_ENABLE, 0b10);        // Timer-Interrupt im IRC freigeben
  //
  out32(GPIO_BASE+GPIO_DIR,0x1fffe0);          // GPIO-Richtung auf Ausgang
  out32(GPIO_BASE+GPIO_DO,0x43210);
  //
  while(1) {
    asm ("wfi");                               // Auf Timer-Interrupt warten
    in32(TIMER_BASE+TIMER_STATUS);             // Timer-Interrupt ruecksetzen
    SiebenSeg_val = (th5_val<<12) | (th4_val<<8) | (th3_val<<4) | (main_val&0xf);
    out32(GPIO_BASE+GPIO_DO,SiebenSeg_val<<5); // Wert ausgeben
  }
}
unsigned int Thread2_Stack[0x40]; // Stack fuer Thread2
//---------------------------------------------------------------------
int main( void ) {
  //
  riscv_Launch(Thread2,0x2, Thread2_Stack);
  riscv_Launch(Thread3,0x3, Thread3_Stack);
  riscv_Launch(Thread4,0x4, Thread4_Stack);
  riscv_Launch(Thread5,0x5, Thread5_Stack);
  riscv_Launch(Thread6,0x6, Thread6_Stack);
  riscv_Launch(Thread7,0x7, Thread7_Stack);
  riscv_Launch(Thread8,0x8, Thread8_Stack);
  riscv_Launch(Thread9,0x9, Thread9_Stack);
  riscv_Launch(ThreadA,0xA, ThreadA_Stack);
  riscv_Launch(ThreadB,0xB, ThreadB_Stack);
  riscv_Launch(ThreadC,0xC, ThreadC_Stack);
  riscv_Launch(ThreadD,0xD, ThreadD_Stack);
  riscv_Launch(ThreadE,0xE, ThreadE_Stack);
  riscv_Launch(ThreadF,0xF, ThreadF_Stack);
  // Endlessly increment a variable.
  unsigned int counter;
  counter=STARTWERT;
  while ( 1 ) {
    counter++;
    main_val = counter;
    counter=UP(counter);
    main_val = counter;
  }
  return 0;
}
//---------------------------------------------------------------------
