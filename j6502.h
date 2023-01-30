//
// Created by josh on 1/22/23.
//


#ifndef EMU6502_J6502_H
#define EMU6502_J6502_H

#include <cstdint>
#include <string>
#include <vector>
#include <map>


typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

// Get Forward Declared
class Bus;

class j6502 {
public:
    enum FLAGS {
        C = (1 << 0), // Carry Bit
        Z = (1 << 1), // Zero
        I = (1 << 2), // Disable Interrupts
        D = (1 << 3), // Decimal Mode (unimplemented)
        B = (1 << 4), // Break
        U = (1 << 5), // Unused
        V = (1 << 6), // Overflow
        N = (1 << 7), // Negative
    };
    j6502();
    ~j6502();
    u8 status = 0x00; // Status Register
    u8 a = 0x00; // Accumulator
    u8 x = 0x00; // X Register
    u8 y = 0x00; // Y Register
    u8 sp = 0x00; // Stack Pointer
    u16 pc = 0x0000; // Program Counter
    // Facilitate emulation
    u8 fetched = 0x00;
    u16 addr_abs = 0x0000;
    u16 addr_rel = 0x00;
    u8 opcode = 0x00;
    u8 cycles = 0;
    uint32_t clock_count = 0;
    u16 temp = 0x00;

    Bus *bus = nullptr;
    struct INSTRUCTION {
        std::string name;

        u8 (j6502::*operate)(void) = nullptr;

        u8 (j6502::*addrmode)(void) = nullptr;

        u8 cycles = 0;
    };
    std::vector<INSTRUCTION> lookup;

    void ConnectBus(Bus *n){ bus = n; };
    u8 read(u16 addr);
    void write(u16 addr, u8 d);
    u8 GetFlag(FLAGS f);
    void SetFlag(FLAGS f, bool v);
    u8 fetch();

    std::map<u16, std::string> disassemble(u16 start, u16 stop);

    void nmi();
    void irq();
    void reset();
    void clock();
    u8 CLC();u8 CLD();
    u8 ZP0();u8 ZPX();u8 ABS();u8 REL();
    u8 ZPY();u8 IMM();u8 IMP();u8 ABX();
    u8 STX();u8 STA();u8 SED();u8 SEC();
    u8 RTS();u8 RTI();u8 ROR();u8 ROL();
    u8 PLP();u8 PLA();u8 PHP();u8 PHA();
    u8 BVS();u8 BVC();u8 BRK();u8 BPL();
    u8 BNE();u8 BMI();u8 BIT();u8 BEQ();
    u8 BCS();u8 BCC();u8 ASL();u8 SBC();
    u8 ADC();u8 AND();u8 DEY();u8 DEX();
    u8 DEC();u8 CPY();u8 EOR();u8 INC();
    u8 INX();u8 INY();u8 JMP();u8 JSR();
    u8 LDA();u8 LDX();u8 LDY();u8 LSR();
    u8 ORA();u8 ABY();u8 IND();u8 IZX();
    u8 IZY();u8 CLI();u8 CLV();u8 CMP();
    u8 CPX();u8 SEI();u8 XXX();u8 NOP();

    u8 TYA();u8 TXS();u8 TXA();
    u8 TSX();
    u8 TAY();u8 TAX();u8 STY();

    bool complete();
};


#endif //EMU6502_J6502_H
