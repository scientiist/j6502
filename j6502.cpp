//
// Created by josh on 1/22/23.
//

#include <map>
#include "j6502.h"
#include "Bus.h"

using a = j6502;

j6502::j6502() {

    lookup = {
            {"BRK", &a::BRK, &a::IMM, 7},
            {"ORA", &a::ORA, &a::IZX, 6},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 3},
            {"ORA", &a::ORA, &a::ZP0, 3},
            {"ASL", &a::ASL, &a::ZP0, 5},
            {"???", &a::XXX, &a::IMP, 5},
            {"PHP", &a::PHP, &a::IMP, 3},
            {"ORA", &a::ORA, &a::IMM, 2},
            {"ASL", &a::ASL, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::NOP, &a::IMP, 4},
            {"ORA", &a::ORA, &a::ABS, 4},
            {"ASL", &a::ASL, &a::ABS, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"BPL", &a::BPL, &a::REL, 2},
            {"ORA", &a::ORA, &a::IZY, 5},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 4},
            {"ORA", &a::ORA, &a::ZPX, 4},
            {"ASL", &a::ASL, &a::ZPX, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"CLC", &a::CLC, &a::IMP, 2},
            {"ORA", &a::ORA, &a::ABY, 4},
            {"???", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 7},
            {"???", &a::NOP, &a::IMP, 4},
            {"ORA", &a::ORA, &a::ABX, 4},
            {"ASL", &a::ASL, &a::ABX, 7},
            {"???", &a::XXX, &a::IMP, 7},
            {"JSR", &a::JSR, &a::ABS, 6},
            {"AND", &a::AND, &a::IZX, 6},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"BIT", &a::BIT, &a::ZP0, 3},
            {"AND", &a::AND, &a::ZP0, 3},
            {"ROL", &a::ROL, &a::ZP0, 5},
            {"???", &a::XXX, &a::IMP, 5},
            {"PLP", &a::PLP, &a::IMP, 4},
            {"AND", &a::AND, &a::IMM, 2},
            {"ROL", &a::ROL, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 2},
            {"BIT", &a::BIT, &a::ABS, 4},
            {"AND", &a::AND, &a::ABS, 4},
            {"ROL", &a::ROL, &a::ABS, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"BMI", &a::BMI, &a::REL, 2},
            {"AND", &a::AND, &a::IZY, 5},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 4},
            {"AND", &a::AND, &a::ZPX, 4},
            {"ROL", &a::ROL, &a::ZPX, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"SEC", &a::SEC, &a::IMP, 2},
            {"AND", &a::AND, &a::ABY, 4},
            {"???", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 7},
            {"???", &a::NOP, &a::IMP, 4},
            {"AND", &a::AND, &a::ABX, 4},
            {"ROL", &a::ROL, &a::ABX, 7},
            {"???", &a::XXX, &a::IMP, 7},
            {"RTI", &a::RTI, &a::IMP, 6},
            {"EOR", &a::EOR, &a::IZX, 6},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 3},
            {"EOR", &a::EOR, &a::ZP0, 3},
            {"LSR", &a::LSR, &a::ZP0, 5},
            {"???", &a::XXX, &a::IMP, 5},
            {"PHA", &a::PHA, &a::IMP, 3},
            {"EOR", &a::EOR, &a::IMM, 2},
            {"LSR", &a::LSR, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 2},
            {"JMP", &a::JMP, &a::ABS, 3},
            {"EOR", &a::EOR, &a::ABS, 4},
            {"LSR", &a::LSR, &a::ABS, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"BVC", &a::BVC, &a::REL, 2},
            {"EOR", &a::EOR, &a::IZY, 5},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 4},
            {"EOR", &a::EOR, &a::ZPX, 4},
            {"LSR", &a::LSR, &a::ZPX, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"CLI", &a::CLI, &a::IMP, 2},
            {"EOR", &a::EOR, &a::ABY, 4},
            {"???", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 7},
            {"???", &a::NOP, &a::IMP, 4},
            {"EOR", &a::EOR, &a::ABX, 4},
            {"LSR", &a::LSR, &a::ABX, 7},
            {"???", &a::XXX, &a::IMP, 7},
            {"RTS", &a::RTS, &a::IMP, 6},
            {"ADC", &a::ADC, &a::IZX, 6},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 3},
            {"ADC", &a::ADC, &a::ZP0, 3},
            {"ROR", &a::ROR, &a::ZP0, 5},
            {"???", &a::XXX, &a::IMP, 5},
            {"PLA", &a::PLA, &a::IMP, 4},
            {"ADC", &a::ADC, &a::IMM, 2},
            {"ROR", &a::ROR, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 2},
            {"JMP", &a::JMP, &a::IND, 5},
            {"ADC", &a::ADC, &a::ABS, 4},
            {"ROR", &a::ROR, &a::ABS, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"BVS", &a::BVS, &a::REL, 2},
            {"ADC", &a::ADC, &a::IZY, 5},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 4},
            {"ADC", &a::ADC, &a::ZPX, 4},
            {"ROR", &a::ROR, &a::ZPX, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"SEI", &a::SEI, &a::IMP, 2},
            {"ADC", &a::ADC, &a::ABY, 4},
            {"???", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 7},
            {"???", &a::NOP, &a::IMP, 4},
            {"ADC", &a::ADC, &a::ABX, 4},
            {"ROR", &a::ROR, &a::ABX, 7},
            {"???", &a::XXX, &a::IMP, 7},
            {"???", &a::NOP, &a::IMP, 2},
            {"STA", &a::STA, &a::IZX, 6},
            {"???", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 6},
            {"STY", &a::STY, &a::ZP0, 3},
            {"STA", &a::STA, &a::ZP0, 3},
            {"STX", &a::STX, &a::ZP0, 3},
            {"???", &a::XXX, &a::IMP, 3},
            {"DEY", &a::DEY, &a::IMP, 2},
            {"???", &a::NOP, &a::IMP, 2},
            {"TXA", &a::TXA, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 2},
            {"STY", &a::STY, &a::ABS, 4},
            {"STA", &a::STA, &a::ABS, 4},
            {"STX", &a::STX, &a::ABS, 4},
            {"???", &a::XXX, &a::IMP, 4},
            {"BCC", &a::BCC, &a::REL, 2},
            {"STA", &a::STA, &a::IZY, 6},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 6},
            {"STY", &a::STY, &a::ZPX, 4},
            {"STA", &a::STA, &a::ZPX, 4},
            {"STX", &a::STX, &a::ZPY, 4},
            {"???", &a::XXX, &a::IMP, 4},
            {"TYA", &a::TYA, &a::IMP, 2},
            {"STA", &a::STA, &a::ABY, 5},
            {"TXS", &a::TXS, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 5},
            {"???", &a::NOP, &a::IMP, 5},
            {"STA", &a::STA, &a::ABX, 5},
            {"???", &a::XXX, &a::IMP, 5},
            {"???", &a::XXX, &a::IMP, 5},
            {"LDY", &a::LDY, &a::IMM, 2},
            {"LDA", &a::LDA, &a::IZX, 6},
            {"LDX", &a::LDX, &a::IMM, 2},
            {"???", &a::XXX, &a::IMP, 6},
            {"LDY", &a::LDY, &a::ZP0, 3},
            {"LDA", &a::LDA, &a::ZP0, 3},
            {"LDX", &a::LDX, &a::ZP0, 3},
            {"???", &a::XXX, &a::IMP, 3},
            {"TAY", &a::TAY, &a::IMP, 2},
            {"LDA", &a::LDA, &a::IMM, 2},
            {"TAX", &a::TAX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 2},
            {"LDY", &a::LDY, &a::ABS, 4},
            {"LDA", &a::LDA, &a::ABS, 4},
            {"LDX", &a::LDX, &a::ABS, 4},
            {"???", &a::XXX, &a::IMP, 4},
            {"BCS", &a::BCS, &a::REL, 2},
            {"LDA", &a::LDA, &a::IZY, 5},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 5},
            {"LDY", &a::LDY, &a::ZPX, 4},
            {"LDA", &a::LDA, &a::ZPX, 4},
            {"LDX", &a::LDX, &a::ZPY, 4},
            {"???", &a::XXX, &a::IMP, 4},
            {"CLV", &a::CLV, &a::IMP, 2},
            {"LDA", &a::LDA, &a::ABY, 4},
            {"TSX", &a::TSX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 4},
            {"LDY", &a::LDY, &a::ABX, 4},
            {"LDA", &a::LDA, &a::ABX, 4},
            {"LDX", &a::LDX, &a::ABY, 4},
            {"???", &a::XXX, &a::IMP, 4},
            {"CPY", &a::CPY, &a::IMM, 2},
            {"CMP", &a::CMP, &a::IZX, 6},
            {"???", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"CPY", &a::CPY, &a::ZP0, 3},
            {"CMP", &a::CMP, &a::ZP0, 3},
            {"DEC", &a::DEC, &a::ZP0, 5},
            {"???", &a::XXX, &a::IMP, 5},
            {"INY", &a::INY, &a::IMP, 2},
            {"CMP", &a::CMP, &a::IMM, 2},
            {"DEX", &a::DEX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 2},
            {"CPY", &a::CPY, &a::ABS, 4},
            {"CMP", &a::CMP, &a::ABS, 4},
            {"DEC", &a::DEC, &a::ABS, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"BNE", &a::BNE, &a::REL, 2},
            {"CMP", &a::CMP, &a::IZY, 5},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 4},
            {"CMP", &a::CMP, &a::ZPX, 4},
            {"DEC", &a::DEC, &a::ZPX, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"CLD", &a::CLD, &a::IMP, 2},
            {"CMP", &a::CMP, &a::ABY, 4},
            {"NOP", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 7},
            {"???", &a::NOP, &a::IMP, 4},
            {"CMP", &a::CMP, &a::ABX, 4},
            {"DEC", &a::DEC, &a::ABX, 7},
            {"???", &a::XXX, &a::IMP, 7},
            {"CPX", &a::CPX, &a::IMM, 2},
            {"SBC", &a::SBC, &a::IZX, 6},
            {"???", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"CPX", &a::CPX, &a::ZP0, 3},
            {"SBC", &a::SBC, &a::ZP0, 3},
            {"INC", &a::INC, &a::ZP0, 5},
            {"???", &a::XXX, &a::IMP, 5},
            {"INX", &a::INX, &a::IMP, 2},
            {"SBC", &a::SBC, &a::IMM, 2},
            {"NOP", &a::NOP, &a::IMP, 2},
            {"???", &a::SBC, &a::IMP, 2},
            {"CPX", &a::CPX, &a::ABS, 4},
            {"SBC", &a::SBC, &a::ABS, 4},
            {"INC", &a::INC, &a::ABS, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"BEQ", &a::BEQ, &a::REL, 2},
            {"SBC", &a::SBC, &a::IZY, 5},
            {"???", &a::XXX, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 8},
            {"???", &a::NOP, &a::IMP, 4},
            {"SBC", &a::SBC, &a::ZPX, 4},
            {"INC", &a::INC, &a::ZPX, 6},
            {"???", &a::XXX, &a::IMP, 6},
            {"SED", &a::SED, &a::IMP, 2},
            {"SBC", &a::SBC, &a::ABY, 4},
            {"NOP", &a::NOP, &a::IMP, 2},
            {"???", &a::XXX, &a::IMP, 7},
            {"???", &a::NOP, &a::IMP, 4},
            {"SBC", &a::SBC, &a::ABX, 4},
            {"INC", &a::INC, &a::ABX, 7},
            {"???", &a::XXX, &a::IMP, 7},
    };
}

j6502::~j6502() {

}

// Reads an 8bit byte from the bus, located at the specified 16-bit address
u8 j6502::read(u16 addr) { return bus->read(addr, false); }

void j6502::write(u16 addr, u8 d) { bus->write(addr, d); }

u8 j6502::GetFlag(FLAGS f)
{
    return ((status & f) > 0) ? 1 : 0;
}

void j6502::SetFlag(FLAGS F, bool v) {
    if (v)
        status |= F;
    else
        status &= ~F;
}

u8 j6502::fetch() {
    if (!(lookup[opcode].addrmode == &j6502::IMP))
        fetched = read(addr_abs);
    return fetched;
}

#pragma region Addressing Modes

// Address Mode: Implied
// There is no addtional data required for this instruction.
// This instruction does something very simple like set a status bit
u8 j6502::IMP() {
    fetched = a;
    return 0;
}

// Address Mode: Immediate
// The instruction expects the next byte to be used as a value
// So prep the read address to point to next byte
u8 j6502::IMM() {
    addr_abs = pc++;
    return 0;
}

// Address Mode: Zero Page
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first 0xFF bytes of address range. Clearly this only requires one
// byte instead of the usual two.
u8 j6502::ZP0() {
    addr_abs = read(pc);
    pc++;
    addr_abs &= 0x00FF;
    return 0;
}

// Address Mode: Zero Page X offset
// Contents of X register is added to the supplied single bit address.
// Useful for iterating through ranges within the first page;
u8 j6502::ZPX() {
    addr_abs = (read(pc) + x);
    pc++;
    addr_abs &= 0x00FF;
    return 0;
}

// Address Mode: Zero Page Y offset
// Ditto, but uses Y register for offset
u8 j6502::ZPY() {
    addr_abs = (read(pc) + y);
    pc++;
    addr_abs &= 0x00FF;
    return 0;
}

// Address Mode: Relative
// Exclusive to branch instructions. Address must reside within -128 to +127
// of the branch instruction, i.e. you can't directly branch to any address
// in the addressable range
u8 j6502::REL() {
    addr_rel = read(pc);
    pc++;
    if (addr_rel & 0x80)
        addr_rel |= 0xFF00;
    return 0;
}

// Address Mode: Absolute
// A full 16-bit address is loaded and used
u8 j6502::ABS() {
    u16 lo = read(pc);
    pc++;
    u16 hi = read(pc);
    pc++;
    addr_abs = (hi << 8) | lo;
    return 0;
}

// Address Mode: Absolute with X offset
// Contents of X register is added to the supplied two byte address
// if the resulting address changes the page, an additional clock cycle is required
u8 j6502::ABX() {
    u16 lo = read(pc);
    pc++;
    u16 hi = read(pc);
    pc++;

    addr_abs = (hi << 8) | lo;
    addr_abs += x;

    if ((addr_abs & 0xFF00) != (hi << 8))
        return 1;
    else
        return 0;
}

// Address Mode: Absolute with Y offset
// Contents of Y register is added to the supplied two byte address
// if the resulting address changes the page, an additional clock cycle is required
u8 j6502::ABY() {
    u16 lo = read(pc);
    pc++;
    u16 hi = read(pc);
    pc++;
    addr_abs = (hi << 8) | lo;
    addr_abs += y;

    if ((addr_abs & 0xFF00) != (hi << 8))
        return 1;
    else
        return 0;
}

// REEEEEEE: Pointers!!!

// Address Mode: Indirect
// The supplied 16-bit address is read to get the actual 16-bit address.
// This instruction is unusual in that it has a hardware bug.
// to emulate it "accurately", we also emulate the bug
// If the low byte of the given address is 0xFF, then to read the high byte
// of the actual address we need to cross a page boundary
// This doesn't actually work on the chip as designed, instead it wraps back around
// in the same page, yielding an invalid address
u8 j6502::IND() {
    u16 ptr_lo = read(pc);
    pc++;
    u16 ptr_hi = read(pc);
    pc++;

    u16 ptr = (ptr_hi << 8) | ptr_lo;

    if (ptr_lo == 0x00FF) // Simulate Page Boundary Hardware Bug
        addr_abs = (read(ptr & 0xFF00) << 8) | read(ptr + 0);
    else
        addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
}

// Address Mode: Indirect X
// The supplied 8-bit address is offset by X Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
u8 j6502::IZX() {
    u16 t = read(pc);
    pc++;

    u16 lo = read((u16) (t + (u16) x) & 0x00FF);
    u16 hi = read((u16) (t + (u16) x + 1) & 0x00FF);

    addr_abs = (hi << 8) | lo;

    return 0;
}

// Address Mode: Indirect Y
// The supplied 8-bit address indexes a location in page 0x00. From
// here the actual 16-bit address is read, and the contents of
// Y Register is added to it to offset it. If the offset causes a
// change in page then an additional clock cycle is required.
u8 j6502::IZY() {
    u16 t = read(pc);
    pc++;

    u16 lo = read(t & 0x00FF);
    u16 hi = read((t + 1) & 0x00FF);

    addr_abs = (hi << 8) | lo;
    addr_abs += y;

    if ((addr_abs & 0xFF00) != (hi << 8))
        return 1;
    else
        return 0;
}


#pragma endregion

#pragma region Opcodes

// Instruction: Clear Carry Flag
// Function: C = 0
u8 j6502::CLC() {
    SetFlag(C, false);
    return 0;
}

// Instruction: Clear Decimal Flag
// Function: D = 0
u8 j6502::CLD() {
    SetFlag(D, false);
    return 0;
}

// Instruction: Disable Interrupts / Clear Interrupt Flag
// Function: I = 0
u8 j6502::CLI() {
    SetFlag(I, false);
    return 0;
}

// Instruction: Clear Overflow Flag
// Function: V = 0
u8 j6502::CLV() {
    SetFlag(V, false);
    return 0;
}

// Instruction: Compare Accumulator
// Function: C <- A >= M   Z <- (A - M) == 0
// Flags Out: N, C, Z
u8 j6502::CMP() {
    fetch();
    temp = (u16) a - (u16) fetched;
    SetFlag(C, a >= fetched);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 1;
}

// Instruction: Compare X Register
// Function: C <- X >= M Z <- (X - M) == 0
// Flags Out: N, C, Z
u8 j6502::CPX() {
    fetch();
    temp = (u16) x - (u16) fetched;
    SetFlag(C, x >= fetched);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 0;
}

// Instruction: Compare Y Register
// Function: C <- Y >= M Z <- (Y - M) == 0
// Flags Out: M, C, Z
u8 j6502::CPY() {
    fetch();
    temp = (u16) y - (u16) fetched;
    SetFlag(C, y >= fetched);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 0;

}
// Instruction: Decrement Value at Memory Location
u8 j6502::DEC()
{
    fetch();
    temp = fetched - 1;
    write(addr_abs, temp & 0x00FF);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 0;
}
// Instruction: Decrement X Register
// Function: X = X - 1
// Flags Out: N, Z
u8 j6502::DEX()
{
    x--;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}
// Instruction: Decrement Y Register
// Function Y = Y - 1
// Flags: N, Z
u8 j6502::DEY()
{
    y--;
    SetFlag(Z, y = 0x00);
    SetFlag(N, y & 0x80);
    return 0;
}
// Instruction: Bitwise Logic XOR
u8 j6502::EOR()
{
    fetch();
    a = a ^ fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
}
// Instruction: Increment Value at Memory Locatio
// Function: M = M + 1
// Flags out: N, Z
u8 j6502::INC()
{
    fetch();
    temp = fetched + 1;
    write(addr_abs, temp & 0x00FF);
    SetFlag(Z, (temp & 0x00FF) == 0x00FF);
    SetFlag(N, temp & 0x0080);
    return 0;
}
// Instruction: Increment X Register
// Function: X = X + 1
// Flags Out: N, Z
u8 j6502::INX()
{
    x++;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}
// Instruction: Increment Y Register
// Function: Y = Y + 1
// Flags: N, Z
u8 j6502::INY()
{
    fetch();
    temp = fetched + 1;
    write(addr_abs, temp & 0x00FF);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 0;
}
// Instruction: Jump To Location
// Function: pc = address
u8 j6502::JMP()
{
    pc = addr_abs;
    return 0;
}
// Instruction: Jump To Sub-Routine
// Function: Push current pc to stack, pc = address
u8 j6502::JSR()
{
    pc--;
    write(0x0100 + sp, (pc >> 8) & 0x00FF);
    sp--;
    write(0x0100 + sp, pc & 0x00FF);
    sp--;

    pc = addr_abs;
    return 0;
}
// Instruction: Load The Accumulator
// Function: A = M
// Flags Out: N, Z
u8 j6502::LDA()
{
    fetch();
    a = fetched;
    SetFlag(Z, a = 0x00);
    SetFlag(N, a & 0x80);
    return 1;
}
// Instruction: Load the X Register
// Function: X = M
// Flags Out: N, Z
u8 j6502::LDX()
{
    fetch();
    x = fetched;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 1;
}
// Instruction: Load the Y Register
u8 j6502::LDY()
{
    fetch();
    y = fetched;
    SetFlag(Z, y == 0x00);
    SetFlag(N, y & 0x80);
}

u8 j6502::LSR()
{
    fetch();
    SetFlag(C, fetched & 0x0001);
    temp = fetched >> 1;
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &j6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

u8 j6502::NOP() {
    switch (opcode) {
        case 0x1C:
        case 0x3C:
        case 0x5C:
        case 0x7C:
        case 0xDC:
        case 0xFC:
            return 1;
            break;
    }
    return 0;
}

// Instruction: Bitwise Logic OR
// Function: A = A | M
// Flags out : N, Z
u8 j6502::ORA() {
    fetch();
    a = a | fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 1;
}

// Instruction: Disable Interrupts
// Instruction: Bitwise Logic AND
// Function: A = A & M
// Flags Out: N, Z
u8 j6502::AND() {
    fetch();
    a = a & fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
}

// Instruction: Add with Carry In
// Function: A = A + M + C
// Flags Out: C, V, N, Z
//
// Info: The purpose of this function is to add a value to the accumulator and a carry bit
// If the result is > 255 there is an overflow, thus setting carry bit.
// This allows you to chain together ADC instructions to add numbers larger than 8-bits.
// This in itself is simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow
u8 j6502::ADC() {
    fetch();
    temp = (u16) a + (u16)fetched + (u16)GetFlag(C);
    SetFlag(C, temp > 255);
    SetFlag(Z, (temp & 0x00FF) == 0);
    SetFlag(V, (~((u16)a ^ (u16)fetched) & ((u16)a ^ (u16)temp)) & 0x0080);
    SetFlag(N, temp & 0x80);
    a = temp & 0x00FF;
    return 1;
}

// Instruction: Subtraction with Borrow In
// Function: A = A - M - (1-C)
// Flags out: C, V, N, Z
u8 j6502::SBC() {
    fetch();
    u16 value = ((u16) fetched) ^ 0x00FF;

    temp = (u16) a + (u16) value + (u16) GetFlag(C);
    SetFlag(C, temp > 255);
    SetFlag(Z, (temp & 0x00FF) == 0);
    SetFlag(N, temp & 0x80);
    SetFlag(V, (~((u16) a ^ (u16) fetched) & ((u16) a ^ (u16) temp)) & 0x0080);
    a = temp & 0x00FF;
    return 1;
}

// Instruction: Arithmetic Shift Left
// Function: A = C <- (A << 1) <- 0
// Flags Out: N, Z, C
u8 j6502::ASL() {
    fetch();
    u16 temp = (u16) fetched << 1;
    SetFlag(C, (temp & 0xFF00) > 0);
    SetFlag(Z, (temp & 0x00FF) == 0x00);
    SetFlag(N, temp & 0x80);

    if (lookup[opcode].addrmode = &j6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

// Instruction: Branch if Carry Clear
// Function: if (C==0) pc = address;
u8 j6502::BCC() {
    if (GetFlag(C) == 0) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Carry Set
// Function: if (C==1) pc = address;
u8 j6502::BCS() {
    if (GetFlag(C) == 1) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Equal
// Function: if(Z == 1) pc = address;
u8 j6502::BEQ() {
    if (GetFlag(Z) == 1) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

u8 j6502::BIT() {
    fetch();
    temp = a & fetched;
    SetFlag(Z, (temp & 0x00FF) == 0x00);

}

// Instruction: Branch if Negative
// Function: if (N==1) pc = address
u8 j6502::BMI() {
    if (GetFlag(N) == 1) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Not Equal
// Function: if (Z == 0) pc = address;
u8 j6502::BNE() {
    if (GetFlag(Z) == 0) {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Positive
// Function: if(N == 0) pc = address;
u8 j6502::BPL() {
    if (GetFlag(N) == 0) {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;

    }
    return 0;
}

// Instruction: Break
// Function: Program Sourced Interrupt
u8 j6502::BRK() {
    pc++;
    SetFlag(I, 1);
    write(0x0100 + sp, (pc >> 8) & 0x00FF);
    sp--;
    write(0x0100 + sp, pc & 0x00FF);
    sp--;

    SetFlag(B, 1);
    write(0x0100 + sp, status);
    sp--;
    SetFlag(B, 0);

    pc = (u16) read(0xFFFE) | ((u16) read(0xFFFF) << 8);
    return 0;
}

// Instruction: Branch if overflow clear
// Function: if(V == 0) pc = address
u8 j6502::BVC() {
    if (GetFlag(V) == 0) {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;

    }
    return 0;
}

// Instruction: Branch if overflow set
// Function: if(V == 1) pc = address
u8 j6502::BVS() {
    if (GetFlag(V) == 1) {
        cycles++;
        addr_abs = pc + addr_rel;
        if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
            cycles++;
        }
        pc = addr_abs;
    }
    return 0;
}

// Instruction: Push accumulator to stack
// Function: A - > Stack
u8 j6502::PHA() {
    write(0x0100 + sp, a);
    sp--;
    return 0;
}
// Instruction: Push Status Register to Stack
// Function: status -> stack
// Note: Break flag is set to 1 before push
u8 j6502::PHP() {

}
// Instruction: Pop accumulator off Stack
// Function: A <- stack
// Flags out: N, Z
u8 j6502::PLA() {
    sp++;
    a = read(0x100 + sp);
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 0;
}
// Instruction: Pop Status Register off Stack
// Function: Status <- stack
u8 j6502::PLP()
{
    sp++;
    status = read(0x0100+sp);
    SetFlag(U, 1);
    return 0;
}

u8 j6502::ROL()
{
    fetch();
    temp = (u16)(fetched << 1) | GetFlag(C);
    SetFlag(C, temp & 0xFF00);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &j6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

u8 j6502::ROR()
{
    fetch();
    temp = (u16)(GetFlag(C) << 7) | (fetched >> 1);
    SetFlag(C, fetched & 0x01);
    SetFlag(Z, (temp & 0x00FF) == 0x00);
    SetFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &j6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

u8 j6502::RTI() {
    sp++;
    status = read(0x0100 + sp);
    status &= ~B;
    status &= ~U;

    sp++;
    pc = (u16) read(0x0100 + sp);
    sp++;
    pc |= (u16) read(0x0100 + sp) << 8;
    return 0;
}

u8 j6502::RTS()
{
    sp++;
    pc = (u16)read(0x0100 + sp);
    sp++;
    pc |= (u16)read(0x0100 + sp) << 8;

    pc++;
    return 0;
}
// Instruction: Set Carry Flag
// Function: C = 1
u8 j6502::SEC() {
    SetFlag(C, true);
    return 0;
}
// Instruction: Set Decimal Flag
u8 j6502::SED()
{
    SetFlag(D, true);
    return 0;
}
// Instruction: Set Interrupt Flag / Enable Interrupts
u8 j6502::SEI()
{
    SetFlag(I, true);
    return 0;
}
// Instruction: Store Accumulator at Address
u8 j6502::STA()
{
    write(addr_abs, a);
    return 0;
}
// Instruction: Store X Register at Address
// Function: M = X
u8 j6502::STX()
{
    write(addr_abs, x);
    return 0;
}
// Instruction: Store Y Register at Address
// Function:    M = Y
u8 j6502::STY()
{
    write(addr_abs, y);
    return 0;
}


// Instruction: Transfer Accumulator to X Register
// Function:    X = A
// Flags Out:   N, Z
u8 j6502::TAX()
{
    x = a;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}


// Instruction: Transfer Accumulator to Y Register
// Function:    Y = A
// Flags Out:   N, Z
u8 j6502::TAY()
{
    y = a;
    SetFlag(Z, y == 0x00);
    SetFlag(N, y & 0x80);
    return 0;
}


// Instruction: Transfer Stack Pointer to X Register
// Function:    X = stack pointer
// Flags Out:   N, Z
u8 j6502::TSX()
{
    x = sp;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}


// Instruction: Transfer X Register to Accumulator
// Function:    A = X
// Flags Out:   N, Z
u8 j6502::TXA()
{
    a = x;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 0;
}


// Instruction: Transfer X Register to Stack Pointer
// Function:    stack pointer = X
u8 j6502::TXS()
{
    sp = x;
    return 0;
}


// Instruction: Transfer Y Register to Accumulator
// Function:    A = Y
// Flags Out:   N, Z
u8 j6502::TYA()
{
    a = y;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 0;
}

u8 j6502::XXX() {
}

#pragma endregion


// Perform one clock cycle of emulation
void j6502::clock() {
    if (cycles == 0) {
        opcode = read(pc);
        SetFlag(U, true);
        pc++;
        cycles = lookup[opcode].cycles;
        u8 additional_cycle1 = (this->*lookup[opcode].addrmode)();
        u8 additional_cycle2 = (this->*lookup[opcode].operate)();

        cycles += (additional_cycle1 & additional_cycle2);
    }
    cycles--;
}

#pragma region Interrupts

// Reset Interrupt
void j6502::reset() {
    a = 0;
    x = 0;
    y = 0;
    sp = 0xFD;
    status = 0x00 | U;

    addr_abs = 0xFFFC; // Set
    u16 lo = read(addr_abs + 0);
    u16 hi = read(addr_abs + 1);

    pc = (hi << 8) | lo;

    addr_rel = 0x0000;
    addr_abs = 0x0000;
    fetched = 0x00;

    cycles = 8;
}

// Interrupt Request Handler
void j6502::irq() {
    if (GetFlag(I) == 0) {
        write(0x0100 + sp, (pc >> 8) & 0x00FF);
        sp--;
        write(0x0100 + sp, (pc & 0x00FF));
        sp--;
        SetFlag(B, 0);
        SetFlag(U, 1);
        SetFlag(I, 1);
        write(0x100 + sp, status);
        sp--;

        addr_abs = 0xFFFE;
        u16 lo = read(addr_abs + 0);
        u16 hi = read(addr_abs + 1);
        pc = (hi << 8) | lo;

        cycles = 7;
    }
}

// NonMaskable IRQ
void j6502::nmi() {
    write(0x0100 + sp, (pc >> 8) & 0x00FF);
    sp--;
    write(0x0100 + sp, (pc & 0x00FF));
    sp--;
    SetFlag(B, 0);
    SetFlag(U, 1);
    SetFlag(I, 1);
    write(0x100 + sp, status);
    sp--;

    addr_abs = 0xFFFA;
    u16 lo = read(addr_abs + 0);
    u16 hi = read(addr_abs + 1);
    pc = (hi << 8) | lo;

    cycles = 8;
}

#pragma endregion


// Return from interrupt


std::map<u16, std::string> j6502::disassemble(u16 start, u16 stop) {
    uint32_t addr = start;
    u8 value = 0x00, lo = 0x00, hi = 0x00;
    std::map<u16, std::string> mapLines;
    u16 line_addr = 0;

    // A convenient utility to convert variables into
    // hex strings because "modern C++"'s method
    // with streams is atrocious
    auto hex = [](uint32_t n, u8 d)
    {
        std::string s(d, '0');
        for (int i = d - 1; i >= 0; i--, n >>= 4)
            s[i] = "0123456789ABCDEF"[n & 0xF];
        return s;
    };
    // Starting at the specified address we read an instruction
    // byte, which in turn yields information from the lookup table
    // as to how many additional bytes we need to read and what the
    // addressing mode is. I need this info to assemble human readable
    // syntax, which is different depending upon the addressing mode

    // As the instruction is decoded, a std::string is assembled
    // with the readable output
    while (addr <= (uint32_t)stop)
    {
        line_addr = addr;

        // Prefix line with instruction address
        std::string sInst = "$" + hex(addr, 4) + ": ";

        // Read instruction, and get its readable name
        u8 opcode = bus->read(addr, true); addr++;
        sInst += lookup[opcode].name + " ";

        // Get oprands from desired locations, and form the
        // instruction based upon its addressing mode. These
        // routines mimmick the actual fetch routine of the
        // 6502 in order to get accurate data as part of the
        // instruction
        if (lookup[opcode].addrmode == &j6502::IMP)
        {
            sInst += " {IMP}";
        }
        else if (lookup[opcode].addrmode == &j6502::IMM)
        {
            value = bus->read(addr, true); addr++;
            sInst += "#$" + hex(value, 2) + " {IMM}";
        }
        else if (lookup[opcode].addrmode == &j6502::ZP0)
        {
            lo = bus->read(addr, true); addr++;
            hi = 0x00;
            sInst += "$" + hex(lo, 2) + " {ZP0}";
        }
        else if (lookup[opcode].addrmode == &j6502::ZPX)
        {
            lo = bus->read(addr, true); addr++;
            hi = 0x00;
            sInst += "$" + hex(lo, 2) + ", X {ZPX}";
        }
        else if (lookup[opcode].addrmode == &j6502::ZPY)
        {
            lo = bus->read(addr, true); addr++;
            hi = 0x00;
            sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
        }
        else if (lookup[opcode].addrmode == &j6502::IZX)
        {
            lo = bus->read(addr, true); addr++;
            hi = 0x00;
            sInst += "($" + hex(lo, 2) + ", X) {IZX}";
        }
        else if (lookup[opcode].addrmode == &j6502::IZY)
        {
            lo = bus->read(addr, true); addr++;
            hi = 0x00;
            sInst += "($" + hex(lo, 2) + "), Y {IZY}";
        }
        else if (lookup[opcode].addrmode == &j6502::ABS)
        {
            lo = bus->read(addr, true); addr++;
            hi = bus->read(addr, true); addr++;
            sInst += "$" + hex((u16)(hi << 8) | lo, 4) + " {ABS}";
        }
        else if (lookup[opcode].addrmode == &j6502::ABX)
        {
            lo = bus->read(addr, true); addr++;
            hi = bus->read(addr, true); addr++;
            sInst += "$" + hex((u16)(hi << 8) | lo, 4) + ", X {ABX}";
        }
        else if (lookup[opcode].addrmode == &j6502::ABY)
        {
            lo = bus->read(addr, true); addr++;
            hi = bus->read(addr, true); addr++;
            sInst += "$" + hex((u16)(hi << 8) | lo, 4) + ", Y {ABY}";
        }
        else if (lookup[opcode].addrmode == &j6502::IND)
        {
            lo = bus->read(addr, true); addr++;
            hi = bus->read(addr, true); addr++;
            sInst += "($" + hex((u16)(hi << 8) | lo, 4) + ") {IND}";
        }
        else if (lookup[opcode].addrmode == &j6502::REL)
        {
            value = bus->read(addr, true); addr++;
            sInst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
        }

        // Add the formed string to a std::map, using the instruction's
        // address as the key. This makes it convenient to look for later
        // as the instructions are variable in length, so a straight up
        // incremental index is not sufficient.
        mapLines[line_addr] = sInst;
    }

    return mapLines;
}

bool j6502::complete() {
    return cycles == 0;
}



