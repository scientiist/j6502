//
// Created by josh on 1/22/23.
//

#ifndef EMU6502_BUS_H
#define EMU6502_BUS_H
#include <cstdint>
#include <array>



#include "j6502.h"

class Bus {
public:
    Bus();
    ~Bus();
public:
    j6502 cpu;
    std::array<uint8_t, 64*1024> ram;

    void write(uint16_t addr, uint8_t data);

    uint8_t read(uint16_t addr, bool readOnly);
};


#endif //EMU6502_BUS_H
