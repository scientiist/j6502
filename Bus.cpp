//
// Created by josh on 1/22/23.
//

#include "Bus.h"
Bus::Bus()
{

    cpu.ConnectBus(this);
    // Clear ram contents
    for (auto &i : ram) i = 0x00;
}
Bus::~Bus()
{

}
void Bus::write(uint16_t addr, uint8_t data)
{
    if (addr >= 0x0000 && addr <= 0xFFFF)
        ram[addr] = data;
}
uint8_t Bus::read(uint16_t addr, bool readOnly)
{
    if (addr >= 0x0000 && addr <= 0xFFFF)
        return ram[addr];
}