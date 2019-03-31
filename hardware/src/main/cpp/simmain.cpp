//
// Copyright 2015 Jeff Bush
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <getopt.h>
#include <stdint.h>
#include <stdio.h>
#include "Testbench.h"

namespace
{

template <int w, int d>
void writeMemoryToFile(const char *filename, const mem_t<w, d> &memory, uint32_t start, size_t length)
{
    static_assert(w == 32, "word size for memory must be 32");
    if (((start + length) / 4) - 1 > d)
    {
        printf("memory dump past end of memory, truncating output file\n");
        length = d - (start + length);
    }

    FILE *file = ::fopen(filename, "wb");
    if (!file)
    {
        perror("writeMemoryToFile failed");
        return;
    }

    for (uint32_t addr = start / 4, end = (start + length) / 4; addr < end; addr++)
    {
        if (::fwrite(&memory.contents[addr].values[0], 4, 1, file) != 1)
        {
            perror("fwrite error");
            return;
        }
    }

    ::fclose(file);
}

void usage()
{
    printf("USAGE: simulator [options] <initial_memory.hex>\n");
    printf("  -w  Dump a waveform trace to trace.vcd\n");
    printf("  -m  Dump memory image <filename,start,length>\n");
    printf("  -c  Total clock cycles to run for\n");
}

}

int main (int argc, char* argv[])
{
    int c;
    bool enableWaveform = false;
    bool enableMemoryDump = false;
    uint32_t memDumpBase = 0;
    size_t memDumpLength = 0;
    int maxCycles = 20000;
    char memDumpFilename[256];

    while ((c = getopt(argc, argv, "wd:c:")) != -1)
    {
        switch(c)
        {
            case 'w':
                enableWaveform = true;
                break;

            case 'd':
            {
                // Memory dump, of the form:
                //  filename,start,length
                const char *separator = strchr(optarg, ',');
                if (separator == NULL)
                {
                    fprintf(stderr, "bad format for memory dump\n");
                    usage();
                    return 1;
                }

                strncpy(memDumpFilename, optarg, separator - optarg);
                memDumpFilename[separator - optarg] = '\0';

                memDumpBase = strtol(separator + 1, NULL, 16);
                separator = strchr(separator + 1, ',');
                if (separator == NULL)
                {
                    fprintf(stderr, "bad format for memory dump\n");
                    usage();
                    return 1;
                }

                memDumpLength = strtol(separator + 1, NULL, 16);
                enableMemoryDump = true;
                break;
            }

            case 'c':
                maxCycles = atoi(optarg);
                break;

            case '?':
                usage();
                break;
        }
    }

    Testbench_t *module = new Testbench_t();
    module->init();

    if (optind < argc)
    {
        // Read memory initialization file
        if (module->Testbench_systemMemory__memory.read_hex(argv[optind]) == 0)
        {
            perror("Error reading hex file: ");
            return 1;
        }
    }

    FILE *waveformFile = nullptr;
    if (enableWaveform)
    {
        waveformFile = fopen("trace.vcd", "w");
        module->set_dumpfile(waveformFile);
    }

    // Reset
    module->clock_lo(dat_t<1>(1));
    module->clock_hi(dat_t<1>(1));
    if (enableWaveform)
        module->mod_t::dump();	// Write initial waveform values

    int totalCycles;
    for (totalCycles = 0; totalCycles < maxCycles && !module->Testbench__io_halt.values[0]; totalCycles++)
    {
        module->clock_lo(dat_t<1>(0));
        module->clock_hi(dat_t<1>(0));
        module->print(stdout);
        if (enableWaveform)
            module->mod_t::dump();	// Write waveform file updates
    }

    printf("ran for %d cycles\n", totalCycles);
    if (waveformFile)
        fclose(waveformFile);

    // Write memory contents
    writeMemoryToFile(memDumpFilename, module->Testbench_systemMemory__memory,
                      memDumpBase, memDumpLength);
}

