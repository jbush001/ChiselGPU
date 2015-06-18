#include <stdio.h>
#include "TestBench.h"

namespace
{

template <int w, int d>
void writeMemoryToFile(const char *filename, const mem_t<w, d> &memory)
{
	static_assert(w == 32, "word size for memory must be 32");

	FILE *file = ::fopen(filename, "wb");
	if (!file) 
	{
		perror("writeMemoryToFile failed");
		return;
	}
	
	for (int addr = 0; addr < d; addr++) 
	{
		if (::fwrite(&memory.contents[addr].values[0], 4, 1, file) != 1)
		{
			perror("fwrite error");
			return;
		}
	}

	::fclose(file);
}

}

int main (int argc, char* argv[]) 
{
	Testbench_t *module = new Testbench_t();
	module->init();
	Testbench_api_t *api = new Testbench_api_t();
	api->init(module);
	FILE *f = fopen("trace.vcd", "w");
	module->set_dumpfile(f);

	// Reset
   	module->clock_lo(dat_t<1>(1));
   	module->clock_hi(dat_t<1>(1));
	module->mod_t::dump();	// Write initial waveform values

	for (int cycle = 0; cycle < 20000; cycle++)
	{
	    module->clock_lo(dat_t<1>(0));
	   	module->clock_hi(dat_t<1>(0));
		module->print(stdout);
		module->mod_t::dump();	// Write waveform file updates
	}

	fclose(f);

	// Write memory contents
	writeMemoryToFile("memory.bin", module->Testbench_systemMemory__memory);
}

