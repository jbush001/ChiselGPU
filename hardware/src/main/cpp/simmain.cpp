#include <stdio.h>
#include "TestBench.h"

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

	for (int cycle = 0; cycle < 100; cycle++)
	{
	    module->clock_lo(dat_t<1>(0));
	   	module->clock_hi(dat_t<1>(0));
		module->print(stdout);
		module->mod_t::dump();	// Write waveform file updates
	}

	fclose(f);

	// Write memory conntents
	module->Testbench_systemMemory__memory.print();
}

