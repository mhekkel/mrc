// 	mrc, A simple resource compiler.
//	
//	Use this program to make data available to your program without
//	having to distribute this data in separate files.
//
//	Example usage: mrc -o myrsrc.o rsrc/
//
//	This will create an object file called myrsrs.o containing the data for all file found in the rsrc/ directory.

#include <iostream>
#include <boost/program_options.hpp>

#define VERSION "0.1"

using namespace std;
namespace po = boost::program_options;




int main(int argc, char* argv[])
{
	try
	{
		po::options_description visible_options("mrc " VERSION " options" );
		visible_options.add_options()
			("help,h",								"Display help message")
			("version",								"Print version")
			("output,o",	po::value<string>(),	"Output file, this file is in the default object file format for this OS.")
			("dump heaeder file",					"This will print out the header file you need to include in your program to access your resources")
			("verbose,v",							"Verbose output");
	
		po::options_description hidden_options("hidden options");
		hidden_options.add_options()
			("input,i",		po::value<vector<string>>(),	"Input files");
	
		po::options_description cmdline_options;
		cmdline_options.add(visible_options).add(hidden_options);
	
		po::positional_options_description p;
		p.add("output", 1);
		p.add("input", -1);
		
		po::variables_map vm;
		po::store(po::command_line_parser(argc, argv).options(cmdline_options).positional(p).run(), vm);
		po::notify(vm);
		
		if (vm.count("version"))
		{
			cout << argv[0] << " version " VERSION << endl;
			exit(0);
		}
	
		if (vm.count("help") or vm.count("input") == 0)
		{
			cerr << visible_options << endl;
			exit(1);
		}
	
	
	
		
		
	}
	catch (const exception& ex)
	{
		cerr << "Error executing mrc: " << ex.what() << endl;
		exit(1);
	}
	
	return 0;
}
