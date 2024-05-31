#include <memory>
#include <cstdio>
#include <fstream>
#include <ncurses.h>

#include "../Interpreter.h"
#include "../Module.h"
#include "../m_cuda/M_Cuda.h"
#include "../m_gauss/M_Gauss.h"
#include "../m_lp/M_LP.h"
#include "Ch2Module.h"

using namespace std;

string load_file(string filename) {
    ifstream infile { filename };
    if (!infile) {
        throw string("Can't open file: ") + filename;
    }

    string result { istreambuf_iterator<char>(infile), istreambuf_iterator<char>() };
    return result;
}

int main(int c, char* argv[]) {
    try {
        string filename = "sumArraysOnGPU.forthic";
        if (c >= 2)   filename = argv[1];

        Interpreter interp;
        interp.RegisterModule(shared_ptr<Module>(new M_Cuda()));
        interp.RegisterModule(shared_ptr<Module>(new Ch2Module()));
        interp.RegisterModule(shared_ptr<Module>(new M_Gauss()));
        interp.RegisterModule(shared_ptr<Module>(new M_LP()));
        interp.Run(load_file(filename));
    }
    catch (const char *message) {
        printf("EXCEPTION: %s\n", message);
    }
    catch (string message) {
        printf("EXCEPTION: %s\n", message.c_str());
    }
    return 0;
}
