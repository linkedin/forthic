//==============================================================================
// File:            forrth.cpp
// Created date :   11/19/2017
// Last update :    11/24/2017
// Author:          Rino Jose
// Description:     Implements control loop for Forrth interpreter
//

#include "stdafx.h"

// Functions defined in Assembly.
// StoreInput_ copies an input buffer to a message buffer in Assembly-land.
// ProcessInput_ parses the message buffer and executes the words in it.
// Initialize_ sets up the Forrth interpreter
extern "C" int Execute_(const char* src, int len);
extern "C" void Initialize_();
extern "C" void Exercise_();


//-------------------------------------------------------------------------------
// LoadForrthBlock: Loads a forrth block from file and copies it to dest
//
// Last update : 12/02/2017
// Description:  Given a forrth ID like "1", tries to open a file named
//               "BLOCK-1.forrth". If successful, copies contents to dest
//               and returns number of characters copied.
//
// Input:        forrth_id: ID of block to load
//               dest: where to write contents of block
//
// Returns:      Num of chars copied
//
extern "C" int LoadForrthBlock(const char* forrth_id, char* dest) {
	// Construct filename to load
	std::string filename = "BLOCK-";
	filename += forrth_id;
	filename += ".forrth";

	std::ifstream blockFile(filename);
	blockFile.get(dest, 1024, 0);
	blockFile.close();

	// Replace \n with ' '
	char* cp = dest;
	while (*cp) {
		if (*cp == '\n')
			*cp = ' ';
		cp++;
	}
	return 0;
}

void forLinker() {
	printf("");
	sscanf_s("", "");
}

void forrth() {
	Initialize_();

	std::string input;

	// Control loop
	while (1) {
		// Get input and execute
		std::cout << "> ";
		std::getline(std::cin, input);
		Execute_(input.c_str(), input.length());
	}
}

void exercise() {
	Exercise_();

}

int main()
{
	forrth();
    return 0;
}

