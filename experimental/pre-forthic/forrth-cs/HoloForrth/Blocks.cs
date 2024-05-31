//==============================================================================
// File:            Blocks.cs
// Created date:    12/10/2017
// Last update:     12/10/2017
// Author:          Rino Jose
// Description:     Defines blocks used in HoloForrth.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HoloForrth
{
    class Blocks
    {
        static bool isInitialized = false;
        static Dictionary<string, string> blocks;
        static string block_1 = @"
# BLOCK1 - Sample block
: taco SAMPLE SAMPLE ;
";

        static string block_2 = @"
# BLOCK2 - Sample block
LOAD 1
: 2taco taco taco ;
";

        //-------------------------------------------------------------------------------
        /// <summary>Associates an ID with each block</summary>
        //
        // Last update:  12/10/2017
        public static void Initialize()
        {
            blocks = new Dictionary<string, string>();
            blocks["1"] = block_1;
            blocks["2"] = block_2;
            isInitialized = true;
        }


        public static string Load(string block_id)
        {
            if (!isInitialized)
            {
                Initialize();
            }
            return blocks[block_id];
        }
    }
}
