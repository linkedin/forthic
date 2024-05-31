//==============================================================================
// File:            CoreWords.cs
// Created date:    12/09/2017
// Last update:     12/10/2017
// Author:          Rino Jose
// Description:     Adds core words to Forrth interpreter
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;

namespace HoloForrth
{
    class CoreWords
    {
        static Entry ExitDefinitionEntry = new Entry("ExitDefinition", new Entry.EntryRoutine(ExitDefinition));
        //-------------------------------------------------------------------------------
        /// <summary>Creates a new entry definition and puts compiler into compiling mode</summary>
        //
        // Last update:  12/09/2017
        static void StartDefinition(Forrth f, Entry entry)
        {
            // Get next word and use that as the entry's word
            string word = f.GetNextWord();
            f.Cur_definition = new Entry(word, new Entry.EntryRoutine(ExecuteDefinition));

            // Turn on compiling mode
            f.Compiling = true;
        }

        //-------------------------------------------------------------------------------
        /// <summary>Ends the current definition</summary>
        //
        // Last update:  12/09/2017
        static void EndDefinition(Forrth f, Entry entry)
        {
            // Compile ExitDefinitionEntry into Cur_definition
            f.Cur_definition.parameters.Add(ExitDefinitionEntry);

            // Add Cur_definition to dictionary
            f.Dictionary.Add(f.Cur_definition);
            f.Cur_definition = null;

            // Turn compiling mode off
            f.Compiling = false;
        }

        //-------------------------------------------------------------------------------
        /// <summary>Exits a definition execution</summary>
        //
        // Last update:  12/09/2017
        static void ExitDefinition(Forrth f, Entry entry)
        {
            f.ReturnStack.Pop();
            if (f.ReturnStack.Count == 0)
            {
                f.ExecutingDefinition = false;
            }
        }

        //-------------------------------------------------------------------------------
        /// <summary>Executes a definition</summary>
        //
        // Last update:  12/09/2017
        static void ExecuteDefinition(Forrth f, Entry entry)
        {
            InstructionPointer ip = new InstructionPointer(entry, 0);
            f.ReturnStack.Push(ip);
            f.ExecutingDefinition = true;
        }

        //-------------------------------------------------------------------------------
        /// <summary>Pops string from stack and executes it</summary>
        //
        // Last update:  12/10/2017
        static void Interpret(Forrth f, Entry entry)
        {
            f.Interpret((string)f.Stack.Pop());
        }

        //-------------------------------------------------------------------------------
        /// <summary>Reads next word and uses that as an Block ID to load and interpret</summary>
        //
        // Last update:  12/10/2017
        static void Load(Forrth f, Entry entry)
        {
            string block_id = f.GetNextWord();
            string block = Blocks.Load(block_id);
            f.Interpret(block);
        }

        //-------------------------------------------------------------------------------
        /// <summary>Adds words related to definition and strings</summary>
        //
        // Last update:  12/10/2017
        static public void AddCoreWords(Forrth f)
        {
            f.Dictionary.Add(new Entry(":", new Entry.EntryRoutine(StartDefinition)));
            f.Dictionary.Add(new Entry(";", new Entry.EntryRoutine(EndDefinition), true));
            f.Dictionary.Add(new Entry("INTERPRET", new Entry.EntryRoutine(Interpret)));
            f.Dictionary.Add(new Entry("LOAD", new Entry.EntryRoutine(Load)));
        }
    }
}
