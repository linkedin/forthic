//==============================================================================
// File:            Entry.cs
// Created date:    12/04/2017
// Last update:     12/09/2017
// Author:          Rino Jose
// Description:     Implements a dictionary entry
//

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HoloForrth
{
    class Entry
    {
        public delegate void EntryRoutine(Forrth f, Entry entry);

        public string word;
        public List<object> parameters;
        public EntryRoutine Routine;
        public bool immediate;
        

        public Entry(string word, EntryRoutine routine)
        {
            this.word = word;
            this.Routine = routine;
            parameters = new List<object>();
            immediate = false;
        }

        public Entry(string word, EntryRoutine routine, bool immediate) : this(word, routine)
        {
            this.immediate = immediate;
        }
    }
}
