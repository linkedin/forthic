//==============================================================================
// File:            Forrth.cs
// Created date:    12/03/2017
// Last update:     12/10/2017
// Author:          Rino Jose
// Description:     Implements Forrth interpreter
//
using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.System;
using Windows.UI.Core;
using System.Diagnostics;
using System.Text.RegularExpressions;

namespace HoloForrth
{
    // Points to the next Entry.parameter to execute
    class InstructionPointer
    {
        public Entry entry;
        public int instructionIndex;

        public InstructionPointer(Entry entry, int index)
        {
            this.entry = entry;
            this.instructionIndex = index;
        }
    }

    class Forrth : IInputProcessor
    {
        LinkedListNode<string> curWordNode;
        List<Entry> dictionary;
        Stack stack;
        Stack returnStack;
        Stack curWordStack;
        bool compiling;
        bool executingDefinition;
        Entry curDefinition;

        public Stack Stack { get => stack; set => stack = value; }
        internal List<Entry> Dictionary { get => dictionary; set => dictionary = value; }
        internal Entry Cur_definition { get => curDefinition; set => curDefinition = value; }
        public bool Compiling { get => compiling; set => compiling = value; }
        public Stack ReturnStack { get => returnStack; set => returnStack = value; }
        public bool ExecutingDefinition { get => executingDefinition; set => executingDefinition = value; }

        void PrintHi(Forrth f, Entry entry)
        {
            Debug.WriteLine("HOWDY!");
        }

        public Forrth()
        {
            dictionary = new List<Entry>();
            dictionary.Add(new Entry("SAMPLE", new Entry.EntryRoutine(PrintHi)));
            stack = new Stack();
            returnStack = new Stack();
            curWordStack = new Stack();
            compiling = false;
            executingDefinition = false;

            // Add base lexicons
            CoreWords.AddCoreWords(this);
        }

        //-------------------------------------------------------------------------------
        /// <summary>Breaks input into linked list of words</summary>
        //
        // We split by lines and then by "# " so we can remove comments
        //
        // Last update:  12/10/2017
        public LinkedList<string> Wordify(string input)
        {
            // Split input into lines
            string[] lines = Regex.Split(input, @"\n");

            // For each line, split by "# " and take first element
            List<string> elements = new List<string>();
            foreach (string line in lines)
            {
                elements.Add(Regex.Split(line, @"# ")[0]);
            }

            // For each element, split by whitespace, gather words into a word list and return
            LinkedList<string> result = new LinkedList<string>();
            foreach (string element in elements)
            {
                string[] words = Regex.Split(element, @"\s+");
                foreach (string word in words)
                {
                    if (word.Count() > 0)
                    {
                        result.AddLast(word);
                    }
                }
            }
            return result;
        }

        //-------------------------------------------------------------------------------
        /// <summary>Breaks input into words and executes each one</summary>
        //
        // Last update:  12/09/2017
        void IInputProcessor.processInput(string input)
        {
            // Break input into words
            LinkedList<string> word_list = Wordify(input);
            curWordNode = word_list.First;

            // Iterate across linked list until done
            try
            {
                while (curWordNode != null || returnStack.Count > 0)
                {
                    // GetNextEntry advances curWordNode (eventually)
                    Entry entry = GetNextEntry();

                    // Restore curWordNode, if necessary
                    if (curWordNode == null && curWordStack.Count > 0)
                    {
                        curWordNode = (LinkedListNode<string>) curWordStack.Pop();
                    }

                    // If not compiling (or entry is immediate), execute entry; otherwise compile into current definition
                    if (!compiling || entry.immediate)
                    {
                        // Execute entry
                        entry.Routine(this, entry);
                    }
                    else
                    {
                        // Compile entry
                        curDefinition.parameters.Add(entry);
                    }
                }
            }
            catch (Exception e)
            {
                Debug.WriteLine(e.Message);
                ResetForrthState();
            }
        }


        //-------------------------------------------------------------------------------
        /// <summary>Pushes </summary>
        //
        // Last update:  12/09/2017
        public void Interpret(string input)
        {
            // Break input into words
            LinkedList<string> word_list = Wordify(input);

            // Push curWordNode onto stack
            curWordStack.Push(curWordNode);

            // Set curWordNode to start of word_list
            curWordNode = word_list.First;
        }

        //-------------------------------------------------------------------------------
        /// <summary>Gets the next word and advances curWordNode</summary>
        //
        // Last update:  12/09/2017
        public string GetNextWord()
        {
            string result = curWordNode.Value;
            curWordNode = curWordNode.Next;
            return result;
        }

        void ResetForrthState()
        {
            stack = new Stack();
            returnStack = new Stack();
            curWordStack = new Stack();
            curWordNode = null;
            compiling = false;
        }

        Entry GetNextEntry()
        {
            // If not executing a definition, get the entry for the next word; otherwise get next instruction in definition
            if (!executingDefinition)
            {
                return GetNextEntry_W();
            }
            else
            {
                return GetNextEntry_I();
            }
        }


        //-------------------------------------------------------------------------------
        /// <summary>Tries to find entry or literal for current word</summary>
        //
        // Last update:  12/06/2017
        Entry GetNextEntry_W()
        {
            // Look up cur word in the dictionary
            Entry result = FindEntry(curWordNode.Value);

            // If can't find cur word, try treating as a literal
            if (result == null)
            {
                result = FindLiteralEntry(curWordNode.Value);
            }

            // If can't find a literal, raise an exception.
            if (result == null)
            {
                throw new Exception("Unknown word: " + curWordNode.Value);
            }
            if (curWordNode != null)
            {
                curWordNode = curWordNode.Next;
            }

            return result;
        }

        //-------------------------------------------------------------------------------
        /// <summary>Gets entry pointed to by instructionPointer and advances instruction index</summary>
        //
        // Last update:  12/09/2017
        Entry GetNextEntry_I()
        {
            InstructionPointer ip = (InstructionPointer) returnStack.Peek();
            Entry result = (Entry)ip.entry.parameters[ip.instructionIndex];
            ip.instructionIndex++;
            return result;
        }


        //-------------------------------------------------------------------------------
        /// <summary>Checks for number and then string literals</summary>
        //
        // Last update:  12/09/2017
        Entry FindLiteralEntry(string word)
        {
            if (Int32.TryParse(word, out int intValue))
            {
                return new PseudoEntryPushValue(intValue);
            }
            if (Double.TryParse(word, out double doubleValue))
            {
                return new PseudoEntryPushValue(doubleValue);
            }
            if (word == ".\"")
            {
                curWordNode = curWordNode.Next;
                string strValue = ExtractString();
                return new PseudoEntryPushValue(strValue);
            }
            return null;
        }


        //-------------------------------------------------------------------------------
        /// <summary>Iterates through words until finds a word that ends with { " }, returning concatenation</summary>
        //
        // Last update:  12/09/2017
        string ExtractString()
        {
            List<string> pieces = new List<string>();
            while (curWordNode != null)
            {
                string word = curWordNode.Value;
                curWordNode = curWordNode.Next;

                if (word[word.Count() - 1] == '"')
                {
                    pieces.Add(word.Substring(0, word.Count() - 1));
                    break;
                }
                pieces.Add(word);
            }
            // Back up one word since we went one too far
            curWordNode = curWordNode.Previous;
            string result = String.Join(" ", pieces);        
            return result;
        }

        //-------------------------------------------------------------------------------
        /// <summary>Searches for entry with matching word in dictionary</summary>
        //
        // Last update:  12/06/2017
        Entry FindEntry(string word)
        {
            Entry result = null;
            for (int i=0; i < dictionary.Count; i++)
            {
                // Iterate in reverse
                int index = dictionary.Count - (i + 1);
                if (dictionary[index].word == word)
                {
                    result = dictionary[index];
                    break;
                }
            }
            return result;
        }
    }
}
