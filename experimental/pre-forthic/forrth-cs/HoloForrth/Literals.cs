using System.Diagnostics;

namespace HoloForrth
{
    class PseudoEntryPushValue : Entry
    {
        public PseudoEntryPushValue(object value) : base("PseudoEntryPushValue", PushValue)
        {
            parameters.Add(value);  // Param 0 is the value to push
        }

        static void PushValue(Forrth f, Entry entry)
        {
            object value = entry.parameters[0];
            f.Stack.Push(value);
        }
    }
}