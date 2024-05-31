//==============================================================================
// File:            MessageBuffer.cs
// Created date:    12/03/2017
// Last update:     12/03/2017
// Author:          Rino Jose
// Description:     Implements MessageBuffer helper class that converts a stream
//                  of keydown and keyup events into input strings.
//

using System;
using System.Diagnostics;
using Windows.System;
using Windows.UI.Core;

namespace HoloForrth {

    public interface IInputProcessor
    {
        void processInput(String input);
    }

    class MessageBuffer
    {
        String inputBuffer = "";
        bool shiftDown = false;
        IInputProcessor processor;

        //-------------------------------------------------------------------------------
        /// <summary>Creates a message buffer with a processor to call when input is ready</summary>
        //
        // Last update:  12/03/2017
        public MessageBuffer(IInputProcessor processor)
        {
            this.processor = processor;
        }

        public string InputBuffer { get => inputBuffer; set => inputBuffer = value; }

        //-------------------------------------------------------------------------------
        /// <summary>Tracks state of SHIFT keys</summary>
        //
        // Last update:  12/03/2017
        public void OnKeyDown(KeyEventArgs args)
        {
            VirtualKey key = args.VirtualKey;
            if (key == VirtualKey.LeftShift || key == VirtualKey.RightShift || key == VirtualKey.Shift)
            {
                shiftDown = true;
                return;
            }
        }

        //-------------------------------------------------------------------------------
        /// <summary>Tracks state of SHIFT, acts on ENTER, stores chars in inputBuffer</summary>
        //
        // Last update:  12/03/2017
        public void OnKeyUp(KeyEventArgs args)
        {
            VirtualKey key = args.VirtualKey;

            if (key == VirtualKey.LeftShift || key == VirtualKey.RightShift || key == VirtualKey.Shift)
            {
                shiftDown = false;
                return;
            }

            if (key == VirtualKey.Enter)
            {
                processor.processInput(inputBuffer);
                inputBuffer = "";
            }
            else
            {
                inputBuffer += ToChar(key);
            }
        }

        //-------------------------------------------------------------------------------
        /// <summary>Converts VirtualKey values to ASCII</summary>
        //
        // Last update:  12/03/2017
        private char ToChar(VirtualKey key)
        {
            char result = (char) key;

            // Convert oddly handled keys
            switch (result)
            {
                case (char) 0xba:  // SEMICOLON virtual key
                    result = ';';
                    break;

                case (char)0xbd: // '-' virtual key
                    result = '-';
                    break;

                case (char)0xbb: // '=' virtual key
                    result = '=';
                    break;

                case (char)0xbe: // '.' virtual key
                    result = '.';
                    break;

                case (char)0xbc: // ',' virtual key
                    result = ',';
                    break;

                case (char)0xde: // "'" virtual key                
                    result = '\'';
                    break;
            }

            // Handle lowercasing of alpha chars
            if (result >= 'A' && result <= 'Z')
            {
                if (!shiftDown)
                {
                    result = (char)(result + 32); // Lowercase
                }
            }
            // Handle shifted chars
            if (shiftDown)
            {
                switch (result)
                {
                    case ';':
                        result = ':';
                        break;

                    case '-':
                        result = '_';
                        break;

                    case '=':
                        result = '+';
                        break;

                    case '1':
                        result = '!';
                        break;

                    case '2':
                        result = '@';
                        break;

                    case '3':
                        result = '#';
                        break;

                    case '4':
                        result = '$';
                        break;

                    case '5':
                        result = '%';
                        break;

                    case '6':
                        result = '^';
                        break;

                    case '7':
                        result = '&';
                        break;

                    case '8':
                        result = '*';
                        break;

                    case '9':
                        result = '(';
                        break;

                    case '0':
                        result = ')';
                        break;

                    case '.':
                        result = '>';
                        break;

                    case ',':
                        result = '<';
                        break;

                    case '\'':
                        result = '"';
                        break;

                    default:
                        break;
                }
            }
            return result;
        }

    }
}
