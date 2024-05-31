%%==============================================================================
%% File:            ferrth.erl
%% Created date:    12/16/2017
%% Last update:     12/18/2017
%% Author:          Rino Jose
%% Description:     Run Forrth strings
%%

-module(ferrth).

%%==============================================================================
%% EXPORTS
-export([run/1]).


%%==============================================================================
%% MACROS
-define(space, 32).
-define(newline, 10).

% Parse states
-define(start, 0).
-define(normal, 1).
-define(checkString, 2).
-define(collectString, 3).
-define(checkComment, 4).
-define(skipComment, 5).


%%==============================================================================
%% RECORDS

-record(parse_state, {words=[], state=?start, cur_word = ""}).
-record(entry, {word="", routine, parameters=[], immediate=false}).
-record(instruction_pointer, {index=1, entry}).
-record(forrth, {
    dictionary=[],
    stack=[],
    return_stack=[],
    compiling=false,
    cur_definition,
    executing_definition=false,
    ip,
    input=[]
}).

%-------------------------------------------------------------------------------
% run/1
%
% Last update:  12/16/2017
% Description:  Main entry point to Forrth interpreter. Runs a Forrth string.
%
% Returns:      forrth record with new state
%
run(Input) ->
    Forrth = init(),
    Words = parse_input(Input, #parse_state{}),
    NextForrth = Forrth#forrth{input=Words},
    execute(NextForrth)
.


%-------------------------------------------------------------------------------
% execute/1
%
% Last update:  12/17/2017
% Description:  Executes whatever input is in ForrthInit
%
% Returns:      forrth record with new state
%
execute(ForrthInit) ->
    {Entry, Forrth} = get_next_entry(ForrthInit),
    if
        Entry == undefined ->
            Forrth;
        element(1, Entry) == unknown ->
            {unknown, CurWord} = Entry,
            forrth_error(Forrth, "Unknown word: ~s~n", [CurWord]);
        true ->
            NextForrth = if
                Entry#entry.immediate ->
                    execute_entry(Forrth, Entry);
                Forrth#forrth.compiling ->
                    compile_entry(Forrth, Entry);
                true ->
                    execute_entry(Forrth, Entry)
            end,
            execute(NextForrth)
    end
.

%-------------------------------------------------------------------------------
% forrth_error/3
%
% Last update:  12/17/2017
% Description:  Logs an error and returns a forrth record with cleared stacks
%
% Returns:      forrth record with new state
%
forrth_error(Forrth, Format, Terms) ->
    % TODO: Log message
    io:format(Format, Terms),
    Forrth#forrth{stack=[], return_stack=[], compiling=false}
.
    
%-------------------------------------------------------------------------------
% execute_entry/2
%
% Last update:  12/17/2017
% Description:  Executes an Entry
%
% Returns:      forrth record with new state
%
execute_entry(Forrth, #entry{routine=Routine} = Entry) ->
    Routine(Forrth, Entry)
.


%-------------------------------------------------------------------------------
% compile_entry/2
%
% Last update:  12/17/2017
% Description:  Compiles an entry into the current definition
%
% Returns:      forrth record with new state
%
compile_entry(#forrth{cur_definition=CurDef} = Forrth, Entry) ->
    Params = CurDef#entry.parameters,
    CurDefNext = CurDef#entry{parameters = Params ++ [Entry]},
    Forrth#forrth{cur_definition=CurDefNext}
.

%-------------------------------------------------------------------------------
% get_next_entry/2
%
% Last update:  12/17/2017
% Description:  If executing a definition, returns next instruction in the
%               definition; otherwise return entry corresponding to next word in
%               the Forrth input
%
% Returns:      {entry, forrth}
%
get_next_entry(#forrth{executing_definition=ExecDef} = Forrth) ->
    if
        ExecDef ->
            get_next_instruction(Forrth);
        true ->
            get_next_word_entry(Forrth)
    end
.


%-------------------------------------------------------------------------------
% get_next_instruction/2
%
% Last update:  12/17/2017
% Description:  Returns next instruction in the definition being executed.
%
% Returns:      {entry, forrth}
%
get_next_instruction(#forrth{ip=IP} = Forrth) ->
    #instruction_pointer{index=Index, entry=Definition} = IP,
    Params = Definition#entry.parameters,
    InstructionEntry = lists:nth(Index, Params),
    IPNext = IP#instruction_pointer{index=Index+1},
    {InstructionEntry, Forrth#forrth{ip=IPNext}}
.


%-------------------------------------------------------------------------------
% get_next_word_entry/2
%
% Last update:  12/17/2017
% Description:  Returns entry corresponding to next word in Forrth input
%
% Returns:      {entry, forrth}
%
get_next_word_entry(#forrth{input=Input} = Forrth) ->
    if
        Input == [] ->
            {undefined, Forrth};
        true ->
            [CurWord|Rest] = Input,
            Entry = find_entry(Forrth, CurWord),
            {Entry, Forrth#forrth{input=Rest}}
    end
.

%-------------------------------------------------------------------------------
% find_entry/2
%
% Last update:  12/17/2017
% Description:  Searches forrth dictionary given a Word. If can't find word, try
%               interpreting as a literal.
%
% NOTE: The convention for the dictionary is that the most recent entries are first
%
% Returns:      entry record | undefined
%
find_entry(#forrth{dictionary=[Last|Rest]} = Forrth, Word) ->
    if
        Last#entry.word == Word -> Last;
        true -> find_entry(Forrth#forrth{dictionary=Rest}, Word)
    end
;
find_entry(#forrth{dictionary=[]} = Forrth, Word) ->
    make_literal_entry(Forrth, Word)
.

%-------------------------------------------------------------------------------
% push_param0/2
%
% Last update:  12/17/2017
% Description:  Pushes first parameter of entry onto forrth stack
%
% Returns:      updated forrth record
%
push_param0(#forrth{stack=Stack} = Forrth, #entry{parameters=[Val|_]}) ->
    Forrth#forrth{stack=[Val] ++ Stack}
.
    
    
%-------------------------------------------------------------------------------
% make_literal_entry/2
%
% Last update:  12/17/2017
% Description:  Creates a literal entry based on the specified Word
%
% Returns:      pseudo entry | {unknown, CurWord}
%
make_literal_entry(_Forrth, Word) ->
    {Integer, IntRest} = string:to_integer(Word),
    
    if
        (Integer == error) or (IntRest /= []) ->
            % Try float
            {Float, FloatRest} = string:to_float(Word),
            if
                (Float == error) or (FloatRest /= []) -> {unknown, Word};
                true -> #entry{routine=fun push_param0/2, parameters=[Float]}
            end;
        true -> #entry{routine=fun push_param0/2, parameters=[Integer]}
    end
.

%-------------------------------------------------------------------------------
% entry_print_hi: Returns a PRINT-HI entry
%
entry_print_hi(Word) ->
    #entry{ word=Word, 
            routine=fun(Forrth, _Entry) -> io:format("HOWDY!~n"), Forrth end
    }
.

%-------------------------------------------------------------------------------
% entry_interpret: Returns an INTERPRET entry
%
% Pops string from stack, parses it into words, adds words to beginning of input
%
entry_interpret(Word) ->
    #entry{ word=Word, 
            routine=fun(#forrth{input=Input} = Forrth, _Entry) -> 
                [String|RestStack] = Forrth#forrth.stack,
                Words = parse_input(String, #parse_state{}),
                Forrth#forrth{input=Words ++ Input, stack=RestStack}
            end
    }
.

%-------------------------------------------------------------------------------
% entry_load: Returns a LOAD entry
%
% Reads next input word as block ID, reads a file "BLOCK-<id>.forrth", and executes it
%
entry_load(Word) ->
    #entry{ word=Word, 
            routine=fun(#forrth{input=Input} = Forrth, _Entry) -> 
                [BlockId|RestInput] = Input,
                Filename = "BLOCK-" ++ BlockId ++ ".forrth",
                {Result, Content} = file:read_file(Filename),
                if
                    Result == ok ->
                        % TODO: Figure out why we have to droplast here
                        Words = lists:droplast(parse_input(binary_to_list(Content), #parse_state{})),
                        Forrth#forrth{input=Words ++ RestInput};
                    true ->
                        forrth_error(Forrth, "Can't read file: ~s, ~s~n", [Filename, Content])
                end
            end
    }
.
        
%-------------------------------------------------------------------------------
% entry_dot_quote: Returns a { ." } entry
%
% Grabs next word from stack and pushes it onto the stack
%
entry_dot_quote(Word) ->
    #entry{ word=Word, 
            routine=fun(#forrth{input=Input, stack=Stack} = Forrth, _Entry) ->
                [String|Rest] = Input,
                Forrth#forrth{input=Rest, stack=[String] ++ Stack}
            end
    }
.
        
%-------------------------------------------------------------------------------
% entry_l_paren: Returns a { ( } entry to start a paren comment
%
% Grabs words until we see ')'
%
entry_l_paren(Word) ->
    #entry{ word=Word, 
            immediate=true,
            routine=fun(#forrth{input=Input} = Forrth, _Entry) ->
                NewInput = skip_till_past(Input, ")"),
                Forrth#forrth{input=NewInput}
            end
    }
.


%-------------------------------------------------------------------------------
% skip_till_past/2
%
% Skips through a list of words until we hit the StopWord (inclusive)
%
skip_till_past([Word|Rest], StopWord) ->
    if
        Word == StopWord -> Rest;
        true -> skip_till_past(Rest, StopWord)
    end
;
skip_till_past([], _StopWord) ->
    []
.
        
            
%-------------------------------------------------------------------------------
% Routine to execute a definition
%
execute_definition(#forrth{ip=IP, return_stack=RetStack} = Forrth, Entry) ->
    RetStackNext = [IP] ++ RetStack,
    Forrth#forrth{
        ip=#instruction_pointer{index=1, entry=Entry},
        return_stack=RetStackNext,
        executing_definition=true
    }
.

%-------------------------------------------------------------------------------
% entry_start_definition: Returns a { : } entry
%
entry_start_definition(Word) ->
    #entry{ word=Word,
            routine=fun(Forrth, _Entry) -> 
                % Use next input word as the word for the new entry
                [NextWord|Rest] = Forrth#forrth.input,
                Forrth#forrth{input=Rest,
                              cur_definition=#entry{word=NextWord, routine=fun execute_definition/2},
                              compiling=true}
            end
    }
.

%-------------------------------------------------------------------------------
% Exits a definition being executed
%
entry_exit_definition() ->
    #entry{ 
            routine=fun(Forrth, _Entry) -> 
                [PrevIP|Rest] = Forrth#forrth.return_stack,
                if
                    Rest == [] ->
                        Forrth#forrth{return_stack=Rest, ip=PrevIP, executing_definition=false};
                    true ->
                        Forrth#forrth{return_stack=Rest, ip=PrevIP}
                end
            end
    }
.

%-------------------------------------------------------------------------------
% entry_end_definition: Returns a { ; } entry
%
entry_end_definition(Word) ->
    #entry{ word=Word,
            immediate=true,
            routine=fun(#forrth{cur_definition=CurDef, dictionary=Dictionary} = Forrth, _Entry) ->
                % Add exit definition to CurDef.parameters
                Params = CurDef#entry.parameters,
                FinishedDef = CurDef#entry{parameters=Params ++ [entry_exit_definition()]},
                Forrth#forrth{cur_definition=undefined, compiling=false, dictionary=[FinishedDef] ++ Dictionary}
            end
    }
.


%-------------------------------------------------------------------------------
% init/0
%
% Last update:  12/17/2017
% Description:  Creates a new forrth record with core words
%
% Returns:      forrth record
%
init() ->
        #forrth{dictionary=[entry_print_hi("PRINT-HI"),
                            entry_start_definition(":"),
                            entry_end_definition(";"),
                            entry_dot_quote(".\""),
                            entry_l_paren("("),
                            entry_interpret("INTERPRET"),
                            entry_load("LOAD")
                        ]
        }
.
    
%-------------------------------------------------------------------------------
% parse_input/2
%
% Last update:  12/16/2017
% Description:  Parses a string into a list of words.
%               This handles the Forrth { ." } word by collecting a string and
%               then adding a {."} word followed by the collected string.
%
% Returns:      List of parsed words
%
parse_input([Char|Rest], #parse_state{cur_word= CurWord, words=Words, state=State} = P) ->
    if
        State == ?start ->
            if
                (Char == ?space) or (Char == ?newline) -> parse_input(Rest, P);
                Char == $# -> parse_input(Rest, P#parse_state{state=?checkComment, cur_word=CurWord++[Char]});
                Char == $. -> parse_input(Rest, P#parse_state{state=?checkString, cur_word=CurWord++[Char]});
                true -> parse_input(Rest, P#parse_state{state=?normal, cur_word=CurWord++[Char]})
            end;
            
        State == ?normal ->
            if
                (Char == ?space) or (Char == ?newline) -> parse_input(Rest, P#parse_state{state=?start, cur_word = "", words=Words ++ [CurWord]});
                Char == $. -> parse_input(Rest, P#parse_state{state=?checkString, cur_word=CurWord++[Char]});
                Char == $# -> parse_input(Rest, P#parse_state{state=?checkComment, cur_word=CurWord++[Char]});
                true -> parse_input(Rest, P#parse_state{state=?normal, cur_word=CurWord++[Char]})
            end;

        State == ?checkComment ->
            if
                Char == ?space -> parse_input(Rest, P#parse_state{cur_word="", state=?skipComment});
                true -> parse_input(Rest, P#parse_state{state=?normal, cur_word=CurWord++[Char]})
            end;

        State == ?checkString ->
            if
                Char == $" -> parse_input(Rest, P#parse_state{cur_word="", state=?collectString});
                true -> parse_input(Rest, P#parse_state{state=?normal, cur_word=CurWord++[Char]})
            end;

        State == ?skipComment ->
            if
                Char == ?newline ->
                    parse_input(Rest, P#parse_state{cur_word="", state=?start});
                true -> parse_input(Rest, P)
            end;
        
        State == ?collectString ->
            if
                Char == $" ->
                    parse_input(Rest, P#parse_state{cur_word="", words=Words++[".\"", string:substr(CurWord,2)], state=?start});
                true -> parse_input(Rest, P#parse_state{cur_word=CurWord++[Char]})
            end
    end
    ;
parse_input([], #parse_state{words=Words, cur_word=CurWord, state=State} = P) ->
    Result = if
        State == ?collectString ->
            P#parse_state{cur_word="", words=Words++[".\"", string:substr(CurWord,2)], state=?start};
        true ->
            P#parse_state{words = Words ++ [CurWord], cur_word=""}
    end,
    Result#parse_state.words
.    