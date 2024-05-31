#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "../Interpreter.h"
#include "../W_PushItem.h"

#include "I_AsArray.h"
#include "I_AsModule.h"
#include "M_Global.h"
#include "S_Float.h"
#include "S_Int.h"
#include "../S_String.h"
#include "S_Address.h"
#include "S_TimePoint.h"

// =============================================================================
// Words

// ( a -- )
// Pops word from stack
class W_Pop : public Word
{
public:
    W_Pop(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        interp->StackPop();
    }
};


// ( a b -- b a )
// Swaps top two items
class W_Swap : public Word
{
public:
    W_Swap(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto b = interp->StackPop();
        auto a = interp->StackPop();
        interp->StackPush(b);
        interp->StackPush(a);
    }
};


// ( modules -- )
// Adds modules to current module's using module list
class W_UseModules : public Word
{
public:
    W_UseModules(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto item = interp->StackPop();
        vector<shared_ptr<StackItem>> modules = AsArray(item);
        for (int i = 0; i < modules.size(); i++) {
            shared_ptr<Module> m = AsModule(modules[i]);
            interp->CurModule()->UseModule(m);
        }
    }
};


// ( names -- )
// Creates variables in current module
class W_Variables : public Word
{
public:
    W_Variables(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto names = AsArray(interp->StackPop());

        for (int i = 0; i < names.size(); i++) {
            string name = AsString(names[i]);
            interp->CurModule()->EnsureVariable(name);
        }
    }
};


// ( value variable -- )
// Sets variable value
class W_Bang : public Word
{
public:
    W_Bang(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto variable_item = interp->StackPop();
        S_Variable* variable = dynamic_cast<S_Variable*>(variable_item.get());
        auto value = interp->StackPop();
        variable->SetValue(value);
    }
};


// ( variable -- value )
// Gets variable value
class W_At : public Word
{
public:
    W_At(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto variable_item = interp->StackPop();
        S_Variable* variable = dynamic_cast<S_Variable*>(variable_item.get());
        interp->StackPush(variable->GetValue());
    }
};


// ( -- )
// Prints param stack
class W_DotS : public Word
{
public:
    W_DotS(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        stack<shared_ptr<StackItem>> temp_stack;

        int stack_size = interp->StackSize();

        if (stack_size == 0) {
            printf("[]\n");
            return;
        }

        for (int i=0; i < stack_size; i++) {
            auto item = interp->StackPop();
            printf("[%d] %s\n", i, item->StringRep().c_str());
            temp_stack.push(item);
        } 

        // Push items back
        for (int i=0; i < stack_size; i++) {
            auto item = temp_stack.top();
            temp_stack.pop();
            interp->StackPush(item);
        } 
    }
};


// ( l r -- res )
class W_Arith : public Word
{
public:
    W_Arith(string name, string op) : Word(name), op(op) {};

    virtual void Execute(Interpreter *interp) {
        float r = AsFloat(interp->StackPop());
        float l = AsFloat(interp->StackPop());

        float res = 0.0;
        int int_res = 0;
        if      (op == "+")    res = l + r;
        else if (op == "-")    res = l - r;
        else if (op == "*")    res = l * r;
        else if (op == "/")    res = l / r;
        else if (op == "<<")   int_res = (int)l << (int)r;
        else                  throw string("Unknown operation: ") + op;

        if (op == "<<") {
            interp->StackPush(shared_ptr<S_Int>(new S_Int(int_res)));
        }
        else {
            interp->StackPush(shared_ptr<S_Float>(new S_Float(res)));
        }
    }
protected:
    string op;
};


// ( item -- )
class W_Print : public Word
{
public:
    W_Print(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto item = interp->StackPop();
        printf("%s\n", item->AsString().c_str());
    }
};


// ( item -- )
class W_Stop : public Word
{
public:
    W_Stop(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        throw "STOP";
    }
};


// ( ms -- )
class W_MSleep : public Word
{
public:
    W_MSleep(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int msec = AsInt(interp->StackPop());
        usleep(msec*1000);
    }
};

// ( strings -- string )
class W_Concat : public Word
{
public:
    W_Concat(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto strings = AsArray(interp->StackPop());
        string result;
        for (int i=0; i < strings.size(); i++) {
            result += strings[i]->AsString();
        }
        interp->StackPush(S_String::New(result));
    }
};

// ( -- "\n" )
class W_Endl : public Word
{
public:
    W_Endl(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        interp->StackPush(S_String::New("\n"));
    }
};


// ( items str -- )
class W_Foreach : public Word
{
public:
    W_Foreach(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        string str = AsString(interp->StackPop());
        auto items = AsArray(interp->StackPop());
        for (int i=0; i < items.size(); i++) {
            interp->StackPush(items[i]);
            interp->Run(str);
        }
    }
};



// ( num-bytes -- address )
class W_Malloc : public Word
{
public:
    W_Malloc(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num_bytes = AsInt(interp->StackPop());
        void* ref = malloc(num_bytes);
        interp->StackPush(shared_ptr<S_Address>(new S_Address(ref)));
    }
};


// ( address value num-bytes -- )
class W_Memset : public Word
{
public:
    W_Memset(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num_bytes = AsInt(interp->StackPop());
        int value = AsInt(interp->StackPop());
        void* address = AsVoidStar(interp->StackPop());
        memset(address, value, num_bytes);
    }
};


// ( address -- )
class W_Free : public Word
{
public:
    W_Free(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        void* address = AsVoidStar(interp->StackPop());
        free(address);
    }
};

// ( forthic -- ? )
class W_Interpret : public Word
{
public:
    W_Interpret(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto forthic = AsString(interp->StackPop());
        interp->Run(forthic);
    }
};

// ( forthic -- ? )
class W_SlashInterpret : public Word
{
public:
    W_SlashInterpret(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto forthic = AsString(interp->StackPop());
        interp->ContextPush(nullptr);
        interp->Run(forthic);
        interp->ContextPop();
    }
};

// ( names -- )
class W_Publish : public Word
{
public:
    W_Publish(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto names = AsArray(interp->StackPop());
        auto parent_module = interp->ParentModule();
        for (int i=0; i < names.size(); i++) {
            auto w = interp->FindWord(AsString(names[i]));
            parent_module->AddWord(w);
        }
    }
};

// ( -- time_point )
class W_Now : public Word
{
public:
    W_Now(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        interp->StackPush(S_TimePoint::New(high_resolution_clock::now()));
    }
};

// ( l_time_point r_time_point -- ms )
class W_Since : public Word
{
public:
    W_Since(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto r_time_point = AsTimePoint(interp->StackPop());
        auto l_time_point = AsTimePoint(interp->StackPop());
        auto duration = r_time_point - l_time_point;
        long long result = duration_cast<milliseconds>(duration).count();
        interp->StackPush(S_Int::New(result));
    }
};

// =============================================================================
// M_Global

M_Global::M_Global() : Module("Forthic.global")
{
    AddWord(shared_ptr<Word>(new W_Pop("POP")));
    AddWord(shared_ptr<Word>(new W_Swap("SWAP")));
    AddWord(shared_ptr<Word>(new W_UseModules("USE-MODULES")));
    AddWord(shared_ptr<Word>(new W_Variables("VARIABLES")));
    AddWord(shared_ptr<Word>(new W_Bang("!")));
    AddWord(shared_ptr<Word>(new W_At("@")));
    AddWord(shared_ptr<Word>(new W_DotS(".s")));
    AddWord(shared_ptr<Word>(new W_Arith("+", "+")));
    AddWord(shared_ptr<Word>(new W_Arith("-", "-")));
    AddWord(shared_ptr<Word>(new W_Arith("*", "*")));
    AddWord(shared_ptr<Word>(new W_Arith("/", "/")));
    AddWord(shared_ptr<Word>(new W_Arith("<<", "<<")));
    AddWord(shared_ptr<Word>(new W_Print("PRINT")));
    AddWord(shared_ptr<Word>(new W_Stop("STOP")));
    AddWord(shared_ptr<Word>(new W_MSleep("MSLEEP")));
    AddWord(shared_ptr<Word>(new W_Concat("CONCAT")));
    AddWord(shared_ptr<Word>(new W_Endl("ENDL")));
    AddWord(shared_ptr<Word>(new W_Foreach("FOREACH")));
    AddWord(shared_ptr<Word>(new W_Malloc("MALLOC")));
    AddWord(shared_ptr<Word>(new W_Memset("MEMSET")));
    AddWord(shared_ptr<Word>(new W_Free("FREE")));
    AddWord(shared_ptr<Word>(new W_Interpret("INTERPRET")));
    AddWord(shared_ptr<Word>(new W_SlashInterpret("/INTERPRET")));
    AddWord(shared_ptr<Word>(new W_Publish("PUBLISH")));

    AddWord(shared_ptr<Word>(new W_Now("NOW")));
    AddWord(shared_ptr<Word>(new W_Since("SINCE")));
}


shared_ptr<Word> M_Global::treat_as_float(string name)
{
    try {
        float value = stof(name);
        return shared_ptr<Word>(new W_PushItem(name, shared_ptr<StackItem>(new S_Float(value))));
    }
    catch (...) {
        return nullptr;
    }
}


shared_ptr<Word> M_Global::treat_as_int(string name)
{
    try {
        string::size_type sz;
        int value = stoi(name, &sz);
        char c = name[sz];
        if (c == '.' || c == 'e' || c == 'E') return nullptr;
        else  return shared_ptr<Word>(new W_PushItem(name, shared_ptr<StackItem>(new S_Int(value))));
    }
    catch (...) {
        return nullptr;
    }
}


shared_ptr<Word> M_Global::treat_as_literal(string name)
{
    shared_ptr<Word> result = nullptr;
    if (result == nullptr)  result = treat_as_int(name);
    if (result == nullptr)  result = treat_as_float(name);
    return result;
}
