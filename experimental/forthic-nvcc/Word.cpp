#include "Word.h"

Word::Word(string name) : name(name) {
}

Word::~Word() {
}

void Word::Execute(Interpreter *interp) {
    // By default, do nothing
}


string Word::GetName() {
    return name;
}
