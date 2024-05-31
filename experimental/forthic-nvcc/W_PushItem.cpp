#include "W_PushItem.h"
#include "Interpreter.h"


W_PushItem::W_PushItem(string word_name, shared_ptr<StackItem> i) : Word(word_name), item(i)
{
}


void W_PushItem::Execute(Interpreter *interp)
{
    interp->StackPush(item);
}
