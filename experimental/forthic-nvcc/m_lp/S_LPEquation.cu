#include <sstream>
#include "../m_global/I_AsFloat.h"
#include "S_LPEquation.h"

S_LPEquation::S_LPEquation(vector<shared_ptr<StackItem>> coeff_vals, string name) : name(name) {
    num_coeffs = coeff_vals.size();

    coeffs = (float*)malloc(num_coeffs*sizeof(float));
    if (coeffs == nullptr)   throw "S_LPEquation - malloc failed";

    // Copy values over
    float* cur_val = coeffs;
    for (int i=0; i < coeff_vals.size(); i++) {
        *cur_val++ = AsFloat(coeff_vals[i]);
    }
}

S_LPEquation::~S_LPEquation() {
    free((void*)coeffs);
}

shared_ptr<S_LPEquation> S_LPEquation::New(vector<shared_ptr<StackItem>> items, string name) {
    auto result = shared_ptr<S_LPEquation>(new S_LPEquation(items, name));
    return result;
}


string S_LPEquation::StringRep() {
    stringstream builder;
    builder << "S_LPEquation: " << name;
    return builder.str();
}

string S_LPEquation::AsString() {
    return StringRep();
}


S_LPEquation* AsLPEquationItem(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<S_LPEquation*>(item.get())) {
        return i;
    }
    else {
        throw item->StringRep() + " is not an S_LPEquation";
    }
}
