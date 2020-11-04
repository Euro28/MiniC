#pragma once
#include "params.h"

std::string ParameterASTnode::to_string() const {
    std::stringstream ss;
    ss << Ident << " '" << type_to_string(Type) << "'";
    return ss.str();
}

std::string ParamsASTnode::to_string() const {
    std::stringstream ss;
    for (int i = 0; i < Params.size(); i++) {
      ss << " | `-ParmVarDecl " << Params.at(i)->to_string() << std::endl;
    }
    return ss.str();
}

std::string ParamsASTnode::list_types() const {
    std::string result;
    for (int i=0; i < Params.size(); i++) {
      result += Params.at(i)->getType();
      if (i != Params.size()-1)
        result += ",";
    }
    return result;
}