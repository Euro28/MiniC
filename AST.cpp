#include "AST.h"
#include <iostream>

ProgramASTnode::ProgramASTnode(std::vector<std::unique_ptr<FuncProto>> extern_list,
    std::vector<std::unique_ptr<DeclarationASTnode>> decl_list)
    : Extern_list(std::move(extern_list)), Decl_list(std::move(decl_list)) {}