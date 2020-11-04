#pragma once
#include "llvm/Target/TargetMachine.h"
#include <string>

class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual llvm::Value *codegen() = 0;
  virtual std::string to_string() const {};
};