#pragma once
#include "llvm/Target/TargetMachine.h"
#include "lexer.h"
#include <string>
#include <sstream>
#include <memory>

class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual llvm::Value *codegen() = 0;
  virtual std::string to_string() const {};
};

// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTnode(TOKEN tok, int val) : Val(val), Tok(tok) {}

  virtual llvm::Value *codegen() override {};

  std::string to_string() const override {return std::to_string(Val);}
};

// FloatASTnode - class for float literals like 56.2
class FloatASTnode : public ASTnode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTnode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual llvm::Value *codegen() override {};

  std::string to_string() const override {return std::to_string(Val);}
};

// BoolASTnode - class for boolean literals, true or false.
class BoolASTnode : public ASTnode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTnode(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  virtual llvm::Value *codegen() override {};
  std::string to_string() const override {return std::to_string(Val);}
};

class IdentASTnode : public ASTnode {
  std::string Name;
  TOKEN Tok;

public:
  IdentASTnode(TOKEN tok, const std::string &name) 
  : Tok(tok), Name(name) {}
  virtual llvm::Value *codegen() override {};
  std::string to_string() const override {return Name;}
};

