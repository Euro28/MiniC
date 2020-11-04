#ifndef PARAMS_H
#define PARAMS_H
#include "AST.h"

class ParameterASTnode : public ASTnode {
  int Type;
  std::string Ident;

public:
  virtual llvm::Value *codegen() override {};

  ParameterASTnode(int type, const std::string &ident) 
  : Type(type), Ident(ident) {}

  std::string getType() {return type_to_string(Type);}
  std::string getIdent() {return Ident;}
  std::string to_string() const override;
};

//Class that is associated with function/extern definition
//that tells how many arguments and if the argument is void
class ParamsASTnode : public ASTnode {
  std::vector<std::unique_ptr<ParameterASTnode>> Params;
  bool isVoid;

public:
  ParamsASTnode(std::vector<std::unique_ptr<ParameterASTnode>> params) 
  : Params(std::move(params)), isVoid(false) {} 
  ParamsASTnode(bool is_void) : isVoid(is_void) {}
  ParamsASTnode() {}

  bool getIsVoid() {return isVoid;}
  std::vector<std::unique_ptr<ParameterASTnode>> getParams() {return std::move(Params);}
  std::string to_string() const override;
  std::string list_types() const;
  virtual llvm::Value *codegen() override {};
};

#endif