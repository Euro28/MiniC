#ifndef AST_H
#define AST_H
#include "llvm/Target/TargetMachine.h"
#include "lexer.h"
#include <sstream>
using namespace llvm;

/// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string(int level) const {};
};

//indenting to make AST easier to read
std::string indent(int level) {
  std::stringstream ss; 
  for (int i=0; i < level; i++)
    ss << "\t";
  return ss.str();
}

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

class ParamsASTnode;
class BlockASTnode;
class VariableDeclarationASTnode;
class StatementASTnode;
class IfStatementASTnode;
class ExpresssionStatementASTnode;
class ReturnStatementASTnode;
class WhileStatementASTnode;
class ExpressionASTnode;
class ArgumentListASTnode;
class StatementListASTnode;


class VariableDeclarationASTnode : public ASTnode {
  int Var_type;
  std::string Identifier;

public:

  VariableDeclarationASTnode(int var_type, const std::string &identifier)
  : Var_type(var_type), Identifier(identifier) {}

  std::string to_string(int level) const override;

  std::string getId() { return Identifier;}
  int getType() {return Var_type;}

  Value *codegen() override;
};

class LocalDeclarationsASTnode : public ASTnode {
  //this is just a vector of variable declarations
  std::vector<std::unique_ptr<VariableDeclarationASTnode>> Local_decls;

public:
  LocalDeclarationsASTnode(std::vector<std::unique_ptr<VariableDeclarationASTnode>> local_decls)
  : Local_decls(std::move(local_decls)) {}

  std::vector<std::unique_ptr<VariableDeclarationASTnode>> getDecls() {
    return std::move(Local_decls);
  }
  std::string to_string(int level) const override;
  virtual Value *codegen() override {};
};

//if there is an assignLHS there is an assign expression, if no assignLHS then its just an rval and output that
class ExpressionASTnode : public ASTnode {
  std::string AssignLHS;
  std::unique_ptr<ExpressionASTnode> Assign;
  std::unique_ptr<ASTnode> Rval;

public:
  ExpressionASTnode(std::unique_ptr<ExpressionASTnode> assign, const std::string &LHS) 
  : Assign(std::move(assign)), AssignLHS(LHS) {}

  ExpressionASTnode(std::unique_ptr<ASTnode> rval, const std::string &LHS)
  : Rval(std::move(rval)), AssignLHS(LHS) {}

  ExpressionASTnode(std::unique_ptr<ASTnode> rval)
  : Rval(std::move(rval)) {}

  std::string to_string(int level) const override;

  virtual Value *codegen() override {};
 
};

class ExpresssionStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;
  bool Colon;

public: 
  ExpresssionStatementASTnode(std::unique_ptr<ExpressionASTnode> expr) 
  : Expr(std::move(expr)), Colon(false) {}

  ExpresssionStatementASTnode(bool colon) : Colon(colon) {}

  std::string to_string(int level) const override {
    if (!Colon)
      return Expr->to_string(level);
    else return ";";
  }

  virtual Value *codegen() override {};
};

class ReturnStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;

public:
  virtual Value *codegen() override {};

  ReturnStatementASTnode(std::unique_ptr<ExpressionASTnode> expr)
  : Expr(std::move(expr)) {}

  ReturnStatementASTnode() {}

  std::string to_string(int level) const override;
};

class WhileStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;
  std::unique_ptr<StatementASTnode> Stmt;

public:
  virtual Value *codegen() override {};

  WhileStatementASTnode(std::unique_ptr<ExpressionASTnode> expr,std::unique_ptr<StatementASTnode> stmt)
  : Expr(std::move(expr)), Stmt(std::move(stmt)) {}

  std::string to_string(int level) const override;
};

class StatementListASTnode : public ASTnode {
  //this is just a vector of statesments
  std::vector<std::unique_ptr<StatementASTnode>> Stmt_list;

public:
  StatementListASTnode(std::vector<std::unique_ptr<StatementASTnode>> stmt_list)
  : Stmt_list(std::move(stmt_list)) {}

  StatementListASTnode() {}

  std::vector<std::unique_ptr<StatementASTnode>> getStmts() {
    return std::move(Stmt_list);
  }

  std::string to_string(int level) const override;

  virtual Value *codegen() override {};
};

class BlockASTnode : public ASTnode {
  std::unique_ptr<LocalDeclarationsASTnode> Local_decls;
  std::unique_ptr<StatementListASTnode> Stmt_list;
  //0 or more local declarations followed by 0 or more statement lists

public:
  virtual Value *codegen() override {};

  BlockASTnode(std::unique_ptr<LocalDeclarationsASTnode> local_decls, std::unique_ptr<StatementListASTnode> stmt_list)
  : Local_decls(std::move(local_decls)), Stmt_list(std::move(stmt_list)) {}

  std::string to_string(int level) const override;

};

class ElseStatementASTnode : public ASTnode  {
  std::unique_ptr<BlockASTnode> Block;

public:
  virtual Value *codegen() override {};

  ElseStatementASTnode(std::unique_ptr<BlockASTnode> block)
  : Block(std::move(block)) {}

  ElseStatementASTnode() {}

  std::string to_string(int level) const override;
};

class IfStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;
  std::unique_ptr<BlockASTnode> Block;
  std::unique_ptr<ElseStatementASTnode> Else;

public:
  virtual Value *codegen() override {};

  IfStatementASTnode(std::unique_ptr<ExpressionASTnode> expr, std::unique_ptr<BlockASTnode> block, 
  std::unique_ptr<ElseStatementASTnode> else_ptr) 
  : Expr(std::move(expr)), Block(std::move(block)), Else(std::move(else_ptr)) {}

  std::string to_string(int level) const override;

};

class StatementASTnode : public ASTnode {
  //this is either an expr_stmt, if, while, block, or return stmt
  std::unique_ptr<IfStatementASTnode> If_stmt;
  std::unique_ptr<BlockASTnode> Block;
  std::unique_ptr<ExpresssionStatementASTnode> Expr_stmt;
  std::unique_ptr<WhileStatementASTnode> While_stmt;
  std::unique_ptr<ReturnStatementASTnode> Return_stmt;

public:
  StatementASTnode(std::unique_ptr<ReturnStatementASTnode> return_stmt)
  : Return_stmt(std::move(return_stmt)) {}

  StatementASTnode(std::unique_ptr<ExpresssionStatementASTnode> expr_stmt)
  : Expr_stmt(std::move(expr_stmt)) {}

  StatementASTnode(std::unique_ptr<IfStatementASTnode> if_stmt)
  : If_stmt(std::move(if_stmt)) {}

  StatementASTnode(std::unique_ptr<BlockASTnode> block)
  : Block(std::move(block)) {}

  StatementASTnode(std::unique_ptr<WhileStatementASTnode> while_stmt)
  : While_stmt(std::move(while_stmt)) {}

  std::string to_string(int level) const override;
  virtual Value *codegen() override {};
};


class ParameterASTnode : public ASTnode {
  int Type;
  std::string Ident;

public:
  virtual Value *codegen() override {};

  ParameterASTnode(int type, const std::string &ident) 
  : Type(type), Ident(ident) {}

  int getType() {return Type;}

  std::string getIdent() {return Ident;}

  std::string to_string(int level) const override;

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

  bool getIsVoid() {
    return isVoid;
  };

  std::vector<std::unique_ptr<ParameterASTnode>> getParams() {
    return std::move(Params);
  }

  std::string to_string(int level) const override;

  std::string list_types() const {
    std::string result;
    for (int i=0; i < Params.size(); i++) {
      result += type_to_string(Params.at(i)->getType());
      if (i != Params.size()-1)
        result += ",";
    }
    return result;
  }

  virtual Value *codegen() override {};
};

class FuncProto: public ASTnode {
  int Token_type;
  std::string Identifier;
  std::unique_ptr<ParamsASTnode> Params;

public:
  FuncProto(int token_type, const std::string &identifier, std::unique_ptr<ParamsASTnode> params)
  : Token_type(token_type), Identifier(identifier), Params(std::move(params)) {}

  std::string getIdent() {return Identifier;}
  int getType() {return Token_type;}

  std::unique_ptr<ParamsASTnode> getParam() {
    return std::move(Params);
  }
  std::string paramString() {
    return Params->list_types();
  }

  std::string ParamASTstring(int level) {
    return Params->to_string(level);
  }

  std::string to_string(int level) const override;

  Function *codegen() override;
};

class FunctionDeclarationASTnode : public ASTnode {
  std::unique_ptr<BlockASTnode> Block;
  std::unique_ptr<FuncProto> Proto;

public:

  FunctionDeclarationASTnode(std::unique_ptr<FuncProto> proto, std::unique_ptr<BlockASTnode> block)
  : Proto(std::move(proto)), Block(std::move(block)) {}
  
  Value *codegen() override;

  std::string ParamASTline(int level) {
    return Proto->ParamASTstring(level);
  }


  std::string to_string(int level) const override;
};

class DeclarationASTnode : public ASTnode {
  std::unique_ptr<VariableDeclarationASTnode> Var_decl;
  std::unique_ptr<FunctionDeclarationASTnode> Fun_decl;

public:
  Value *codegen() override;

  DeclarationASTnode(std::unique_ptr<VariableDeclarationASTnode> var_decl) 
  : Var_decl(std::move(var_decl)) {}

  std::unique_ptr<VariableDeclarationASTnode> getVarDecl() {
    return std::move(Var_decl);
  }

  DeclarationASTnode(std::unique_ptr<FunctionDeclarationASTnode> fun_decl)
  : Fun_decl(std::move(fun_decl)) {}

  std::unique_ptr<FunctionDeclarationASTnode> getFunDecl() {
    return std::move(Fun_decl);
  }

  std::string to_string(int level) const override;
};

class DeclarationListASTnode : public ASTnode {
  //will contain a vector of declarations
  std::vector<std::unique_ptr<DeclarationASTnode>> List_decl;

public:
  virtual Value *codegen() override {};

  DeclarationListASTnode(std::vector<std::unique_ptr<DeclarationASTnode>> list_decl)
  : List_decl(std::move(list_decl)) {}

  std::vector<std::unique_ptr<DeclarationASTnode>> getDecls() {
    return std::move(List_decl);
  }

  std::string to_string(int level) const override;

  DeclarationListASTnode() {}
};


class ExternListASTnode : public ASTnode { //seg fault is caused by defining the pointers of List_Extern but
//not instantiating it in the constructor
  std::vector<std::unique_ptr<FuncProto>> List_Extern;

public:
  ExternListASTnode(std::vector<std::unique_ptr<FuncProto>> list_extern)
  : List_Extern(std::move(list_extern)) {}

  ExternListASTnode() {}

  std::vector<std::unique_ptr<FuncProto>> getExterns() {
    return std::move(List_Extern);
  };

  virtual Value *codegen() override {};

  std::string to_string(int level) const override;

  //will contain a vector of externs
};

//A program is just a list of extern declarations and then 
//list of normal declartions or just a list of declarations
class ProgramASTnode : public ASTnode {
  std::vector<std::unique_ptr<FuncProto>> Extern_list; //list of externs
  std::vector<std::unique_ptr<DeclarationASTnode>> Decl_list;
  

public: 
  ProgramASTnode(std::unique_ptr<ExternListASTnode> extern_list, std::unique_ptr<DeclarationListASTnode> decl_list);
 //have to move because the list classes have smart pointers as their members

  Value *codegen() override ;

  std::string to_string(int level) const override {
    std::stringstream ss;
    if (Extern_list) 
      ss << Extern_list->to_string(level);
    ss << Decl_list->to_string(level);
    return ss.str();
  }
};


class TypeSpecASTnode : public ASTnode {
  int Type;

public:
  TypeSpecASTnode(int type) : Type(type) {}
  virtual Value *codegen() override {};

  int getType() {
    return Type;
  }
};

class VariableTypeASTnode : public ASTnode {
  int Type;

public:
  VariableTypeASTnode(int type) : Type(type) {}
  virtual Value *codegen() override {};

  int getType() {
    return Type;
  }
};


class BinExpressionASTnode : public ASTnode {
  int Op;
  std::unique_ptr<ASTnode> LHS, RHS;

public:
  BinExpressionASTnode(std::unique_ptr<ASTnode> lhs, int op, std::unique_ptr<ASTnode> rhs)
  : LHS(std::move(lhs)), Op(op), RHS(std::move(rhs)) {}

  std::string to_string(int level) const override;
  
  virtual Value *codegen() override {};
};

/*List(vector) of arguments which are just expressions*/
class ArgumentListASTnode : public ASTnode {
  std::vector<std::unique_ptr<ExpressionASTnode>> Arg_list;

public:
  virtual Value *codegen() override {};

  ArgumentListASTnode(std::vector<std::unique_ptr<ExpressionASTnode>> arg_list)
  : Arg_list(std::move(arg_list)) {}

  std::vector<std::unique_ptr<ExpressionASTnode>> getArgs() {
    return std::move(Arg_list);
  }

  std::string to_string(int level) const override;
};


/*Either a list of arguments or no arguments*/
class ArgumentsASTnode : public ASTnode {
  std::unique_ptr<ArgumentListASTnode> Arg_list;

public:
  virtual Value *codegen() override {};

  ArgumentsASTnode(std::unique_ptr<ArgumentListASTnode> arg_list)
  : Arg_list(std::move(arg_list)) {}

  ArgumentsASTnode() {}

  std::unique_ptr<ArgumentListASTnode> getArgList() {
    return std::move(Arg_list);
  }

  std::string to_string(int level) const override {return Arg_list->to_string(level);}
};

// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTnode(TOKEN tok, int val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override {};
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
  std::string to_string(int level) const override;
};
// FloatASTnode - class for float literals like 56.2
class FloatASTnode : public ASTnode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTnode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override {};

  std::string to_string(int level) const override;
};

// BoolASTnode - class for boolean literals, true or false.
class BoolASTnode : public ASTnode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTnode(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override {};

  std::string to_string(int level) const override;
};

class IdentASTnode : public ASTnode {
  std::string Name;
  TOKEN Tok;

public:
  IdentASTnode(TOKEN tok, const std::string &name) 
  : Tok(tok), Name(name) {}

  virtual Value *codegen() override {};

  std::string to_string(int level) const override;
};

// FunctionCallASTnode - node that represetns function calls
class FunctionCallASTnode : public ASTnode {
  std::string Name;
  std::unique_ptr<ArgumentsASTnode> Args;

public:
  virtual Value *codegen() override {};

  FunctionCallASTnode(const std::string &name, std::unique_ptr<ArgumentsASTnode> args)
  : Name(name), Args(std::move(args)) {}

  std::string to_string(int level) const override;

};

class UnaryOperatorASTnode : public ASTnode {
  std::string Prefix = "";
  std::unique_ptr<ASTnode> Element; //can be one of ident, function call, int, float, bool lit

public:
  UnaryOperatorASTnode(const std::string &prefix, std::unique_ptr<ASTnode> element) 
  : Prefix(prefix), Element(std::move(element)) {}

  std::unique_ptr<ASTnode> getElement() {return std::move(Element);}
  std::string getPrefix() {return Prefix;}
  std::string to_string(int level) const override;

  virtual Value *codegen() override {};
};

#endif