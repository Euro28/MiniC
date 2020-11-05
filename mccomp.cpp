#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>
#include <tuple>
#include <sstream>
#include "lexer.h"

using namespace llvm;
using namespace llvm::sys;


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

/// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const {};
};

class VariableDeclarationASTnode : public ASTnode {
  int Var_type;
  std::string Identifier;

public:

  VariableDeclarationASTnode(int var_type, const std::string &identifier)
  : Var_type(var_type), Identifier(identifier) {}

  std::string to_string() const override {
    std::stringstream ss;
    ss << Identifier << " '" << type_to_string(Var_type) << "'";
    return ss.str();
  }

  std::string getId() { return Identifier;}

  int getType() {return Var_type;}


  virtual Value *codegen() override {};
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

  std::string to_string() const override {
    std::stringstream ss;
    for (int i = 0; i < Local_decls.size(); i++) {
      ss << "   |-VarDecl used " << Local_decls.at(i)->to_string() ;
    }
    return ss.str();
  }



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

  std::string to_string() const override {
    if (Rval) {
      return Rval->to_string();
    }
    std::string result = "The Expression is ";
    result += AssignLHS;
    result += " = ";
    result += Assign->to_string();
    return result;
  }

  virtual Value *codegen() override {};
 
};

class ExpresssionStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;
  bool Colon;

public: 
  ExpresssionStatementASTnode(std::unique_ptr<ExpressionASTnode> expr) 
  : Expr(std::move(expr)), Colon(false) {}

  ExpresssionStatementASTnode(bool colon) : Colon(colon) {}

  std::string to_string() const override {
    if (!Colon)
      return Expr->to_string();
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

  std::string to_string() const override {
    if (Expr) {
      std::stringstream ss;
      ss << "return " << Expr->to_string() << ";";
      return ss.str();
    }
    return "return ;";
  }
};

class WhileStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;
  std::unique_ptr<StatementASTnode> Stmt;

public:
  virtual Value *codegen() override {};

  WhileStatementASTnode(std::unique_ptr<ExpressionASTnode> expr,std::unique_ptr<StatementASTnode> stmt)
  : Expr(std::move(expr)), Stmt(std::move(stmt)) {}

  std::string to_string() const override {
    std::stringstream ss;
    ss << "This is a while statement";
    return ss.str();
  }
};

class StatementListASTnode : public ASTnode {
  //this is just a vector of statesments
  std::vector<std::unique_ptr<StatementASTnode>> Stmt_list;

public:
  StatementListASTnode(std::vector<std::unique_ptr<StatementASTnode>> stmt_list)
  : Stmt_list(std::move(stmt_list)) {}

  std::vector<std::unique_ptr<StatementASTnode>> getStmts() {
    return std::move(Stmt_list);
  }

  std::string to_string() const override;

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

  std::string to_string() const override {
    std::stringstream ss;
    ss << Local_decls->to_string() << std::endl << Stmt_list->to_string();
    return ss.str();
  }

};

class ElseStatementASTnode : public ASTnode  {
  std::unique_ptr<BlockASTnode> Block;

public:
  virtual Value *codegen() override {};

  ElseStatementASTnode(std::unique_ptr<BlockASTnode> block)
  : Block(std::move(block)) {}

  ElseStatementASTnode() {}

  std::string to_string() const override {
    if (Block) {
      return Block->to_string();
    }
    return "";
  }

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

  std::string to_string() const override {
    std::stringstream ss;
    ss << "if (" << Expr->to_string() << " ) " << Block->to_string() << " else " << Else->to_string();
    return ss.str();
  };

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

  std::string to_string() const override {
    if (Expr_stmt) {
      return Expr_stmt->to_string();
    } 
    else if (Block) {
      return Block->to_string();
    }
    else if (If_stmt) {
      return If_stmt->to_string();
    }
    else if (While_stmt) {
      return While_stmt->to_string();
    } 
    else if (Return_stmt) {
      return Return_stmt->to_string();
    }
  }
  virtual Value *codegen() override {};
};


class ParameterASTnode : public ASTnode {
  int Type;
  std::string Ident;

public:
  virtual Value *codegen() override {};

  ParameterASTnode(int type, const std::string &ident) 
  : Type(type), Ident(ident) {}

  std::string getType() {return type_to_string(Type);}

  std::string getIdent() {return Ident;}

  std::string to_string() const override {
    std::stringstream ss;
    ss << Ident << " '" << type_to_string(Type) << "'";
    return ss.str();
  };

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

  std::string to_string() const override {
    std::stringstream ss;
    for (int i = 0; i < Params.size(); i++) {
      ss << " | `-ParmVarDecl " << Params.at(i)->to_string() << std::endl;
    }
    return ss.str();
  }

  std::string list_types() const {
    std::string result;
    for (int i=0; i < Params.size(); i++) {
      result += Params.at(i)->getType();
      if (i != Params.size()-1)
        result += ",";
    }
    return result;
  }

  virtual Value *codegen() override {};
};

class FunctionDeclarationASTnode : public ASTnode {
  int Fun_type;
  std::string Identifier;
  std::unique_ptr<ParamsASTnode> Params;
  std::unique_ptr<BlockASTnode> Block;

public:

  FunctionDeclarationASTnode(
    int fun_type, const std::string &identifier, std::unique_ptr<ParamsASTnode> params, std::unique_ptr<BlockASTnode> block)
  : Fun_type(fun_type), Identifier(identifier), Params(std::move(params)), Block(std::move(block)) {}
  
  virtual Value *codegen() override {};

  std::unique_ptr<ParamsASTnode> getParams() {
    return std::move(Params);
  }

  std::string to_string() const override {
    std::stringstream ss;
    ss << Identifier << "'" << type_to_string(Fun_type) << " ("  << Params->list_types() << ")'" << std::endl;
    ss << Params->to_string();
    if (Block)
      ss << " `-BlockStatement" << std::endl << Block->to_string();

    //output local decls
      //out stmt list
    return ss.str();
  }
};

class DeclarationASTnode : public ASTnode {
  std::unique_ptr<VariableDeclarationASTnode> Var_decl;
  std::unique_ptr<FunctionDeclarationASTnode> Fun_decl;

public:
  virtual Value *codegen() override {};

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

  std::string to_string() const override {
    std::stringstream ss;
    if (Var_decl)
      ss << "|-VarDecl " << Var_decl->to_string();
    if (Fun_decl) 
      ss << "`-FunctionDecl " << Fun_decl->to_string();
    
    return ss.str();
    
  }


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

  std::string to_string() const override {
    std::stringstream ss;
    for (int i = 0; i < List_decl.size(); i++) {
      ss << List_decl.at(i)->to_string();
      if (i != List_decl.size()-1)
        ss << std::endl;
    }
    return ss.str();
  }

  DeclarationListASTnode() {}
};


class ExternASTnode: public ASTnode {
  int Token_type;
  std::string Identifier;
  std::unique_ptr<ParamsASTnode> Params;

public:
  ExternASTnode(int token_type, const std::string &identifier, std::unique_ptr<ParamsASTnode> params)
  : Token_type(token_type), Identifier(identifier), Params(std::move(params)) {}

  std::string getIdent() {
    return Identifier;
  }

  std::string to_string() const override {
    std::stringstream ss;
    ss << " " << Identifier << " '" << type_to_string(Token_type) << " (" << Params->list_types() << ")' extern" << std::endl;
    ss << Params->to_string();
    return ss.str();
  }


  virtual Value *codegen() override {};

};

class ExternListASTnode : public ASTnode { //seg fault is caused by defining the pointers of List_Extern but
//not instantiating it in the constructor
  std::vector<std::unique_ptr<ExternASTnode>> List_Extern;

public:
  ExternListASTnode(std::vector<std::unique_ptr<ExternASTnode>> list_extern)
  : List_Extern(std::move(list_extern)) {}

  ExternListASTnode() {}

  std::vector<std::unique_ptr<ExternASTnode>> getExterns() {
    return std::move(List_Extern);
  };

  virtual Value *codegen() override {};

  std::string to_string() const override {
    std::stringstream ss;
    for (int i=0;i < List_Extern.size(); i++) {
      ss << "|-FunctionDecl used" << List_Extern.at(i)->to_string();
    }
    return ss.str();
  }

  //will contain a vector of externs
};

//A program is just a list of extern declarations and then 
//list of normal declartions or just a list of declarations
class ProgramASTnode : public ASTnode {
  std::unique_ptr<ExternListASTnode> Extern_list;
  std::unique_ptr<DeclarationListASTnode> Decl_list;

public: 
  ProgramASTnode(std::unique_ptr<ExternListASTnode> extern_list, std::unique_ptr<DeclarationListASTnode> decl_list)
  : Extern_list(std::move(extern_list)), Decl_list(std::move(decl_list)) {} //have to move because the list classes have smart pointers as their members

  ProgramASTnode(std::unique_ptr<DeclarationListASTnode> decl_list) 
  : Decl_list(std::move(decl_list)) {}
  virtual Value *codegen() override {};

  std::string to_string() const override {
    std::stringstream ss;
    ss << Extern_list->to_string() << Decl_list->to_string();
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

  std::string to_string() const override {
    std::string result;
    result += LHS->to_string();
    result += type_to_string(Op);
    result += RHS->to_string();
    return result;
  }
  

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

  std::string to_string() const override {
    std::stringstream ss;
    ss << "(";
    for (int i = 0; i<Arg_list.size(); i++) {
      ss << Arg_list.at(i)->to_string();
    }
    ss << ")";
    return ss.str();
  }
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

  std::string to_string() const override {return Arg_list->to_string();}
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
  std::string to_string() const override {
    return std::to_string(Val);
  }
};
// FloatASTnode - class for float literals like 56.2
class FloatASTnode : public ASTnode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTnode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override {};

  std::string to_string() const override {
    return std::to_string(Val);
  }
};

// BoolASTnode - class for boolean literals, true or false.
class BoolASTnode : public ASTnode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTnode(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override {};

  std::string to_string() const override {
    return std::to_string(Val);
  }
};

class IdentASTnode : public ASTnode {
  std::string Name;
  TOKEN Tok;

public:
  IdentASTnode(TOKEN tok, const std::string &name) 
  : Tok(tok), Name(name) {}

  virtual Value *codegen() override {};

  std::string to_string() const override {
    return Name;
  }
};

// FunctionCallASTnode - node that represetns function calls
class FunctionCallASTnode : public ASTnode {
  std::string Name;
  std::unique_ptr<ArgumentsASTnode> Args;

public:
  virtual Value *codegen() override {};

  FunctionCallASTnode(const std::string &name, std::unique_ptr<ArgumentsASTnode> args)
  : Name(name), Args(std::move(args)) {}

  std::string to_string() const override {
    std::string result = Name;
    result += Args->to_string();
    return result;
  }

};

std::string StatementListASTnode::to_string() const {
  std::stringstream ss;
    for (int i = 0; i < Stmt_list.size(); i++) {
      ss << Stmt_list.at(i)->to_string();
    }
  return ss.str();
}

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

void LogError(TOKEN tok, const std::string &Str) {
  std::cerr << Str << " on line:" << tok.lineNo << " and column: " << tok.columnNo << std::endl;
  std::exit(EXIT_FAILURE);
}

template<typename T>
std::unique_ptr<T> LogErrorPtr(TOKEN tok, const std::string &Str) {
  LogError(CurTok, Str);
  return nullptr;
}

template<typename T>
std::vector<std::unique_ptr<T>> LogErrorVector(TOKEN tok, const std::string &Str) {
  LogError(CurTok, Str);
  return {};
}

//matching terminals and in case of failure output message saves 
// repeated lines of code
static bool match(int match, const std::string &name) {
  if (CurTok.type == match) 
    return true;
  std::string result = "Expected ";
  result += name;

  LogError(CurTok,result);
}


/* Add function calls for each production */
static std::unique_ptr<ExternListASTnode> ParseExternList();
static std::vector<std::unique_ptr<ExternASTnode>> ParseExternListPrime();
static std::unique_ptr<ParamsASTnode> ParseParams();
static std::unique_ptr<ExternASTnode> ParseExtern();
static std::unique_ptr<ParameterASTnode> ParseParam();
static std::vector<std::unique_ptr<ParameterASTnode>> ParseParamListPrime();
static std::unique_ptr<ParamsASTnode> ParseParamList();
static TOKEN lookahead(int);
static std::unique_ptr<DeclarationListASTnode> ParseDeclList();
static std::unique_ptr<DeclarationASTnode> ParseDecl();
static std::unique_ptr<VariableDeclarationASTnode> ParseVarDecl();
static std::vector<std::unique_ptr<DeclarationASTnode>> ParseDeclListPrime();
static std::unique_ptr<LocalDeclarationsASTnode> ParseLocalDecls();
static std::unique_ptr<ASTnode> ParseTerm();
static std::unique_ptr<ASTnode> ParseEquiv();
static std::unique_ptr<ASTnode> ParseEquivPrime(std::unique_ptr<ASTnode>);
static std::unique_ptr<ASTnode> ParseRvalPrime(std::unique_ptr<ASTnode>);
static std::unique_ptr<ASTnode> ParseTermPrime(std::unique_ptr<ASTnode>);
static std::unique_ptr<StatementListASTnode> ParseStmtList();
static std::unique_ptr<ASTnode> ParseRel();
static std::unique_ptr<ASTnode> ParseRelPrime(std::unique_ptr<ASTnode>);
static std::unique_ptr<ASTnode> ParseSubExpr();
static std::unique_ptr<ASTnode> ParseSubExprPrime(std::unique_ptr<ASTnode>);
static std::unique_ptr<ASTnode> ParseFactor();
static std::unique_ptr<ASTnode> ParseFactorPrime(std::unique_ptr<ASTnode>);
static std::unique_ptr<ASTnode> ParseElement();
static std::unique_ptr<ArgumentListASTnode> ParseArgList();
static std::unique_ptr<ExpressionASTnode> ParseExpr();
static std::unique_ptr<ArgumentsASTnode> ParseArgs();
static std::unique_ptr<StatementASTnode> ParseStmt();

static std::unique_ptr<TypeSpecASTnode> ParseTypeSpec() {
  if (CurTok.type == INT_TOK || CurTok.type == VOID_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    auto result = std::make_unique<TypeSpecASTnode>(CurTok.type);
    getNextToken(); //eat int/float/bool/void
    return result;
  } 
  else return LogErrorPtr<TypeSpecASTnode>(CurTok, "expected one of int, float or bool or void");
}

static std::unique_ptr<VariableTypeASTnode> ParseVarType() {
  if (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    auto result = std::make_unique<VariableTypeASTnode>(CurTok.type);
    getNextToken(); //eat int/float/bool
    return result;
  } 
  else return LogErrorPtr<VariableTypeASTnode>(CurTok,"expected one of int, float or bool");
}

// program ::= extern_list decl_list
//          | decl_list
static std::unique_ptr<ProgramASTnode> ParseProgram() {
  if (CurTok.type == EXTERN) { //PREDICT(program ::= extern_list decl_list) = {extern}
    auto extern_list = ParseExternList();
    if (extern_list) {
      std::cout << "extern_list is valid" << std::endl;
      auto decl_list = ParseDeclList();
      if (decl_list) {
        std::cout << "Decl list is valid " << std::endl;
        return std::make_unique<ProgramASTnode>(std::move(extern_list),std::move(decl_list));
      }
    }
  } 
  else if (CurTok.type == INT_TOK || CurTok.type == VOID_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    // PREDICT(program ::= decl_list) = {int, void, float, bool}
    auto decl_list = ParseDeclList();
    if (decl_list)
      return std::make_unique<ProgramASTnode>(std::move(decl_list)); 
  }
  return LogErrorPtr<ProgramASTnode>(CurTok, "Expected one of extern, int, void, float, bool"); //start of program must be one of listed
}

// decl_list ::= decl decl_list_prime
static std::unique_ptr<DeclarationListASTnode> ParseDeclList() {
  std::vector<std::unique_ptr<DeclarationASTnode>> decl_list;

  auto decl = ParseDecl();
  if (decl) {
    decl_list.push_back(std::move(decl));
    auto decl_list_prime = ParseDeclListPrime();
    for (int i = 0; i < decl_list_prime.size(); i++) {
      decl_list.push_back(std::move(decl_list_prime.at(i)));
    }
    return std::make_unique<DeclarationListASTnode>(std::move(decl_list));
  }

  return nullptr; //never actually reaching here because if decl is a nullptr then logerror was already called
}

static std::vector<std::unique_ptr<DeclarationASTnode>> ParseDeclListPrime() {
  if (CurTok.type == EOF_TOK) { //PREDICT(decl_list_prime ::= epsilon) = {EOF}
    return {};
  } 
  else if (CurTok.type == VOID_TOK || CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) { //decl_list_prime ::= decl decl_list_prime
    auto decl_list = ParseDeclList();
    if (decl_list)
      return decl_list->getDecls();
  }
  
  return {}; //never actually reachign here because if decl_list evaluates to false then logerror has been called
}

static std::unique_ptr<BlockASTnode> ParseBlock() {
  if (match(LBRA, "{")) {
    getNextToken(); //eat {
    auto local_decls = ParseLocalDecls();
    if (local_decls) {
      auto stmt_list = ParseStmtList();
      if (stmt_list) {
        if (CurTok.type == RBRA) { // THE ISSUE IS HERE MOTHER FUCKER
          getNextToken(); //eat }
          return std::make_unique<BlockASTnode>(std::move(local_decls), std::move(stmt_list));
        } 
      }
    }
  }
  return nullptr;
}


static std::unique_ptr<ASTnode> ParseRval() {
  auto term = ParseTerm();
  if (term) {
    auto rval_prime = ParseRvalPrime(std::move(term));
    return rval_prime;
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseRvalPrime(std::unique_ptr<ASTnode> term1) {
  if (CurTok.type == OR) {
    getNextToken(); //eat ||
    auto term2 = ParseTerm();
    if (term2) {
      auto node1 = std::make_unique<BinExpressionASTnode>(std::move(term1),OR, std::move(term2));
      auto rval_prime = ParseRvalPrime(std::move(node1));
      return rval_prime;
    }
  } else if (CurTok.type == SC || CurTok.type == RPAR ||
  CurTok.type == COMMA) { //PREDICT(rval_prime -> ε) = {';', ')', ','}
    return term1;
  } else return LogErrorPtr<BinExpressionASTnode>(CurTok,"Expected one of '||', ';', ')', ','");

  return nullptr;
}

static std::unique_ptr<ASTnode> ParseTerm() {
  auto equiv = ParseEquiv();
  if (equiv) {
    auto term_prime = ParseTermPrime(std::move(equiv));
    return term_prime;
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseTermPrime(std::unique_ptr<ASTnode> equiv1) {
  if (CurTok.type == AND) {
    getNextToken(); //eat &&
    auto equiv2 = ParseEquiv();
    if (equiv2) {
      auto node1 = std::make_unique<BinExpressionASTnode>(std::move(equiv1), AND, std::move(equiv2));
      auto term_prime = ParseTermPrime(std::move(node1));
      return term_prime;
    }
  } else if (CurTok.type == OR || CurTok.type == SC ||
  CurTok.type == RPAR || CurTok.type == COMMA) { //PREDICT(term_prime ::= ε) = {'||', ';', ')', ','}
    return equiv1;
  } else return LogErrorPtr<BinExpressionASTnode>(CurTok, "Expected one of '&&', '||',';', ')', ','");

  return nullptr;
}

static std::unique_ptr<ASTnode> ParseEquiv() {
  auto rel = ParseRel();
  if (rel) {
    auto equiv_prime = ParseEquivPrime(std::move(rel));
    return equiv_prime;
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseEquivPrime(std::unique_ptr<ASTnode> rel1) {
  if (CurTok.type == EQ || CurTok.type == NE) {
    int Op = CurTok.type;
    getNextToken(); // eat one of '!=', '=='
    auto rel2 = ParseRel();
    if (rel2) {
      auto node2 = std::make_unique<BinExpressionASTnode>(std::move(rel1),Op, std::move(rel2));
      auto equiv_prime = ParseEquivPrime(std::move(node2));
      return equiv_prime;
    }
  }
  else if (CurTok.type == OR || CurTok.type == AND || CurTok.type == SC
  || CurTok.type == RPAR || CurTok.type == COMMA) { //PREDICT(equiv_prime ::= ε) = {'||', '&&', ';', ')', ','}
    return rel1;
  } else return LogErrorPtr<BinExpressionASTnode>(CurTok, "Expected one of '==','!=', '&&', '||',';', ')', ','");

  return nullptr;
}

static std::unique_ptr<ASTnode> ParseRel() {
  auto subexpr = ParseSubExpr();
  if (subexpr) {
    auto rel_prime = ParseRelPrime(std::move(subexpr));
    return rel_prime;
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseRelPrime(std::unique_ptr<ASTnode> subexpr1) {
  if (CurTok.type == LE || CurTok.type == LT || CurTok.type == GE || CurTok.type == GT) {
    int Op = CurTok.type;
    getNextToken(); //eat one of <, <=, >, >=
    auto subexpr2 = ParseSubExpr();
    if (subexpr2) {
      auto node3 = std::make_unique<BinExpressionASTnode>(std::move(subexpr1), Op, std::move(subexpr2));
      auto rel_prime = ParseRelPrime(std::move(node3));
      return rel_prime;
    }
      
  }
  else if (CurTok.type == EQ || CurTok.type == NE || CurTok.type == OR 
  || CurTok.type == AND || CurTok.type == SC|| CurTok.type == RPAR 
  || CurTok.type == COMMA) { //PREDICT(rel_prime ::= ε) = {'==', '!=', '||', '&&', ';', ')', ','}
    return subexpr1;
  } else return LogErrorPtr<BinExpressionASTnode>(CurTok, "Expected one of '<','<=', '>', '>=','==','!=', '&&', '||',';', ')', ','");

  return nullptr;
}

static std::unique_ptr<ASTnode> ParseSubExpr() {
  auto factor = ParseFactor();
  if (factor) {
    auto subexpr_prime = ParseSubExprPrime(std::move(factor));
    return subexpr_prime;
  }
  return nullptr;
}

static std::unique_ptr<ASTnode> ParseSubExprPrime(std::unique_ptr<ASTnode> factor1) {
  if (CurTok.type == PLUS || CurTok.type == MINUS) {
    int Op = CurTok.type;
    getNextToken(); //eat one of + or -
    auto factor2 = ParseFactor();
    if (factor2) {
      auto node4 = std::make_unique<BinExpressionASTnode>(std::move(factor1),Op,std::move(factor2));
      auto subexpr_prime = ParseSubExprPrime(std::move(node4));
      return subexpr_prime;
    }
  }
  else if (CurTok.type == LE || CurTok.type == LT || CurTok.type == GE || CurTok.type == GT 
  || CurTok.type == EQ || CurTok.type == NE || CurTok.type == OR || CurTok.type == AND 
  || CurTok.type == SC|| CurTok.type == RPAR || CurTok.type == COMMA) {
    return factor1;
  } else return LogErrorPtr<BinExpressionASTnode>(CurTok, "Expected one of '+','-','<','<=', '>', '>=','==','!=', '&&', '||',';', ')', ','");

  return nullptr;
}

static std::unique_ptr<ASTnode> ParseFactor() {
  auto element = ParseElement();
  if (element) {
    auto factor_prime = ParseFactorPrime(std::move(element));
    return factor_prime;
  }
  //return nullptr;
}

static std::unique_ptr<ASTnode> ParseFactorPrime(std::unique_ptr<ASTnode> element1) {
  if (CurTok.type == ASTERIX || CurTok.type == DIV || CurTok.type == MOD) {
    int Op = CurTok.type;
    getNextToken(); //eat one of *, /, %
    auto element2 = ParseElement();
    if (element2) {
      auto node5 = std::make_unique<BinExpressionASTnode>(std::move(element1),Op,std::move(element2));
      auto factor_prime = ParseFactorPrime(std::move(node5));
      return factor_prime;
    }
  }
  else if (CurTok.type == PLUS || CurTok.type == MINUS || CurTok.type == LE || 
  CurTok.type == LT || CurTok.type == GE || CurTok.type == GT || CurTok.type == EQ || 
  CurTok.type == NE || CurTok.type == OR || CurTok.type == AND || CurTok.type == SC
  || CurTok.type == RPAR || CurTok.type == COMMA) {
    return std::move(element1);
  } else return LogErrorPtr<ASTnode>(CurTok, "Expected one of '*','/','%', '+','-','<','<=', '>', '>=','==','!=', '&&', '||',';', ')', ','");

  return nullptr;
}

static std::unique_ptr<ASTnode> ParseElement() {
  TOKEN lookahead1 = lookahead(1);

  if (CurTok.type == IDENT && lookahead1.type == LPAR) { //function call
    std::string ident = IdentifierStr;
    TOKEN ident_token = CurTok;

    getNextToken(); // eat ident
    getNextToken(); // eat (
    auto args = ParseArgs();
    if (args) {
      if (match(RPAR,")")) {
        getNextToken(); //eat )
        return std::make_unique<FunctionCallASTnode>(ident, std::move(args));
      }
    }
  } 
  else if (CurTok.type == IDENT) { 
    std::string ident = IdentifierStr;
    TOKEN ident_token = CurTok;

    getNextToken(); //eat ident
    return std::make_unique<IdentASTnode>(ident_token,ident);
  }
  else if (CurTok.type == INT_LIT) {
    int val = IntVal;
    TOKEN int_token = CurTok;
    getNextToken(); //eat int_lit
    return std::make_unique<IntASTnode>(int_token,val);
  }
  else if (CurTok.type == FLOAT_LIT) {
    float val = FloatVal;
    TOKEN float_token = CurTok;
    getNextToken(); //eat float_lit
    return std::make_unique<FloatASTnode>(float_token, val);
  }
  else if (CurTok.type == BOOL_LIT) {
    bool val = BoolVal;
    TOKEN bool_token = CurTok;
    getNextToken(); //eat bool_lit
    return std::make_unique<BoolASTnode>(bool_token, val);
  } 
  else if (CurTok.type == MINUS) {

  } else return LogErrorPtr<ASTnode>(CurTok, "Expected one of '-','!', '(', identifier, int_lit, float_lit, bool_lit");
  return nullptr;
}



static std::vector<std::unique_ptr<ExpressionASTnode>> ParseArgListPrime() {
  if (CurTok.type == COMMA) {
    getNextToken(); // eat ,
    auto arg_list = ParseArgList();
    if (arg_list) {
      return arg_list->getArgs();
    }
  }
  else if (CurTok.type == RPAR) { //PREDICT(arg_list_prime ::= ε) = {')'}
    return {};
  } else return LogErrorVector<ExpressionASTnode>(CurTok, "Expectecd one of ',' or ')'");
}

static std::unique_ptr<ArgumentListASTnode> ParseArgList() {
  std::vector<std::unique_ptr<ExpressionASTnode>> arg_list;
  auto expr = ParseExpr();
  if (expr) {
    arg_list.push_back(std::move(expr));
    auto arg_list_prime = ParseArgListPrime();
    for (int i = 0; i < arg_list_prime.size(); i++) {
      arg_list.push_back(std::move(arg_list_prime.at(i)));
    }
    return std::make_unique<ArgumentListASTnode>(std::move(arg_list));
  }

  return nullptr;
}

static std::unique_ptr<ArgumentsASTnode> ParseArgs() {
  if (CurTok.type == RPAR) { //PREDICT(Args ::= ε) = {')'}
    return std::make_unique<ArgumentsASTnode>();
  }
  else if (CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT
  || CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT
  || CurTok.type == BOOL_LIT) {
    auto arg_list = ParseArgList();
    if (arg_list) 
      return std::make_unique<ArgumentsASTnode>(std::move(arg_list));

  } else return LogErrorPtr<ArgumentsASTnode>(CurTok, "Expected one of ')', ident, '-', '!', '(', int lit, float lit, bool lit");
}

static std::unique_ptr<ExpressionASTnode> ParseExpr() {
  TOKEN lookahead1 = lookahead(1);

  if (lookahead1.type == ASSIGN) { //expand by expr ::= IDENT "=" expr
    if (match(IDENT,"Identifier")) {
      std::string ident = IdentifierStr;
      getNextToken(); //eat identifier
      getNextToken(); //eat =     //we know this from lookahead
      auto expr = ParseExpr();
      if (expr) //depending on if expression is another assign or binop return that node
        return std::make_unique<ExpressionASTnode>(std::move(expr),ident);
    }
  }
  else {
    auto rval = ParseRval();
    if (rval)
      return std::make_unique<ExpressionASTnode>(std::move(rval));
  }
  return nullptr;
}

static std::unique_ptr<ExpresssionStatementASTnode> ParseExprStmt() {
  if (CurTok.type == SC) //PREDICT(expr_stmt ::= ;) = {';'}
    return std::make_unique<ExpresssionStatementASTnode>(true);
  else { 
    //PREDICT(expr_stmt ::= expr ;) = { IDENT, "-", "!", "(", int_lit, float_lit, bool_lit}
    //however this parse function is only called if we know the current token is an element of PREDICT(expr_stmt ::= expr ;)
    auto expr = ParseExpr();
    if (expr) {
      if (match(SC,";")) {
        getNextToken(); //eat ;
        return std::make_unique<ExpresssionStatementASTnode>(std::move(expr));
      }
    }
  }

  return nullptr;
}

static std::unique_ptr<ElseStatementASTnode> ParseElseStmt() {
  if (CurTok.type == ELSE) {
    getNextToken(); // eat else
    auto block = ParseBlock();
    if (block) 
      return std::make_unique<ElseStatementASTnode>(std::move(block));
  }
  else if (CurTok.type == LBRA || CurTok.type == SC || CurTok.type == IF 
  || CurTok.type == WHILE || CurTok.type == RETURN || CurTok.type == IDENT
  || CurTok.type == MINUS || CurTok.type == NOT || CurTok.type == LPAR
  || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT || CurTok.type == BOOL_LIT
  || CurTok.type == RBRA ) { //FOLLOW(if_stmt)
    return std::make_unique<ElseStatementASTnode>();
  } else return LogErrorPtr<ElseStatementASTnode>(CurTok, "Expected one of 'else','{', ';', if, while, return , identifier, '-', '!', '(', int_lit, float_lit, bool_lit, '}'");

  return nullptr;
}

static std::unique_ptr<IfStatementASTnode> ParseIfStmt() {
  if (match(IF,"if")) {
    getNextToken(); // eat if
    if (match(LPAR,"(")) {
      getNextToken(); //eat (
      auto expr = ParseExpr(); //eats expr
      if (expr) {
        if (match(RPAR,")")) {
          getNextToken(); //eat )
          auto block = ParseBlock();
          if (block) {
            auto else_stmt = ParseElseStmt();
            if (else_stmt) {
              return std::make_unique<IfStatementASTnode>(std::move(expr), std::move(block), std::move(else_stmt));
            }
          }
        }
      }
    }
  }
}

static std::unique_ptr<ReturnStatementASTnode> ParseReturnStmt() {
  if (match(RETURN, "return")) {
    getNextToken(); //eat return
    TOKEN lookahead1 = lookahead(1);
    if (lookahead1.type == SC) { //expr term
      auto expr = ParseExpr();
      if (expr) {
        if (match(SC,";")) {
          getNextToken(); //eat sc
          return std::make_unique<ReturnStatementASTnode>(std::move(expr));
        }
      }
    }
  }
  return nullptr;
}

static std::unique_ptr<WhileStatementASTnode> ParseWhileStmt() {
  if (CurTok.type == WHILE) {
    getNextToken(); //eat while
    if (match(LPAR,"(")) {
      getNextToken(); //eat (
      auto expr = ParseExpr();
      if (expr) {
        if (match(RPAR,")")) {
          getNextToken(); //eat )
          auto stmt = ParseStmt();
          if (stmt)
            return std::make_unique<WhileStatementASTnode>(std::move(expr),std::move(stmt));
        }
      }
    }
  }
}

static std::unique_ptr<StatementASTnode> ParseStmt() {
  if (CurTok.type == SC || CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT ||
  CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT || 
  CurTok.type == BOOL_LIT) {
    //PREDICT(stmt ::= expr_stmt) = {";", IDENT, "-", "!", "(", int_lit, float_lit, bool_lit}
    auto expr_stmt = ParseExprStmt();
    if (expr_stmt)
      return std::make_unique<StatementASTnode>(std::move(expr_stmt));
  }
  else if (CurTok.type == LBRA) { //PREDICT(stmt ::= block) = {"{"}
    auto block = ParseBlock();
    if (block)
      return std::make_unique<StatementASTnode>(std::move(block));
  } 
  else if (CurTok.type == IF) { //PREDICT(stmt ::= if_stmt) = {"if"}
    auto if_stmt = ParseIfStmt();
    if (if_stmt)
      return std::make_unique<StatementASTnode>(std::move(if_stmt));
  } 
  else if (CurTok.type == WHILE) { //PREDICT(stmt ::= while_stmt) = {"while"}
    auto while_stmt = ParseWhileStmt();
    if (while_stmt)
      return std::make_unique<StatementASTnode>(std::move(while_stmt));
  } 
  else if (CurTok.type == RETURN) {//PREDICT(stmt ::= return_stmt) = {"return"}
    auto return_stmt = ParseReturnStmt();
    return std::make_unique<StatementASTnode>(std::move(return_stmt));
  }

  return nullptr;
}


static std::unique_ptr<StatementListASTnode> ParseStmtList() {
  std::vector<std::unique_ptr<StatementASTnode>> stmt_list = {};
  if (CurTok.type == LBRA || CurTok.type == SC || CurTok.type == IF || CurTok.type == WHILE ||
  CurTok.type == RETURN || CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT ||
  CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT || CurTok.type == BOOL_LIT) {
    //PREDICT(stmt_list ::= stmt stmt_list) = 
    //{"{", ";", "if", "while", "return", IDENT, "-", "not", "(", int_lit, float_lit, bool_lit }
    auto stmt = ParseStmt();
    if (stmt) {
      stmt_list.push_back(std::move(stmt));
      auto stmt_list_prime = ParseStmtList();
      if (!stmt_list_prime->getStmts().empty()) { 
       /* for (int i = 0; i < stmt_list_prime->getStmts().size();i++) {
          stmt_list.push_back(std::move(stmt_list_prime->getStmts().at(i)));
        }*/
      }
      return std::make_unique<StatementListASTnode>(std::move(stmt_list));
    }
  }
  else if (CurTok.type == RBRA) { //PREDICT(stmt_list ::= ε) = {'}'}
    return std::make_unique<StatementListASTnode>(std::move(stmt_list));
  } else return LogErrorPtr<StatementListASTnode>(CurTok,"Expected { ; if while return ident - ! ( int_lit float_lit bool_lit }");

  return nullptr;
}

static std::unique_ptr<LocalDeclarationsASTnode> ParseLocalDecls() {
  std::vector<std::unique_ptr<VariableDeclarationASTnode>> local_decls;

  if (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) { //PREDICT(local_decls ::= local_decl local_decls) = {int, float, bool}
    auto decl = ParseVarDecl(); 
    if (decl) {
      local_decls.push_back(std::move(decl));
      auto local_decl_list = ParseLocalDecls();

      if (local_decl_list->getDecls().size() > 0) { //if there are local declaration to add to vector
        for (int i = 0; i < local_decl_list->getDecls().size(); i++) {
          local_decls.push_back(std::move(local_decl_list->getDecls().at(i)));
        }
      }
      return std::make_unique<LocalDeclarationsASTnode>(std::move(local_decls));
    }
  } else if (CurTok.type == LBRA || CurTok.type == SC || CurTok.type == IF || CurTok.type == WHILE
  || CurTok.type == RETURN || CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT
  || CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT 
  || CurTok.type == BOOL_LIT || CurTok.type == RBRA) { 
    //PREDICT(local_decls ::= ε) = {"{", ";", "if", "while", "return", IDENT, "-", "!", "(", int_lit, float_lit, bool_lit, "}" }
    return std::make_unique<LocalDeclarationsASTnode>(std::move(local_decls));
  }
  return LogErrorPtr<LocalDeclarationsASTnode>(CurTok, "Expected either a local declaration or statement");
}

//if fun_decl is called 
static std::unique_ptr<FunctionDeclarationASTnode> ParseFunDecl() {
  auto type = ParseTypeSpec();
  if (type) {
    if (match(IDENT,"Identifier")) {
      std::string ident = IdentifierStr;
      getNextToken(); //eat ident
      if (match(LPAR,"(")) {
        getNextToken(); //eat (
        auto params = ParseParams(); //eat params
        if (params) {
          if (match(RPAR,")")) {
            getNextToken(); //eat )
            auto block = ParseBlock();
            if (block)
              return std::make_unique<FunctionDeclarationASTnode>(type->getType(), ident, std::move(params), std::move(block));
          }
        }
      }
    }
  }
  return nullptr;
}

// decl ::= var_decl | fun_decl
//unable to determine which production to apply without lookahead of 2
static std::unique_ptr<DeclarationASTnode> ParseDecl() {
  TOKEN lookahead_2 = lookahead(2);

  if (lookahead_2.type == SC) { //variable declaration
    auto var_decl = ParseVarDecl();
    if (var_decl)
      return std::make_unique<DeclarationASTnode>(std::move(var_decl));
  }
  else if (lookahead_2.type == LPAR) { //function declaration
    auto fun_decl = ParseFunDecl();
    if (fun_decl)
      return std::make_unique<DeclarationASTnode>(std::move(fun_decl));
  }
  //if lookahead 2 is not ; or ( we know its wrong but a syntax error might occur earlier
  //and we should return that error message instead
  auto type = ParseTypeSpec();
  if (type) {
    if (match(IDENT,"Identifier")) {
      getNextToken();
      return LogErrorPtr<DeclarationASTnode>(CurTok,"Expected one of ; or (");
    }
  }

  return nullptr;
}

static std::unique_ptr<VariableDeclarationASTnode> ParseVarDecl() {
  auto type = ParseVarType(); //eat type
  if (type) {
    if (match(IDENT, "Identifier")) {
      std::string ident = IdentifierStr;
      getNextToken(); //eat ident
      if (match(SC, ";")) {
        getNextToken(); //eat ;
        return std::make_unique<VariableDeclarationASTnode>(type->getType(), ident);
      }
    }
  }
  return nullptr;
}

// extern_list ::= extern extern_list_prime
static std::unique_ptr<ExternListASTnode> ParseExternList() {
  std::vector<std::unique_ptr<ExternASTnode>> extern_list;

  auto extern_ = ParseExtern();
  if (extern_) {
    extern_list.push_back(std::move(extern_));
    auto extern_list_prime = ParseExternListPrime();
    for (int i = 0; i < extern_list_prime.size(); i++) {
      extern_list.push_back(std::move(extern_list_prime.at(i)));
    }
    return std::make_unique<ExternListASTnode>(std::move(extern_list));
  } 
  return nullptr; //change to error message
}

static std::vector<std::unique_ptr<ExternASTnode>> ParseExternListPrime() {
  
  if (CurTok.type == EXTERN) {
    auto extern_list = ParseExternList();
    if (extern_list) 
      return extern_list->getExterns();
  } else if (CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK || CurTok.type == INT_TOK || CurTok.type == VOID_TOK) {
    return {};
  }
  return LogErrorVector<ExternASTnode>(CurTok, "Expected one of extern, float, int, void, bool"); 
}

static std::unique_ptr<ExternASTnode> ParseExtern() {
  if (match(EXTERN, "extern")) {
    getNextToken(); //eat extern
    auto type = ParseTypeSpec();
    if (type) {
      if (match(IDENT, "identifier")) {
        std::string ident = IdentifierStr;
        getNextToken(); //eat IDENT
        if (match(LPAR, "(")) {
          getNextToken(); //eat (
          auto params = ParseParams(); // eats all characters in parameter PLEASE MAKE ERROR MESSAGES FOR PARAMS
          if (params) { //params still returns null pointer
            if (match(RPAR, ")")) {
              getNextToken(); //eat )
              if (match(SC, ";")) {
                getNextToken(); //eat ;
                return std::make_unique<ExternASTnode>(type->getType(), ident, std::move(params));
              }
            }
          }
        }
      }
    }
  }
  return nullptr;
} //return nullptr and let root parse functions display the error message

static std::unique_ptr<ParamsASTnode> ParseParams() {
  if (CurTok.type == VOID_TOK) { //PREDICT(params ::= "void") = {"void"}
    getNextToken();
    return std::make_unique<ParamsASTnode>(true); 
  } 
  else if (CurTok.type == RPAR) { //PREDICT(params ::= ε) = {')'}
    return std::make_unique<ParamsASTnode>();
  } 
  else if (CurTok.type == INT_TOK || CurTok.type == BOOL_TOK || CurTok.type == FLOAT_TOK) {
      //PREDICT(params ::= param_list) = {'int', 'bool', 'void'}
      auto param_list = ParseParamList();
      return std::move(param_list);
  }
  return nullptr; //change to error
}

static std::unique_ptr<ParamsASTnode> ParseParamList() {
  std::vector<std::unique_ptr<ParameterASTnode>> param_list;

  auto param = ParseParam();
  if (param) {
    param_list.push_back(std::move(param));
    auto param_list_prime = ParseParamListPrime();
    for (int i = 0; i < param_list_prime.size(); i++) 
      param_list.push_back(std::move(param_list_prime.at(i)));
    return std::make_unique<ParamsASTnode>(std::move(param_list));
  }
  return nullptr; //change to error message at some date
}

static std::vector<std::unique_ptr<ParameterASTnode>> ParseParamListPrime() {
  if (CurTok.type == RPAR) { //PREDICT(param_list_prime ::= epsilon) = {)}
    return {};
  } else if (CurTok.type == COMMA) { //PREDICT(param_list_prime ::= "," param param_list_prime) = {,}
      getNextToken(); // eat ,
      auto param_list = ParseParamList();
      if (param_list)
        return param_list->getParams();
  }
  return LogErrorVector<ParameterASTnode>(CurTok, "Expected one of ) or ,"); //change to error message that expected one of rpar or comma
}

static std::unique_ptr<ParameterASTnode> ParseParam() {
  auto type = ParseVarType(); //eats the variable type
  if (type) {
    if (match(IDENT, "Identifier")) {
      std::string str = IdentifierStr;
      getNextToken(); // eat ident
      return std::make_unique<ParameterASTnode>(type->getType(), str);
    }
  }
  return nullptr; //change to error log that outputs message and returns nullptr ( should be expected ident )
}


// program ::= extern_list decl_list
static void parser() {
  // add body
  getNextToken();
  auto program = ParseProgram();
  std::cout << program->to_string() << std::endl;
}

/*Supply an look ahead value and returns the token at that lookahead*/
static TOKEN lookahead(int ahead) {
  TOKEN tokens [ahead]; 

  for (int i = 0; i < ahead; i++) {
    tokens[i] = CurTok;
    getNextToken();
  }

  TOKEN lookahead = CurTok;
  putBackToken(lookahead);

  for (int i = ahead-1; i >= 0; i--) {
    putBackToken(tokens[i]);
  }

  getNextToken();
  return lookahead;
}
//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ASTnode &ast) {
  os << ast.to_string();
  return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc == 2) {
    pFile = fopen(argv[1], "r");
    if (pFile == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;

  // get the first token
  /*getNextToken();
  while (CurTok.type != EOF_TOK) {
    fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
            CurTok.type);
    getNextToken();
  } */
  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  parser();
  fprintf(stderr, "Parsing Finished\n");

  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::F_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  // TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}
