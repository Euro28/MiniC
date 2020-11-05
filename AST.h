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
  virtual std::string to_string() const {};
};

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

#endif