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


//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//
class FuncProto;
class DeclarationASTnode;
class BlockASTnode;
class ExpressionASTnode;
class StatementASTnode;


//A program is just a list of extern declarations and then 
//list of normal declartions or just a list of declarations
class ProgramASTnode : public ASTnode {
  std::vector<std::unique_ptr<FuncProto>> Extern_list; //list of externs
  std::vector<std::unique_ptr<DeclarationASTnode>> Decl_list;
  

public: 
  ProgramASTnode(std::vector<std::unique_ptr<FuncProto>> extern_list, std::vector<std::unique_ptr<DeclarationASTnode>> decl_list);
  Value *codegen() override;
  std::string to_string(int level) const override;
};

class ParameterASTnode : public ASTnode {
  int Type;
  std::string Ident;

public:
  virtual Value *codegen() override {};
  ParameterASTnode(int type, const std::string &ident);
  int getType();
  std::string getIdent();
  std::string to_string(int level) const override;
};

//function prototype doubles as extern definition
class FuncProto: public ASTnode {
  int Type;
  std::string Identifier;
  std::vector<std::unique_ptr<ParameterASTnode>> Params;

public:
  FuncProto(int type, std::string const &identifier, std::vector<std::unique_ptr<ParameterASTnode>> params);
  std::string to_string(int level) const override;
  Value *codegen() override;
  Function *codegenF();
  std::string getName();
  int getType();
};

class FunctionDeclarationASTnode : public ASTnode {
  std::unique_ptr<FuncProto> Proto;
  std::unique_ptr<BlockASTnode> Block;

public:
  FunctionDeclarationASTnode(std::unique_ptr<FuncProto> proto, std::unique_ptr<BlockASTnode> block);
  Value *codegen() override;
  std::string to_string(int level) const override;
  std::string getName();
};

// FunctionCallASTnode - node that represetns function calls
class FunctionCallASTnode : public ASTnode {
  std::string Name;
  std::vector<std::unique_ptr<ExpressionASTnode>> Args;

public:
  Value *codegen() override;

  FunctionCallASTnode(std::string const &name, std::vector<std::unique_ptr<ExpressionASTnode>> args);
  std::string to_string(int level) const override;

};

class VariableDeclarationASTnode : public ASTnode {
  int Type;
  std::string Identifier;

public:

  VariableDeclarationASTnode(int var_type, const std::string &identifier);
  std::string to_string(int level) const override;
  std::string getId();
  int getType();
  Value *codegen() override;
};

class VariableCallASTnode : public ASTnode {
  std::string Ident;

public:
  VariableCallASTnode(std::string const &ident);
  std::string to_string(int level) const override;
  Value *codegen() override;
};

//if there is an assignLHS there is an assign expression, if no assignLHS then its just an rval and output that
class ExpressionASTnode : public ASTnode {
  std::string LHS;
  std::unique_ptr<ExpressionASTnode> Assign;
  std::unique_ptr<ASTnode> Rval; //can be any of element ::=

public:
  ExpressionASTnode(std::unique_ptr<ExpressionASTnode> assign, const std::string &lhs);
  ExpressionASTnode(std::unique_ptr<ASTnode> rval);
  std::string to_string(int level) const override;
  Value *codegen() override;
 
};

// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTnode(TOKEN tok, int val);
  Value *codegen() override;
  std::string to_string(int level) const override;
};
// FloatASTnode - class for float literals like 56.2
class FloatASTnode : public ASTnode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTnode(TOKEN tok, float val);
  Value *codegen() override;
  std::string to_string(int level) const override;
};

// BoolASTnode - class for boolean literals, true or false.
class BoolASTnode : public ASTnode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTnode(TOKEN tok, bool val);
  Value *codegen() override;
  std::string to_string(int level) const override;
};

class BinExpressionASTnode : public ASTnode {
  int Op;
  std::unique_ptr<ASTnode> LHS, RHS;

public:
  BinExpressionASTnode(std::unique_ptr<ASTnode> lhs, int op, std::unique_ptr<ASTnode> rhs);
  std::string to_string(int level) const override;
  Value *codegen() override;
};

class UnaryOperatorASTnode : public ASTnode {
  int PrefixOp;
  std::unique_ptr<ASTnode> Element; //can be one of ident, function call, int, float, bool lit

public:
  UnaryOperatorASTnode(int prefix_op, std::unique_ptr<ASTnode> element);
  std::string to_string(int level) const override;
  Value *codegen() override;
};

class BlockASTnode : public ASTnode {
  std::vector<std::unique_ptr<VariableDeclarationASTnode>> LocalDecls;
  std::vector<std::unique_ptr<StatementASTnode>> Stmt_list;
  bool Else; //if this varible is true then there is an statement
  //0 or more local declarations followed by 0 or more statement lists

public:
  Value *codegen() override;
  BlockASTnode(std::vector<std::unique_ptr<VariableDeclarationASTnode>> local_decls, std::vector<std::unique_ptr<StatementASTnode>> stmt_list);
  BlockASTnode();
  bool hasElse();
  std::string to_string(int level) const override;
  

};


class WhileStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;
  std::unique_ptr<StatementASTnode> Stmt;

public:
  Value *codegen() override;
  WhileStatementASTnode(std::unique_ptr<ExpressionASTnode> expr,std::unique_ptr<StatementASTnode> stmt);
  std::string to_string(int level) const override;
};

class IfStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;
  std::unique_ptr<BlockASTnode> Block;
  std::unique_ptr<BlockASTnode> Else;

public:
  Value *codegen() override;
  IfStatementASTnode(std::unique_ptr<ExpressionASTnode> expr, std::unique_ptr<BlockASTnode> block, 
  std::unique_ptr<BlockASTnode> else_stmt);
  std::string to_string(int level) const override;
};

class ReturnStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;

public:
  Value *codegen() override;
  ReturnStatementASTnode(std::unique_ptr<ExpressionASTnode> expr);
  ReturnStatementASTnode() {} //inline constructor
  std::string to_string(int level) const override;
};





//inline constructors and function definitions
class ExpressionStatementASTnode : public ASTnode {
  std::unique_ptr<ExpressionASTnode> Expr;
  bool Colon;

public: 
  ExpressionStatementASTnode(std::unique_ptr<ExpressionASTnode> expr) 
  : Expr(std::move(expr)), Colon(false) {}

  ExpressionStatementASTnode(bool colon) : Colon(colon) {}

  std::string to_string(int level) const override;

  Value *codegen() override;
};
class StatementASTnode : public ASTnode {
  //this is either an expr_stmt, if, while, block, or return stmt
  std::unique_ptr<IfStatementASTnode> If_stmt;
  std::unique_ptr<BlockASTnode> Block;
  std::unique_ptr<ExpressionStatementASTnode> Expr_stmt;
  std::unique_ptr<WhileStatementASTnode> While_stmt;
  std::unique_ptr<ReturnStatementASTnode> Return_stmt;

public:
  StatementASTnode(std::unique_ptr<ReturnStatementASTnode> return_stmt);
  StatementASTnode(std::unique_ptr<ExpressionStatementASTnode> expr_stmt);
  StatementASTnode(std::unique_ptr<IfStatementASTnode> if_stmt);
  StatementASTnode(std::unique_ptr<BlockASTnode> block);
  StatementASTnode(std::unique_ptr<WhileStatementASTnode> while_stmt);
  std::string to_string(int level) const override;
  Value *codegen() override;
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


#endif