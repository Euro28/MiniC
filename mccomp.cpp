#include "mccomp.h"
#include "AST.h"
#include "parser.h"

//===----------------------------------------------------------------------===//
// AST nodes - put function constructors and function definitions here
//===----------------------------------------------------------------------===//

ProgramASTnode::ProgramASTnode(std::vector<std::unique_ptr<FuncProto>> extern_list,
    std::vector<std::unique_ptr<DeclarationASTnode>> decl_list)
: Extern_list(std::move(extern_list)), Decl_list(std::move(decl_list)) {}


ParameterASTnode::ParameterASTnode(int type, const std::string &ident)
: Ident(ident), Type(type) {}
int ParameterASTnode::getType() {return Type;}
std::string ParameterASTnode::getIdent() {return Ident;}

//constructor
FuncProto::FuncProto(int type, std::string const &identifier, std::vector<std::unique_ptr<ParameterASTnode>> params)
: Type(type), Identifier(identifier), Params(std::move(params)) {}
std::string FuncProto::getName() {return Identifier;}
int FuncProto::getType() {return Type;}

FunctionDeclarationASTnode::FunctionDeclarationASTnode(std::unique_ptr<FuncProto> proto, std::unique_ptr<BlockASTnode> block)
: Proto(std::move(proto)), Block(std::move(block)) {}
std::string FunctionDeclarationASTnode::getName() {return Proto->getName();}

FunctionCallASTnode::FunctionCallASTnode(std::string const &name, std::vector<std::unique_ptr<ExpressionASTnode>> args)
: Name(name), Args(std::move(args)) {}

VariableDeclarationASTnode::VariableDeclarationASTnode(int type, const std::string &identifier)
: Type(type), Identifier(identifier) {}
std::string VariableDeclarationASTnode::getId() {return Identifier;}
int VariableDeclarationASTnode::getType() {return Type;}

VariableCallASTnode::VariableCallASTnode(std::string const &ident) : Ident(ident) {}

//this is 
ExpressionASTnode::ExpressionASTnode(std::unique_ptr<ExpressionASTnode> assign, const std::string &lhs)
: LHS(lhs), Assign(std::move(assign)) {}
ExpressionASTnode::ExpressionASTnode(std::unique_ptr<ASTnode> rval) : Rval(std::move(rval)) {}

IntASTnode::IntASTnode(TOKEN tok, int val) : Val(val), Tok(tok) {}
FloatASTnode::FloatASTnode(TOKEN tok, float val) : Val(val), Tok(tok) {}
BoolASTnode::BoolASTnode(TOKEN tok, bool val) : Val(val), Tok(tok) {}

BinExpressionASTnode::BinExpressionASTnode(std::unique_ptr<ASTnode> lhs, int op, std::unique_ptr<ASTnode> rhs)
  : LHS(std::move(lhs)), Op(op), RHS(std::move(rhs)) {}

UnaryOperatorASTnode::UnaryOperatorASTnode(int prefix_op, std::unique_ptr<ASTnode> element) 
  : PrefixOp(prefix_op), Element(std::move(element)) {}


IfStatementASTnode::IfStatementASTnode(std::unique_ptr<ExpressionASTnode> expr, std::unique_ptr<BlockASTnode> block, 
  std::unique_ptr<BlockASTnode> else_stmt) 
  : Expr(std::move(expr)), Block(std::move(block)), Else(std::move(else_stmt)) {}

WhileStatementASTnode::WhileStatementASTnode(std::unique_ptr<ExpressionASTnode> expr,std::unique_ptr<StatementASTnode> stmt)
  : Expr(std::move(expr)), Stmt(std::move(stmt)) {}

BlockASTnode::BlockASTnode(std::vector<std::unique_ptr<VariableDeclarationASTnode>> local_decls, std::vector<std::unique_ptr<StatementASTnode>> stmt_list)
  : LocalDecls(std::move(local_decls)), Stmt_list(std::move(stmt_list)), Else(true) {}
BlockASTnode::BlockASTnode() : Else(false) {}
bool BlockASTnode::hasElse() {return Else;}

ReturnStatementASTnode::ReturnStatementASTnode(std::unique_ptr<ExpressionASTnode> expr)
  : Expr(std::move(expr)) {}


StatementASTnode::StatementASTnode(std::unique_ptr<ReturnStatementASTnode> return_stmt)
  : Return_stmt(std::move(return_stmt)) {}
StatementASTnode::StatementASTnode(std::unique_ptr<ExpressionStatementASTnode> expr_stmt)
  : Expr_stmt(std::move(expr_stmt)) {}
StatementASTnode::StatementASTnode(std::unique_ptr<IfStatementASTnode> if_stmt)
  : If_stmt(std::move(if_stmt)) {}
StatementASTnode::StatementASTnode(std::unique_ptr<BlockASTnode> block)
  : Block(std::move(block)) {}
StatementASTnode::StatementASTnode(std::unique_ptr<WhileStatementASTnode> while_stmt)
  : While_stmt(std::move(while_stmt)) {} 


ExpressionStatementASTnode::ExpressionStatementASTnode(std::unique_ptr<ExpressionASTnode> expr)
  : Expr(std::move(expr)), Colon(false) {}
ExpressionStatementASTnode::ExpressionStatementASTnode(bool colon)
  : Colon(colon) {}

DeclarationASTnode::DeclarationASTnode(std::unique_ptr<VariableDeclarationASTnode> var_decl)
  : Var_decl(std::move(var_decl)) {}
DeclarationASTnode::DeclarationASTnode(std::unique_ptr<FunctionDeclarationASTnode> fun_decl)
  : Fun_decl(std::move(fun_decl)) {}
std::unique_ptr<VariableDeclarationASTnode> DeclarationASTnode::getVarDecl() {return std::move(Var_decl);}
std::unique_ptr<FunctionDeclarationASTnode> DeclarationASTnode::getFunDecl() {return std::move(Fun_decl);}

//===----------------------------------------------------------------------===//
// AST nodes (to_string) - put in one place to make it easier to form AST
//===----------------------------------------------------------------------===//

std::string ProgramASTnode::to_string(int level) const {
  std::stringstream ss;
  
  for (auto const &ext : Extern_list) {
    ss << "|-ExternDecl" << std::endl;
    ss << ext->to_string(level+1);
  }
  for (auto const &decl : Decl_list)
    ss << decl->to_string(level);
  
  return ss.str();
}

std::string ParameterASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-ParamVarDecl " << Ident << " '" << type_to_string(Type) << "'" << std::endl;
  return ss.str();
}

std::string FuncProto::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-FunctionDecl " << Identifier << " '" << type_to_string(Type) << " (";

  for (auto const &param : Params) {
    ss << type_to_string(param->getType());
    if (param != Params.back()) //add comma for each new parameter except the last one
      ss << ",";
  }
  ss << ")' " << std::endl;
 
  for (auto const &param : Params) 
    ss << param->to_string(level+1);
  
  return ss.str();

}

std::string DeclarationASTnode::to_string(int level) const {
  std::stringstream ss;
  if (Var_decl)
    ss << indent(level) << Var_decl->to_string(level);
  if (Fun_decl)
    ss << indent(level) << Fun_decl->to_string(level);

  return ss.str();
}

std::string FunctionDeclarationASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << Proto->to_string(level) << Block->to_string(level+1);
  return ss.str();
}

std::string BlockASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level+1) << "|-BlockStmt "<< std::endl;
  
  for (auto const &decl : LocalDecls) {
    ss << decl->to_string(level+2);
  }

  for (auto const &stmt : Stmt_list) {
    ss << stmt->to_string(level+2);
  }
  return ss.str();
}

std::string VariableDeclarationASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-VarDecl " << Identifier << " '" << type_to_string(Type) << "'" << std::endl;
  return ss.str();
}

std::string StatementASTnode::to_string(int level) const {
  if (If_stmt) {
    return If_stmt->to_string(level);
  }
  else if (Return_stmt) {
    return Return_stmt->to_string(level);
  }
  else if (While_stmt) {
    return While_stmt->to_string(level);
  }
  else if (Expr_stmt) {
    return Expr_stmt->to_string(level);
  }
  else if (Block) {
    return Block->to_string(level);
  }
  return "";
}

std::string ReturnStatementASTnode::to_string(int level) const {
  if (Expr) {
      std::stringstream ss;
      ss << indent(level) << "|-ReturnStmt " << std::endl;
      ss << Expr->to_string(level+1);
      return ss.str();
    }
  return indent(level) + "|-ReturnStmt";
}

std::string IfStatementASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-IfStmt ";
  if (Else->hasElse())
    ss << "has_else";
  
  ss << std::endl << Expr->to_string(level+1) << Block->to_string(level);

  if (Else->hasElse())
    ss << indent(level+1) << "|ElseStmt "<<  std::endl << Else->to_string(level+1);
  return ss.str();
}

std::string ExpressionStatementASTnode::to_string(int level) const {
  if (!Colon)
      return Expr->to_string(level);
  else return ";";
}

std::string ExpressionASTnode::to_string(int level) const {
    if (Rval) { //goes to one of binop, literal function call. or unary or variable call or iteself
      return Rval->to_string(level);      
    }

    std::stringstream ss; //if here it is an assign 
    ss << indent(level) << "|-BinaryOperator '='" << std::endl;
    ss << indent(level+1) << "|-Identifier '"<< LHS << "'" << std::endl;
    ss  << Assign->to_string(level+1);
    return ss.str();
}

std::string VariableCallASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-VarCall " << Ident << std::endl;
  return ss.str();
}

std::string FunctionCallASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-FunctionCall '" << Name << "'" << std::endl;
  for (auto const &arg : Args) {
    ss << arg->to_string(level+1);
    if (arg != Args.back())
      ss << std::endl;
  }
  return ss.str();
}

std::string BinExpressionASTnode::to_string(int level) const {
  std::stringstream ss;

  ss << indent(level) << "|-BinaryOperator " << type_to_string(Op) << std::endl;
  ss <<  LHS->to_string(level+1);
  ss << RHS->to_string(level+1);
  return ss.str();
}

std::string UnaryOperatorASTnode::to_string(int level) const {
  std::stringstream ss;

  ss << indent(level) << "|-UnaryOperator " << "'" << type_to_string(PrefixOp) << "'" << std::endl;
  ss << Element->to_string(level+1);

  return ss.str();
}

std::string FloatASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-FloatLiteral '" << Val << "'" << std::endl;
  return ss.str();
}

std::string IntASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-IntegerLiteral '" << Val << "'" << std::endl;
  return ss.str();
}

std::string BoolASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-BooleanLiteral '" << Val << "'" << std::endl;
  return ss.str();
}

std::string WhileStatementASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-WhileStmt" << std::endl;
  ss << Expr->to_string(level+1);
  ss << Stmt->to_string(level);
  return ss.str();
}

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//
//NOTE THE PREDICT SETS ARE THE FIRST+ SETS SPECIFIED IN THE LECTURES.
//TAKE note when ever we are checking the result of a parse like say auto block = ParseBlock()
//if (!block) return nullptr is ok as any error messages would be output in ParseBlock() and execution exited
//as the cw spec says the first error found is sufficient.


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

// program ::= extern_list decl_list
//          | decl_list
std::unique_ptr<ProgramASTnode> ParseProgram() {
  if (CurTok.type == EXTERN) { //PREDICT(program ::= extern_list decl_list) = {extern}
    auto extern_list = ParseExternList(); 
    auto decl_list = ParseDeclList(); 
    return std::make_unique<ProgramASTnode>(std::move(extern_list),std::move(decl_list));
  } 
  else if (CurTok.type == INT_TOK || CurTok.type == VOID_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    // PREDICT(program ::= decl_list) = {int, void, float, bool}
    auto decl_list = ParseDeclList();

    std::vector<std::unique_ptr<FuncProto>> extern_list = {};
    return std::make_unique<ProgramASTnode>(std::move(extern_list), std::move(decl_list)); 
  } else return LogErrorPtr<ProgramASTnode>(CurTok, "Expected one of extern, int, void, float, bool"); //start of program must be one of listed
}

// extern_list ::= extern extern_list'
std::vector<std::unique_ptr<FuncProto>> ParseExternList() {
  getNextToken(); //eat extern 

  auto proto = ParseProto(); //eat type_spec IDENT "(" params ")"

  if (CurTok.type != SC)
    return LogErrorVector<FuncProto>(CurTok,"Expected ;");

  getNextToken(); //eat ;

  if (proto) {
    auto extern_list_prime = ParseExternListPrime();
    extern_list_prime.insert(extern_list_prime.begin(), std::move(proto));
    return std::move(extern_list_prime);
  }

  return {}; 
}

//extern_list' ::= extern extern_list' | ε
std::vector<std::unique_ptr<FuncProto>> ParseExternListPrime() {
  std::vector<std::unique_ptr<FuncProto>> extern_list;
  
  if (CurTok.type == EXTERN) {
    auto extern_list_prime = ParseExternList(); //eat extern extern_list'
    return std::move(extern_list_prime);
  } else if (CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK || CurTok.type == INT_TOK || CurTok.type == VOID_TOK) {
    return extern_list; //extern_list' ::= ε
  }
  return LogErrorVector<FuncProto>(CurTok, "Expected one of extern, float, int, void, bool"); 
}

//Function prototype used in function definition and as an extern
std::unique_ptr<FuncProto> ParseProto() {
  auto type = ParseTypeSpec(); //eat type
  if (!type)
    return nullptr;
  
  if (CurTok.type != IDENT)
    return LogErrorPtr<FuncProto>(CurTok,"Expected an identifier");

  std::string ident = IdentifierStr;
  getNextToken(); //eat ident
  if (CurTok.type != LPAR)
    return LogErrorPtr<FuncProto>(CurTok,"Expected an (");
  
  getNextToken(); //eat (
  auto params = ParseParams(); //eat params

  if (CurTok.type != RPAR)
    return LogErrorPtr<FuncProto>(CurTok,"Expected an )");

  getNextToken(); // eat )

  return std::make_unique<FuncProto>(type, ident, std::move(params));
} 

//type_spec ::= "void" | var_type
int ParseTypeSpec() {
  if (CurTok.type == VOID_TOK) {
    int Type =  CurTok.type;
    getNextToken(); //eat void
    return Type;
  } else return ParseVarType();
}

//var_type ::= "int" | "float" | "bool"
int ParseVarType() {
  if (CurTok.type != INT_TOK && CurTok.type != FLOAT_TOK && CurTok.type != BOOL_TOK)
    LogError(CurTok,"Expected one of int, float, void or bool");
  int Type = CurTok.type;
  getNextToken(); //eat int/float/bool
  return Type;
}

// params ::= param_list | "void" | ε
std::vector<std::unique_ptr<ParameterASTnode>> ParseParams() {
  if (CurTok.type == VOID_TOK) { //PREDICT(params ::= "void") = {"void"}
    getNextToken(); //eat void;
    return {};
  } 
  else if (CurTok.type == RPAR) { //PREDICT(params ::= ε) = {')'}
    return {};
  } 
  else if (CurTok.type == INT_TOK || CurTok.type == BOOL_TOK || CurTok.type == FLOAT_TOK) {
      //PREDICT(params ::= param_list) = {'int', 'bool', 'void'}
      auto param_list = ParseParamList();
      return std::move(param_list);
  }

  return LogErrorVector<ParameterASTnode>(CurTok, "Expected on of void, int, bool, float or )");
}

//param_list ::= param param_list'
std::vector<std::unique_ptr<ParameterASTnode>> ParseParamList() {

  auto param = ParseParam();
  if (param) {
    auto param_list_prime = ParseParamListPrime();
    param_list_prime.insert(param_list_prime.begin(), std::move(param));
    return std::move(param_list_prime);
  }
  return {}; //any error message in param is caught in ParseParam
}

//param_list' ::= "," param param_list' | ε
std::vector<std::unique_ptr<ParameterASTnode>> ParseParamListPrime() {
  if (CurTok.type == RPAR) { //PREDICT(param_list_prime ::= epsilon) = {)}
    return {};
  } else if (CurTok.type == COMMA) { //PREDICT(param_list_prime ::= "," param param_list_prime) = {,}
      getNextToken(); // eat ,

      auto param_list = ParseParamList();
      return std::move(param_list);
  }
  return LogErrorVector<ParameterASTnode>(CurTok, "Expected one of ) or ,"); 
}

//param ::= var_type IDENT
std::unique_ptr<ParameterASTnode> ParseParam() {
  auto type = ParseVarType(); //eats int/float/bool

  if (!type)  
    return nullptr;
  
  if (CurTok.type != IDENT)
    return LogErrorPtr<ParameterASTnode>(CurTok,"Expected identifier");
  
  std::string ident = IdentifierStr;
  getNextToken(); //eat ident
  return std::make_unique<ParameterASTnode>(type, ident);
}

// decl_list ::= decl decl_list'
std::vector<std::unique_ptr<DeclarationASTnode>> ParseDeclList() { //change format so you are adding single to returned prime list 
  std::vector<std::unique_ptr<DeclarationASTnode>> decl_list;

  auto decl = ParseDecl(); //eat decl

  if (decl) {
    auto decl_list_prime = ParseDeclListPrime(); //eat decl_list'
    decl_list_prime.insert(decl_list_prime.begin(), std::move(decl));
    return std::move(decl_list_prime);
  }

  return {};
}

//decl_list' ::= decl decl_list' | ε
std::vector<std::unique_ptr<DeclarationASTnode>> ParseDeclListPrime() {
  if (CurTok.type == EOF_TOK) { //PREDICT(decl_list_prime ::= epsilon) = {EOF}
    return {};
  } 
  else if (CurTok.type == VOID_TOK || CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) { //decl_list' ::= decl decl_list'
    auto decl_list = ParseDeclList(); //eat decl decl_list'
    return std::move(decl_list);
  } else return LogErrorVector<DeclarationASTnode>(CurTok, "Expected one of eof, void, int float, bool");
}

// decl ::= var_decl | fun_decl
//unable to determine which production to apply without lookahead of 2
std::unique_ptr<DeclarationASTnode> ParseDecl() {
  TOKEN lookahead_2 = lookahead(2);

  //decl ::= var_decl
  if (lookahead_2.type == SC) { 
    auto var_decl = ParseVarDecl();
    if (var_decl)
      return std::make_unique<DeclarationASTnode>(std::move(var_decl));
  }
  else if (lookahead_2.type == LPAR) { //decl ::= func_decl
    auto fun_decl = ParseFunDecl();
    if (fun_decl)
      return std::make_unique<DeclarationASTnode>(std::move(fun_decl));
  }

  //if lookahead 2 is not ; or ( we know its wrong but a syntax error might occur earlier
  //and we should return that error message instead
  auto type = ParseTypeSpec(); //eat type
  
  if (CurTok.type != IDENT)
    LogErrorPtr<DeclarationASTnode>(CurTok,"Expected an identifier");

  getNextToken(); //eat ident
  return LogErrorPtr<DeclarationASTnode>(CurTok,"Expected one of ; or (");
}

//var_decl ::= var_type IDENT ";"
std::unique_ptr<VariableDeclarationASTnode> ParseVarDecl() {
  auto type = ParseVarType(); //eat type

  if (!type)
    return nullptr;

  if (CurTok.type != IDENT)
    LogErrorPtr<VariableDeclarationASTnode>(CurTok,"Expected identifier");
  
  std::string ident = IdentifierStr;
  getNextToken(); //eat ident
  
  if (CurTok.type != SC)
    LogErrorPtr<VariableDeclarationASTnode>(CurTok,"Expected ;");

  getNextToken(); //eat ;
  return std::make_unique<VariableDeclarationASTnode>(type, ident);
}

//fun_decl ::= type_spec IDENT "(" params ")" block
std::unique_ptr<FunctionDeclarationASTnode> ParseFunDecl() {
  auto proto = ParseProto(); //eat type_spec IDENT "(" params ")"

  if (!proto)
    return nullptr;

  auto block = ParseBlock(); //eat block

  if (!block) 
    return nullptr;

  return std::make_unique<FunctionDeclarationASTnode>(std::move(proto), std::move(block));
}

//block ::= "{" local_decls stmt_list "}"
std::unique_ptr<BlockASTnode> ParseBlock() {
  if (CurTok.type != LBRA)
    return LogErrorPtr<BlockASTnode>(CurTok,"Expected {");

  getNextToken(); //eat {

  auto local_decls = ParseLocalDecls(); //eat local_decls
  auto stmt_list = ParseStmtList(); //eat stmt_list
  
  if (CurTok.type != RBRA)
    return LogErrorPtr<BlockASTnode>(CurTok,"Expected }");

  getNextToken(); //eat }
  return std::make_unique<BlockASTnode>(std::move(local_decls), std::move(stmt_list));
}

//local_decls' ::= local_decl local_decls' | ε
std::vector<std::unique_ptr<VariableDeclarationASTnode>> ParseLocalDeclsPrime() {
  std::vector<std::unique_ptr<VariableDeclarationASTnode>> local_decls;

  if (CurTok.type == LBRA || CurTok.type == SC || CurTok.type == IF || CurTok.type == WHILE
  || CurTok.type == RETURN || CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT
  || CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT 
  || CurTok.type == BOOL_LIT || CurTok.type == RBRA) { 
    //PREDICT(local_decl_prime ::= ε) = {"{", ";", "if", "while", "return", IDENT, "-", "!", "(", int_lit, float_lit, bool_lit, "}" }
    return std::move(local_decls);
  }
  else if (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    //PREDICT(local_decls' ::= local_decl local_decls') = {int, float, bool}
    auto decl = ParseVarDecl();
    if (decl) {
      auto decl_list_prime = ParseLocalDeclsPrime();
      decl_list_prime.insert(decl_list_prime.begin(), std::move(decl));
      return std::move(decl_list_prime);
    }
  } else return LogErrorVector<VariableDeclarationASTnode>(CurTok, "Expected one of { ; if while return identifier - ! ( int_lit float_lit bool_lit }");

  return {};
}

//local_decls ::= local_decls'
std::vector<std::unique_ptr<VariableDeclarationASTnode>> ParseLocalDecls() {
  auto local_decls = ParseLocalDeclsPrime();
  return std::move(local_decls);
}

//stmt_list ::= stmt_list'
std::vector<std::unique_ptr<StatementASTnode>> ParseStmtList() {
  auto stmt_list = ParseStmtListPrime();
  return std::move(stmt_list);
}

//stmt_list' ::= stmt stmt_list' | ε
std::vector<std::unique_ptr<StatementASTnode>> ParseStmtListPrime() {
  std::vector<std::unique_ptr<StatementASTnode>> stmt_list;

  if (CurTok.type == LBRA || CurTok.type == SC || CurTok.type == IF || CurTok.type == WHILE ||
  CurTok.type == RETURN || CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT ||
  CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT || CurTok.type == BOOL_LIT) {
    //PREDICT(stmt_list ::= stmt stmt_list) =  {"{", ";", "if", "while", "return", IDENT, "-", "not", "(", int_lit, float_lit, bool_lit }
    auto stmt = ParseStmt();
    if (stmt) {
      auto stmt_list_prime = ParseStmtListPrime();
      stmt_list_prime.insert(stmt_list_prime.begin(), std::move(stmt));
      return std::move(stmt_list_prime);
    }
  }
  else if (CurTok.type == RBRA) { //stmt_list' ::= ε
    return std::move(stmt_list);
  } else return LogErrorVector<StatementASTnode>(CurTok, "Expected one of { ; if while return ident - ! ( int_lit float_lit bool_lit");

  return {};
}

//stmt ::= expr_stmt | block | if_stmt | while_stmt | return_stmt
std::unique_ptr<StatementASTnode> ParseStmt() {

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
    if (return_stmt)
      return std::make_unique<StatementASTnode>(std::move(return_stmt));
  } else return LogErrorPtr<StatementASTnode>(CurTok, "Expected { if, while, return or stat of expression statement");

  return nullptr;
}

//expr ::= IDENT "=" expr | rval
std::unique_ptr<ExpressionASTnode> ParseExpr() {
  TOKEN lookahead1 = lookahead(1);

  if (lookahead1.type == ASSIGN) { //expand by expr ::= IDENT "=" expr
    if (CurTok.type != IDENT)
      return LogErrorPtr<ExpressionASTnode>(CurTok,"Expected an identifier");

    std::string ident = IdentifierStr;
    getNextToken(); //eat ident
    getNextToken(); //eat =
    auto expr = ParseExpr();
    
    if (!expr)
      return nullptr;
    
    return std::make_unique<ExpressionASTnode>(std::move(expr),ident);
  }
  else {
    auto rval = ParseRval(); //expr ::= rval
    if (!rval)
      return nullptr;

    return std::make_unique<ExpressionASTnode>(std::move(rval));
  }
}

//expr_stmt ::= expr ; | ;
std::unique_ptr<ExpressionStatementASTnode> ParseExprStmt() {
  if (CurTok.type == SC) //PREDICT(expr_stmt ::= ;) = {';'}
    return nullptr;
  else { 
    //PREDICT(expr_stmt ::= expr ;) = { IDENT, "-", "!", "(", int_lit, float_lit, bool_lit}
    //however this parse function is only called if we know the current token is an element of PREDICT(expr_stmt ::= expr ;)
    auto expr = ParseExpr();

    if (!expr)
      return nullptr;

    if (CurTok.type != SC)
      return LogErrorPtr<ExpressionStatementASTnode>(CurTok,"Expected ;");

    getNextToken(); // eat ;
    return std::make_unique<ExpressionStatementASTnode>(std::move(expr));
  }
}

//if_stmt ::= "if" "(" expr ")" stmt
std::unique_ptr<IfStatementASTnode> ParseIfStmt() {
  if (CurTok.type != IF)
    return LogErrorPtr<IfStatementASTnode>(CurTok, "Expected if");

  getNextToken(); //eat if
  if (CurTok.type != LPAR)  
    return LogErrorPtr<IfStatementASTnode>(CurTok, "Expected (");
  
  getNextToken(); // eat (
  auto expr = ParseExpr(); //eat expr

  if (!expr)
    return nullptr;
  
  if (CurTok.type != RPAR)
    return LogErrorPtr<IfStatementASTnode>(CurTok, "Expected )");

  getNextToken(); //eat )
  auto block = ParseBlock(); //eat block
  
  if (!block)
    return nullptr;
  
  auto else_stmt = ParseElseStmt(); //eat else

  if (!else_stmt)
    return nullptr;

  return std::make_unique<IfStatementASTnode>(std::move(expr), std::move(block), std::move(else_stmt));
}

//else_stmt ::= "else" block | ε
std::unique_ptr<BlockASTnode> ParseElseStmt() {
  if (CurTok.type == ELSE) {
    getNextToken(); // eat else
    auto block = ParseBlock();

    if (!block) 
      return nullptr;
    return std::move(block);
  }
  else if (CurTok.type == LBRA || CurTok.type == SC || CurTok.type == IF 
  || CurTok.type == WHILE || CurTok.type == RETURN || CurTok.type == IDENT
  || CurTok.type == MINUS || CurTok.type == NOT || CurTok.type == LPAR
  || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT || CurTok.type == BOOL_LIT
  || CurTok.type == RBRA ) { //FOLLOW(if_stmt)
    
    return std::make_unique<BlockASTnode>();
  } else return LogErrorPtr<BlockASTnode>(CurTok, "Expected one of 'else','{', ';', if, while, return , identifier, '-', '!', '(', int_lit, float_lit, bool_lit, '}'");
}

//while_stmt ::= "while" "(" expr ")" stmt
std::unique_ptr<WhileStatementASTnode> ParseWhileStmt() {
  if (CurTok.type != WHILE)
    return LogErrorPtr<WhileStatementASTnode>(CurTok, "Expected while");
  
  getNextToken(); //eat while
  if (CurTok.type != LPAR)
    return LogErrorPtr<WhileStatementASTnode>(CurTok, "Expected (");

  getNextToken(); //eat (
  auto expr = ParseExpr(); //eat expr
  if (!expr)
    return nullptr;
  
  if (CurTok.type != RPAR)
    return LogErrorPtr<WhileStatementASTnode>(CurTok, "Expected )");
  
  getNextToken(); //eat )
  auto stmt = ParseStmt(); //eat stmt
  if (!stmt) 
    return nullptr;

  return std::make_unique<WhileStatementASTnode>(std::move(expr),std::move(stmt));
}

//return_stmt ::= "return" ";" | "return" expr ";"
std::unique_ptr<ReturnStatementASTnode> ParseReturnStmt() {
  if (CurTok.type == RETURN) {
    getNextToken(); //eat return

    if (CurTok.type == SC) { //expr term //this doesnt work
      getNextToken(); //eat ;
      return std::make_unique<ReturnStatementASTnode>(nullptr);
    }
    else if (CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT
    || CurTok.type ==LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT
    || CurTok.type == BOOL_LIT) { //PREDICT(return_stmt ::= return expr ;)
      auto expr = ParseExpr();

      if (!expr)
        return nullptr;
      
      if (CurTok.type != SC)
        return LogErrorPtr<ReturnStatementASTnode>(CurTok,"expected ;");
      
      getNextToken(); //eat ;
      return std::make_unique<ReturnStatementASTnode>(std::move(expr));

   } else return LogErrorPtr<ReturnStatementASTnode>(CurTok, "Expected one of ; ident - ! ( int_lit float_lit bool_lit");
  }
  return nullptr;
}

//rval ::= term rval'
std::unique_ptr<ASTnode> ParseRval() {
  auto term = ParseTerm();
  if (term) {
    auto rval_prime = ParseRvalPrime(std::move(term));
    return rval_prime;
  }
  return nullptr;
}

//rval' ::= "||" term rval' | ε
std::unique_ptr<ASTnode> ParseRvalPrime(std::unique_ptr<ASTnode> term1) {
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

//term ::= equiv term'
std::unique_ptr<ASTnode> ParseTerm() {
  auto equiv = ParseEquiv();
  if (equiv) {
    auto term_prime = ParseTermPrime(std::move(equiv));
    return term_prime;
  }
  return nullptr;
}

//term' = "&&" equiv term' | ε
std::unique_ptr<ASTnode> ParseTermPrime(std::unique_ptr<ASTnode> equiv1) {
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

//equiv ::= rel equiv'
std::unique_ptr<ASTnode> ParseEquiv() {
  auto rel = ParseRel();
  if (rel) {
    auto equiv_prime = ParseEquivPrime(std::move(rel));
    return equiv_prime;
  }
  return nullptr;
}

//equiv' ::= "==" rel equiv' | "!=" rel equiv' | ε
std::unique_ptr<ASTnode> ParseEquivPrime(std::unique_ptr<ASTnode> rel1) {
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

//rel ::= subexpr rel'
std::unique_ptr<ASTnode> ParseRel() {
  auto subexpr = ParseSubExpr();
  if (subexpr) {
    auto rel_prime = ParseRelPrime(std::move(subexpr));
    return rel_prime;
  }
  return nullptr;
}

/*rel' ::= "<=" subexpr rel'
          | "<" subexpr rel'
          | ">=" subexpr rel'
          | ">" subexpr rel'
          | ε
*/
std::unique_ptr<ASTnode> ParseRelPrime(std::unique_ptr<ASTnode> subexpr1) {
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

//subexpr ::= factor subexpr'
std::unique_ptr<ASTnode> ParseSubExpr() {
  auto factor = ParseFactor();
  if (factor) {
    auto subexpr_prime = ParseSubExprPrime(std::move(factor));
    return subexpr_prime;
  }
  return nullptr;
}

//subexpr' ::= "+" factor subexpr' | "-" factor subexpr' | ε
std::unique_ptr<ASTnode> ParseSubExprPrime(std::unique_ptr<ASTnode> factor1) {
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

//factor ::= element factor'
std::unique_ptr<ASTnode> ParseFactor() {
  auto element = ParseElement();
  if (element) {
    auto factor_prime = ParseFactorPrime(std::move(element));
    return factor_prime;
  }
  return nullptr;
}

/*factor' ::= "*" element factor'
            | "/" element factor'
            | "%" element factor'
            | ε
*/
std::unique_ptr<ASTnode> ParseFactorPrime(std::unique_ptr<ASTnode> element1) {
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

/*element ::= "-" element
            | "!" element
            | "(" expr ")"
            | IDENT
            | IDENT "(" args ")"
            | INT_LIT
            | FLOAT_LIT
            | BOOL_LIT
*/
std::unique_ptr<ASTnode> ParseElement() {
  TOKEN lookahead1 = lookahead(1);

  if (CurTok.type == IDENT && lookahead1.type == LPAR) { //function call
    std::string ident = IdentifierStr;
    TOKEN ident_token = CurTok;

    getNextToken(); // eat ident
    getNextToken(); // eat (
    auto args = ParseArgs();

    if (CurTok.type != RPAR)
      return LogErrorPtr<ASTnode>(CurTok, "Expected ) ");

    getNextToken(); //eat )
    return std::make_unique<FunctionCallASTnode>(ident, std::move(args));
    
    
  } 
  else if (CurTok.type == IDENT) { 
    std::string ident = IdentifierStr;

    getNextToken(); //eat ident
    return std::make_unique<VariableCallASTnode>(ident);
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
  else if (CurTok.type == MINUS || CurTok.type == NOT) {
    int Op = CurTok.type;
    getNextToken(); //eat -/!
    auto element = ParseElement();
    if (element)
      return std::make_unique<UnaryOperatorASTnode>(Op,std::move(element));
  } 
  else if (CurTok.type == LPAR) { // element ::= '(' expr ')'
    getNextToken(); // eat (
    auto expr = ParseExpr();
    if (expr) {
      getNextToken(); // eat )
      return std::move(expr);
    }
  }else return LogErrorPtr<ASTnode>(CurTok, "Expected one of '-','!', '(', identifier, int_lit, float_lit, bool_lit");
  return nullptr;
}

//args ::= arg_list | ε
std::vector<std::unique_ptr<ExpressionASTnode>> ParseArgs() {
  if (CurTok.type == RPAR) { //PREDICT(Args ::= ε) = {')'}
    return {};
  }
  else if (CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT
  || CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT
  || CurTok.type == BOOL_LIT) {
    auto arg_list = ParseArgList();
    
    return std::move(arg_list);

  } 

  return LogErrorVector<ExpressionASTnode>(CurTok, "Expected one of ')', ident, '-', '!', '(', int lit, float lit, bool lit");
}

//arg_list ::= expr arg_list'
std::vector<std::unique_ptr<ExpressionASTnode>> ParseArgList() { //make it so youre returning prime vector
  auto expr = ParseExpr();

  if (expr) {
    auto arg_list_prime = ParseArgListPrime();
    arg_list_prime.insert(arg_list_prime.begin(), std::move(expr));
    return std::move(arg_list_prime);
  }

  return {};
}


//arg list' ::= "," expr arg_list; | ε
std::vector<std::unique_ptr<ExpressionASTnode>> ParseArgListPrime() {
  if (CurTok.type == COMMA) {
    getNextToken(); // eat ,
    auto arg_list = ParseArgList();
    return std::move(arg_list);
  }
  else if (CurTok.type == RPAR) { //PREDICT(arg_list_prime ::= ε) = {')'}
    return {};
  } else return LogErrorVector<ExpressionASTnode>(CurTok, "Expectecd one of ',' or ')'");
}

// program ::= extern_list decl_list
std::unique_ptr<ProgramASTnode> parser() {
  getNextToken();
  auto program = ParseProgram();
  return std::move(program);
}

/*Supply an look ahead value and returns the token at that lookahead*/
TOKEN lookahead(int ahead) {
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

std::string indent(int level) {
  std::stringstream ss;
  for (int i=0; i < level; i++)
    ss << "   ";

  return ss.str();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

//codgen is basically the to_string for the most part but except writing strings to output the information
//we are using the information to generate IR, but general control flow of codegen of ASTnodes is exactly like to_string.
Value *LogErrorV(std::string Str);
FunctionType *getFunctionType(int Type, std::vector<llvm::Type*> Param);
llvm::Type *typeToLLVM(int Type);
Type *max(Value *L, Value *R);
Value *widen(Value *V, Type *T, Type *Widen);
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &Varname, llvm::Type *Type);
bool checkZero(Value *R);
bool checkFloatZero(Value *R);


static LLVMContext TheContext;
std::unique_ptr<Module> TheModule;
static IRBuilder<> Builder(TheContext);

static std::map<std::string, AllocaInst *> NamedValues;
static std::map<std::string, GlobalValue *> GlobalValues;


//gen the entire program
Value *ProgramASTnode::codegen() { 
  for (auto const &ext : Extern_list)
    ext->codegen();
  for (auto const &decl : Decl_list)
    decl->codegen();
  return nullptr;
}

//Function is not covariant of Value so it needs to call it.
Value *FuncProto::codegen() {
  this->codegenF();

  return nullptr;
}

//codegen function proto 
Function *FuncProto::codegenF() {
  std::vector<llvm::Type*> VectorParams;
  for (auto const &param : Params) {
    auto type = typeToLLVM(param->getType());
    VectorParams.push_back(type);
  }

  FunctionType *FT = getFunctionType(Type, VectorParams);

  Function *F = Function::Create(FT, Function::ExternalLinkage, Identifier, TheModule.get());

  unsigned i = 0;
  for (auto &Arg : F->args()) 
    Arg.setName(Params[i++]->getIdent());
  
  return F;
}

//codegen declaration
Value *DeclarationASTnode::codegen() {
  if (Var_decl)
    Var_decl->codegen();
  if (Fun_decl)
    Fun_decl->codegen();
  return nullptr;
}

//codegen bin expression
Value *BinExpressionASTnode::codegen() {
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();

  if (!L || !R)
    return nullptr;
  //Taken from dragon book find max type in hierarchy and convert both values to that type
  //check function definitions for more info
  auto max_type = max(L,R);

  if (!max_type)
    return nullptr;
  
  L = widen(L,L->getType(),max_type);
  R = widen(R,R->getType(),max_type);


  bool isFloat = (max_type == Type::getFloatTy(TheContext));
  bool isInt = (max_type == Type::getInt32Ty(TheContext));
  switch(Op) {
    case AND:
      Builder.CreateAnd(L,R,"andtmp");
    case OR: 
      return Builder.CreateOr(L,R, "ortmp");
    case EQ:
      if (isFloat)
        return Builder.CreateFCmpOEQ(L,R,"eqtmp");
      return Builder.CreateICmpEQ(L,R,"eqtmp");
    case NE:
      if (isFloat)
        return Builder.CreateFCmpONE(L,R,"netmp");
      return Builder.CreateICmpNE(L,R,"netmp");
    case LE:
      if (isFloat)
        return Builder.CreateFCmpOLE(L,R,"letmp");
      return Builder.CreateICmpSLE(L,R,"letmp");
    case LT:
      if (isFloat)
        return Builder.CreateFCmpOLT(L,R,"lttmp");
      return Builder.CreateICmpSLT(L,R,"lttmp");
    case GE:
      if (isFloat)
        return Builder.CreateFCmpOGE(L,R,"getmp");
      return Builder.CreateICmpSGE(L,R,"getmp");
    case GT:
      if (isFloat)
        return Builder.CreateFCmpOGT(L,R,"gttmp");
      return Builder.CreateICmpSGT(L,R, "gttmp");
    case PLUS:
      if (isFloat)
        return Builder.CreateFAdd(L,R,"faddtmp");
      return Builder.CreateAdd(L,R,"addtmp");
    case MINUS:
      if (isFloat)
        return Builder.CreateFSub(L,R,"fsubtmp");
      return Builder.CreateSub(L,R,"subtmp");
    case ASTERIX:
      if (isFloat)
        return Builder.CreateFMul(L,R,"fmultmp");
      return Builder.CreateMul(L,R,"multmp");
    case DIV: // divisor 0 is undefined behaviour stated in lang ref
      if (isFloat) {
        if (checkFloatZero(R))
          LogErrorV("divisor is 0");
        return Builder.CreateFDiv(L,R,"fdivtmp");
      }
      if (checkZero(R))
        LogErrorV("divisor is 0");
      else if (isInt)
        return Builder.CreateSDiv(L,R,"sdivtmp");
      return Builder.CreateUDiv(L,R,"udivtmp");
    case MOD: //divisor 0 is undefined behaviour stated in lang ref
      if (isFloat) {
        if (checkFloatZero(R))
          LogErrorV("divisor is 0");
        return Builder.CreateFRem(L,R,"fmodtmp");
      }
      if (checkZero(R))
        LogErrorV("divisor is 0");
      else if (isInt)
        return Builder.CreateSRem(L,R,"smodtmp");
      return Builder.CreateURem(L,R,"umodtmp");
    default:
      return LogErrorV("invalid binary operator");
  }
}

//if variable called load value
Value *VariableCallASTnode::codegen() {
  Value *V = NamedValues[Ident];
  if (!V) 
    V = GlobalValues[Ident];
    
  if (!V)
    LogErrorV("Unknown variable name :" + Ident);
  
  return Builder.CreateLoad(V, Ident.c_str());
}

Value *VariableDeclarationASTnode::codegen() {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  if (!Builder.GetInsertBlock()) {
    //create global variable

    TheModule->getOrInsertGlobal(Identifier, typeToLLVM(Type));
    GlobalVariable* gVar = TheModule->getNamedGlobal(Identifier);
    gVar->setLinkage(GlobalValue::CommonLinkage);
    gVar->setAlignment((llvm::MaybeAlign)4);

    
    if (typeToLLVM(Type) == Type::getFloatTy(TheContext)) {
      ConstantFP* const_fp_val = ConstantFP::get(TheContext, APFloat(0.0f));
      gVar->setInitializer(const_fp_val);
    } else if (typeToLLVM(Type) == Type::getInt32Ty(TheContext)) {
      ConstantInt* const_int_val = ConstantInt::get(TheContext, APInt(32,0,true));
      gVar->setInitializer(const_int_val);
    } else if (typeToLLVM(Type) == Type::getInt1Ty(TheContext)) {
      ConstantInt* const_bool_val = ConstantInt::get(TheContext, APInt(1,0,false));
      gVar->setInitializer(const_bool_val);
    }


    GlobalValues[Identifier] = gVar;
    return gVar;
  } else {

    //declare local variable, create value then alloca it
    Value *InitVal;
    switch (Type) {
    case FLOAT_TOK:
      InitVal = ConstantFP::get(TheContext, APFloat(0.0f));
      break;
    case INT_TOK:
      InitVal = ConstantInt::get(TheContext, APInt(32, 0, true));
      break;
    case BOOL_TOK:
      InitVal = ConstantInt::get(TheContext, APInt(1, 0, false));
      break;
    default:
      break;
  }
  AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Identifier, typeToLLVM(Type));
  Builder.CreateStore(InitVal, Alloca);

  NamedValues[Identifier] = Alloca;
  }
  return nullptr;

}

//create call to function
Value *FunctionCallASTnode::codegen() {
  Function *CalleeF = TheModule->getFunction(Name);

  if (!CalleeF)
    return LogErrorV("Unknown function referenced");
  
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");
  
  std::vector<Value *> ArgsV;
  for (auto const &arg : Args) {
    ArgsV.push_back(arg->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

//function declaration
Value *FunctionDeclarationASTnode::codegen() {
  Function *TheFunction = TheModule->getFunction(Proto->getName());

  if (!TheFunction)
    TheFunction = Proto->codegenF();

  if (!TheFunction) 
    return nullptr;
  
  BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  //for each argument create an alloca
  NamedValues.clear();
  for (auto &Arg : TheFunction->args()) {
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction,Arg.getName().str(),Arg.getType());
    Builder.CreateStore(&Arg, Alloca);

    NamedValues[Arg.getName().str()] = Alloca;
  }

  Block->codegen(); //maybe change this function return ttype from valyue to functio and include retval part in llvm cp 7

  return nullptr;
}

Value *FloatASTnode::codegen() {
  return ConstantFP::get(TheContext, APFloat(Val));
}

Value *IntASTnode::codegen() {
  return ConstantInt::get(TheContext, APInt(32, Val, true));
}

Value *BoolASTnode::codegen() {
  return ConstantInt::get(TheContext, APInt(1, Val, false));
}


Value *ExpressionASTnode::codegen() {
 if (Rval) { //can be binop, literal, function call, unary, variable call
   return Rval->codegen();
 }

  Value *Val = Assign->codegen(); //codegen the RHS
  if (!Val)
    return nullptr;
  
  AllocaInst *Variable = NamedValues[LHS];
  if (!Variable) {
    auto globalVar = GlobalValues[LHS];
    if (!globalVar)
      return LogErrorV(LHS + "is undefined");
    
    Builder.CreateStore(Val, globalVar); //store RHS into global variable
    return Val;
  }
  Builder.CreateStore(Val, Variable); //store RHS into local variable
  return Val;
}

Value *UnaryOperatorASTnode::codegen() {
  Value *E = Element->codegen(); // function call, variable call, literla, unary , itself

  if (!E)
    return nullptr;
  
  switch(PrefixOp) {
    case NOT:
      return Builder.CreateNot(E, "nottmp");
    case MINUS:
      if (E->getType() == Type::getFloatTy(TheContext))
        return Builder.CreateFNeg(E, "fnegtmp");
      return Builder.CreateNeg(E, "negtmp");
    default:
      return LogErrorV("Unexpected unary operator expected - or !");
  }

  return nullptr;
}

Value *IfStatementASTnode::codegen() {
  Value *CondV = Expr->codegen();
  if (!CondV)
    return nullptr;

  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then", TheFunction);
  BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont", TheFunction);

  if (Else->hasElse()) {
    BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else", TheFunction);
    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    //THEN
    Builder.SetInsertPoint(ThenBB);
    Value *ThenV = Block->codegen();
    Builder.CreateBr(MergeBB);
    ThenBB = Builder.GetInsertBlock();

    //ELSE
    Builder.SetInsertPoint(ElseBB);
    Value *ElseV = Else->codegen();
    Builder.CreateBr(MergeBB);
    ElseBB = Builder.GetInsertBlock();

  } else { //no else section
    Builder.CreateCondBr(CondV, ThenBB,MergeBB);

    //THEN
    Builder.SetInsertPoint(ThenBB);
    Value *ThenV = Block->codegen();
    ThenBB = Builder.GetInsertBlock();

  }

  Builder.SetInsertPoint(MergeBB);
    //leave scope checking for now
  return nullptr;
}

Value *BlockASTnode::codegen() {


  for (auto const &decl : LocalDecls) 
    decl->codegen();
  for (auto const &stmt : Stmt_list) {
    stmt->codegen();
  }
    

  return nullptr;
}

Value *ExpressionStatementASTnode::codegen() {
  if (!Colon)
    Expr->codegen();

  return nullptr;
}

Value *StatementASTnode::codegen() {
  if (If_stmt) {
    If_stmt->codegen();
  }
  else if (Return_stmt) {
    Return_stmt->codegen();
  }
  else if (While_stmt) {
    While_stmt->codegen();
  }
  else if (Expr_stmt) {
    Expr_stmt->codegen();
  }
  else if (Block) {
    Block->codegen();
  }

  return nullptr;
}

Value *ReturnStatementASTnode::codegen() {

  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  if (Expr && TheFunction->getReturnType() != Type::getVoidTy(TheContext)) { //if there is a value to return and 

    auto ret = Expr->codegen();

    if (ret->getType() != TheFunction->getReturnType())
      LogErrorV("Return type does not match function type");

    Builder.CreateRet(ret);
    return ret;
  }
  
  Builder.CreateRet(nullptr);
  return nullptr;

}


Value *WhileStatementASTnode::codegen() {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  BasicBlock *HeaderBB = BasicBlock::Create(TheContext, "header", TheFunction);
  BasicBlock *BodyBB = BasicBlock::Create(TheContext, "body", TheFunction);
  BasicBlock *EndBB = BasicBlock::Create(TheContext, "end", TheFunction);

  Builder.CreateBr(HeaderBB);

  //HEADER
  Builder.SetInsertPoint(HeaderBB);
  Value *CondV = Expr->codegen();

  Builder.CreateCondBr(CondV, BodyBB, EndBB);

  //BODY
  Builder.SetInsertPoint(BodyBB);
  Stmt->codegen();
  BodyBB = Builder.GetInsertBlock();
  Builder.CreateBr(HeaderBB);

  //END
  Builder.SetInsertPoint(EndBB);

  return nullptr;
}

//===----------------------------------------------------------------------===//
// Utility Functions for code gen
//===----------------------------------------------------------------------===//
Value *LogErrorV(std::string Str) { 
  std::cerr << Str << std::endl;
  exit(true);
  return nullptr;
}


//given a Value* that we know is either a float or an int check if it is 0
bool checkZero(Value *Divisor) {
  if (ConstantInt* constInt = dyn_cast<ConstantInt>(Divisor)) {
    if (constInt->getBitWidth() == 32) {
      return (constInt->getSExtValue() == APInt(32,0,true));
    } else if (constInt->getBitWidth() == 1) {
      return (constInt->getSExtValue() == APInt(1,0,false));
    }
  }
  return false;
}

bool checkFloatZero(Value *Divisor) {
  if (ConstantFP* constFloat = dyn_cast<ConstantFP>(Divisor)) 
    return (constFloat->getValue() ==  APFloat(0.0f));
  return false;
}


//widen(v,t,w) generates type conversions if needed to widen contents
//of v of type t into value of type w. returns v if t=w. 
Value *widen(Value *V, Type *T, Type *Widen) {
  if (T == Widen)
    return V;
  else if (T == Type::getInt32Ty(TheContext) && Widen == Type::getFloatTy(TheContext))
    return Builder.CreateSIToFP(V, Type::getFloatTy(TheContext), "castfloat");
  else if (T == Type::getInt1Ty(TheContext) && Widen == Type::getFloatTy(TheContext))
    return Builder.CreateUIToFP(V, Type::getFloatTy(TheContext), "castfloat");
  else if (T == Type::getInt1Ty(TheContext) && Widen == Type::getInt32Ty(TheContext))
    return Builder.CreateIntCast(V, Type::getInt32Ty(TheContext), true, "castint");
  else
    LogErrorV("Type cast not valid");
  
  return nullptr;
  /*possitble pairs of values (which differentiate) and their max are
  i f = f
  f i = f
  i b = i
  b i = i
  f b = f
  b f = f
  Therefore if widen is a float other variable is either int/bool and if 
  widen is an int then other variable must be a bool these are the only cases
  to handle.*/
}


//taken from dragon book max(l,r) takes two types
//and returns the max of the types on the widening heirarchy
//here float > int/bool(which is really int)
//declares an error if l or r is not in the hierarchy
Type *max(Value *L, Value *R) {
  auto l = L->getType();
  auto r = R->getType();

  if ((l != Type::getFloatTy(TheContext) && l != Type::getInt32Ty(TheContext)
  && l != Type::getInt1Ty(TheContext)) || (r != Type::getFloatTy(TheContext) 
  && r != Type::getInt32Ty(TheContext) && r != Type::getInt1Ty(TheContext))) {
    LogErrorV("Type specified is not int, float or bool");
  }
    
  //if any type is not in heirarchy return error;

  /*if either is a float then max in float 
  if reached elseif both are either int32 or int1 if there is an
  int32 it is int32 else they are both int1 if they are equal just return 
  any of l or r.*/
  if (l != r) {
    if (l == Type::getFloatTy(TheContext) || r == Type::getFloatTy(TheContext))
      return Type::getFloatTy(TheContext);
    else if (l == Type::getInt32Ty(TheContext) || r == Type::getInt32Ty(TheContext))
      return Type::getInt32Ty(TheContext);
    else 
      return Type::getInt1Ty(TheContext);
  }

  return l;


}

//given a integer type return llvm functiontype
FunctionType *getFunctionType(int Type, std::vector<llvm::Type*> Param) {
  switch(Type) {
    case VOID_TOK:
      return FunctionType::get(Type::getVoidTy(TheContext),Param,false);
    case FLOAT_TOK:
      return FunctionType::get(Type::getFloatTy(TheContext), Param, false);
    case INT_TOK:
      return FunctionType::get(Type::getInt32Ty(TheContext),Param,false);
    case BOOL_TOK:
      return FunctionType::get(Type::getInt1Ty(TheContext), Param, false);
    default:
      return nullptr;
  }
}

//given a type returns a llvm type
llvm::Type *typeToLLVM(int Type) {
  switch (Type)
  {
  case INT_TOK:
    return llvm::Type::getInt32Ty(TheContext);
  case BOOL_TOK:
    return llvm::Type::getInt1Ty(TheContext);
  case FLOAT_TOK:
    return llvm::Type::getFloatTy(TheContext);
  default:
    return llvm::Type::getDoubleTy(TheContext);
  }
}

static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &Varname, llvm::Type *Type) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());  
  return TmpB.CreateAlloca(Type,0,Varname.c_str());
}

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ASTnode &ast) {
  os << ast.to_string(0);
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

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  auto program = parser();
  std::cout << program->to_string(0) << std::endl;
  fprintf(stderr, "Parsing Finished\n");
  
  program->codegen();

  TheModule->print(llvm::errs(), nullptr);
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
