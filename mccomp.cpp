#include "mccomp.h"
#include "AST.h"
#include "parser.h"

//===----------------------------------------------------------------------===//
// AST nodes - to_string for AST output (putting them all in one place makes it easier to tailor)
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
  : LocalDecls(std::move(local_decls)), Stmt_list(std::move(stmt_list)) {}

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
  if (Else)
    ss << "has_else" << std::endl;
  
  ss << Expr->to_string(level+1) << Block->to_string(level);
  ss << indent(level) << "|ElseStmt "<<  std::endl << Else->to_string(level);
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

//If you simply use Curtok.type == x 
//whenever that fails you need a specific error output
//this prevents replication of that error output.
bool match(int match) {
  if (CurTok.type == match) 
    return true;
  std::string result = "Expected ";
  result += type_to_string(match);

  LogError(CurTok,result);
}


// program ::= extern_list decl_list
//          | decl_list
std::unique_ptr<ProgramASTnode> ParseProgram() {
  if (CurTok.type == EXTERN) { //PREDICT(program ::= extern_list decl_list) = {extern}
    auto extern_list = ParseExternList(); //should return a vector
    auto decl_list = ParseDeclList(); //should be a vector
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
  getNextToken(); //eat extern (we know its extern as we only execute ParseExternList() if Curtok.type == Extern)

  auto proto = ParseProto();

  if (CurTok.type != SC)
    return LogErrorVector<FuncProto>(CurTok,"Expected ;");

  getNextToken(); //eat ;

  if (proto) {
    auto extern_list_prime = ParseExternListPrime();
    extern_list_prime.insert(extern_list_prime.begin(), std::move(proto));
    return std::move(extern_list_prime);
  }

  return {}; //change to error message
}

//extern_list' ::= extern extern_list' | epsilon
std::vector<std::unique_ptr<FuncProto>> ParseExternListPrime() {
  std::vector<std::unique_ptr<FuncProto>> extern_list;
  
  if (CurTok.type == EXTERN) {
    auto extern_list_prime = ParseExternList();
    return std::move(extern_list_prime);
  } else if (CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK || CurTok.type == INT_TOK || CurTok.type == VOID_TOK) {
    return extern_list;
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

  auto decl = ParseDecl();

  if (decl) {
    auto decl_list_prime = ParseDeclListPrime();
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
  else if (CurTok.type == VOID_TOK || CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) { //decl_list_prime ::= decl decl_list_prime
    auto decl_list = ParseDeclList();
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
  else if (lookahead_2.type == LPAR) { //function declaration
    auto fun_decl = ParseFunDecl();
    if (fun_decl)
      return std::make_unique<DeclarationASTnode>(std::move(fun_decl));
  }

  //if lookahead 2 is not ; or ( we know its wrong but a syntax error might occur earlier
  //and we should return that error message instead
  auto type = ParseTypeSpec(); //eat type
  if (!type)
    return nullptr;
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
  auto proto = ParseProto();

  if (!proto)
    return nullptr;

  auto block = ParseBlock();

  if (!block) //error message already displayed in ParseBlock();
    return nullptr;

  return std::make_unique<FunctionDeclarationASTnode>(std::move(proto), std::move(block));
}

//block ::= "{" local_decls stmt_list "}"
std::unique_ptr<BlockASTnode> ParseBlock() {
  if (CurTok.type != LBRA)
    return LogErrorPtr<BlockASTnode>(CurTok,"Expected {");

  getNextToken(); //eat {

  auto local_decls = ParseLocalDecls();
  auto stmt_list = ParseStmtList();
  
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
  else if (CurTok.type == RBRA) {
    return std::move(stmt_list);
  } else return LogErrorVector<StatementASTnode>(CurTok, "Expected one of { ; if while return ident - ! ( int_lit float_lit bool_lit");
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
    auto rval = ParseRval();
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
    std::unique_ptr<BlockASTnode> else_block;
    return std::move(else_block);
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
      return std::make_unique<ReturnStatementASTnode>();
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
  //return nullptr;
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
    if (match(RPAR)) {
      getNextToken(); //eat )
      return std::make_unique<FunctionCallASTnode>(ident, std::move(args));
    }
    
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
  std::cerr << "returning nullptr in parserElement() " << std::endl;
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
static LLVMContext TheContext;
std::unique_ptr<Module> TheModule;
static IRBuilder<> Builder(TheContext);

Value *ProgramASTnode::codegen() {
  for (auto const &ext : Extern_list)
    ext->codegen();
  for (auto const &decl : Decl_list)
    decl->codegen();
}




Value *BinExpressionASTnode::codegen() {
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();

  if (!L || !R)
    return nullptr;

  //widen both L and R
  //int and bool are of the same level and float is above them



  switch(Op) {
    case AND:
      return Builder.CreateAnd(L,R, "andtmp");
    case OR:
      return Builder.CreateOr(L,R, "ortmp");
    case EQ:
      
    case PLUS:
      return Builder.CreateBinOp(Instruction::BinaryOps::Add, L, R, "addtmp");
      //or if float
    case MINUS:
      return Builder.CreateBinOp(Instruction::BinaryOps::Sub, L, R, "subtmp");
    case ASTERIX:
      return Builder.CreateBinOp(Instruction::BinaryOps::Mul, L, R, "multmp");
    

  }
}

Value *FuncProto::codegen() {
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



Value *FloatASTnode::codegen() {
  return ConstantFP::get(TheContext, APFloat(Val));
}

Value *IntASTnode::codegen() {
  return ConstantInt::get(TheContext, APInt(32, Val, true));
}

Value *BoolASTnode::codegen() {
  return ConstantInt::get(TheContext, APInt(1, Val, false));
}


//===----------------------------------------------------------------------===//
// Utility Functions for code gen
//===----------------------------------------------------------------------===//

Value *widen(Value *V, Type *T, Type *Widen) {
  

}


//taken from dragon book max(l,r) takes two types
//and returns the max of the types on the widening heirarchy
//here float > int/bool(which is really int)
//declares an error if l or r is not in the hierarchy
Type *max(Value *L, Value *R) {
  auto l = L->getType();
  auto r = R->getType();

  /*if either is a float then max in float 
  if reached else if both are either int32 or int1 if there is an
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

static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &Varname, int Type) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());  
  return TmpB.CreateAlloca(typeToLLVM(Type),0,Varname.c_str());
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

  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  auto program = parser();
  std::cout << program->to_string(0) << std::endl;
  fprintf(stderr, "Parsing Finished\n");

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
