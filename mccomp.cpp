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
#include "AST.h"



using namespace llvm;
using namespace llvm::sys;

//===----------------------------------------------------------------------===//
// AST nodes - to_string for AST output (putting them all in one place makes it easier to tailor)
//===----------------------------------------------------------------------===//


std::string ExternListASTnode::to_string(int level) const {
  std::stringstream ss;
  if (List_Extern.empty())
    return "";
  for (int i=0;i < List_Extern.size(); i++) {
    ss << "|-FunctionDecl used" << List_Extern.at(i)->to_string(level+1);
  }
  return ss.str();
}

std::string FuncProto::to_string(int level) const {
  std::stringstream ss;
  ss << " " << Identifier << " '" << type_to_string(Token_type) << " (" << Params->list_types() << ")' extern " << std::endl;
  ss << Params->to_string(level);
  return ss.str();
}

std::string ParamsASTnode::to_string(int level) const {
  std::stringstream ss;
    for (int i = 0; i < Params.size(); i++) {
      ss << indent(level) << "|`-ParmVarDecl " << Params.at(i)->to_string(level) << std::endl;
    }
  return ss.str();
}

std::string ParameterASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << Ident << " '" << type_to_string(Type) << "'";
  return ss.str();
}

std::string DeclarationListASTnode::to_string(int level) const {
  std::stringstream ss;
  for (int i = 0; i < List_decl.size(); i++) { //function or variable declarations
    ss << List_decl.at(i)->to_string(level);
  }
  return ss.str();
}

std::string DeclarationASTnode::to_string(int level) const {
  std::stringstream ss;

  if (Var_decl) //global variable declaration there is never an indent
    ss << Var_decl->to_string(level);
  if (Fun_decl) 
    ss << Fun_decl->to_string(level);
    
  return ss.str();
}

std::string VariableDeclarationASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-VarDecl " << Identifier << " '" << type_to_string(Var_type) << "'";
  return ss.str();
}

std::string FunctionDeclarationASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-FunctionDecl " << Proto->getIdent() << " '" << type_to_string(Proto->getType()) << " ("  << Proto->paramString() << ")'" << std::endl;
  ss << Proto->ParamASTstring(level+1);
  if (Block)
    ss << Block->to_string(level+1);
  return ss.str();
}

std::string BlockASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-BlockStmt "<< std::endl;
  ss << Local_decls->to_string(level+1);

  if (Stmt_list) 
    ss << Stmt_list->to_string(level+1);
  
  return ss.str();
}

std::string LocalDeclarationsASTnode::to_string(int level) const {
  std::stringstream ss;
    for (int i = 0; i < Local_decls.size(); i++) {
      ss << indent(level) << "|-LocalDeclStmt" << std::endl;
      ss << Local_decls.at(i)->to_string(level+1) << std::endl;;
    }
  return ss.str();
}

std::string StatementListASTnode::to_string(int level) const {
  std::stringstream ss;
    for (int i = 0; i < Stmt_list.size(); i++) {
      ss << Stmt_list.at(i)->to_string(level);
    }
  return ss.str();
}

std::string StatementASTnode::to_string(int level) const {
  if (Expr_stmt) {
    return Expr_stmt->to_string(level);
  } 
  else if (Block) {
    return Block->to_string(level);
  }
  else if (If_stmt) {
    return If_stmt->to_string(level);
  }
  else if (While_stmt) {
    return While_stmt->to_string(level);
  } 
  else if (Return_stmt) {
    return Return_stmt->to_string(level);
  }
}

std::string ExpressionASTnode::to_string(int level) const {
    if (Rval) { //goes to one of binop, literal function call. eventually unary
      return Rval->to_string(level);
    }

    std::stringstream ss; //if here it is an assign 
    ss << indent(level) << "|-BinaryOperator '='" << std::endl;
    ss << indent(level+1) << "|-Identifier '"<< AssignLHS << "'" << std::endl;
    ss  << Assign->to_string(level+1);
    return ss.str();
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

std::string WhileStatementASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-WhileStmt" << std::endl;
  ss << Expr->to_string(level+1);
  ss << Stmt->to_string(level+1);
  return ss.str();
}

std::string ElseStatementASTnode::to_string(int level) const {
  if (Block) {
      return Block->to_string(level);
    }
  return "";
}

std::string IfStatementASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-IfStmt ";
  if (Else)
    ss << "has_else" << std::endl;
  
  ss << Expr->to_string(level+1) << Block->to_string(level+1);
  ss << Else->to_string(level+1);
  return ss.str();
}


std::string BinExpressionASTnode::to_string(int level) const {
  std::stringstream ss;

  ss << indent(level) << "|-BinaryOperator " << type_to_string(Op) << std::endl;
  ss <<  LHS->to_string(level+1);
  ss << RHS->to_string(level+1);
  return ss.str();
}

std::string ArgumentListASTnode::to_string(int level) const {
  std::stringstream ss;
  for (int i = 0; i<Arg_list.size(); i++) {
    ss << Arg_list.at(i)->to_string(level);
  }
  return ss.str();
}

std::string FunctionCallASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-CallExpr " << std::endl;
  ss << indent(level+1) << "|-Identifier '" << Name << "'" << std::endl;
  ss << Args->to_string(level+1);
  return ss.str();
}



std::string UnaryOperatorASTnode::to_string(int level) const {
  std::stringstream ss;

  ss << indent(level) << "|-UnaryOperator " << "'" << Prefix << "'" << std::endl;
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

std::string IdentASTnode::to_string(int level) const {
  std::stringstream ss;
  ss << indent(level) << "|-Identifier '" << Name << "'" << std::endl;
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
static bool match(int match) {
  if (CurTok.type == match) 
    return true;
  std::string result = "Expected ";
  result += type_to_string(match);

  LogError(CurTok,result);
}


/* Add function calls for each production */
static std::unique_ptr<ExternListASTnode> ParseExternList();
static std::vector<std::unique_ptr<FuncProto>> ParseExternListPrime();
static std::unique_ptr<ParamsASTnode> ParseParams();
static std::unique_ptr<FuncProto> ParseProto();
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
  if (CurTok.type != INT_TOK && CurTok.type != VOID_TOK && CurTok.type != FLOAT_TOK && CurTok.type != BOOL_TOK)
    return LogErrorPtr<TypeSpecASTnode>(CurTok, "expected one of int, float or bool or void");
  
  auto result = std::make_unique<TypeSpecASTnode>(CurTok.type);
  getNextToken(); //eat int/float/bool/void
  return result;
}

static std::unique_ptr<VariableTypeASTnode> ParseVarType() {
  if (CurTok.type != INT_TOK && CurTok.type != FLOAT_TOK && CurTok.type != BOOL_TOK) 
    return LogErrorPtr<VariableTypeASTnode>(CurTok,"expected one of int, float or bool");

  auto result = std::make_unique<VariableTypeASTnode>(CurTok.type);
  getNextToken(); //eat int/float/bool
  return result;
}

// program ::= extern_list decl_list
//          | decl_list
static std::unique_ptr<ProgramASTnode> ParseProgram() {
  if (CurTok.type == EXTERN) { //PREDICT(program ::= extern_list decl_list) = {extern}
    auto extern_list = ParseExternList();

    if (!extern_list)
      return nullptr;
    
    auto decl_list = ParseDeclList();

    if (!decl_list)
      return nullptr;
    
    return std::make_unique<ProgramASTnode>(std::move(extern_list),std::move(decl_list));
  } 
  else if (CurTok.type == INT_TOK || CurTok.type == VOID_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    // PREDICT(program ::= decl_list) = {int, void, float, bool}
    auto decl_list = ParseDeclList();

    if (!decl_list)
      return nullptr;

    return std::make_unique<ProgramASTnode>(std::move(decl_list)); 
  } else return LogErrorPtr<ProgramASTnode>(CurTok, "Expected one of extern, int, void, float, bool"); //start of program must be one of listed
}

// decl_list ::= decl decl_list_prime
static std::unique_ptr<DeclarationListASTnode> ParseDeclList() { //change format so you are adding single to returned prime list 
  std::vector<std::unique_ptr<DeclarationASTnode>> decl_list;

  auto decl = ParseDecl();

  if (!decl)
    return nullptr;

  decl_list.push_back(std::move(decl));
  auto decl_list_prime = ParseDeclListPrime();
  for (int i = 0; i < decl_list_prime.size(); i++) {
    decl_list.push_back(std::move(decl_list_prime.at(i)));
  }
  return std::make_unique<DeclarationListASTnode>(std::move(decl_list));
}

static std::vector<std::unique_ptr<DeclarationASTnode>> ParseDeclListPrime() {
  if (CurTok.type == EOF_TOK) { //PREDICT(decl_list_prime ::= epsilon) = {EOF}
    return {};
  } 
  else if (CurTok.type == VOID_TOK || CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) { //decl_list_prime ::= decl decl_list_prime
    auto decl_list = ParseDeclList();
    if (decl_list)
      return std::move(decl_list->getDecls());
  } else return LogErrorVector<DeclarationASTnode>(CurTok, "Expected one of eof, void, int float, bool");
}

static std::unique_ptr<BlockASTnode> ParseBlock() {
  if (CurTok.type != LBRA)
    return LogErrorPtr<BlockASTnode>(CurTok,"Expected {");

  getNextToken(); //eat {

  auto local_decls = ParseLocalDecls();
  if (!local_decls)
    return nullptr;

  auto stmt_list = ParseStmtList();
  if (!stmt_list)
    return nullptr;

  if (CurTok.type != RBRA)
    return LogErrorPtr<BlockASTnode>(CurTok,"Expected }");

  getNextToken(); //eat }
  return std::make_unique<BlockASTnode>(std::move(local_decls), std::move(stmt_list));
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
      if (match(RPAR)) {
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
  else if (CurTok.type == MINUS || CurTok.type == NOT) {
    int Op = CurTok.type;
    getNextToken(); //eat -/!
    auto element = ParseElement();
    if (element)
      return std::make_unique<UnaryOperatorASTnode>(type_to_string(Op),std::move(element));
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

static std::unique_ptr<ArgumentListASTnode> ParseArgList() { //make it so youre returning prime vector
  std::vector<std::unique_ptr<ExpressionASTnode>> arg_list;
  auto expr = ParseExpr();

  if (!expr)
    return nullptr;

  arg_list.push_back(std::move(expr));
  auto arg_list_prime = ParseArgListPrime();
  for (int i = 0; i < arg_list_prime.size(); i++) {
    arg_list.push_back(std::move(arg_list_prime.at(i)));
  }
  return std::make_unique<ArgumentListASTnode>(std::move(arg_list));

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

static std::unique_ptr<ExpresssionStatementASTnode> ParseExprStmt() {
  if (CurTok.type == SC) //PREDICT(expr_stmt ::= ;) = {';'}
    return std::make_unique<ExpresssionStatementASTnode>(true);
  else { 
    //PREDICT(expr_stmt ::= expr ;) = { IDENT, "-", "!", "(", int_lit, float_lit, bool_lit}
    //however this parse function is only called if we know the current token is an element of PREDICT(expr_stmt ::= expr ;)
    auto expr = ParseExpr();

    if (!expr)
      return nullptr;

    if (CurTok.type != SC)
      return LogErrorPtr<ExpresssionStatementASTnode>(CurTok,"Expected ;");

    getNextToken(); // eat ;
    return std::make_unique<ExpresssionStatementASTnode>(std::move(expr));
  }
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
}

static std::unique_ptr<IfStatementASTnode> ParseIfStmt() {
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

static std::unique_ptr<ReturnStatementASTnode> ParseReturnStmt() {
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

static std::unique_ptr<WhileStatementASTnode> ParseWhileStmt() {
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
    if (return_stmt)
      return std::make_unique<StatementASTnode>(std::move(return_stmt));
  }

  return nullptr;
}

//local decls and statement list implementation is wrong fml

static std::vector<std::unique_ptr<StatementASTnode>> ParseStmtListPrime() {
  std::vector<std::unique_ptr<StatementASTnode>> stmt_list;
  if (CurTok.type == LBRA || CurTok.type == SC || CurTok.type == IF || CurTok.type == WHILE ||
  CurTok.type == RETURN || CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT ||
  CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT || CurTok.type == BOOL_LIT) {
    //PREDICT(stmt_list ::= stmt stmt_list) =  {"{", ";", "if", "while", "return", IDENT, "-", "not", "(", int_lit, float_lit, bool_lit }
    auto stmt = ParseStmt();
    if (stmt) {
      stmt_list.push_back(std::move(stmt));
      auto stmt_list_prime = ParseStmtListPrime();
      for (int i = 0; i < stmt_list_prime.size(); i++) {
        stmt_list.push_back(std::move(stmt_list_prime.at(i)));
      }
      return std::move(stmt_list);
    }
  }
  else if (CurTok.type == RBRA) {
    return std::move(stmt_list);
  } else return LogErrorVector<StatementASTnode>(CurTok, "Expected one of { ; if while return ident - ! ( int_lit float_lit bool_lit");
}


static std::unique_ptr<StatementListASTnode> ParseStmtList() {
  auto stmt_list = ParseStmtListPrime();
  return std::make_unique<StatementListASTnode>(std::move(stmt_list));
}


static std::vector<std::unique_ptr<VariableDeclarationASTnode>> ParseLocalDeclsPrime() {
  std::vector<std::unique_ptr<VariableDeclarationASTnode>> local_decls;

  if (CurTok.type == LBRA || CurTok.type == SC || CurTok.type == IF || CurTok.type == WHILE
  || CurTok.type == RETURN || CurTok.type == IDENT || CurTok.type == MINUS || CurTok.type == NOT
  || CurTok.type == LPAR || CurTok.type == INT_LIT || CurTok.type == FLOAT_LIT 
  || CurTok.type == BOOL_LIT || CurTok.type == RBRA) { 
    //PREDICT(local_decl_prime ::= ε) = {"{", ";", "if", "while", "return", IDENT, "-", "!", "(", int_lit, float_lit, bool_lit, "}" }
    return std::move(local_decls);
  }
  else if (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    //PREDICT(local_decls ::= local_decl local_decls) = {int, float, bool}
    auto decl = ParseVarDecl();
    if (decl) {
      local_decls.push_back(std::move(decl));
      auto decl_list_prime = ParseLocalDeclsPrime();
      for (int i = 0; i < decl_list_prime.size(); i++) {
        local_decls.push_back(std::move(decl_list_prime.at(i)));
      }
      return std::move(local_decls);
    }
  } else return LogErrorVector<VariableDeclarationASTnode>(CurTok, "Expected one of { ; if while return identifier - ! ( int_lit float_lit bool_lit }");
}


static std::unique_ptr<LocalDeclarationsASTnode> ParseLocalDecls() {
  auto local_decls = ParseLocalDeclsPrime();
  return std::make_unique<LocalDeclarationsASTnode>(std::move(local_decls));

}


//if fun_decl is called 
static std::unique_ptr<FunctionDeclarationASTnode> ParseFunDecl() {
  auto proto = ParseProto();

  if (!proto)
    return nullptr;

  auto block = ParseBlock();

  if (!block) //error message already displayed in ParseBlock();
    return nullptr;

  return std::make_unique<FunctionDeclarationASTnode>(std::move(proto), std::move(block));
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
  if (!type)
    return nullptr;
  if (CurTok.type != IDENT)
    LogErrorPtr<DeclarationASTnode>(CurTok,"Expected an identifier");

  getNextToken(); //eat ident
  return LogErrorPtr<DeclarationASTnode>(CurTok,"Expected one of ; or (");
}

static std::unique_ptr<VariableDeclarationASTnode> ParseVarDecl() {
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
  return std::make_unique<VariableDeclarationASTnode>(type->getType(), ident);
}

// extern_list ::= extern extern_list_prime
static std::unique_ptr<ExternListASTnode> ParseExternList() {
  std::vector<std::unique_ptr<FuncProto>> extern_list;

  getNextToken(); //eat extern (we know its extern as we only execute ParseExternList() if Curtok.type == Extern)

  auto extern_ = ParseProto();

  if (CurTok.type != SC)
    return LogErrorPtr<ExternListASTnode>(CurTok,"Expected ;");

  getNextToken(); //eat ;

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

static std::vector<std::unique_ptr<FuncProto>> ParseExternListPrime() {
  
  if (CurTok.type == EXTERN) {
    auto extern_list = ParseExternList();
    if (extern_list) 
      return extern_list->getExterns();
  } else if (CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK || CurTok.type == INT_TOK || CurTok.type == VOID_TOK) {
    return {};
  }
  return LogErrorVector<FuncProto>(CurTok, "Expected one of extern, float, int, void, bool"); 
}

static std::unique_ptr<FuncProto> ParseProto() {
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
  if (!params)
    return nullptr;

  if (CurTok.type != RPAR)
    return LogErrorPtr<FuncProto>(CurTok,"Expected an )");

  getNextToken(); // eat )

  return std::make_unique<FuncProto>(type->getType(), ident, std::move(params));
} 

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
  return LogErrorPtr<ParamsASTnode>(CurTok, "Expected on of void, int, bool, float or )");
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

  if (!type)  
    return nullptr;
  
  if (CurTok.type != IDENT)
    return LogErrorPtr<ParameterASTnode>(CurTok,"Expected identifier");
  
  std::string ident = IdentifierStr;
  getNextToken(); //eat ident
  return std::make_unique<ParameterASTnode>(type->getType(), ident);

}


// program ::= extern_list decl_list
static std::unique_ptr<ProgramASTnode> parser() {
  getNextToken();
  auto program = ParseProgram();
  return std::move(program);
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

FunctionType *getFunctionType(int,std::vector<llvm::Type*>);
llvm::Type *typeToLLVM(int Type);
static AllocaInst *CreateEntryBlockAlloca(Function *, const std::string &, int);

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;

Value *LogErrorV(std::string const &Str) {
  std::cerr << "IR error" << std::endl;
  return nullptr;
}

Value *ProgramASTnode::codegen() {
  TheModule = std::make_unique<Module>("mini-c",TheContext);
  for (auto const &ext : Extern_list->getExterns()) 
    ext->codegen();
    
  for (auto const &decl : Decl_list->getDecls()) {
    decl->codegen();
  }
    


  return nullptr;
}

Value *DeclarationASTnode::codegen() {
  if (Fun_decl)
    Fun_decl->codegen();

  return nullptr;
}

//generate prototype for extern
Function *FuncProto::codegen() {
  
  std::vector<llvm::Type*> typeVector;
  auto paramList = Params->getParams();

  for (auto const &param : paramList) //for each extern param populate the vector
    typeVector.push_back(typeToLLVM(param->getType()));
    
  FunctionType *FT = getFunctionType(Token_type, typeVector);
  
  Function *F = 
    Function::Create(FT, Function::ExternalLinkage, Identifier, TheModule.get());

  unsigned i = 0;
  std::string name;
  for (auto &Arg : F->args()) {
    name = paramList[i++]->getIdent();
    Arg.setName(name);
  }

  return F;
}

Value *FunctionDeclarationASTnode::codegen() {
  Function *TheFunction = TheModule->getFunction(Proto->getIdent());

  if (!TheFunction) {
    TheFunction = Proto->codegen();
  }

  if (!TheFunction) 
    return nullptr;
  
  if (!TheFunction->empty())
    return (Function*)LogErrorV("Function" + Proto->getIdent() + "cannot be redefined"); //change this

  //create a new basic block to start insertion into
  BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  //record the function arguments in the NamedValues map.
  NamedValues.clear();
  
  
  unsigned i = 0;
  for (auto &Arg : TheFunction->args()) {  
    NamedValues[std::string(Arg.getName())] = &Arg;
  }
  
  
  fprintf(stderr, "Big time\n");
  


  return nullptr;



}

Value *VariableDeclarationASTnode::codegen() {

  
  if (Builder.GetInsertBlock()) { //in function so its a local declaration
    Function *TheFunction = Builder.GetInsertBlock()->getParent();

    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Identifier, Var_type);

  } else {
    //create global variable
  }
  //get the current function
 

  return nullptr;
}
//===----------------------------------------------------------------------===//
// Utility Functions for code gen
//===----------------------------------------------------------------------===//

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
