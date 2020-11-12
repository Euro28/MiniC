#ifndef PARSER_H
#define PARSER_H

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
#include "lexer.h"



using namespace llvm;
using namespace llvm::sys;


std::unique_ptr<ProgramASTnode> ParseProgram();
std::vector<std::unique_ptr<FuncProto>> ParseExternList();
std::vector<std::unique_ptr<FuncProto>> ParseExternListPrime();
std::unique_ptr<FuncProto> ParseProto();
int ParseTypeSpec();
int ParseVarType();
std::vector<std::unique_ptr<ParameterASTnode>> ParseParams();
std::vector<std::unique_ptr<ParameterASTnode>> ParseParamList();
std::vector<std::unique_ptr<ParameterASTnode>> ParseParamListPrime() ;
std::unique_ptr<ParameterASTnode> ParseParam();
std::vector<std::unique_ptr<DeclarationASTnode>> ParseDeclList();
std::vector<std::unique_ptr<DeclarationASTnode>> ParseDeclListPrime();
std::unique_ptr<DeclarationASTnode> ParseDecl();
std::unique_ptr<VariableDeclarationASTnode> ParseVarDecl();
std::unique_ptr<FunctionDeclarationASTnode> ParseFunDecl();
std::unique_ptr<BlockASTnode> ParseBlock();
std::vector<std::unique_ptr<VariableDeclarationASTnode>> ParseLocalDeclsPrime();
std::vector<std::unique_ptr<VariableDeclarationASTnode>> ParseLocalDecls();
std::vector<std::unique_ptr<StatementASTnode>> ParseStmtList();
std::vector<std::unique_ptr<StatementASTnode>> ParseStmtListPrime();
std::unique_ptr<StatementASTnode> ParseStmt();
std::unique_ptr<ExpressionASTnode> ParseExpr() ;
std::unique_ptr<ExpresssionStatementASTnode> ParseExprStmt() ;
std::unique_ptr<IfStatementASTnode> ParseIfStmt();
std::unique_ptr<BlockASTnode> ParseElseStmt();
std::unique_ptr<WhileStatementASTnode> ParseWhileStmt();
std::unique_ptr<ReturnStatementASTnode> ParseReturnStmt();
std::unique_ptr<ASTnode> ParseRval();
std::unique_ptr<ASTnode> ParseRvalPrime(std::unique_ptr<ASTnode> term1);
std::unique_ptr<ASTnode> ParseTerm();
std::unique_ptr<ASTnode> ParseTermPrime(std::unique_ptr<ASTnode> equiv1) ;
std::unique_ptr<ASTnode> ParseEquiv();
std::unique_ptr<ASTnode> ParseEquivPrime(std::unique_ptr<ASTnode> rel1) ;
std::unique_ptr<ASTnode> ParseRel();
std::unique_ptr<ASTnode> ParseRelPrime(std::unique_ptr<ASTnode> subexpr1);
std::unique_ptr<ASTnode> ParseSubExpr();
std::unique_ptr<ASTnode> ParseSubExprPrime(std::unique_ptr<ASTnode> factor1) ;
std::unique_ptr<ASTnode> ParseFactor();
std::unique_ptr<ASTnode> ParseFactorPrime(std::unique_ptr<ASTnode> element1);
std::unique_ptr<ASTnode> ParseElement();
std::vector<std::unique_ptr<ExpressionASTnode>> ParseArgs();
std::vector<std::unique_ptr<ExpressionASTnode>> ParseArgList();
std::vector<std::unique_ptr<ExpressionASTnode>> ParseArgListPrime();

TOKEN lookahead(int ahead);
std::unique_ptr<ProgramASTnode> parser();

#endif