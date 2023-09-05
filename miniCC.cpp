#include "llvm/ADT/APFloat.h"
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
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <system_error>
#include <utility>
#include <vector>
#include <fstream>
#include <iostream>

using namespace llvm;
using namespace llvm::sys;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
  tok_eof = -1,

  // commands
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,

  // control
  tok_if = -6,
  tok_else = -8,
  tok_while = -10,
  tok_ret = -11,

  // declare
  tok_double = -13
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

static std::ifstream file;              // soure file

/// gettok - Return the next token from standard input.
static int gettok() {
  static char LastChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = file.get();

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum(LastChar = file.get()))
      IdentifierStr += LastChar;
    if (IdentifierStr == "return")
      return tok_ret;
    if (IdentifierStr == "if")
      return tok_if;
    if (IdentifierStr == "else")
      return tok_else;
    if (IdentifierStr == "while")
      return tok_while;
    if (IdentifierStr == "double")
      return tok_double;
    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = file.get();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return tok_number;
  }


  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = file.get();
  return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

/// DeclarationlAST
class DeclarationlAST {

public:
    virtual ~DeclarationlAST() = default;

    virtual Value *codegen();
};

/// ProgramAST - top level node for the LLVM program.
class ProgramAST {
    std::vector<std::unique_ptr<DeclarationlAST>> GlobalDecls;
public:
    ProgramAST(std::vector<std::unique_ptr<DeclarationlAST>> GlobalDecls) : GlobalDecls(std::move(GlobalDecls)) {}
    ~ProgramAST() = default;

    Value *codegen();
};

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;

  virtual Value *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}

  Value *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}

  Value *codegen() override;
  const std::string &getName() const { return Name; }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

  Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  Value *codegen() override;
};

/// StatementAST - statement base class
class StatementAST {
public:
    virtual ~StatementAST() = default;

    virtual Value *codegen();
};

/// StatementBlockAST - class for a block of statements
class StatementBlockAST {
    std::vector<std::unique_ptr<StatementAST>> Statements;
public:
    StatementBlockAST(std::vector<std::unique_ptr<StatementAST>> Statements) : Statements(std::move(Statements)) {}
    ~StatementBlockAST() = default;

    Value *codegen();
};

/// variableDeclStmtAST - class for a declaration of a variable.
class VarDeclStmtAST : public DeclarationlAST, public StatementAST {
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

public:
  VarDeclStmtAST(
      std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames)
      : VarNames(std::move(VarNames)) {}

  Value *codegen() override;
};

/// IfStatementAST - If statement class
class IfStatementAST : public StatementAST {
  std::unique_ptr<ExprAST> Cond;
  std::unique_ptr<StatementBlockAST> Then;
  std::unique_ptr<StatementBlockAST> Else;
public:
    IfStatementAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<StatementBlockAST> Then,
                std::unique_ptr<StatementBlockAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    Value *codegen() override;
};

/// WhileStatementAST - Statement class for while.
class WhileStatementAST : public StatementAST {
  std::unique_ptr<ExprAST> cond;
  std::unique_ptr<StatementBlockAST> Body;

public:
  WhileStatementAST(std::unique_ptr<ExprAST> cond, std::unique_ptr<StatementBlockAST> Body) : cond(std::move(cond)), Body(std::move(Body)) {}

  Value *codegen() override;
};

/// RetStatementAST - Statement class for return.
class RetStatementAST : public StatementAST {
  std::unique_ptr<ExprAST> expr;

public:
  RetStatementAST(std::unique_ptr<ExprAST> expr) : expr(std::move(expr)) {}

  Value *codegen() override;
};

/// ExprStatementAST - Statement class for expression.
class ExprStatementAST : public StatementAST {
  std::unique_ptr<ExprAST> expr;

public:
  ExprStatementAST(std::unique_ptr<ExprAST> expr) : expr(std::move(expr)) {}

  Value *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes), as well as if it is an operator.
class PrototypeAST : public DeclarationlAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args,
               bool IsOperator = false, unsigned Prec = 0)
      : Name(Name), Args(std::move(Args)) {}

  Function *codegen();
  const std::string &getName() const { return Name; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST : public DeclarationlAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<StatementBlockAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<StatementBlockAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

  Function *codegen();
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0)
    return -1;
  return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken(); // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;

  if (CurTok != ')')
    return LogError("expected ')'");
  getNextToken(); // eat ).
  return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  // Call.
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  // Eat the ')'.
  getNextToken();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  }
}

/// binoprhs
///   ::= ('+' unary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop.
    int BinOp = CurTok;
    getNextToken(); // eat binop

    // Parse the primary expression after the binary operator.
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= double id '(' double id {, double id} ')'
static std::unique_ptr<PrototypeAST> ParsePrototype(std::string& FnName) {
  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_double) {
    getNextToken(); // eat 'double'
    if (CurTok != tok_identifier) {
      return LogErrorP("Expected identifier after 'double'");
    }
    ArgNames.push_back(IdentifierStr);
    getNextToken(); // eat identifier
    if (CurTok == ')') {
        break;
    }
    if (CurTok != ',') {
      return LogErrorP("Expected ',' after identifier");
    }
  }
  // success.
  getNextToken(); // eat ')'.

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}
static std::unique_ptr<StatementBlockAST> ParseStmtBlock();
/// if_statement ::= 'if' '(' expression ')' statementBlock
static std::unique_ptr<IfStatementAST> ParseIfStmt() {
  getNextToken(); // eat if.
  getNextToken(); // eat (.
  auto E = ParseExpression();
  if (!E) {
    LogError("Expected expression");
    return nullptr;
  }
  getNextToken(); // eat )
  if (CurTok != '{') {
    LogError("Expected '{' of statement block");
    return nullptr;
  }
  auto StmtBlock = ParseStmtBlock();
  if (!StmtBlock) {
    LogError("Expected then statement block");
    return nullptr;
  }
  if (CurTok!= tok_else) {
    LogError("Expected else");
  }
  getNextToken(); // eat else
  if (CurTok!= '{') {
    LogError("Expected '{' of statement block");
    return nullptr;

  }
  auto StmtBlockElse = ParseStmtBlock();
  if (!StmtBlockElse) {
    LogError("Expected else statement block");
    return nullptr;

  }
  return std::make_unique<IfStatementAST>(std::move(E), std::move(StmtBlock), std::move(StmtBlockElse));
}

/// while_statement ::= 'while' '(' expression ')' statementBlock
static std::unique_ptr<WhileStatementAST> ParseWhileStmt() {
  getNextToken(); // eat while.
  getNextToken(); // eat (.
  auto E = ParseExpression();
  if (!E) {
    LogError("Expected expression");
    return nullptr;
  }
  getNextToken(); // eat )
  if (CurTok != '{') {
    LogError("Expected '{' of statement block");
    return nullptr;
  }
  auto StmtBlock = ParseStmtBlock();
  if (!StmtBlock) {
    LogError("Expected statement block");
    return nullptr;
  }
  return std::make_unique<WhileStatementAST>(std::move(E), std::move(StmtBlock));
}

/// return_statement ::='return' expression ';'
static std::unique_ptr<RetStatementAST> ParseReturnStmt() {
  getNextToken(); // eat return.
  auto E = ParseExpression();
  if (!E) {
    LogError("Expected expression");
    return nullptr;
  }
  getNextToken(); // eat ;
  return std::make_unique<RetStatementAST>(std::move(E));
}

/// variable_declaration_statement ::= 'double' identifier [ = expr] {, identifier [ = expr]} ';'
static std::unique_ptr<VarDeclStmtAST> ParseVarDeclStmt(std::string &Name) {
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

  while (true) {
    std::string Name = IdentifierStr;
    getNextToken(); // eat identifier.

    // Read the optional initializer.
    std::unique_ptr<ExprAST> Init = nullptr;
    if (CurTok == '=') {
      getNextToken(); // eat the '='.

      Init = ParseExpression();
      if (!Init)
        return nullptr;
    }

    VarNames.push_back(std::make_pair(Name, std::move(Init)));

    // End of var list, exit loop.
    if (CurTok != ',')
      break;
    getNextToken(); // eat the ','.

    if (CurTok != tok_identifier) {
        LogError("expected identifier list after double");
        return nullptr;
    }
  }

  if (CurTok != ';') {
    LogError("expected ';' after variables declaration");
    return nullptr;
  }
  getNextToken(); // eat ';'.
  return std::make_unique<VarDeclStmtAST>(std::move(VarNames));
}

/// expression_statement ::= expression ';'
static std::unique_ptr<ExprStatementAST> ParseExprStatement() {
  auto E = ParseExpression();
  if (!E) {
    LogError("Expected expression");
    return nullptr;
  }
  getNextToken(); // eat ';'.
  return std::make_unique<ExprStatementAST>(std::move(E));
}

/// StatementBlockAST ::= {statement}+
/// statement ::= if_statement | while_statement | variable_declaration_statement| return_statement | expression_statement | empty_statement
static std::unique_ptr<StatementBlockAST> ParseStmtBlock() {
    std::vector<std::unique_ptr<StatementAST>> Statements;
    if (CurTok != '{') {
        LogErrorP("Expected '{' of statement block");
        return nullptr;
    }
    getNextToken(); // eat '{'.
    while (true) {
        if (CurTok == '}') {
            std::cout << "StatementBlockAST: " << (char)CurTok << std::endl;
             getNextToken(); // eat }
             std::cout << "StatementBlockAST: " << (char)CurTok << std::endl;
            break;
        } else if (CurTok == tok_double) {
            getNextToken(); // eat 'double'
            if (CurTok!= tok_identifier) {
                LogError("Expected identifier after 'double'");
                return nullptr;
            }
            auto VarDecl = ParseVarDeclStmt(IdentifierStr);
            if (!VarDecl) {
                LogErrorP("Expected variable declaration");
                return nullptr;
            }
            Statements.push_back(std::move(VarDecl));
        } else if (CurTok == tok_if) {
            auto IfStmt = ParseIfStmt();
            if (!IfStmt) {
                LogErrorP("Expected if statement");
                return nullptr;
            }
            Statements.push_back(std::move(IfStmt));
        } else if (CurTok == tok_while) {
            auto WhileStmt = ParseWhileStmt();
            if (!WhileStmt) {
                LogErrorP("Expected while statement");
                return nullptr;
            }
            Statements.push_back(std::move(WhileStmt));
        } else if (CurTok == tok_ret) {
            auto ReturnStmt = ParseReturnStmt();
            if (!ReturnStmt) {
                LogErrorP("Expected return statement");
                return nullptr;
            }
            Statements.push_back(std::move(ReturnStmt));
        } else if (CurTok == ';') {
            // empty statement.
            getNextToken(); // eat ;
        } else {
            auto ExprStmt = ParseExprStatement();
            if (!ExprStmt) {
                LogErrorP("Expected expression statement");
                return nullptr;
            }
            Statements.push_back(std::move(ExprStmt));
        }
    }
    return std::make_unique<StatementBlockAST>(std::move(Statements));
}

/// globalDeclaration ::= variableDecl | functionDecl | prototypeDecl
static std::unique_ptr<DeclarationlAST> ParseDeclaration() {
    if (CurTok != tok_double) {
        return LogErrorP("Expected double declaration");
    }
    getNextToken(); // eat double.
    if (CurTok!= tok_identifier) {
        return LogErrorP("Expected identifier declaration");
    }
    std::string name = IdentifierStr;
    getNextToken(); // eat identifier.
    if (CurTok == '(') { // prototype declaration
        std::cout << "Prototype declaration start" << std::endl;
        auto prototype = ParsePrototype(name);
        std::cout << "Prototype declaration success" << std::endl;
        if (!prototype) {
            return LogErrorP("Expected prototype declaration");
        }
        if (CurTok == '{') {
            auto FnBody = ParseStmtBlock();
            std::cout << "Prototype FnBody" << std::endl;

            if (!FnBody) {
                return LogErrorP("Expected statement block");
            }
            return std::make_unique<FunctionAST>(std::move(prototype), std::move(FnBody));
        } else {
            return prototype;
        }
    } else {
        return ParseVarDeclStmt(name);
    }
}

/// program ::= {globalDeclaration}+
static std::unique_ptr<ProgramAST> ParseProgram() {
    std::vector<std::unique_ptr<DeclarationlAST>> GlobalDecls;
    while (CurTok!= tok_eof) {
        std::cout << "Global declaration start: " << CurTok << std::endl;
        auto decl = ParseDeclaration();
        if (!decl) {
            return nullptr;
        }
        GlobalDecls.push_back(std::move(decl));
    }
    return std::make_unique<ProgramAST>(std::move(GlobalDecls));
    
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, AllocaInst *> NamedValues;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();

  // If no existing prototype exists, return null.
  return nullptr;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          StringRef VarName) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Type::getDoubleTy(*TheContext), nullptr, VarName);
}

Value *NumberExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  Value *V = NamedValues[Name];
  if (!V)
    return LogErrorV("Unknown variable name");

  // Load the value.
  return Builder->CreateLoad(Type::getDoubleTy(*TheContext), V, Name.c_str());
}

Value *BinaryExprAST::codegen() {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (Op == '=') {
    // Assignment requires the LHS to be an identifier.
    // This assume we're building without RTTI because LLVM builds that way by
    // default.  If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    VariableExprAST *LHSE = static_cast<VariableExprAST *>(LHS.get());
    if (!LHSE)
      return LogErrorV("destination of '=' must be a variable");
    // Codegen the RHS.
    Value *Val = RHS->codegen();
    if (!Val)
      return nullptr;

    // Look up the name.
    Value *Variable = NamedValues[LHSE->getName()];
    if (!Variable)
      return LogErrorV("Unknown variable name");

    Builder->CreateStore(Val, Variable);
    return Val;
  }

  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    return Builder->CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder->CreateFSub(L, R, "subtmp");
  case '*':
    return Builder->CreateFMul(L, R, "multmp");
  case '<':
    L = Builder->CreateFCmpULT(L, R, "cmptmp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
  default:
    return LogErrorV("invalid binary operator");
  }
}

Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  Function *CalleeF = getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Value *RetStatementAST::codegen() {
  // Emit the return value.
  Value *RetVal = expr->codegen();
  if (!RetVal)
    return nullptr;
  return Builder->CreateRet(RetVal);
}

Value *IfStatementAST::codegen() {
    // Emit the condition.
    Value *CondVal = Cond->codegen();
    if (!CondVal)
        return nullptr;

    // Convert condition to a bool by comparing non-equal to 0.0.
    CondVal = Builder->CreateFCmpONE(
        CondVal, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

    Builder->CreateCondBr(CondVal, ThenBB, ElseBB);
    // Emit then value.
    Builder->SetInsertPoint(ThenBB);

    Value *ThenV = Then->codegen();
    if (!ThenV)
        return nullptr;
    Builder->CreateBr(MergeBB);
    // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
    ThenBB = Builder->GetInsertBlock();
    // Emit else block.
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);

    Value *ElseV = Else->codegen();
    if (!ElseV)
        return nullptr;

    Builder->CreateBr(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = Builder->GetInsertBlock();

    // Emit merge block.
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);
    PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;
}

Value *WhileStatementAST::codegen() {
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop");

    Builder->CreateBr(LoopBB);
    Builder->SetInsertPoint(LoopBB);
    Value *CondV = cond->codegen();
    if (!CondV) {
        return nullptr;
    }
    Builder->CreateCondBr(CondV, LoopBB, AfterBB);
    Value *BodyV = Body->codegen();
    if (!BodyV) {
        return nullptr;
    }
    Builder->CreateBr(LoopBB);

    TheFunction->insert(TheFunction->end(), AfterBB);
    Builder->SetInsertPoint(AfterBB);
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Value *VarDeclStmtAST::codegen() {
  std::vector<AllocaInst *> OldBindings;

  Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Register all variables and emit their initializer.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
    const std::string &VarName = VarNames[i].first;
    ExprAST *Init = VarNames[i].second.get();

    // Emit the initializer before adding the variable to scope, this prevents
    // the initializer from referencing the variable itself, and permits stuff
    // like this:
    //  var a = 1 in
    //    var a = a in ...   # refers to outer 'a'.
    Value *InitVal;
    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal)
        return nullptr;
    } else { // If not specified, use 0.0.
      InitVal = ConstantFP::get(*TheContext, APFloat(0.0));
    }

    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    Builder->CreateStore(InitVal, Alloca);

    // Remember the old variable binding so that we can restore the binding when
    // we unrecurse.
    OldBindings.push_back(NamedValues[VarName]);

    // Remember this binding.
    NamedValues[VarName] = Alloca;
  }
  return Constant::getNullValue(Type::getDoubleTy(*TheContext));

}

Value *ExprStatementAST::codegen() {
    // Emit the expression.
    Value *ExprVal = expr->codegen();
    if (!ExprVal)
        return nullptr;
    return ExprVal;

}

Value *StatementBlockAST::codegen() {
    // Emit the statements.
    for (auto &Stmt : Statements)
        Stmt->codegen();
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *FT =
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}

Function *FunctionAST::codegen() {
  // First, check for an existing function from a previous 'extern' declaration.
  Function *TheFunction = TheModule->getFunction(Proto->getName());

  if (!TheFunction)
    TheFunction = Proto->codegen();

  if (!TheFunction)
    return nullptr;

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  for (auto &Arg : TheFunction->args()) {
    // Create an alloca for this variable.
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());

    // Store the initial value into the alloca.
    Builder->CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table.
    NamedValues[std::string(Arg.getName())] = Alloca;
  }

  if (Value *RetVal = Body->codegen()) {
    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

Value *ProgramAST::codegen() {
    for (auto &decl : GlobalDecls) {
        decl->codegen();
    }
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManager() {
  // Open a new module.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("my cool jit", *TheContext);

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char* argv[]) {
  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " <filename>\n";
    return 1;
  }

  std::string input_file;
  auto Filename = "output.o";
  for (int i = 1; i < argc; i++) {
      std::string arg = argv[i];
      if (arg == "-o" && i + 1 < argc) {
          Filename = argv[i+1];
          i++;
      } else {
          input_file = arg;
      }
  }
  file = std::ifstream(input_file, std::ios::in);
  if (!file.is_open()) {
    std::cerr << "Failed to open file: " << argv[1] << "\n";
    return 1;
  }
  getNextToken();

  InitializeModuleAndPassManager();

  // Run the main "interpreter loop" now.
  auto program =  ParseProgram();
  if (!program) {
    std::cerr << "Failed to parse program: " << argv[1] << "\n";
    return 1;
  }
  auto ret = program->codegen();
  if (!ret) {
    std::cerr << "Failed to generate code: " << argv[1] << "\n";
    return 1;

  }

  // Initialize the target registry etc.
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  auto TargetTriple = sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(TargetTriple);

  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = std::optional<Reloc::Model>();
  auto TheTargetMachine =
      Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

  TheModule->setDataLayout(TheTargetMachine->createDataLayout());

  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }

  legacy::PassManager pass;
  auto FileType = CGFT_ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(*TheModule);
  dest.flush();

  outs() << "Wrote " << Filename << "\n";

  return 0;
}