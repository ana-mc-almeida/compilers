#ifndef __SIMPLE_TARGETS_POSTFIX_WRITER_H__
#define __SIMPLE_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <optional>
#include <set>
#include <sstream>
#include <stack>
#include <optional>
#include <unordered_set>
#include <cdk/emitters/basic_postfix_emitter.h>
#include <cdk/types/functional_type.h>

namespace til {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer : public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
    int _lbl;
    std::shared_ptr<cdk::functional_type> _functionType;
    int _function;
    bool _isMain;
    long _offset = 0;
    std::vector<std::pair<std::string, std::string>> *_functionLoopLabels;
    bool _forceOutsideFunction = false; // whether to force future declarations to be global
    bool _inFunctionArgs = false;
    std::stack<std::string> _functionLabels; // (history of) label of current visiting function
    std::string _currentFunctionRetLabel; // where to jump when a return occurs
    std::optional<std::string> _externalFunctionName; // name of external function to be called, if any
    std::set<std::string> _externalFunctionsToDeclare; // set of external functions to declare
    // ^ (history of) labels of current visiting function's loops; pair (condition, end)
    bool _visitedFinalInstruction = false; // whether a final instruction was visited; no others should follow in this block



  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol> &symtab, cdk::basic_postfix_emitter &pf) :
      basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0), _function(0), _isMain(false) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    inline bool inFunction() {
      return !_forceOutsideFunction && !_functionLabels.empty();
    }

    template<class T>
    inline bool isInstanceOf(cdk::basic_node * const node) {
      return dynamic_cast<T*>(node) != nullptr;
    }
    template<class T, class... Rest, typename std::enable_if<sizeof...(Rest) != 0, int>::type = 0>
    inline bool isInstanceOf(cdk::basic_node * const node) {
      return dynamic_cast<T*>(node) != nullptr || isInstanceOf<Rest...>(node);
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

    void prepareAddictiveExpression(cdk::binary_operation_node * const node, int lvl);
    void prepareMultiplicativeExpression(cdk::binary_operation_node * const node, int lvl);
    void prepareComparisonExpression(cdk::binary_operation_node * const node, int lvl);
    void cast(std::shared_ptr<cdk::basic_type> from, std::shared_ptr<cdk::basic_type> to);
    void declareExternalFunctionIfNeeded(std::string symbol);
    void acceptCovariantNode(std::shared_ptr<cdk::basic_type> const target_type, cdk::expression_node * const node, int lvl);

    template<size_t P, typename T>
    void executeLoopControlInstruction(T * const node);

  };

#define THROW_ERROR_FOR_NODE(subject, msg) { \
  std::cerr << subject->lineno() << ": " << msg << std::endl; \
  return; \
}
#define THROW_ERROR(msg) THROW_ERROR_FOR_NODE(node, msg)

} // til

#endif
