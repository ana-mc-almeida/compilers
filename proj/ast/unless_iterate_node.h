#ifndef __TIL_AST_UNLESS_ITERATE_NODE_H__
#define __TIL_AST_UNLESS_ITERATE_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>

namespace til {

  /**
   * Class for describing unless iterate nodes.
   */
  class unless_iterate_node : public cdk::lvalue_node {
    cdk::expression_node *_condition;
    cdk::expression_node *_vector;
    cdk::expression_node*_count;
    cdk::expression_node*_function;

  public:
    unless_iterate_node(int lineno,
      cdk::expression_node *condition,
      cdk::expression_node *vector,
      cdk::expression_node *count,
      cdk::expression_node *function) :
      cdk::lvalue_node(lineno), _condition(condition), _vector(vector), _count(count), _function(function) {
    }

    cdk::expression_node *condition() { return _condition; }

    cdk::expression_node *vector() { return _vector; }

    cdk::expression_node *count() { return _count; }

    cdk::expression_node *function() { return _function; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_unless_iterate_node(this, level); }

  };

} // til

#endif
