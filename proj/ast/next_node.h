#ifndef __TIL_AST_NEXT_NODE_H__
#define __TIL_AST_NEXT_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing next nodes.
   */
  class next_node : public cdk::basic_node {
    int _level;

  public:
    next_node(int lineno, int level) :
        basic_node(lineno), _level(level) {
    }

    int level() { return _level; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_next_node(this, level); }

  };

} // til

#endif
