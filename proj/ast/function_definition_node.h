#ifndef __TIL_AST_FUNCTION_DEFINITION_NODE_H__
#define __TIL_AST_FUNCTION_DEFINITION_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/typed_node.h>
#include <cdk/types/basic_type.h>
#include <cdk/types/functional_type.h>
#include <cdk/types/primitive_type.h>
#include "block_node.h"

namespace til {

  /**
   * Class for describing function definition nodes.
   */
  class function_definition_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;
    til::block_node *_block;
    bool _isMain;

  public:
    function_definition_node(int lineno, cdk::sequence_node *arguments,
      std::shared_ptr<cdk::basic_type> returnType, til::block_node *block) :
      cdk::expression_node(lineno), _arguments(arguments), _block(block), _isMain(false) {
      std::vector<std::shared_ptr<cdk::basic_type>> inputTypes;
      for (size_t i = 0; i < arguments->size(); i++) {
        inputTypes.push_back(dynamic_cast<cdk::typed_node*>(arguments->node(i))->type());
      }

      this->type(cdk::functional_type::create(inputTypes, returnType));
    }

    function_definition_node(int lineno, til::block_node *block) :
      cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _block(block), _isMain(true) {
      this->type(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)));
    }

    cdk::sequence_node *arguments() { return _arguments; }

    til::block_node *block() { return _block; }

    bool isMain() { return _isMain; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_function_definition_node(this, level); }

  };

} // til

#endif
