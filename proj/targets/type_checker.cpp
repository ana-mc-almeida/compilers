#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>
#include "type_checker.h"

#include "til_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

static std::shared_ptr<cdk::primitive_type> create_double() {
  return cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);
}

static std::shared_ptr<cdk::primitive_type> create_int() {
  return cdk::primitive_type::create(4, cdk::TYPE_INT);
}

static std::shared_ptr<cdk::primitive_type> create_unspec() {
  return cdk::primitive_type::create(0, cdk::TYPE_UNSPEC);
}

static std::shared_ptr<cdk::reference_type> create_pointer(std::shared_ptr<cdk::basic_type> referenced) {
  return cdk::reference_type::create(4, referenced);
}

// if lax id true, double is considered equal to int
static bool deepTypeComparison(std::shared_ptr<cdk::basic_type> left, std::shared_ptr<cdk::basic_type> right, bool lax) {

  if (left->name() == cdk::TYPE_UNSPEC || right->name() == cdk::TYPE_UNSPEC) {
    return false;
  }

  if (left->name() == cdk::TYPE_FUNCTIONAL)
  {
    if (right->name() != cdk::TYPE_FUNCTIONAL) {
      return false;
    }

    auto left_functional = cdk::functional_type::cast(left);
    auto right_functional = cdk::functional_type::cast(right);

    if (left_functional->input_length() != right_functional->input_length()
      || left_functional->output_length() != right_functional->output_length()) {
      return false;
    }

    for (size_t i = 0; i < left_functional->input_length(); i++) {
      if (!deepTypeComparison(right_functional->input(i), left_functional->input(i), lax)) {
        return false;
      }
    }

    for (size_t i = 0; i < left_functional->output_length(); i++) {
      if (!deepTypeComparison(left_functional->output(i), right_functional->output(i), lax)) {
        return false;
      }
    }

    return true;
  }
  else if (right->name() == cdk::TYPE_FUNCTIONAL) { // FIXME not sure if this is needed
    return false;
  }
  else if (left->name() == cdk::TYPE_POINTER) {
    if (right->name() != cdk::TYPE_POINTER) {
      return false;
    }
    auto left_pointer = cdk::reference_type::cast(left);
    auto right_pointer = cdk::reference_type::cast(right);
    return deepTypeComparison(left_pointer->referenced(), right_pointer->referenced(), false);
  }
  else if (right->name() == cdk::TYPE_POINTER) { // FIXME not sure if this is needed
    return false;
  }
  else if (lax && left->name() == cdk::TYPE_DOUBLE) {
    return right->name() == cdk::TYPE_DOUBLE || right->name() == cdk::TYPE_INT;
  }
  else {
    return left == right;
  }

}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (auto& n : node->nodes()) {
    n->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(create_double());
}

void til::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(create_int());
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void til::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl, bool acceptDouble) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (!node->argument()->is_typed(cdk::TYPE_INT)
    && !(acceptDouble && node->argument()->is_typed(cdk::TYPE_DOUBLE))) {
    throw std::string("wrong type in argument of unary expression");
  }
  else if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(create_int());
  }

  node->type(node->argument()->type());
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
  processUnaryExpression(node, lvl, true);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
  processUnaryExpression(node, lvl, true);
}

void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  processUnaryExpression(node, lvl, false);
}

//---------------------------------------------------------------------------

/**
 * U + U = I
 * U + I = I
 * U + D = D
 * U + P = P
 *
 * I + I = I
 * I + D = D
 * I + P = P
 *
 * D + D = D
 *
 * P + P = P
 */
void til::type_checker::processBinaryArithmeticExpression(cdk::binary_operation_node *const node, int lvl,
  bool acceptDoubles, bool acceptOnePointer, bool acceptBothPointers) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT) || (acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->type(node->right()->type());
    }
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(create_int());
      node->type(create_int());
    }
    else if (acceptOnePointer && node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->type(node->right()->type());

      if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        node->left()->type(create_int());
      }
    }
    else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }

    if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
      node->left()->type(node->type());
    }
  }
  else if (acceptDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
      node->type(create_double());
    }
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(create_double());
      node->type(create_double());
    }
    else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  }
  else if (acceptOnePointer && node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT)) {
      node->type(node->left()->type());
    }
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(create_int());
      node->type(node->left()->type());
    }
    else if (acceptBothPointers && deepTypeComparison(node->left()->type(), node->right()->type(), false)) {
      node->type(create_int());
    }
    else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  }
  else {
    throw std::string("wrong type in left argument of arithmetic binary expression");
  }
}

void til::type_checker::processMultiplicationExpression(cdk::binary_operation_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, true, false, false);
}

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, true, true, false);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, true, true, false);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processMultiplicationExpression(node, lvl);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processMultiplicationExpression(node, lvl);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, false, false, false);
}

//---------------------------------------------------------------------------

void til::type_checker::processBinaryPredicateExpression(cdk::binary_operation_node *const node, int lvl, bool acceptDoubles, bool acceptPointers) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_INT)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
    }
    else if (!node->right()->is_typed(cdk::TYPE_INT)
      && !(acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))
      && !(acceptPointers && node->right()->is_typed(cdk::TYPE_POINTER))) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  }
  else if (acceptDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
    }
    else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  }
  else if (acceptPointers && node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(create_int());
    }
    else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_POINTER)) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  }
  else if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->left()->type(create_int());
      node->right()->type(create_int());
    }
    else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->left()->type(create_int());
    }
    else if (node->right()->is_typed(cdk::TYPE_INT) || (acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->left()->type(node->right()->type());
    }
    else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  }
  else {
    throw std::string("wrong type in left argument of binary expression");
  }

  node->type(create_int());
}


// only integers and doubles are allowed in comparative expressions
void til::type_checker::processBinaryComparativeExpression(cdk::binary_operation_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, true, false);
}

void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryComparativeExpression(node, lvl);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryComparativeExpression(node, lvl);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryComparativeExpression(node, lvl);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryComparativeExpression(node, lvl);
}


void til::type_checker::processEqExpression(cdk::binary_operation_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, true, true);
}

void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processEqExpression(node, lvl);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processEqExpression(node, lvl);
}

void til::type_checker::processLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, false, false);
}

void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processLogicalExpression(node, lvl);
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  }
  else {
    throw id;
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  }
  catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl);
  node->rvalue()->accept(this, lvl);

  if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
    node->rvalue()->type(node->lvalue()->type());
  }
  else if (
    node->rvalue()->is_typed(cdk::TYPE_POINTER) &&
    node->lvalue()->is_typed(cdk::TYPE_POINTER)
    ) {
    auto lref = cdk::reference_type::cast(node->lvalue()->type());
    auto rref = cdk::reference_type::cast(node->rvalue()->type());

    if (rref->referenced()->name() == cdk::TYPE_UNSPEC
      || rref->referenced()->name() == cdk::TYPE_VOID
      || lref->referenced()->name() == cdk::TYPE_VOID) {
      node->rvalue()->type(node->lvalue()->type());
    }
  }

  if (!deepTypeComparison(node->lvalue()->type(), node->rvalue()->type(), true)) {
    throw std::string("wrong type in right argument of assignment expression");
  }

  node->type(node->lvalue()->type());

}

//---------------------------------------------------------------------------

void til::type_checker::do_evaluation_node(til::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(create_int());
  }
  else if (node->argument()->is_typed(cdk::TYPE_POINTER)) {
    auto ref = cdk::reference_type::cast(node->argument()->type());

    if (ref != nullptr && ref->referenced()->name() == cdk::TYPE_UNSPEC) {
      node->argument()->type(create_pointer(create_int()));
    }
  }
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);

  for (const auto& argument : node->arguments()->nodes()) {
    auto child = static_cast<cdk::typed_node*>(argument);

    if (child->is_typed(cdk::TYPE_UNSPEC)) {
      child->type(create_int());
    }
    else if (
      !child->is_typed(cdk::TYPE_INT) &&
      !child->is_typed(cdk::TYPE_DOUBLE) &&
      !child->is_typed(cdk::TYPE_STRING)) {
      throw std::string("wrong type in argument of print expression");
    }
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->type(create_unspec());
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);

  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(create_int());
  }
  else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in condition of loop expression");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_stop_node(til::stop_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(create_int());
  }

  node->type(create_int());
}

//---------------------------------------------------------------------------

void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
  auto symbol = _symtab.find("@", 1);
  if (symbol == nullptr) {
    throw std::string("return statement outside program block");
  }

  std::shared_ptr<cdk::functional_type> functionType = cdk::functional_type::cast(symbol->type());

  auto rettype = functionType->output(0);
  auto rettype_name = rettype->name();

  if (node->value() == nullptr) {
    if (rettype_name != cdk::TYPE_VOID) {
      throw std::string("non-void function must return a value");
    }
    return;
  }

  if (rettype_name == cdk::TYPE_VOID) {
    throw std::string("void function must not return a value");
  }

  node->value()->accept(this, lvl + 2);

  if (!deepTypeComparison(rettype, node->value()->type(), true)) {
    throw std::string("wrong type in return statement");
  }

}

//---------------------------------------------------------------------------

void til::type_checker::do_next_node(til::next_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_index_node(til::index_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->pointer()->accept(this, lvl + 2);

  if (!node->pointer()->is_typed(cdk::TYPE_POINTER)) {
    throw std::string("wrong type in base of index expression");
  }

  node->index()->accept(this, lvl + 2);
  if (node->index()->is_typed(cdk::TYPE_UNSPEC)) {
    node->index()->type(create_int());
  }
  else if (!node->index()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in index of index expression");
  }

  auto pointerType = cdk::reference_type::cast(node->pointer()->type());

  if (pointerType->referenced()->name() == cdk::TYPE_UNSPEC) {
    pointerType = create_pointer(create_int());
    node->pointer()->type(pointerType);
  }

  node->type(pointerType->referenced());
}

//---------------------------------------------------------------------------

void til::type_checker::do_block_node(til::block_node *const node, int lvl) {
  // TODO
}

//---------------------------------------------------------------------------

void til::type_checker::do_null_node(til::null_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->type(create_pointer(create_unspec()));
}

//---------------------------------------------------------------------------

void til::type_checker::do_address_node(til::address_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 2);

  if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    auto ref = cdk::reference_type::cast(node->lvalue()->type());
    if (ref->referenced()->name() == cdk::TYPE_VOID) {
      node->type(node->lvalue()->type());
      return;
    }
  }

  node->type(create_pointer(node->lvalue()->type()));
}

//---------------------------------------------------------------------------

void til::type_checker::do_alloc_node(til::alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(create_int());
  }
  else if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in argument of alloc expression");
  }

  node->type(create_pointer(create_unspec()));
}

//---------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node, int lvl) {
  if (node->type() == nullptr) {
    node->initializer()->accept(this, lvl + 2);

    if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
      node->initializer()->type(create_int());
    }
    else if (node->initializer()->is_typed(cdk::TYPE_POINTER)) {
      auto reference = cdk::reference_type::cast(node->initializer()->type());
      if (reference->referenced()->name() == cdk::TYPE_UNSPEC) {
        node->initializer()->type(cdk::reference_type::create(4,
          create_int()));
      }
    }
    else if (node->initializer()->is_typed(cdk::TYPE_VOID)) {
      throw std::string("cannot declare variable of type void");
    }

    node->type(node->initializer()->type());
  }
  else {
    if (node->initializer() != nullptr) {
      node->initializer()->accept(this, lvl + 2);

      if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
        if (node->is_typed(cdk::TYPE_DOUBLE)) {
          node->initializer()->type(node->type());
        }
        else {
          node->initializer()->type(create_int());
        }
      }
      else if (
        node->is_typed(cdk::TYPE_POINTER) &&
        node->initializer()->is_typed(cdk::TYPE_POINTER)
        ) {
        auto nodeRef = cdk::reference_type::cast(node->type());
        auto initializerRef = cdk::reference_type::cast(node->initializer()->type());
        if (initializerRef->referenced()->name() == cdk::TYPE_UNSPEC
          || initializerRef->referenced()->name() == cdk::TYPE_VOID
          || nodeRef->referenced()->name() == cdk::TYPE_VOID) {
          node->initializer()->type(node->type());
        }
      }

      if (!deepTypeComparison(node->type(), node->initializer()->type(), true)) {
        throw std::string("wrong type in initializer of declaration expression");
      }
    }
  }

  if (node->qualifier() == tEXTERNAL && !node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    throw std::string("external declaration must be a function");
  }

  auto symbol = make_symbol(node->identifier(), node->type(), node->qualifier());

  if (_symtab.insert(node->identifier(), symbol)) {
    _parent->set_new_symbol(symbol);
    return;
  }

  // redeclaration
  auto prev = _symtab.find(node->identifier());

  if (prev != nullptr && prev->qualifier() == tFORWARD) {
    if (deepTypeComparison(prev->type(), symbol->type(), false)) {
      _symtab.replace(node->identifier(), symbol);
      _parent->set_new_symbol(symbol);
      return;
    }
  }

  throw std::string("redeclaration of variable '" + node->identifier() + "'");
}

//---------------------------------------------------------------------------

void til::type_checker::do_function_definition_node(til::function_definition_node *const node, int lvl) {
  auto function = make_symbol("@", node->type());
  function->isMain(node->isMain());

  if (!_symtab.insert(function->name(), function)) {
    _symtab.replace(function->name(), function);
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_function_call_node(til::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;

  std::shared_ptr<cdk::functional_type> functionalType;

  if (node->function() == nullptr) { // recursive call; "@"
    auto symbol = _symtab.find("@", 1);
    if (symbol == nullptr) {
      throw std::string("recursive call outside function");
    }
    else if (symbol->isMain()) {
      throw std::string("recursive call inside program block");
    }

    functionalType = cdk::functional_type::cast(symbol->type());
  }
  else {
    node->function()->accept(this, lvl);

    if (!node->function()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      throw std::string("wrong type in function call");
    }

    functionalType = cdk::functional_type::cast(node->function()->type());
  }

  if (functionalType->input()->length() != node->arguments()->size()) {
    throw std::string("wrong number of arguments in function call");
  }

  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));
    arg->accept(this, lvl);

    auto paramtype = functionalType->input(i);

    if (arg->is_typed(cdk::TYPE_UNSPEC)) {
      if (paramtype->name() == cdk::TYPE_DOUBLE) {
        arg->type(create_double());
      }
      else {
        arg->type(create_int());
      }
    }
    else if (arg->is_typed(cdk::TYPE_POINTER) && paramtype->name() == cdk::TYPE_POINTER) {
      auto paramref = cdk::reference_type::cast(paramtype);
      auto argref = cdk::reference_type::cast(arg->type());

      if (argref->referenced()->name() == cdk::TYPE_UNSPEC
        || argref->referenced()->name() == cdk::TYPE_VOID
        || paramref->referenced()->name() == cdk::TYPE_VOID) {
        arg->type(paramtype);
      }
    }

    if (!deepTypeComparison(paramtype, arg->type(), true)) {
      throw std::string("wrong type for argument in function call");
    }
  }

  node->type(functionalType->output(0));
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);

  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(create_int());
  }
  else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in condition of if expression");
  }
}

void til::type_checker::do_if_else_node(til::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);

  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(create_int());
  }
  else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in condition of if expression");
  }
}
