#ifndef __SIMPLE_TARGETS_SYMBOL_H__
#define __SIMPLE_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace til {

  class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    int _qualifier;
    int _offset = 0; // 0 means global
    bool _isMain = false;

  public:
    symbol(const std::string &name, std::shared_ptr<cdk::basic_type> type, int qualifier) :
      _type(type), _name(name), _qualifier(qualifier) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    const std::string &name() const {
      return _name;
    }
    int qualifier() const {
      return _qualifier;
    }
    int offset() const {
      return _offset;
    }
    int offset(int o) {
      return _offset = o;
    }
    bool isMain() const {
      return _isMain;
    }
    bool isMain(bool b) {
      return _isMain = b;
    }
  };

  inline auto make_symbol(const std::string &name, std::shared_ptr<cdk::basic_type> type, int qualifier = 1) {
    return std::make_shared<symbol>(name, type, qualifier);
  }

} // til

#endif
