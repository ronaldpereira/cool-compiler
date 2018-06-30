#include <assert.h>
#include <list>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness
{
    Basic,
    NotBasic
};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode>
{
  private:
    List<CgenNode> *nds;
    ostream &str;

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();
    void code_class_name_table();
    void code_prototype_objects();
    void code_dispatch_table();
    void code_object_initializers();
    void code_class_methods();
    void code_object_table();

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);
    void first_pass();

  public:
    CgenClassTable(Classes, ostream &str);
    void code();
    CgenNodeP root();
    CgenNodeP lookup_node(Symbol name);
    CgenNodeP lookup_tag(int tag);
};

class ObjectLocation
{
  private:
    char *reg;
    int offset;

  public:
    ObjectLocation(char *reg_, int offset_)
    {
        reg = reg_;
        offset = offset_;
    };
    char *get_register() { return reg; };
    int get_offset() { return offset; };
};

class CgenNode : public class__class
{
  private:
    CgenNodeP parentnd;
    List<CgenNode> *children;
    Basicness basic_status;

    int tag;
    int max_class_tag;
    std::list<Feature> *methods;
    std::list<Feature> *attrs;
    SymbolTable<Symbol, ObjectLocation> *vars;

    void set_tag();

  public:
    CgenNode(Class_ c,
             Basicness bstatus,
             CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int get_tag() { return tag; }
    int get_max_tag() { return max_class_tag; }
    std::list<Feature> *get_methods() { return methods; }
    std::list<Feature> *get_attrs() { return attrs; }
    SymbolTable<Symbol, ObjectLocation> *get_vars() { return vars; }
    int basic() { return (basic_status == Basic); }
    void first_pass();
    void code_class_name_table(ostream &s);
    void code_prototype_object(ostream &s);
    void code_dispatch_table(ostream &s);
    void code_object_initializer(ostream &s);
    void code_class_methods(ostream &s);
    void code_object_table(ostream &s);
};

class BoolConst
{
  private:
    int val;

  public:
    BoolConst(int);
    void code_def(ostream &, int boolclasstag);
    void code_ref(ostream &) const;
};
