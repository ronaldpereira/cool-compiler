#ifndef COOL_TREE_H
#define COOL_TREE_H

#include <list>
#include "tree.h"
#include "cool-tree.handcode.h"

typedef class Program_class *Program;

class Program_class : public tree_node
{
  public:
    tree_node *copy() { return copy_Program(); }
    virtual Program copy_Program() = 0;

#ifdef Program_EXTRAS
    Program_EXTRAS
#endif
};

typedef class Class__class *Class_;

class Class__class : public tree_node
{
  public:
    tree_node *copy() { return copy_Class_(); }
    virtual Class_ copy_Class_() = 0;

#ifdef Class__EXTRAS
    Class__EXTRAS
#endif
};

typedef class Feature_class *Feature;

class Feature_class : public tree_node
{
  public:
    Symbol parent;

    tree_node *copy() { return copy_Feature(); }
    void set_parent(Symbol parent_) { parent = parent_; }
    virtual Feature copy_Feature() = 0;
    virtual Symbol get_name() = 0;
    virtual void first_pass(std::list<Feature> *methods,
                            std::list<Feature> *attrs) = 0;
    virtual void code_prototype_object(ostream &s) = 0;
    virtual void code_dispatch_table(ostream &s) = 0;
    virtual void code_object_initializer(std::list<Feature> *attrs,
                                         ostream &s) = 0;
    virtual void code_class_method(ostream &s) = 0;

#ifdef Feature_EXTRAS
    Feature_EXTRAS
#endif
};

typedef class Formal_class *Formal;

class Formal_class : public tree_node
{
  public:
    tree_node *copy() { return copy_Formal(); }
    virtual Formal copy_Formal() = 0;
    virtual Symbol get_name() = 0;

#ifdef Formal_EXTRAS
    Formal_EXTRAS
#endif
};

typedef class Expression_class *Expression;

class Expression_class : public tree_node
{
  public:
    tree_node *copy() { return copy_Expression(); }
    virtual Expression copy_Expression() = 0;

#ifdef Expression_EXTRAS
    Expression_EXTRAS
#endif
};

typedef class Case_class *Case;

class Case_class : public tree_node
{
  public:
    tree_node *copy() { return copy_Case(); }
    virtual Case copy_Case() = 0;
    virtual Symbol get_type_decl() = 0;
    virtual int get_tag() = 0;
    virtual int get_max_tag() = 0;
    virtual void set_tag(int tag_) = 0;
    virtual void set_max_tag(int max_tag_) = 0;
    virtual void code(ostream &s) = 0;

#ifdef Case_EXTRAS
    Case_EXTRAS
#endif
};

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;

typedef list_node<Feature> Features_class;
typedef Features_class *Features;

typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;

typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;

typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

class program_class : public Program_class
{
  public:
    Classes classes;

  public:
    program_class(Classes a1)
    {
        classes = a1;
    }
    Program copy_Program();
    void dump(ostream &stream, int n);

#ifdef Program_SHARED_EXTRAS
    Program_SHARED_EXTRAS
#endif
#ifdef program_EXTRAS
        program_EXTRAS
#endif
};

class class__class : public Class__class
{
  public:
    Symbol name;
    Symbol parent;
    Features features;
    Symbol filename;

  public:
    class__class(Symbol a1, Symbol a2, Features a3, Symbol a4)
    {
        name = a1;
        parent = a2;
        features = a3;
        filename = a4;
    }
    Class_ copy_Class_();
    void dump(ostream &stream, int n);

#ifdef Class__SHARED_EXTRAS
    Class__SHARED_EXTRAS
#endif
#ifdef class__EXTRAS
        class__EXTRAS
#endif
};

class method_class : public Feature_class
{
  public:
    Symbol name;
    Formals formals;
    Symbol return_type;
    Expression expr;

  public:
    method_class(Symbol a1, Formals a2, Symbol a3, Expression a4)
    {
        name = a1;
        formals = a2;
        return_type = a3;
        expr = a4;
    }
    Feature copy_Feature();
    void dump(ostream &stream, int n);
    Symbol get_name() { return name; };
    void first_pass(std::list<Feature> *methods,
                    std::list<Feature> *attrs);
    void code_prototype_object(ostream &s){};
    void code_dispatch_table(ostream &s);
    void code_object_initializer(std::list<Feature> *attrs, ostream &s){};
    void code_class_method(ostream &s);

#ifdef Feature_SHARED_EXTRAS
    Feature_SHARED_EXTRAS
#endif
#ifdef method_EXTRAS
        method_EXTRAS
#endif
};

class attr_class : public Feature_class
{
  public:
    Symbol name;
    Symbol type_decl;
    Expression init;

  public:
    attr_class(Symbol a1, Symbol a2, Expression a3)
    {
        name = a1;
        type_decl = a2;
        init = a3;
    }
    Feature copy_Feature();
    void dump(ostream &stream, int n);
    Symbol get_name() { return name; };
    void first_pass(std::list<Feature> *methods,
                    std::list<Feature> *attrs);
    void code_prototype_object(ostream &s);
    void code_dispatch_table(ostream &s){};
    void code_object_initializer(std::list<Feature> *attrs, ostream &s);
    void code_class_method(ostream &s){};

#ifdef Feature_SHARED_EXTRAS
    Feature_SHARED_EXTRAS
#endif
#ifdef attr_EXTRAS
        attr_EXTRAS
#endif
};

class formal_class : public Formal_class
{
  public:
    Symbol name;
    Symbol type_decl;

  public:
    formal_class(Symbol a1, Symbol a2)
    {
        name = a1;
        type_decl = a2;
    }
    Symbol get_name() { return name; };
    Formal copy_Formal();
    void dump(ostream &stream, int n);

#ifdef Formal_SHARED_EXTRAS
    Formal_SHARED_EXTRAS
#endif
#ifdef formal_EXTRAS
        formal_EXTRAS
#endif
};

class branch_class : public Case_class
{
  public:
    Symbol name;
    Symbol type_decl;
    Expression expr;
    int tag;
    int max_tag;

  public:
    branch_class(Symbol a1, Symbol a2, Expression a3)
    {
        name = a1;
        type_decl = a2;
        expr = a3;
    }
    Symbol get_type_decl() { return type_decl; };
    int get_tag() { return tag; }
    int get_max_tag() { return max_tag; }
    void set_tag(int tag_) { tag = tag_; };
    void set_max_tag(int max_tag_) { max_tag = max_tag_; };
    void code(ostream &s);
    Case copy_Case();
    void dump(ostream &stream, int n);

#ifdef Case_SHARED_EXTRAS
    Case_SHARED_EXTRAS
#endif
#ifdef branch_EXTRAS
        branch_EXTRAS
#endif
};

class assign_class : public Expression_class
{
  public:
    Symbol name;
    Expression expr;

  public:
    assign_class(Symbol a1, Expression a2)
    {
        name = a1;
        expr = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef assign_EXTRAS
        assign_EXTRAS
#endif
};

class static_dispatch_class : public Expression_class
{
  public:
    Expression expr;
    Symbol type_name;
    Symbol name;
    Expressions actual;

  public:
    static_dispatch_class(Expression a1, Symbol a2, Symbol a3, Expressions a4)
    {
        expr = a1;
        type_name = a2;
        name = a3;
        actual = a4;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef static_dispatch_EXTRAS
        static_dispatch_EXTRAS
#endif
};

class dispatch_class : public Expression_class
{
  public:
    Expression expr;
    Symbol name;
    Expressions actual;

  public:
    dispatch_class(Expression a1, Symbol a2, Expressions a3)
    {
        expr = a1;
        name = a2;
        actual = a3;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef dispatch_EXTRAS
        dispatch_EXTRAS
#endif
};

class cond_class : public Expression_class
{
  public:
    Expression pred;
    Expression then_exp;
    Expression else_exp;

  public:
    cond_class(Expression a1, Expression a2, Expression a3)
    {
        pred = a1;
        then_exp = a2;
        else_exp = a3;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef cond_EXTRAS
        cond_EXTRAS
#endif
};

class loop_class : public Expression_class
{
  public:
    Expression pred;
    Expression body;

  public:
    loop_class(Expression a1, Expression a2)
    {
        pred = a1;
        body = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef loop_EXTRAS
        loop_EXTRAS
#endif
};

class typcase_class : public Expression_class
{
  public:
    Expression expr;
    Cases cases;

  public:
    typcase_class(Expression a1, Cases a2)
    {
        expr = a1;
        cases = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef typcase_EXTRAS
        typcase_EXTRAS
#endif
};

class block_class : public Expression_class
{
  public:
    Expressions body;

  public:
    block_class(Expressions a1)
    {
        body = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef block_EXTRAS
        block_EXTRAS
#endif
};

class let_class : public Expression_class
{
  public:
    Symbol identifier;
    Symbol type_decl;
    Expression init;
    Expression body;

  public:
    let_class(Symbol a1, Symbol a2, Expression a3, Expression a4)
    {
        identifier = a1;
        type_decl = a2;
        init = a3;
        body = a4;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef let_EXTRAS
        let_EXTRAS
#endif
};

class plus_class : public Expression_class
{
  public:
    Expression e1;
    Expression e2;

  public:
    plus_class(Expression a1, Expression a2)
    {
        e1 = a1;
        e2 = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef plus_EXTRAS
        plus_EXTRAS
#endif
};

class sub_class : public Expression_class
{
  public:
    Expression e1;
    Expression e2;

  public:
    sub_class(Expression a1, Expression a2)
    {
        e1 = a1;
        e2 = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef sub_EXTRAS
        sub_EXTRAS
#endif
};

class mul_class : public Expression_class
{
  public:
    Expression e1;
    Expression e2;

  public:
    mul_class(Expression a1, Expression a2)
    {
        e1 = a1;
        e2 = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef mul_EXTRAS
        mul_EXTRAS
#endif
};

class divide_class : public Expression_class
{
  public:
    Expression e1;
    Expression e2;

  public:
    divide_class(Expression a1, Expression a2)
    {
        e1 = a1;
        e2 = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef divide_EXTRAS
        divide_EXTRAS
#endif
};

class neg_class : public Expression_class
{
  public:
    Expression e1;

  public:
    neg_class(Expression a1)
    {
        e1 = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef neg_EXTRAS
        neg_EXTRAS
#endif
};

class lt_class : public Expression_class
{
  public:
    Expression e1;
    Expression e2;

  public:
    lt_class(Expression a1, Expression a2)
    {
        e1 = a1;
        e2 = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef lt_EXTRAS
        lt_EXTRAS
#endif
};

class eq_class : public Expression_class
{
  public:
    Expression e1;
    Expression e2;

  public:
    eq_class(Expression a1, Expression a2)
    {
        e1 = a1;
        e2 = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef eq_EXTRAS
        eq_EXTRAS
#endif
};

class leq_class : public Expression_class
{
  public:
    Expression e1;
    Expression e2;

  public:
    leq_class(Expression a1, Expression a2)
    {
        e1 = a1;
        e2 = a2;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef leq_EXTRAS
        leq_EXTRAS
#endif
};

class comp_class : public Expression_class
{
  public:
    Expression e1;

  public:
    comp_class(Expression a1)
    {
        e1 = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef comp_EXTRAS
        comp_EXTRAS
#endif
};

class int_const_class : public Expression_class
{
  public:
    Symbol token;

  public:
    int_const_class(Symbol a1)
    {
        token = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef int_const_EXTRAS
        int_const_EXTRAS
#endif
};

class bool_const_class : public Expression_class
{
  public:
    Boolean val;

  public:
    bool_const_class(Boolean a1)
    {
        val = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef bool_const_EXTRAS
        bool_const_EXTRAS
#endif
};

class string_const_class : public Expression_class
{
  public:
    Symbol token;

  public:
    string_const_class(Symbol a1)
    {
        token = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef string_const_EXTRAS
        string_const_EXTRAS
#endif
};

class new__class : public Expression_class
{
  public:
    Symbol type_name;

  public:
    new__class(Symbol a1)
    {
        type_name = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef new__EXTRAS
        new__EXTRAS
#endif
};

class isvoid_class : public Expression_class
{
  public:
    Expression e1;

  public:
    isvoid_class(Expression a1)
    {
        e1 = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef isvoid_EXTRAS
        isvoid_EXTRAS
#endif
};

class no_expr_class : public Expression_class
{
  public:
  public:
    no_expr_class()
    {
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef no_expr_EXTRAS
        no_expr_EXTRAS
#endif
};

class object_class : public Expression_class
{
  public:
    Symbol name;

  public:
    object_class(Symbol a1)
    {
        name = a1;
    }
    Expression copy_Expression();
    void dump(ostream &stream, int n);

#ifdef Expression_SHARED_EXTRAS
    Expression_SHARED_EXTRAS
#endif
#ifdef object_EXTRAS
        object_EXTRAS
#endif
};

Classes nil_Classes();
Classes single_Classes(Class_);
Classes append_Classes(Classes, Classes);
Features nil_Features();
Features single_Features(Feature);
Features append_Features(Features, Features);
Formals nil_Formals();
Formals single_Formals(Formal);
Formals append_Formals(Formals, Formals);
Expressions nil_Expressions();
Expressions single_Expressions(Expression);
Expressions append_Expressions(Expressions, Expressions);
Cases nil_Cases();
Cases single_Cases(Case);
Cases append_Cases(Cases, Cases);
Program program(Classes);
Class_ class_(Symbol, Symbol, Features, Symbol);
Feature method(Symbol, Formals, Symbol, Expression);
Feature attr(Symbol, Symbol, Expression);
Formal formal(Symbol, Symbol);
Case branch(Symbol, Symbol, Expression);
Expression assign(Symbol, Expression);
Expression static_dispatch(Expression, Symbol, Symbol, Expressions);
Expression dispatch(Expression, Symbol, Expressions);
Expression cond(Expression, Expression, Expression);
Expression loop(Expression, Expression);
Expression typcase(Expression, Cases);
Expression block(Expressions);
Expression let(Symbol, Symbol, Expression, Expression);
Expression plus(Expression, Expression);
Expression sub(Expression, Expression);
Expression mul(Expression, Expression);
Expression divide(Expression, Expression);
Expression neg(Expression);
Expression lt(Expression, Expression);
Expression eq(Expression, Expression);
Expression leq(Expression, Expression);
Expression comp(Expression);
Expression int_const(Symbol);
Expression bool_const(Boolean);
Expression string_const(Symbol);
Expression new_(Symbol);
Expression isvoid(Expression);
Expression no_expr();
Expression object(Symbol);

#endif
