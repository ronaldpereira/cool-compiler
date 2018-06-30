#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

static CgenClassTableP tablep;
static CgenNodeP nodep;
static int local_var_offset = 1;
static int max_label = 0;
static int max_tag = 0;
static int stringclasstag;
static int intclasstag;
static int boolclasstag;

Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;

static void initialize_constants(void)
{
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

static char *gc_init_names[] = {
    "_NoGC_Init",
    "_GenGC_Init",
    "_ScnGC_Init"};

static char *gc_collect_names[] = {
    "_NoGC_Collect",
    "_GenGC_Collect",
    "_ScnGC_Collect"};

BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

void program_class::cgen(ostream &os)
{

    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

    os << "\n# end of generated code\n";
}

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream &s)
{
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
      << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream &s)
{
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s)
{
    s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream &s)
{
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s)
{
    s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s)
{
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s)
{
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s)
{
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s)
{
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s)
{
    s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream &s)
{
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s)
{
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s)
{
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s)
{
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s)
{
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s)
{
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s)
{
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s)
{
    s << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream &s)
{
    s << JAL << address << endl;
}

static void emit_return(ostream &s)
{
    s << RET << endl;
}

static void emit_gc_assign(ostream &s)
{
    s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream &s)
{
    s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s)
{
    s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &s)
{
    s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream &s)
{
    s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s)
{
    s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s)
{
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream &s)
{
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

static void emit_push(char *reg, ostream &str)
{
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

static void emit_fetch_int(char *dest, char *source, ostream &s)
{
    emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

static void emit_store_int(char *source, char *dest, ostream &s)
{
    emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s)
{
    emit_push(ACC, s);
    emit_move(ACC, SP, s);
    emit_move(A1, ZERO, s);
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s)
{
    if (source != (char *)A1)
        emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}

static void emit_copy(ostream &s)
{
    s << JAL;
    emit_method_ref(::Object, ::copy, s);
    s << endl;
}

void StringEntry::code_ref(ostream &s)
{
    s << STRCONST_PREFIX << index;
}

void StringEntry::code_def(ostream &s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL
      << WORD << stringclasstag << endl
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl
      << WORD;
    emit_disptable_ref(Str, s);

    s << endl;
    s << WORD;
    lensym->code_ref(s);
    s << endl;
    emit_string_constant(s, str);
    s << ALIGN;
}

void StrTable::code_string_table(ostream &s, int stringclasstag)
{
    for (List<StringEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, stringclasstag);
}

void IntEntry::code_ref(ostream &s)
{
    s << INTCONST_PREFIX << index;
}

void IntEntry::code_def(ostream &s, int intclasstag)
{

    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL
      << WORD << intclasstag << endl
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl
      << WORD;
    emit_disptable_ref(Int, s);

    s << endl;
    s << WORD << str << endl;
}

void IntTable::code_string_table(ostream &s, int intclasstag)
{
    for (List<IntEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, intclasstag);
}

BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const
{
    s << BOOLCONST_PREFIX << val;
}

void BoolConst::code_def(ostream &s, int boolclasstag)
{

    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL
      << WORD << boolclasstag << endl
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl
      << WORD;
    emit_disptable_ref(Bool, s);

    s << endl;
    s << WORD << val << endl;
}

void CgenClassTable::code_global_data()
{
    Symbol main = idtable.lookup_string(MAINNAME);
    Symbol string = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n"
        << ALIGN;

    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    str << INTTAG << LABEL
        << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL
        << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL
        << WORD << stringclasstag << endl;
}

void CgenClassTable::code_global_text()
{
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl
        << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{

    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

void callee_init(ostream &s)
{
    emit_addiu(SP, SP, -3 * WORD_SIZE, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addiu(FP, SP, WORD_SIZE, s);
    emit_move(SELF, ACC, s);
}

void callee_end(int n_args, ostream &s)
{
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, (3 + n_args) * WORD_SIZE, s);
    emit_return(s);
}

std::list<Feature>::iterator name_lookup(std::list<Feature> *l, Symbol name)
{
    std::list<Feature>::iterator it;
    for (it = l->begin(); it != l->end(); ++it)
    {
        if ((*it)->get_name() == name)
        {
            return it;
        }
    }
    return it;
}

int get_offset(std::list<Feature> *l, Symbol name)
{
    std::list<Feature>::iterator it = name_lookup(l, name);
    assert(it != l->end());
    return std::distance(l->begin(), it);
}

int get_attr_offset(std::list<Feature> *attrs, Symbol name)
{
    return get_offset(attrs, name) + DEFAULT_OBJFIELDS;
}

int get_method_offset(Symbol type, Symbol name)
{
    CgenNodeP node = tablep->lookup_node(type);
    assert(node);
    return get_offset(node->get_methods(), name);
}

CgenNodeP CgenClassTable::lookup_node(Symbol name)
{
    if (name == SELF_TYPE)
    {
        return nodep;
    }
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        if (l->hd()->get_name() == name)
        {
            return l->hd();
        }
    }
    return NULL;
}

CgenNodeP CgenClassTable::lookup_tag(int tag)
{
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        if (l->hd()->get_tag() == tag)
        {
            return l->hd();
        }
    }
    return NULL;
}

void attr_class::code_object_initializer(std::list<Feature> *attrs, ostream &s)
{
    if (init->type)
    {
        int offset = get_attr_offset(attrs, name);
        init->code(s);
        emit_store(ACC, offset, SELF, s);
    }
}

void CgenNode::code_object_initializer(ostream &s)
{
    nodep = this;
    emit_init_ref(name, s);
    s << LABEL;
    callee_init(s);
    if (parentnd && parentnd->get_methods() && parentnd->get_attrs())
    {
        s << JAL;
        emit_init_ref(parentnd->get_name(), s);
        s << endl;
    }
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        features->nth(i)->code_object_initializer(attrs, s);
    }
    emit_move(ACC, SELF, s);
    callee_end(0, s);
}

void CgenClassTable::code_object_initializers()
{
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        l->hd()->code_object_initializer(str);
    }
}

void method_class::code_class_method(ostream &s)
{
    emit_method_ref(parent, name, s);
    s << LABEL;
    callee_init(s);
    nodep->get_vars()->enterscope();
    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        nodep->get_vars()->addid(formals->nth(i)->get_name(),
                                 new ObjectLocation(FP, FRAME_OFFSET + formals->len() - 1 - i));
    }
    expr->code(s);
    nodep->get_vars()->exitscope();
    callee_end(formals->len(), s);
}

void CgenNode::code_class_methods(ostream &s)
{
    nodep = this;
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        if (!basic())
        {
            features->nth(i)->code_class_method(s);
        }
    }
}

void CgenClassTable::code_class_methods()
{
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        l->hd()->code_class_methods(str);
    }
}

void CgenNode::code_class_name_table(ostream &s)
{
    StringEntry *entry = stringtable.lookup_string(name->get_string());
    assert(entry);
    s << WORD;
    entry->code_ref(s);
    s << endl;
}

void CgenClassTable::code_class_name_table()
{
    str << CLASSNAMETAB << LABEL;
    for (int i = 0; i < max_tag; i++)
    {
        CgenNodeP node = lookup_tag(i);
        assert(node);
        node->code_class_name_table(str);
    }
}

void CgenClassTable::code_constants()
{

    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}

void attr_class::code_prototype_object(ostream &s)
{
    s << WORD;
    if (type_decl == Str)
    {
        StringEntry *entry = stringtable.lookup_string("");
        assert(entry);
        entry->code_ref(s);
    }
    else if (type_decl == Int)
    {
        IntEntry *entry = inttable.lookup_string("0");
        assert(entry);
        entry->code_ref(s);
    }
    else if (type_decl == Bool)
    {
        falsebool.code_ref(s);
    }
    else
    {
        s << 0;
    }
    s << endl;
}

void CgenNode::code_prototype_object(ostream &s)
{
    s << WORD << "-1" << endl;
    emit_protobj_ref(name, s);
    s << LABEL;
    s << WORD << tag << endl;
    s << WORD << DEFAULT_OBJFIELDS + attrs->size() << endl;
    s << WORD;
    emit_disptable_ref(name, s);
    s << endl;
    for (std::list<Feature>::iterator it = attrs->begin(); it != attrs->end(); it++)
    {
        (*it)->code_prototype_object(s);
    }
}

void CgenClassTable::code_prototype_objects()
{
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        l->hd()->code_prototype_object(str);
    }
}

void CgenNode::code_object_table(ostream &s)
{
    s << WORD;
    emit_protobj_ref(name, s);
    s << endl;
    s << WORD;
    emit_init_ref(name, s);
    s << endl;
}

void CgenClassTable::code_object_table()
{
    str << CLASSOBJTAB << LABEL;
    for (int i = 0; i < max_tag; i++)
    {
        CgenNodeP node = lookup_tag(i);
        assert(node);
        node->code_object_table(str);
    }
}

void method_class::code_dispatch_table(ostream &s)
{
    s << WORD;
    emit_method_ref(parent, name, s);
    s << endl;
}

void CgenNode::code_dispatch_table(ostream &s)
{
    emit_disptable_ref(name, s);
    s << LABEL;
    for (std::list<Feature>::iterator it = methods->begin(); it != methods->end(); it++)
    {
        (*it)->code_dispatch_table(s);
    }
}

void CgenClassTable::code_dispatch_table()
{
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        l->hd()->code_dispatch_table(str);
    }
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s)
{
    tablep = this;

    enterscope();
    if (cgen_debug)
        cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();
    first_pass();

    code();
    exitscope();
}

void CgenClassTable::first_pass()
{
    CgenNodeP root_class = root();
    root_class->first_pass();
}

void CgenNode::set_tag()
{
    tag = max_tag++;
    max_class_tag = tag;
    if (name == Str)
    {
        stringclasstag = tag;
    }
    else if (name == Int)
    {
        intclasstag = tag;
    }
    else if (name == Bool)
    {
        boolclasstag = tag;
    }
}

void CgenNode::first_pass()
{
    set_tag();

    if (parentnd && parentnd->get_methods() && parentnd->get_attrs())
    {
        methods = new std::list<Feature>(parentnd->get_methods()->begin(), parentnd->get_methods()->end());
        attrs = new std::list<Feature>(parentnd->get_attrs()->begin(), parentnd->get_attrs()->end());
    }
    else
    {
        methods = new std::list<Feature>();
        attrs = new std::list<Feature>();
    }

    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        features->nth(i)->first_pass(methods, attrs);
        features->nth(i)->set_parent(name);
    }

    vars = new SymbolTable<Symbol, ObjectLocation>();
    vars->enterscope();
    for (std::list<Feature>::iterator it = attrs->begin(); it != attrs->end(); ++it)
    {
        int offset = get_attr_offset(attrs, (*it)->get_name());
        vars->addid((*it)->get_name(), new ObjectLocation(SELF, offset));
    }

    for (List<CgenNode> *l = children; l; l = l->tl())
    {
        l->hd()->first_pass();
        max_class_tag = std::max(l->hd()->get_max_tag(), max_class_tag);
    }
}

void method_class::first_pass(std::list<Feature> *methods, std::list<Feature> *attrs)
{
    std::list<Feature>::iterator it = name_lookup(methods, name);
    if (it == methods->end())
    {
        methods->push_back(this);
    }
    else
    {
        *it = this;
    }
}

void attr_class::first_pass(std::list<Feature> *methods, std::list<Feature> *attrs)
{
    attrs->push_back(this);
}

void CgenClassTable::install_basic_classes()
{

    Symbol filename = stringtable.add_string("<basic class>");

    addid(No_class,
          new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                       Basic, this));
    addid(SELF_TYPE,
          new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                       Basic, this));
    addid(prim_slot,
          new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                       Basic, this));

    install_class(
        new CgenNode(
            class_(Object,
                   No_class,
                   append_Features(
                       append_Features(
                           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                   filename),
            Basic, this));

    install_class(
        new CgenNode(
            class_(IO,
                   Object,
                   append_Features(
                       append_Features(
                           append_Features(
                               single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                      SELF_TYPE, no_expr())),
                               single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                      SELF_TYPE, no_expr()))),
                           single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
                   filename),
            Basic, this));

    install_class(
        new CgenNode(
            class_(Int,
                   Object,
                   single_Features(attr(val, prim_slot, no_expr())),
                   filename),
            Basic, this));

    install_class(
        new CgenNode(
            class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
            Basic, this));

    install_class(
        new CgenNode(
            class_(Str,
                   Object,
                   append_Features(
                       append_Features(
                           append_Features(
                               append_Features(
                                   single_Features(attr(val, Int, no_expr())),
                                   single_Features(attr(str_field, prim_slot, no_expr()))),
                               single_Features(method(length, nil_Formals(), Int, no_expr()))),
                           single_Features(method(concat,
                                                  single_Formals(formal(arg, Str)),
                                                  Str,
                                                  no_expr()))),
                       single_Features(method(substr,
                                              append_Formals(single_Formals(formal(arg, Int)),
                                                             single_Formals(formal(arg2, Int))),
                                              Str,
                                              no_expr()))),
                   filename),
            Basic, this));
}

void CgenClassTable::install_class(CgenNodeP nd)
{
    Symbol name = nd->get_name();

    if (probe(name))
    {
        return;
    }

    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs)
{
    for (int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

void CgenClassTable::build_inheritance_tree()
{
    for (List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());
}

void CgenClassTable::set_relations(CgenNodeP nd)
{
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
    children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

void CgenClassTable::code()
{
    if (cgen_debug)
        cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug)
        cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug)
        cout << "coding constants" << endl;
    code_constants();

    if (cgen_debug)
        cout << "coding class name table" << endl;
    code_class_name_table();

    if (cgen_debug)
        cout << "coding prototype objects" << endl;
    code_prototype_objects();

    if (cgen_debug)
        cout << "coding object table" << endl;
    code_object_table();

    if (cgen_debug)
        cout << "coding dispatch table" << endl;
    code_dispatch_table();

    if (cgen_debug)
        cout << "coding global text" << endl;
    code_global_text();

    if (cgen_debug)
        cout << "coding object initializers" << endl;
    code_object_initializers();

    if (cgen_debug)
        cout << "coding class methods" << endl;
    code_class_methods();
}

CgenNodeP CgenClassTable::root()
{
    return probe(Object);
}

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) : class__class((const class__class &)*nd),
                                                                       parentnd(NULL),
                                                                       children(NULL),
                                                                       basic_status(bstatus)
{
    stringtable.add_string(name->get_string());
}

void assign_class::code(ostream &s)
{
    expr->code(s);
    ObjectLocation *obj_loc = nodep->get_vars()->lookup(name);
    assert(obj_loc);
    emit_store(ACC, obj_loc->get_offset(), obj_loc->get_register(), s);
}

void dispatch_common(Expression expr, Symbol type_name, Symbol name,
                     Expressions actual, ostream &s)
{
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->code(s);
        emit_push(ACC, s);
    }
    expr->code(s);
    emit_bne(ACC, ZERO, max_label, s);
    StringEntry *entry = stringtable.lookup_string(nodep->get_filename()->get_string());
    assert(entry);
    emit_load_string(ACC, entry, s);
    emit_load_imm(T1, nodep->get_line_number(), s);
    emit_jal("_dispatch_abort", s);

    emit_label_def(max_label, s);

    if (type_name == No_type)
    {
        emit_load(T1, DISPTABLE_OFFSET, ACC, s);
        type_name = expr->get_type();
    }
    else
    {
        emit_partial_load_address(T1, s);
        emit_disptable_ref(type_name, s);
        s << endl;
    }

    int offset = get_method_offset(type_name, name);
    emit_load(T1, offset, T1, s);
    emit_jalr(T1, s);
    max_label++;
}

void static_dispatch_class::code(ostream &s)
{
    dispatch_common(expr, type_name, name, actual, s);
}

void dispatch_class::code(ostream &s)
{
    dispatch_common(expr, No_type, name, actual, s);
}

void cond_class::code(ostream &s)
{
    int label = max_label;
    max_label += 2;

    pred->code(s);
    emit_load_bool(T1, truebool, s);
    emit_beq(ACC, T1, label, s);

    else_exp->code(s);
    emit_branch(label + 1, s);

    emit_label_def(label, s);
    then_exp->code(s);
    emit_label_def(label + 1, s);
}

void loop_class::code(ostream &s)
{
    int label = max_label;
    max_label += 2;

    emit_label_def(label, s);
    pred->code(s);
    emit_load_bool(T1, falsebool, s);
    emit_beq(ACC, T1, label + 1, s);
    body->code(s);
    emit_branch(label, s);

    emit_label_def(label + 1, s);
    emit_load_imm(ACC, 0, s);
}

bool compare_branch(const Case first, const Case second)
{
    return first->get_tag() >= second->get_tag() && first->get_max_tag() <= second->get_max_tag();
}

void branch_class::code(ostream &s)
{
    nodep->get_vars()->enterscope();
    nodep->get_vars()->addid(name, new ObjectLocation(FP, -local_var_offset));
    emit_push(ACC, s);
    local_var_offset++;
    expr->code(s);
    emit_addiu(SP, SP, WORD_SIZE, s);
    local_var_offset--;
    nodep->get_vars()->exitscope();
}

void typcase_class::code(ostream &s)
{
    int label = max_label;
    max_label += cases->len() + 3;

    expr->code(s);
    emit_bne(ACC, ZERO, label, s);
    StringEntry *entry = stringtable.lookup_string(nodep->get_filename()->get_string());
    assert(entry);
    emit_load_string(ACC, entry, s);
    emit_load_imm(T1, nodep->get_line_number(), s);
    emit_jal("_case_abort2", s);

    emit_label_def(label, s);
    emit_push(ACC, s);
    local_var_offset++;
    label++;

    std::list<Case> sorted_cases;
    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        Case case_ = cases->nth(i);
        CgenNodeP node = tablep->lookup_node(case_->get_type_decl());
        assert(node);
        case_->set_tag(node->get_tag());
        case_->set_max_tag(node->get_max_tag());
        sorted_cases.push_back(case_);
    }
    sorted_cases.sort(compare_branch);

    for (std::list<Case>::iterator it = sorted_cases.begin(); it != sorted_cases.end(); it++)
    {
        int i = std::distance(sorted_cases.begin(), it);
        Case case_ = *it;

        emit_label_def(label + i, s);
        emit_load(ACC, 1, SP, s);
        emit_load(T1, TAG_OFFSET, ACC, s);
        emit_blti(T1, case_->get_tag(), label + i + 1, s);
        emit_bgti(T1, case_->get_max_tag(), label + i + 1, s);
        case_->code(s);
        emit_branch(label + sorted_cases.size() + 1, s);
    }

    emit_label_def(label + sorted_cases.size(), s);
    emit_load(ACC, 1, SP, s);
    emit_jal("_case_abort", s);

    emit_label_def(label + sorted_cases.size() + 1, s);
    emit_addiu(SP, SP, WORD_SIZE, s);
    local_var_offset--;
}

void block_class::code(ostream &s)
{
    for (int i = body->first(); body->more(i); i = body->next(i))
    {
        body->nth(i)->code(s);
    }
}

void let_class::code(ostream &s)
{
    init->code(s);
    nodep->get_vars()->enterscope();
    nodep->get_vars()->addid(identifier, new ObjectLocation(FP, -local_var_offset));
    if (init->type == NULL)
    {
        if (type_decl == Str)
        {
            StringEntry *entry = stringtable.lookup_string("");
            emit_load_string(ACC, entry, s);
        }
        else if (type_decl == Int)
        {
            IntEntry *entry = inttable.lookup_string("0");
            emit_load_int(ACC, entry, s);
        }
        else if (type_decl == Bool)
        {
            emit_load_bool(ACC, falsebool, s);
        }
    }
    emit_push(ACC, s);
    local_var_offset++;
    body->code(s);
    emit_addiu(SP, SP, WORD_SIZE, s);
    local_var_offset--;
    nodep->get_vars()->exitscope();
}

void arith_common(Expression e1, Expression e2, ostream &s)
{
    e1->code(s);
    emit_fetch_int(ACC, ACC, s);
    emit_push(ACC, s);
    local_var_offset++;
    e2->code(s);
    emit_copy(s);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, WORD_SIZE, s);
    local_var_offset--;
    emit_fetch_int(T2, ACC, s);
}

void plus_class::code(ostream &s)
{
    arith_common(e1, e2, s);
    emit_addu(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s)
{
    arith_common(e1, e2, s);
    emit_sub(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s)
{
    arith_common(e1, e2, s);
    emit_mul(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s)
{
    arith_common(e1, e2, s);
    emit_div(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s)
{
    e1->code(s);
    emit_copy(s);
    emit_fetch_int(T1, ACC, s);
    emit_neg(T1, T1, s);
    emit_store_int(T1, ACC, s);
}

void lt_common(ostream &s)
{
    emit_load_bool(ACC, falsebool, s);
    emit_branch(max_label, s);

    emit_label_def(max_label + 1, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(max_label, s);

    max_label += 2;
}

void lt_class::code(ostream &s)
{
    arith_common(e1, e2, s);
    emit_blt(T1, T2, max_label + 1, s);
    lt_common(s);
}

void eq_class::code(ostream &s)
{
    int label = max_label;
    max_label++;

    e1->code(s);
    emit_push(ACC, s);
    local_var_offset++;
    e2->code(s);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, WORD_SIZE, s);
    local_var_offset--;
    emit_move(T2, ACC, s);
    emit_load_bool(ACC, truebool, s);
    emit_load_bool(A1, falsebool, s);
    emit_beq(T1, T2, label, s);
    emit_jal("equality_test", s);
    emit_label_def(label, s);
}

void leq_class::code(ostream &s)
{
    arith_common(e1, e2, s);
    emit_bleq(T1, T2, max_label + 1, s);
    lt_common(s);
}

void comp_class::code(ostream &s)
{
    int label = max_label;
    max_label += 2;

    e1->code(s);
    emit_load_bool(T1, truebool, s);
    emit_beq(ACC, T1, label, s);

    emit_load_bool(ACC, truebool, s);
    emit_branch(label + 1, s);

    emit_label_def(label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(label + 1, s);
}

void int_const_class::code(ostream &s)
{

    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream &s)
{
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream &s)
{
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s)
{
    if (type_name == SELF_TYPE)
    {
        emit_load(T1, TAG_OFFSET, SELF, s);
        emit_load_imm(T2, 2 * WORD_SIZE, s);
        emit_mul(T1, T1, T2, s);

        emit_load_address(T2, CLASSOBJTAB, s);
        emit_addu(T2, T1, T2, s);

        emit_load(ACC, 0, T2, s);
        emit_load(T2, 1, T2, s);
        emit_copy(s);
        emit_jalr(T2, s);
    }
    else if (type_name == Bool)
    {
        emit_load_bool(ACC, falsebool, s);
    }
    else
    {
        emit_partial_load_address(ACC, s);
        emit_protobj_ref(type_name, s);
        s << endl;

        emit_copy(s);
        s << JAL;
        emit_init_ref(type_name, s);
        s << endl;
    }
}

void isvoid_class::code(ostream &s)
{
    int label = max_label;
    max_label += 2;

    e1->code(s);
    emit_beq(ACC, ZERO, label, s);

    emit_load_bool(ACC, falsebool, s);
    emit_branch(label + 1, s);

    emit_label_def(label, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(label + 1, s);
}

void no_expr_class::code(ostream &s)
{
    emit_load_imm(ACC, 0, s);
}

void object_class::code(ostream &s)
{
    if (name == self)
    {
        emit_move(ACC, SELF, s);
    }
    else
    {
        ObjectLocation *obj_loc = nodep->get_vars()->lookup(name);
        assert(obj_loc);
        emit_load(ACC, obj_loc->get_offset(), obj_loc->get_register(), s);
    }
}
