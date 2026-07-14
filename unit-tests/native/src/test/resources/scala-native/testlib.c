typedef int (*Callback0)(void);

int exec(Callback0 f) { return f(); }

// used by CStructByValueTest

typedef struct {
    long long a;
    long long b;
} PairLL;
long long sumPairLL(PairLL p) { return p.a + p.b; }
PairLL makePairLL(long long a, long long b) {
    PairLL p;
    p.a = a;
    p.b = b;
    return p;
}

typedef struct {
    double a;
    double b;
    double c;
} Triple;
double sumTriple(Triple t) { return t.a + t.b + t.c; }
Triple makeTriple(double a, double b, double c) {
    Triple t;
    t.a = a;
    t.b = b;
    t.c = c;
    return t;
}

typedef struct {
    int a;
    double b;
} Mixed;
double sumMixed(Mixed m) { return m.a + m.b; }
Mixed makeMixed(int a, double b) {
    Mixed m;
    m.a = a;
    m.b = b;
    return m;
}

typedef struct {
    long long a;
    long long b;
    long long c;
    long long d;
} Big;
long long sumBig(Big x) { return x.a + x.b + x.c + x.d; }
Big makeBig(long long a, long long b, long long c, long long d) {
    Big x;
    x.a = a;
    x.b = b;
    x.c = c;
    x.d = d;
    return x;
}

typedef struct {
    PairLL p;
    int c;
} Nested;
long long sumNested(Nested n) { return n.p.a + n.p.b + n.c; }

PairLL incrementPairLL(PairLL p) {
    PairLL r;
    r.a = p.a + 1;
    r.b = p.b + 1;
    return r;
}

typedef struct {
    signed char a;
    long long b;
} ByteLong;
long long sumByteLong(ByteLong s) { return (long long)s.a + s.b; }
ByteLong makeByteLong(signed char a, long long b) {
    ByteLong s;
    s.a = a;
    s.b = b;
    return s;
}

typedef struct {
    short a;
    int b;
    float c;
} ShortIntFloat;
double sumShortIntFloat(ShortIntFloat s) {
    return (double)s.a + (double)s.b + (double)s.c;
}
ShortIntFloat makeShortIntFloat(short a, int b, float c) {
    ShortIntFloat s;
    s.a = a;
    s.b = b;
    s.c = c;
    return s;
}

typedef struct {
    signed char a;
    signed char b;
    signed char c;
    signed char d;
} FourBytes;
int sumFourBytes(FourBytes s) {
    return (int)s.a + (int)s.b + (int)s.c + (int)s.d;
}
FourBytes makeFourBytes(signed char a, signed char b, signed char c,
                        signed char d) {
    FourBytes s;
    s.a = a;
    s.b = b;
    s.c = c;
    s.d = d;
    return s;
}

typedef struct {
    char a;
    short b;
    long long c;
    int d;
} ManyFields;
long long sumManyFields(ManyFields s) {
    return (long long)s.a + (long long)s.b + s.c + (long long)s.d;
}
ManyFields makeManyFields(char a, short b, long long c, int d) {
    ManyFields s;
    s.a = a;
    s.b = b;
    s.c = c;
    s.d = d;
    return s;
}
