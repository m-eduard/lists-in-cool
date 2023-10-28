(* Think of these as abstract classes *)
class Comparator {
    desc():SELF_TYPE {self};
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class Filter {
    filter(o : Object):Bool {true};
};


(* Specified comparators and filters *)
class ProductFilter inherits Filter {
    filter(o: Object) : Bool {
        case o of
            p: Product => true;
            o: Object => false;
        esac
    };
};

class RankFilter inherits Filter {
    filter(o: Object) : Bool {
        case o of
            r: Rank => true;
            o: Object => false;
        esac
    };
};

class SamePriceFilter inherits Filter {
    filter(o: Object) : Bool {
        case o of
            p: Product => p.getprice() = p@Product.getprice();
            o: Object => false;
        esac
    };
};

class PriceComparator inherits Comparator {
    asc: Bool <- true;

    desc() : SELF_TYPE {{
        asc <- false;
        self;
    }};

    compareTo(o1: Object, o2: Object) : Int {
        if asc then 1 else ~1 fi *
        case o1 of
            p1: Product => case o2 of
                    p2: Product => p1.getprice() - p2.getprice();
                    o2: Object => {abort(); 0;};
                esac;
            o1: Object => {abort(); 0;};
        esac
    };
};

class RankComparator inherits Comparator {
    asc: Bool <- true;

    desc() : SELF_TYPE {{
        asc <- false;
        self;
    }};

    getLevel(rank: Object) : Int {
        case rank of
            p: Private => 1;
            c: Corporal => 2;
            s: Sergent => 3;
            o: Officer => 4;
            r: Rank => {abort(); 0;};
        esac
    };

    compareTo(o1: Object, o2: Object) : Int {
        if asc then 1 else ~1 fi *
        let level1: Int, level2: Int in {
            level1 <- getLevel(o1);
            level2 <- getLevel(o2);

            level1 - level2;
        }
    };
};

class AlphabeticComparator inherits Comparator {
    asc: Bool <- true;

    desc() : SELF_TYPE {{
        asc <- false;
        self;
    }};

    compareTo(o1 : Object, o2 : Object):Int {
        if asc then 1 else ~1 fi *
        case o1 of
            s1: PrintableString => case o2 of
                    s2: PrintableString =>
                        if s1.toString() < s2.toString() then ~1 else
                        if s1 = s2 then 0 else
                        1 fi fi;
                    o2: Object => {abort(); 0;};
                esac;
            o1: Object => {abort(); 0;};
        esac
    };
};