-- Base class that implements a toString method
class PrintableObject inherits Object {
    toString() : String {
        ""
    };

    -- This method might've been omitted if we were allowed to
    -- print the real type of the current instance in toPrettyString(),
    -- using self.type_name(), but in this case, for example, instead
    -- of Bool(true) we would've seen PrintableBool(true) at STDOUT
    getContentType() : String {
        ""
    };

    -- This method will not be overriden in any child class
    -- (it depends on the actual implementation of toString())
    toPrettyString() : String {
        case self of 
            l: List => toString();
            o: Object => getContentType().concat("(").concat(toString()).concat(")");
        esac
    };
};

class PrintableInt inherits PrintableObject {
    value: Int;

    init(v: Int) : PrintableInt {{
        value <- v;
        self;
    }};

    atoi(s: String) : Int {
        let num: Int <- 0, i: Int <- 0 in {
            while i < s.length() loop {
                num <- num * 10 + string2Digit(s.substr(i, 1));
                i <- i + 1;
            } pool;

            num;
        }
    };

    string2Digit(s: String) : Int {
        if s = "0" then 0 else
        if s = "1" then 1 else
        if s = "2" then 2 else
        if s = "3" then 3 else
        if s = "4" then 4 else
        if s = "5" then 5 else
        if s = "6" then 6 else
        if s = "7" then 7 else
        if s = "8" then 8 else
        if s = "9" then 9 else {
            abort();
            0;
        } fi fi fi fi fi fi fi fi fi fi
    };

    digit2String(num: Int) : String {
        if num = 0 then "0" else
        if num = 1 then "1" else
        if num = 2 then "2" else
        if num = 3 then "3" else
        if num = 4 then "4" else
        if num = 5 then "5" else
        if num = 6 then "6" else
        if num = 7 then "7" else
        if num = 8 then "8" else
        if num = 9 then "9" else {
            abort();
            "";
        } fi fi fi fi fi fi fi fi fi fi
    };

    itoa(num: Int) : String {
        if num < 10 then {
            if num < 0 then "-".concat(itoa(~num)) else digit2String(num) fi;
        } else {
            itoa(num / 10).concat(digit2String(num - (num / 10) * 10));
        } fi
    };

    getContentType() : String {
        "Int"
    };

    toString() : String {
        itoa(value)
    };
};

class PrintableString inherits PrintableObject {
    value: String;

    init(v : String) : PrintableString {{
        value <- v;
        self;
    }};

    getContentType() : String {
        "String"
    };

    toString() : String {
        value
    };
};

class PrintableBool inherits PrintableObject {
    value: Bool;

    init(v : Bool) : PrintableBool {{
        value <- v;
        self;
    }};

    getContentType() : String {
        "Bool"
    };

    toString() : String {
        if value then "true" else "false" fi
    };
};

class PrintableIO inherits PrintableObject {
    value: IO;

    init(v : IO) : PrintableIO {{
        value <- v;
        self;
    }};

    getContentType() : String {
        "IO"
    };
};

-- Generic list class that wraps each newly added element
-- into a PrintableObject (base class which has a toString method).
-- It can store other lists, primitive types and objects which
-- inherit from Product / Rank base classes.
class List inherits PrintableObject {
    hd: PrintableObject;
    tl: List;
    size: Int <- 0;

    add(o : Object) : List {{
        if size = 0 then {
            -- Wrap the new element in a PrintableObject
            let wrappedO: PrintableObject <-
                case o of
                    printable: PrintableObject => printable;
                    s: String => new PrintableString.init(s);
                    i: Int => new PrintableInt.init(i);
                    b: Bool => new PrintableBool.init(b);
                    io: IO => new PrintableIO.init(io);
                    list: List => list;
                    obj: Object => {abort(); new PrintableInt.init(0);};
                esac
            in {
                hd <- wrappedO;
            };
        } else {
            if isvoid tl then {
                tl <- new List;
            } else tl fi;

            tl = tl.add(o);
        } fi;

        size <- size + 1;
        self;
    }};

    helperToString(delim: String, indexed: Bool, i: Int) : String {
        if size = 0 then {
            "";
        } else {
            if indexed then
                new PrintableInt.itoa(i).concat(": ").concat(hd.toPrettyString())
            else
                hd.toPrettyString()
            fi.concat(if isvoid tl then "" else if tl.size() = 0 then "" else
                delim.concat(tl.helperToString(delim, indexed, i + 1)) fi fi);
        } fi
    };

    toString() : String {
        "[ ".concat(self.helperToString(", ", false, 1)).concat(" ]")
    };

    toStringIndexed() : String {
        self.helperToString("\n", true, 1)
    };

    size() : Int {
        size
    };

    get(idx: Int) : PrintableObject {
        if idx = 0 then hd else {
            if idx < 0 then {
                abort();
                new PrintableObject;
            } else {
                if size <= idx then {
                    abort();
                    new PrintableObject;
                } else tl.get(idx - 1) fi;
            } fi;
        } fi
    };

    getHd() : PrintableObject {
        hd
    };

    getTl() : List {
        tl
    };

    remove(idx: Int) : List {{
        if size <= idx then {
            abort();
            self;
        } else {
            if idx = 0 then {
                if isvoid tl then {
                    -- It's impossible to make hd equal to void,
                    -- so we just change the size of the current list
                    hd;
                } else {
                    hd <- tl.getHd();
                    tl <- tl.getTl();
                } fi;
            } else {
                if not isvoid tl then {
                    if not tl.size() = 0 then {
                        tl.remove(idx - 1);
                    } else tl fi;
                } else tl fi;
            } fi;

            size <- size - 1;
        } fi;

        self;
    }};

    merge(other: List) : SELF_TYPE {{
        if size = 0 then {
            hd <- other.getHd();
            tl <- other.getTl();
        } else if isvoid tl then {
            tl <- other;
        } else {
            tl.merge(other);
        } fi fi;

        size <- size + other.size();
        self;
    }};

    filterBy(filter: Filter) : SELF_TYPE {{
        -- When hd is void, also the size is 0
        if size = 0 then {
            self;
        } else {
            if not filter.filter(hd) then {
                size <- size - 1;

                if not isvoid tl then {
                    -- When the last element of a list of lists is removed,
                    -- the last list only updates its size to 0, and the previous
                    -- element still points to it, even though its size is 0
                    if not tl.size() = 0 then {
                        hd <- tl.getHd();
                        tl <- tl.getTl();

                        -- Now we have to check if the new element that
                        -- replaced the old head is valid
                        self.filterBy(filter);
                    } else {
                        hd;
                    } fi;
                } else {
                    hd;
                } fi;
            } else {
                if not isvoid tl then {
                    tl.filterBy(filter);
                    size <- tl.size() + 1;
                } else {
                    tl;
                } fi;
            } fi;
        } fi;

        self;
    }};

    sortBy():SELF_TYPE {
        self (* TODO *)
    };
};

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