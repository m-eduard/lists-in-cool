-- Base class that implements a toString method
class PrintableObject inherits Object {
    toString() : String {
        "PrintableObject"
    };
};

class PrintableInt inherits PrintableObject {
    value: Int;

    init(v: Int) : PrintableInt {{
            value <- v;
            self;
    }};

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

    toStringHelper(num: Int) : String {
        if num < 10 then {
            if num < 0 then "-".concat(toStringHelper(~num)) else digit2String(num) fi;
        } else {
            toStringHelper(num / 10).concat(digit2String(num - (num / 10) * 10));
        } fi
    };

    toString() : String {
        toStringHelper(value)
    };
};

class PrintableString inherits PrintableObject {
    value: String;

    init(v : String) : PrintableString {{
        value <- v;
        self;
    }};

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

    toString() : String {
        if value then "true" else "false" fi
    };
};

class List inherits PrintableObject {
    hd: PrintableObject;
    tl: List;
    size: Int <- 0;

    add(o : Object):SELF_TYPE {{
        if isvoid hd then {
            -- Wrap the new element in a PrintableObject
            let wrappedO: PrintableObject <-
                case o of
                    s: String => new PrintableString.init(s);
                    i: Int => new PrintableInt.init(i);
                    b: Bool => new PrintableBool.init(b);
                    printable: PrintableObject => printable;
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

    helperToString(delim: String) : String {
        if isvoid hd then {
            "";
        } else {
            if isvoid tl then {
                hd.toString();
            } else {
                hd.toString().concat(delim).concat(tl.helperToString(delim));
            } fi;
        } fi
    };

    toString() : String {
        "[".concat(self.helperToString(", ")).concat("]")
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

    merge(other : List):SELF_TYPE {
        self (* TODO *)
    };

    filterBy():SELF_TYPE {
        self (* TODO *)
    };

    sortBy():SELF_TYPE {
        self (* TODO *)
    };
};