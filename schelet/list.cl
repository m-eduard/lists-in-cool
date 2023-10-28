-- Generic list class that wraps each newly added element
-- into a PrintableObject (base class which has a toString method).
-- It can store other lists, primitive types and objects which
-- inherit from Product / Rank base classes.
class List inherits PrintableObject {
    hd: PrintableObject;
    tl: List;
    size: Int <- 0;

    wrap(o: Object) : PrintableObject {
        case o of
            printable: PrintableObject => printable;
            s: String => new PrintableString.init(s);
            i: Int => new PrintableInt.init(i);
            b: Bool => new PrintableBool.init(b);
            io: IO => new PrintableIO.init(io);
            list: List => list;
            obj: Object => {abort(); new PrintableInt.init(0);};
        esac
    };

    add(o : Object) : List {{
        if size = 0 then {
            -- Wrap the new element in a PrintableObject
            let wrappedO: PrintableObject <- wrap(o) in {
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

    insert(idx: Int, o: Object) : List {{
        if size <= idx then {
            -- Add the new element at the end of the list
            self.add(o);
        } else {
            if idx = 0 then {
                if size = 0 then {
                    hd <- wrap(o);
                } else {
                    if isvoid tl then {
                        tl <- new List.add(hd);
                    } else {
                        tl.insert(0, hd);
                    } fi;

                    hd <- wrap(o);
                } fi;
            } else if not isvoid tl then {
                    tl.insert(idx - 1, o);
            } else {
                -- it is impossible to have a void tail
                -- and the current index to be different than 0
                out_string("This should never happen");
                abort();
            } fi fi;

            size <- size + 1;
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

    getMin(comparator: Comparator) : Int {
        if size = 0 then ~1 else {
            let minIdx: Int <- 0,
                minVal: PrintableObject <- get(0),
                current: List <- self,
                i: Int <- 0,
                revI: Int <- size - 1
            in {
                while 0 <= revI loop {
                    let compareResult: Int <- comparator.compareTo(current.getHd(), minVal) in {
                        if compareResult < 0 then {
                            minIdx <- i;
                            minVal <- current.getHd();
                        } else {
                            minVal;
                        } fi;
                    };

                    i <- i + 1;
                    revI <- revI - 1;
                    current <- current.getTl();
                } pool;

                minIdx;
            };
        } fi
    };

    sortBy(comparator: Comparator) : SELF_TYPE {{
        if size = 0 then {
            self;
        } else let current: List <- self, i: Int <- size - 1 in {
                while 0 <= i loop {
                    -- Replace the current head with the minimum element found
                    -- (swap the min element found and the current list's head)
                    let minIdx: Int <- current.getMin(comparator),
                        tmpHd: PrintableObject <- current.getHd()
                    in {
                        if minIdx = 0 then {current;} else {
                            current.remove(0);
                            current.insert(0, current.get(minIdx - 1));
                            
                            current.insert(minIdx, tmpHd);
                            current.remove(minIdx + 1);
                        } fi;
                    };

                    current <- current.getTl();
                    i <- i - 1;
                } pool;
            }
        fi;

        self;
    }};
};