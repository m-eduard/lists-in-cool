class Main inherits IO {
    lists: List <- new List;
    looping: Bool <- true;
    cmd: String;
    stringTokenizer: StringTokenizer <- new StringTokenizer;

    load() : SELF_TYPE {{
        let className: String,
            classInitializer: StringTokenizer <- new StringTokenizer,
            newList: List <- new List
        in {
            while not className = "END" loop {
                classInitializer.init(in_string(), " ");
                className <- classInitializer.nextToken();

                if className = "String" then {
                    newList.add(classInitializer.nextToken());
                } else if className = "Int" then {
                    newList.add(new PrintableInt.atoi(classInitializer.nextToken()));
                } else if className = "Bool" then {
                    let value: String <- classInitializer.nextToken() in {
                        newList.add(if value = "true" then true
                            else if value = "false" then false
                            else abort() fi fi
                        );
                    };
                } else if className = "IO" then {
                    newList.add(new IO);
                } else if className = "Soda" then {
                    newList.add(new Soda.init(classInitializer.nextToken(),
                        classInitializer.nextToken(),
                        new PrintableInt.atoi(classInitializer.nextToken())
                    ));
                } else if className = "Coffee" then {
                    newList.add(new Coffee.init(classInitializer.nextToken(),
                        classInitializer.nextToken(),
                        new PrintableInt.atoi(classInitializer.nextToken())
                    ));
                } else if className = "Laptop" then {
                    newList.add(new Laptop.init(classInitializer.nextToken(),
                        classInitializer.nextToken(),
                        new PrintableInt.atoi(classInitializer.nextToken())
                    ));
                } else if className = "Router" then {
                    newList.add(new Router.init(classInitializer.nextToken(),
                        classInitializer.nextToken(),
                        new PrintableInt.atoi(classInitializer.nextToken())
                    ));
                } else if className = "Private" then {
                    newList.add(new Private.init(classInitializer.nextToken()));
                } else if className = "Corporal" then {
                    newList.add(new Corporal.init(classInitializer.nextToken()));
                } else if className = "Sergent" then {
                    newList.add(new Sergent.init(classInitializer.nextToken()));
                } else if className = "Officer" then {
                    newList.add(new Officer.init(classInitializer.nextToken()));
                } else if className = "END" then {
                    newList;
                } else {
                    abort();
                } fi fi fi fi fi fi fi fi fi fi fi fi fi;
            } pool;

            lists.add(newList);
        };

        self;
    }};

    main() : Object {{
        -- First command is a load from STDIN
        -- (the name of the command is not explicitly provided)
        cmd <- "load";

        -- let test: List <- new List.add(1).add(2), other: List <- new List.add(3).add(4) in {
        --     out_string(test.toString());
        --     out_string(other.toString());

        --     out_string(test.merge(other).toString());
        -- };

        while looping loop {
            if cmd = "help" then {
                out_string("Available commands:\n-> load <class_type attr1 attr2 ...> ...\n");
            } else if cmd = "load" then {
                load();
            } else if cmd = "print" then {
                out_string(if stringTokenizer.hasMoreTokens() then
                    lists.get(new PrintableInt.atoi(stringTokenizer.nextToken()) - 1).toString()
                else
                    lists.toStringIndexed()
                fi.concat("\n"));
            } else if cmd = "merge" then {
                let idx1: Int <- new PrintableInt.atoi(stringTokenizer.nextToken()) - 1,
                    idx2: Int <- new PrintableInt.atoi(stringTokenizer.nextToken()) - 1,
                    ls1: PrintableObject <- lists.get(idx1),
                    ls2: PrintableObject <- lists.get(idx2),
                    ls: List
                in {
                    case ls1 of
                        l: List => {
                            case ls2 of
                                l2: List => {
                                    ls <- l.merge(l2);
                                };
                            esac;
                        };
                        o: Object => {abort();};
                    esac;

                    lists.add(ls);

                    if idx1 < idx2 then {
                        lists.remove(idx1);
                        lists.remove(idx2 - 1);
                    } else {
                        if idx1 = idx2 then {
                            abort();
                        } else {
                            lists.remove(idx1);
                            lists.remove(idx2);
                        } fi;
                    } fi;
                };
            } else if cmd = "filterBy" then {
                out_string("print\n");
            } else if cmd = "sortBy" then {
                out_string("print\n");
            } else if cmd = "exit" then {
                abort();
            } else {
                out_string("unknown command\n");
                -- abort();
            } fi fi fi fi fi fi fi;

            stringTokenizer.init(in_string(), " ");
            cmd <- stringTokenizer.nextToken();
            
        } pool;
    }};
};

-- Class that splits a string in a list of tokens
-- using a custom delimiter (only one character strings
-- are supported)
class StringTokenizer inherits IO {
    str: String;
    currentPos: Int;
    delimiter: String;
    tokens: List;

    -- When this method is called, a list of Strings containing all
    -- the individual tokens is created
    init(s: String, delimiter: String) : SELF_TYPE {{
        str <- s;
        currentPos <- 0;
        tokens <- new List;

        if tokens.size() = 0 then {
            let left: Int <- 0, i: Int <- 0 in {
                while i < str.length() loop {
                    if str.substr(i, 1) = delimiter then {
                        tokens.add(str.substr(left, i - left));

                        while if i < str.length()
                                then str.substr(i, 1) = delimiter
                                else false 
                            fi
                        loop {
                            i <- i + 1;
                            left <- i;
                        } pool;
                    } else {
                        -- If the current character is not delimiter,
                        -- just increment the i counter
                        i <- i + 1;
                        left;
                    } fi;
                } pool;

                -- Add the last token to the list
                if left < str.length() then {
                    tokens.add(str.substr(left, str.length() - left));
                } else tokens fi;
            };

            currentPos;
        } else {
            currentPos;
        } fi;

        self;
    }};

    -- Return the current token as String and
    -- pdate the current position in the tokens list
    nextToken() : String {{
        currentPos <- currentPos + 1;
        tokens.get(currentPos - 1).toString();
    }};

    hasMoreTokens() : Bool {{
        not currentPos = tokens.size();
    }};
};
