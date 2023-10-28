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

        while looping loop {
            if cmd = "help" then {
                out_string("Available commands:\n-> load <class_type attr1 attr2 ...> ...\n");
            } else if cmd = "load" then {
                load();
            } else if cmd = "print" then {
                out_string(
                    if stringTokenizer.hasMoreTokens() then
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
                let idx: Int <- new PrintableInt.atoi(stringTokenizer.nextToken()) - 1,
                    ls: PrintableObject <- lists.get(idx),
                    filterType: String <- stringTokenizer.nextToken(),
                    filter: Filter
                in {
                    filter <- if filterType = "ProductFilter" then new ProductFilter else
                        if filterType = "RankFilter" then new RankFilter else
                        if filterType = "SamePriceFilter" then new SamePriceFilter else {
                            abort();
                            new RankFilter;
                        } fi fi fi;

                    case ls of
                        l: List => l.filterBy(filter);
                        o: Object => abort();
                    esac;
                };
            } else if cmd = "sortBy" then {
                let idx: Int <- new PrintableInt.atoi(stringTokenizer.nextToken()) - 1,
                    ls: PrintableObject <- lists.get(idx),
                    comparatorType: String <- stringTokenizer.nextToken(),
                    direction: String <- stringTokenizer.nextToken(),
                    comparator: Comparator
                in {
                    comparator <- if comparatorType = "PriceComparator" then new PriceComparator else
                        if comparatorType = "RankComparator" then new RankComparator else
                        if comparatorType = "AlphabeticComparator" then new AlphabeticComparator else {
                            abort();
                            new PriceComparator;
                        } fi fi fi;
                    
                    if direction = "descendent" then comparator.desc() else comparator fi;

                    case ls of
                        l: List => l.sortBy(comparator);
                        o: Object => abort();
                    esac;
                };
            } else if cmd = "exit" then {
                abort();
            } else {
                out_string("unknown command\n");
                abort();
            } fi fi fi fi fi fi fi;

            stringTokenizer.init(in_string(), " ");
            cmd <- stringTokenizer.nextToken();
            
        } pool;
    }};
};