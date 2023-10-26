(*******************************
 *** Classes Product-related ***
 *******************************)
class Product inherits PrintableObject {
    name : String;
    model : String;
    price : Int;

    init(n : String, m: String, p : Int):SELF_TYPE {{
        name <- n;
        model <- m;
        price <- p;
        self;
    }};

    getprice():Int{ price * 119 / 100 };

    toString():String {
        name.concat(",").concat(model)
    };
};

class Edible inherits Product {
    -- VAT tax is lower for foods
    getprice():Int { price * 109 / 100 };
};

class Soda inherits Edible {
    -- sugar tax is 20 bani
    getprice():Int { price * 109 / 100 + 20 };

    getContentType():String { "Soda" };
};

class Coffee inherits Edible {
    -- this is technically poison for ants
    getprice():Int {price * 119 / 100};

    getContentType():String { "Coffee" };
};

class Laptop inherits Product {
    -- operating system cost included
    getprice():Int {price * 119 / 100 + 499};

    getContentType():String { "Laptop" };
};

class Router inherits Product {
    getContentType():String { "Router" };
};

(****************************
 *** Classes Rank-related ***
 ****************************)
class Rank inherits PrintableObject {
    name : String;

    init(n : String):SELF_TYPE {{
        name <- n;
        self;
    }};

    toString():String {
        -- Hint: what are the default methods of Object?
        name
    };
};

class Private inherits Rank {
    getContentType():String { "Private" };
};

class Corporal inherits Private {
    getContentType():String { "Corporal" };
};

class Sergent inherits Corporal {
    getContentType():String { "Sergent" };
};

class Officer inherits Sergent {
    getContentType():String { "Officer" };
};