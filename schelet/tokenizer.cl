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