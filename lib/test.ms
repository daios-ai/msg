# test.ms — caret/tab alignment test

let expectsStr = fun(s: Str) -> Str do
    s
end

let x = 123

		expectsStr(x)   # <-- this line begins with TWO TAB characters before 'expectsStr'
                      #     keep them as real tabs to test caret positioning
