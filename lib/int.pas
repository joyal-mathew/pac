function string to_hex_string(var int n)
    var string digits hex;
    digits = "0123456789ABCDEF";
    hex = "0000000000000000";

    var int i;
    i = 15;

    while n do
        set_char(hex, i, get_char(digits, n & 15));
        i = i - 1;
        n = n >> 4;
    end

    return hex;
end
