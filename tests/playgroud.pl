let m1 = {};         # empty Map

let m2 = {           # initilize Map with values
    1: "key", 
    2: "key2",
    3: "key3" 
};  

print(m2[1]);        # access item with key  

m1["new_key"] = 10;  # set insert new Key and Value
m2[3] = "changed";   # update value at Key

print(m1);
print(m2);
