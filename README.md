# mini-prettier

`mini-prettier` is a minimal and educational Rust crate designed to format JavaScript strings. This crate provides a simple and efficient way to prettify JavaScript code.

## Features

- Format JavaScript strings

## Example

Here is a simple example of how `mini-prettier` will format a JavaScript string.

input:

```javascript
let p = (2 + 2) * 4;
var a = 1;
const b = "hello世界";
let c = b;
a  =  2;
a =   "12" ;
a;
"12";
let d = a + 1 -3 * 5 ** 10 / 4% 10;
let e = 1 * (2 + 3) * 4;
let f = 1 * ((2 + (3+10)) * 4);
"let e = 2 >=1 + 1>=3";
```

output:

```javascript
let p = (2 + 2) * 4;
var a = 1;
const b = 'hello世界';
let c = b;
a = 2;
a = '12';
a;
('12');
let d = a + 1 - (((3 * 5 ** 10) / 4) % 10);
let e = 1 * (2 + 3) * 4;
let f = 1 * ((2 + (3 + 10)) * 4);
('let e = 2 >=1 + 1>=3');
```
