# ft_turing

## Install

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

```bash
cabal install aeson
```

## Compile & Run
```bash
make && ./ft_turing <config file> <input>
```

## Config

### Unary Addition

```bash
"" => ""
++++ => ""
111+11 => 11111
111+11+1 => 111111
+111+11 => 11111
111+11+ => 11111
+1+1+1++11+ => 11111
```

* Single tape time complexity => O(n^2)
* Multi tape time complexity => O(n)

### 0n1n

```bash
"" => y
0011 => y
011 => n
1100 => n
001 => n
010101 => n
```

* Single tape time complexity => O(n^2)
* Multi tape time complexity => O(n)

### 02n

```bash
"" => y
0 => n
00 => y
000 => n
0000 => y
```

* Single tape time complexity => O(n)
* Multi tape time complexity => O(n)

### palindrome

```bash
"" => y
a => y
aba => y
abaaba => y
baab => y
baba => n
```

* Single tape time complexity => O(n^2)
* Multi tape time complexity => O(n)