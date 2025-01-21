# F3 Programming Language

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Version](https://img.shields.io/badge/version-1.0.0-blue)
![Platform](https://img.shields.io/badge/platform-linux-lightgrey)
![Status](https://img.shields.io/badge/status-experimental-orange)

A simple, powerful, extensible logic programming language based on triples.



## Install

Requires `docker`

``` 
wget -qO- https://raw.githubusercontent.com/Ivan-Tigan/f3-lang/main/install.sh | sudo bash
```
## Get Example

this will download the ./example folder from the repository to your machine

```
wget -qO- https://api.github.com/repos/Ivan-Tigan/f3-lang/tarball | tar xz --wildcards --strip=1 "*/example"
```
```
cd example
```

## Try It

```
f3 -p 3000:3000 run todo.f3
```

