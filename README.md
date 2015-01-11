[![Build Status](https://travis-ci.org/tolysz/possible.svg?branch=master)](https://travis-ci.org/tolysz/possible)
[![Latest Version](https://img.shields.io/hackage/v/possible.svg)](https://hackage.haskell.org/package/possible)

possible
========

Three valued Data.Maybe


Maybe lacks the information if Nothing represents missing or empty value. 
The idea is to have http://en.wikipedia.org/wiki/Three-valued_logic for values.
Depends on Aeson having missing value.

--

-- Depends on modified aeson (i.e. one extra constructor with fields which an be intentionally "Missing"
ask aeson to implement it for us
