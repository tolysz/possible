[![Build Status](https://travis-ci.org/tolysz/possible.svg?branch=master)](https://travis-ci.org/tolysz/possible)
[![Latest Version](https://img.shields.io/hackage/v/possible.svg)](https://hackage.haskell.org/package/possible)


possible
========

Three valued Data.Maybe


Maybe lacks the information if Nothing represents missing or empty value. 
The idea is to have http://en.wikipedia.org/wiki/Three-valued_logic for values.
Depends on Aeson having missing value.

--
It is mainly used in modified [aeson](https://github.com/tolysz/aeson) where it gives the extra information about value being missing present or set to null. Modified `aeson` can derivie `{To,From}JSON` instances for it.

The plan is to be able to support `PATCH` from Google API
eg. https://github.com/tolysz/google-api/blob/master/src/Network/Google/Api/Youtube/Videos.hs
