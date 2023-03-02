loss-per-line
===============

[![GitHub Actions](https://github.com/unhammer/loss-per-line/actions/workflows/ci.yml/badge.svg)](https://github.com/unhammer/loss-per-line/actions/workflows/ci.yml)

Simple cli tool for finding word/char error rates per sentence.

Usage
-----

Get the latest release from
https://github.com/unhammer/loss-per-line/releases , put in somewhere
in `$PATH` and you should be able to

```console
$ printf 'some test sentence\tsome other test sentence here\tignored but retained info after second tab\n' > testset

$ printf 'even more tests\tmore tests\tignored but retained\n' >> testset

$ cat testset
some test sentence	some other test sentence here	ignored but retained info after second tab
even more tests	more tests	ignored but retained

$ <testset loss-per-line WER
0.5	some test sentence	some other test sentence here	ignored but retained info after second tab
0.4	even more tests	more tests	ignored but retained

$ <testset loss-per-line CER
0.6206896551724138	some test sentence	some other test sentence here	ignored but retained info after second tab
0.6666666666666666	even more tests	more tests	ignored but retained
```

Current method is to find Levenshtein difference and normalise by
length of longest input.

Build
-----


```console
$ git clone https://github.com/unhammer/loss-per-line.git
$ cd loss-per-line
$ nix-build
$ result/bin/loss-per-line
```

or with [ghcup](https://www.haskell.org/ghcup/install/) instead of nix:

```console
$ git clone https://github.com/unhammer/loss-per-line.git
$ cd loss-per-line
$ cabal install
$ ~/.local/bin/loss-per-line
```

TODO
-----

- support more metrics? (Damerau-Levenshtein, overlap, Jaccard,
  Hamming, Jaro-Winkler,
  [BLEU](https://gitlab.com/filipg/geval/-/blob/master/src/GEval/BLEU.hs),
  GLEU)

License
-------

This project is released under the GPL v3.
For more details, see [LICENSE](./LICENSE) file.
