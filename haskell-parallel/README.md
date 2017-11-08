# haskell-parallel

Threre are parallel and seqentional implementations of Floyd Warshall algorithm.

To compile parallel version run: <br/>
`ghc -O2 fwsparse_threaded.hs -threaded`

To compile sequential version run: <br/>
`ghc -o fwsparse fwsparse.hs`

To run programm use: <br/>
`./fwsparse_threaded <number of vertices> <number of edges> +RTS -s -N<number of threads>`<br/>
`./fwsparse <number of vertices> <number of edges> +RTS -s`

`+RTS -s`: optional parameters to see benchmarking and details <br/>
`-N4`: number of threads

Example configurations: <br/>
`./fwsparse_threaded 700 300 +RTS -s -N4` <br/>
`./fwsparse 700 300 +RTS -s`

It's possible to check the correctness via manual comparing checksums of graphs or run `runtest` function in main.
