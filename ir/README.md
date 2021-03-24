# Frame Analysis and IR Documentation
Anshu Dwibhashi, Yuanrui Zhang, Clayton Shafer  
ECE 553 Spring 2021

## Usage
`main.sml` is the driver for running escape analysis and semantic analysis on a particular file. After building, run `Main.run "test.tig"`. This will print all fragments and their corresonding labels. To make the output more readable, run `Main.lin "test.tig"`, which will linearize the fragments using Appel's Chapter 8 file, removing all SEQ and ESEQ constructions, and print the result.
## Extra Credit - Integer Arithmetic

## Extra Credit - If Optimizations
