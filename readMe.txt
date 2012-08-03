There are two Symbol tables , One for Constants like Numeric and String Literals
and the other for Identifiers


Running the program as:

bison -d parser.y
flex lexer.l	
g++ -std=c++0x -g lex.yy.c parser.tab.c -ll -o executable

./executable Input_File.txt



Output Files Generated:

Rules.out -- Head of each recognised production rule
symtable.out -- Symbol Table 
